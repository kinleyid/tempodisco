
# Utility functions
logit <- function(x) log(x / (1 - x))
logistic <- function(x) 1 / (1 + exp(-x))
ll <- function(p, x) { # log-likelihood
  sum(log(c(p[x], (1-p)[!x])))
}
ln_lambda <- function(x, lambda) { # box-cox transform
  if (lambda == 0) {
    return(log(x))
  } else {
    return((x**lambda - 1) / lambda)
  }
}
varsigma <- function(x, alpha, lambda) alpha*ln_lambda(x + 1, lambda) + 1

# Discount functions
all_discount_functions <- list(
  'hyperbolic' = function(D, p) 1 / (1 + exp(p['k'])*D),
  'exponential' = function(D, p) exp(-exp(p['k'])*D),
  'inverse-q-exponential' = function(D, p) 1 / (1 + exp(p['k'])*D)**exp(p['s']),
  'nonlinear-time-hyperbolic' = function(D, p) 1 / (1 + exp(p['k'])*D**exp(p['s'])),
  'scaled-exponential' = function(D, p) logistic(p['w'])*exp(-exp(p['k'])*D),
  'dual-systems-exponential' = function(D, p) logistic(p['w'])*exp(-exp(p['k1'])*D) + (1 - logistic(p['w']))*exp(-(exp(p['k2']) + exp(p['k1']))*D),
  'nonlinear-time-exponential' = function(D, p) exp(-exp(p['k'])*D**exp(p['s']))
)
# Plausible parameter ranges
default_param_ranges <- list(
  'hyperbolic' = list(
    k = seq(-5, 5, length.out = 3)
  ),
  'exponential' = list(
    k = seq(-5, 5, length.out = 3)
  ),
  'inverse-q-exponential' = list(
    k = seq(-5, 5, length.out = 3),
    s = seq(-5, 5, length.out = 3)
  ),
  'nonlinear-time-hyperbolic' = list(
    k = seq(-5, 5, length.out = 3),
    s = seq(-5, 5, length.out = 3)
  ),
  'scaled-exponential' = list(
    w = seq(-5, 5, length.out = 3),
    k = seq(-5, 5, length.out = 3)
  ),
  'dual-systems-exponential' = list(
    w = seq(-5, 5, length.out = 3),
    k1 = seq(-5, 5, length.out = 3),
    k2 = seq(-5, 5, length.out = 3)
  ),
  'nonlinear-time-exponential' = list(
    k = seq(-5, 5, length.out = 3),
    s = seq(-5, 5, length.out = 3)
  ),
  'gamma' = list(
    gamma = seq(-5, 5, length.out = 3)
  ),
  'varsigma' = list(
    alpha = seq(-5, 5, length.out = 3),
    lambda = seq(-1, 1, length.out = 3)
  )
)

get_prob_mod_frame <- function(discount_function, dplus, absval) {
  # Get probabilistic model without parameter values specified, but with
  # "structural" aspects (such as the discount function) specified
  
  # What is the discount function?
  discount_func <- all_discount_functions[[discount_function]]
  # Are we using the logit function?
  if (dplus) {
    rel_diff <- function(data, par) logit(data$val_imm/data$val_del) - logit(discount_func(data$del, par))
  } else {
    rel_diff <- function(data, par) data$val_imm/data$val_del - discount_func(data$del, par)
  }
  # Are we accounting for the absolute values of the rewards in any way?
  absval <- switch(absval,
                   'none' = function(data, par) 1,
                   'identity' = function(data, par) data$val_del,
                   'varsigma' = function(data, par) varsigma(data$val_del, alpha = exp(par['alpha']), lambda = par['lambda'])
  )
  frame <- function(data, par) {
    return( logistic( exp(par['gamma']) * absval(data, par) * rel_diff(data, par) ) )
  }
  return(frame)
}

get_nll_fn <- function(data, prob_mod_frame) {
  # Get negative log-likelihood function, given a set of data and a model
  # "frame" with structural aspects specified but parameters unspecified
  
  nll_fn <- function(par) {
    p <- prob_mod_frame(data, par)
    return(-ll(p, data$imm_chosen))
  }
  return(nll_fn)
}

run_optimization <- function(nll_fn, param_ranges, silent) {
  # Get the best-fitting optim() object
  
  # Convert to a table of all possible combinations
  param_vals <- as.matrix(do.call(expand.grid, param_ranges))
  # Try each combination
  best_nll <- Inf
  best_optimized <- list()
  for (val_idx in 1:nrow(param_vals)) {
    try( # Optimization may fail
      {
        optimized <- optim(
          fn = nll_fn,
          par = param_vals[val_idx, ]
        )
        if (optimized$value < best_nll) {
          best_nll <- optimized$value
          best_optimized <- optimized
        }
      },
      silent = silent
    )
  }
  return(best_optimized)
}

#' Probabilistic delay discounting model
#'
#' Compute a probabilistic model for a single subject's delay discounting
#' @param data A data frame with columns `val_imm` and `val_del` for the values of the immediate and delayed rewards, `del` for the delay, and `imm_chosen` (Boolean) for whether the immediate reward was chosen
#' @param discount_function A vector of strings specifying the name of the discount functions to use. Options are `'hyperbolic'`, `'exponential'`, `'inverse-q-exponential'`, `'nonlinear-time-hyperbolic'`, `'scaled-exponential'`, `'dual-systems-exponential'`, and `'nonlinear-time-exponential'`. Default is `'all'`, meaning every discount function is tested and the one with the best AIC is selected.
#' @param absval A string specifying how the absolute value of the delayed reward should be accounted for. Defaults to `'none'`. Other options are `'identity'` (linear scaling) and `'varsigma'` (flexible nonlinear scaling)
#' @param dplus A Boolean specifying whether the model should satisfy the desiderata that subjects should always prefer something over nothing (i.e., nonzero delayed reward over nothing) and the same reward sooner rather than later
#' @param param_ranges A list containing the starting values to try for each parameter. Defaults to `c(-5, 0, 5)` for most parameters
#' @param silent A Boolean specifying whether the call to `optim` (which occurs in a `try` block) should be silent on error
#' @return A list from `optim` with additional components specifying the AIC, the discount function, and the probabilistic model
#' @note The `par` component of the output list is for internal use. For statistical analyses, use the `untransformed_parameters`. `par` contains the parameters after various transformations intended to keep them within certain bounds (e.g., k parameters should never be negative)
#' @examples 
#' # Generate data
#' df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
#' logistic <- function(x) 1 / (1 + exp(-x))
#' logit <- function(x) log(x / (1 - x))
#' prob <- logistic(logit(df$val_imm / df$val_del) - logit(1 / (1 + 0.001*df$del)))
#' df$imm_chosen <- runif(nrow(df)) < prob
#' # Fit model
#' mod <- dd_prob_model(df)
#' print(mod$discount_function_name)
#' @export
dd_prob_model <- function(data, discount_function = 'all', absval = 'none', dplus = T, param_ranges = NULL, silent = T) {
  
  # Set parameter ranges
  tmp <- default_param_ranges
  if (!is.null(param_ranges)) {
    tmp[names(param_ranges)] <- param_ranges
  }
  param_ranges <- tmp
  # Set discount functions
  if (discount_function == 'all') {
    discount_function <- names(all_discount_functions)
  }
  
  # Check args
  if (missing(data)) {
    stop('data unspecified')
  }
  for (d_f in discount_function) {
    if (!(d_f %in% names(all_discount_functions))) {
      valid_opts <- paste(sprintf('\n- "%s"', names(all_discount_functions)), collapse = '')
      stop(sprintf('"%s" is not a recognized discount function. Valid options are: %s', d_f, valid_opts))
    }
  }
  
  # Get a table of combinations of model settings
  prob_mod_arg_permutations <- do.call(
    expand.grid,
    c(
      c(as.list(environment()))[c('discount_function', 'absval', 'dplus')],
      list(stringsAsFactors = F)
    )
  )
  
  # Run optimization on each combination of model settings
  best_aic <- Inf
  best_output <- list()
  for (perm_idx in 1:nrow(prob_mod_arg_permutations)) {
    prob_mod_args <- prob_mod_arg_permutations[perm_idx, ]
    # Get prob. model with the given settings but parameter values unspecified
    prob_mod_frame <- do.call(get_prob_mod_frame, prob_mod_args)
    nll_fn <- get_nll_fn(data, prob_mod_frame)
    # Get parameter ranges
    curr_param_ranges <- param_ranges[[prob_mod_args$discount_function]]
    curr_param_ranges <- c(curr_param_ranges, param_ranges$gamma)
    if (prob_mod_args$absval == 'varsigma') {
      curr_param_ranges <- c(curr_param_ranges, param_ranges$varsigma)
    }
    
    # Run optimization
    optimized <- run_optimization(nll_fn, curr_param_ranges, silent)
    curr_aic <- 2*length(optimized$par) + 2*optimized$value
    if (curr_aic < best_aic) {
      best_aic <- curr_aic
      if (prob_mod_args$discount_function == 'bos-mcclure') {
        # Ensure k1 < k2
        if (optimized$par['k1'] > optimized$par['k2']) {
          tmp <- optimized$par['k1']
          optimized$par['k1'] <- optimized$par['k2']
          optimized$par['k2'] <- tmp
          optimized$par['w'] <- -optimized$par['w']
        }
      }
      optimized$AIC <- curr_aic
      optimized$discount_function_name <- prob_mod_args$discount_function
      optimized$discount_function <- all_discount_functions[[prob_mod_args$discount_function]]
      optimized$prob_model <- prob_mod_frame
      best_output <- optimized
    }
  }
  best_output$data <- data
  # Give untransformed parameters
  u_p <- best_output$par
  idx <- grepl('k|s', names(u_p))
  u_p[idx] <- exp(u_p[idx])
  idx <- names(u_p) == 'w'
  u_p[idx] <- logistic(u_p[idx])
  best_output$untransformed_parameters <- u_p
  return(best_output)
}

#' Predict indifference points
#'
#' Predict the indifference point at a given delay
#' @param mod A probabilistic delay discounting model. See `dd_prob_model`
#' @param del A vector of delays. Defaults to the delays from the data used to fit the model
#' @return A vector of indifference points
#' @examples
#' # Generate data
#' df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
#' logistic <- function(x) 1 / (1 + exp(-x))
#' logit <- function(x) log(x / (1 - x))
#' prob <- logistic(logit(df$val_imm / df$val_del) - logit(1 / (1 + 0.001*df$del)))
#' df$imm_chosen <- runif(nrow(df)) < prob
#' # Fit model
#' mod <- dd_prob_model(df)
#' indiffs <- predict_indiffs(mod)
#' plot(indiffs ~ log(mod$data$del), type = 'l')
#' @export
predict_indiffs <- function(mod, del = NULL) {
  if (is.null(del)) {
    del <- mod$data$del
  }
  indiffs <- mod$discount_function(del, mod$par)
  return(indiffs)
}

#' Predict decision probabilities
#'
#' Predict the probability of selecting the immediate reward
#' @param mod A probabilistic delay discounting model. See `dd_prob_model`
#' @param data A data frame with columns `val_imm`, `val_del`, and `del`, specifying the immediate and delayed rewards, and the delay of the delayed reward. Defaults to the data used to fit the model
#' @return A vector of probabilities
#' @examples 
#' # Generate data
#' df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
#' logistic <- function(x) 1 / (1 + exp(-x))
#' logit <- function(x) log(x / (1 - x))
#' prob <- logistic(logit(df$val_imm / df$val_del) - logit(1 / (1 + 0.001*df$del)))
#' df$imm_chosen <- runif(nrow(df)) < prob
#' # Fit model
#' mod <- dd_prob_model(df)
#' prob_imm <- predict_prob_imm(mod)
#' boxplot(prob_imm ~ mod$data$imm_chosen)
#' @export
predict_prob_imm <- function(mod, data = NULL) {
  if (is.null(data)) {
    data <- mod$data
  }
  probs <- mod$prob_model(data, mod$par)
  return(probs)
}
