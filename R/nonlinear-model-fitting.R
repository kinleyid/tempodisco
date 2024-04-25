
# Utility functions
p2o <- function(p) (1 / (1/p - 1)) # proportion/probability to odds
o2p <- function(o) (1 / (1/o + 1)) # odds to proportion/probability
logit <- function(x) log(1 / (1/x - 1))
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
eps <- .Machine$double.eps # For later use in Laplace smoothing
laplace_smooth <- function(p) p <- eps + (1 - 2*eps)*p

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

none_func <- function(D, p) {
  p <- logistic(p[grep('\\d', names(p))])
  a <- approx(x = as.numeric(names(p)), y = p, xout = D)
  return(a$y)
}

get_discount_function <- function(func_name) {
  if (func_name == 'none') {
    func <- none_func
  } else {
    func <- all_discount_functions[[func_name]]
  }
  return(func)
}

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
  ),
  'none' = NA
)

get_prob_mod_frame <- function(discount_function, fixed.ends, choice.rule, absval) {
  # Get probabilistic model without parameter values specified, but with
  # "structural" aspects (such as the discount function) specified
  
  # What is the discount function?
  discount_func <- get_discount_function(discount_function)
  
  # What is the choice rule?
  if (choice.rule == 'exponential') {
    # Are we using the logit function?
    if (fixed.ends) {
      rel_diff <- function(data, par) logit(data$val_imm/data$val_del) - logit(discount_func(data$del, par))
    } else {
      rel_diff <- function(data, par) data$val_imm/data$val_del - discount_func(data$del, par)
    }
    # Are we accounting for the absolute values of the rewards in any way?
    absval <- switch(absval,
                     'none' = function(data, par) 1,
                     'linear' = function(data, par) data$val_del,
                     'nonlinear' = function(data, par) varsigma(data$val_del, alpha = exp(par['alpha']), lambda = par['lambda'])
    )
    frame <- function(data, par) {
      return( logistic( exp(par['gamma']) * absval(data, par) * rel_diff(data, par) ) )
    }
  } else if (choice.rule == 'power') {
    if (fixed.ends) {
      R <- function(data) p2o(data$val_imm/data$val_del)
      f <- function(data, par) p2o(discount_func(data$del, par))
    } else {
      R <- function(data) data$val_imm/data$val_del
      f <- function(data, par) discount_func(data$del, par)
    }
    frame <- function(data, par) {
      return( (1 + (f(data, par)/R(data))**exp(par['gamma']))**-1 )
    }
  }
  
  return(frame)
}

get_nll_fn <- function(data, prob_mod_frame) {
  # Get negative log-likelihood function, given a set of data and a model
  # "frame" with structural aspects specified but parameters unspecified
  
  nll_fn <- function(par) {
    p <- laplace_smooth(prob_mod_frame(data, par))
    return(-ll(p, data$imm_chosen))
  }
  return(nll_fn)
}

get_rss_fn <- function(data, discount_function) {
  # Get residual sum of squares function
  
  discount_func <- all_discount_functions[[discount_function]]
  rss_fn <- function(par) {
    pred <- discount_func(data$del, par)
    return(sum((data$indiff - pred)**2))
  }
  return(rss_fn)
}

run_optimization <- function(fn, param_ranges, silent) {
  # Get the best-fitting optim() object
  # Convert to a table of all possible combinations
  param_vals <- as.matrix(do.call(expand.grid, param_ranges))
  # Try each combination
  best_value <- Inf
  best_optimized <- list()
  for (val_idx in 1:nrow(param_vals)) {
    try( # Optimization may fail
      {
        optimized <- optim(
          fn = fn,
          par = param_vals[val_idx, ],
          control = list('warn.1d.NelderMead' = F,
                         maxit = 1000)
        )
        if (optimized$value < best_value) {
          best_value <- optimized$value
          best_optimized <- optimized
        }
      },
      silent = silent
    )
  }
  return(best_optimized)
}

untransform <- function(par) {
  # Get untransformed parameters
  u_p <- par
  idx <- grepl('k|s|gamma', names(u_p))
  u_p[idx] <- exp(u_p[idx])
  idx <- grepl('\\d', names(u_p)) | names(u_p) == 'w'
  u_p[idx] <- logistic(u_p[idx])
  return(u_p)
}

get_ED50 <- function(mod) {
  u_p <- mod$untransformed_parameters
  ED50 <- switch(
    mod$discount_function,
    "hyperbolic"= 1/u_p['k'],
    "exponential"= log(2)/u_p['k'],
    "inverse-q-exponential" = (2^(1/u_p['s']) - 1) / u_p['k'],
    "nonlinear-time-hyperbolic" = (1/u_p['k']) ^ (1/u_p['s']),
    "nonlinear-time-exponential" = (log(2)/u_p['k'])^(1/u_p['s']),
    "scaled-exponential" = log(2*u_p['w'])/u_p['k'],
    "dual-systems-exponential" = NA,
    "none" = NA
  )
  if (mod$discount_function == 'dual-systems-exponential') {
    # No analytic solution, therefore optimize
    optim_func <- function(cand) {
      (predict_indiffs(mod, exp(cand)) - 0.5)**2
    }
    optimized <- optim(fn = optim_func, par = 0, method = 'BFGS')
    ED50 <- exp(optimized$par)
  }
  names(ED50) <- NULL
  
  return(ED50)
}

#' Probabilistic delay discounting model
#'
#' Compute a probabilistic model for a single subject's delay discounting
#' @param data A data frame with columns `val_imm` and `val_del` for the values of the immediate and delayed rewards, `del` for the delay, and `imm_chosen` (Boolean) for whether the immediate reward was chosen
#' @param discount_function A vector of strings specifying the name of the discount functions to use. Options are `'hyperbolic'`, `'exponential'`, `'inverse-q-exponential'`, `'nonlinear-time-hyperbolic'`, `'scaled-exponential'`, `'dual-systems-exponential'`, `'nonlinear-time-exponential'`, and `'none'`. Default is `'all'`, meaning every discount function is tested and the one with the best AIC is selected. When `'none'` is used, each indifference point is estimated as a separate parameter, and these indifference points can be accessed through the `untransformed_parameters` component of the output.
#' @param choice.rule A string specifying whether the `'exponential'` (default) or `'power'` choice rule should be used.
#' @param absval A string specifying how the absolute value of the delayed reward should be accounted for when the choice rule is `'exponential'`. Ignored when the choice rule is `'power'`. Defaults to `'none'`. Other options are `'linear'` (linear scaling) and `'nonlinear'` (flexible nonlinear scaling)
#' @param fixed.ends A Boolean specifying whether the model should satisfy the desiderata that subjects should always prefer something over nothing (i.e., nonzero delayed reward over nothing) and the same reward sooner rather than later
#' @param param_ranges A list containing the starting values to try for each parameter. Defaults to `c(-5, 0, 5)` for most parameters
#' @param silent A Boolean specifying whether the call to `optim` (which occurs in a `try` block) should be silent on error
#' @return A list from `optim` with additional components specifying the AIC, the ED50, the discount function, and the probabilistic model
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
#' print(mod$discount_function)
#' @export
dd_prob_model <- function(data, discount_function = 'all', absval = 'none', choice.rule = 'exponential', fixed.ends = T, param_ranges = NULL, silent = T) {
  
  # Set parameter ranges
  tmp <- default_param_ranges
  if (!is.null(param_ranges)) {
    # Overwrite defaults
    for (group in names(default_param_ranges)) {
      for (param_name in names(param_ranges)) {
        if (param_name %in% names(tmp[[group]])) {
          tmp[[group]][[param_name]] <- param_ranges[[param_name]]
        }
      }
    }
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
  # Required data columns
  req_cols <- c('val_imm', 'val_del', 'del', 'imm_chosen')
  cols <- names(data)
  missing_cols <- c()
  for (req_col in req_cols) {
    if (!(req_col %in% cols)) {
      missing_cols <- c(missing_cols, req_col)
    }
  }
  if (length(missing_cols) > 0) {
    stop(sprintf('Missing data column(s): %s', paste(missing_cols, collapse = ', ')))
  }
  # Ensure imm_chosen is logical
  data$imm_chosen <- as.logical(data$imm_chosen)
  # Endpoints
  if (fixed.ends) {
    R0 <- subset(data, val_imm == 0)
    if (any(R0$imm_chosen)) {
      stop('Participant chose an immediate reward with a value of 0. When desid=True, this makes the negative log likelihood function impossible to optimize.')
    }
    R1 <- subset(data, val_imm == val_del)
    if (any(!R1$imm_chosen)) {
      stop('Participant chose a delayed reward of equal value to an immediate reward. When desid=True, this makes the negative log likelihood function impossible to optimize.')
    }
  }
  # While we're at it
  if (any(data$val_imm > data$val_del)) {
    stop('The data contains cases where val_imm exceeds val_del')
  }
  # Valid discount function
  for (d_f in discount_function) {
    if (!(d_f %in% c(names(all_discount_functions), 'none'))) {
      valid_opts <- paste(sprintf('\n- "%s"', names(all_discount_functions)), collapse = '')
      stop(sprintf('"%s" is not a recognized discount function. Valid options are: %s', d_f, valid_opts))
    }
  }
  if (!(choice.rule %in% c('exponential', 'power'))) {
    stop(sprintf('choice.rule must be either "exponential" or "power" (currently %s)', choice.rule))
  }
  
  # Get a table of combinations of model settings
  prob_mod_arg_permutations <- do.call(
    expand.grid,
    c(
      c(as.list(environment()))[c('discount_function',
                                  'absval',
                                  'fixed.ends',
                                  'choice.rule')],
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
    if (prob_mod_args$discount_function == 'none') {
      unique_delays <- unique(data$del)
      curr_param_ranges <- as.list(rep(0.5, length(unique_delays)))
      names(curr_param_ranges) <- sprintf('%s', unique_delays)
    } else {
      curr_param_ranges <- param_ranges[[prob_mod_args$discount_function]]
    }
    curr_param_ranges <- c(curr_param_ranges, param_ranges$gamma)
    if (prob_mod_args$absval == 'nonlinear') {
      curr_param_ranges <- c(curr_param_ranges, param_ranges$varsigma)
    }
    
    # Run optimization
    optimized <- run_optimization(nll_fn, curr_param_ranges, silent)
    if (length(optimized) == 0) {
      stop('Optimization failed; optim() returned an error for every choice of initial parameter values')
    }
    curr_aic <- 2*length(optimized$par) + 2*optimized$value
    if (curr_aic < best_aic) {
      best_aic <- curr_aic
      if (prob_mod_args$discount_function == 'dual-systems-exponential') {
        # Ensure k1 < k2
        if (optimized$par['k1'] > optimized$par['k2']) {
          tmp <- optimized$par['k1']
          optimized$par['k1'] <- optimized$par['k2']
          optimized$par['k2'] <- tmp
          optimized$par['w'] <- -optimized$par['w']
        }
      }
      optimized$AIC <- curr_aic
      optimized$discount_function <- prob_mod_args$discount_function
      best_output <- optimized
    }
  }
  best_output$data <- data
  
  # Get untransformed parameters
  best_output$untransformed_parameters <- untransform(best_output$par)
  
  # Compute ED50
  best_output$ED50 <- get_ED50(best_output)
  
  # Add other metadata
  best_output$choice.rule <- choice.rule
  best_output$fixed.ends <- fixed.ends
  best_output$absval <- absval
  
  return(best_output)
}

#' Deterministic delay discounting model
#'
#' Compute a deterministic model for a single subject's delay discounting
#' @param data A data frame with columns `indiff` for the pre-computed indifference points and `del` for the delay
#' @param discount_function A vector of strings specifying the name of the discount functions to use. Options are `'hyperbolic'`, `'exponential'`, `'inverse-q-exponential'`, `'nonlinear-time-hyperbolic'`, `'scaled-exponential'`, `'dual-systems-exponential'`, and `'nonlinear-time-exponential'`. Default is `'all'`, meaning every discount function is tested and the one with the best AIC is selected.
#' @param param_ranges A list containing the starting values to try for each parameter. Defaults to `c(-5, 0, 5)` for most parameters
#' @param silent A Boolean specifying whether the call to `optim` (which occurs in a `try` block) should be silent on error
#' @return A list from `optim` with additional components specifying the AIC, the ED50, the discount function, and the probabilistic model
#' @note The `par` component of the output list is for internal use. For statistical analyses, use the `untransformed_parameters`. `par` contains the parameters after various transformations intended to keep them within certain bounds (e.g., k parameters should never be negative)
#' @examples 
#' # Generate data
#' df <- data.frame(del = exp(1:10), indiff = 1 / (1 + 0.001*exp(1:10)))
#' # Fit model
#' mod <- dd_det_model(df)
#' print(mod$discount_function)
#' @export
dd_det_model <- function(data, discount_function = 'all', param_ranges = NULL, silent = T) {
  
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
  # Required data columns
  req_cols <- c('indiff', 'del')
  cols <- names(data)
  missing_cols <- c()
  for (req_col in req_cols) {
    if (!(req_col %in% cols)) {
      missing_cols <- c(missing_cols, req_col)
    }
  }
  if (length(missing_cols) > 0) {
    stop(sprintf('Missing data column(s): %s', paste(missing_cols, collapse = ', ')))
  }
  # Valid discount function
  for (d_f in discount_function) {
    if (!(d_f %in% names(all_discount_functions))) {
      valid_opts <- paste(sprintf('\n- "%s"', names(all_discount_functions)), collapse = '')
      stop(sprintf('"%s" is not a recognized discount function. Valid options are: %s', d_f, valid_opts))
    }
  }
  
  # Terms for computing AIC
  N <- nrow(data)
  
  # Run optimization on each candidate discount function
  args <- data.frame(discount_function = discount_function)
  best_crit <- Inf
  best_output <- list()
  for (arg_idx in 1:nrow(args)) {
    discount_function <- args$discount_function[arg_idx]
    # Get residual sum of squares function
    rss_fn <- get_rss_fn(data, discount_function)
    # Get parameter ranges
    curr_param_ranges <- param_ranges[[discount_function]]
    # Run optimization
    optimized <- run_optimization(rss_fn, curr_param_ranges, silent)
    # Compute AIC
    crit <- N*log(optimized$value/N) + 2*(length(optimized$par) + 1)
    if (crit < best_crit) {
      best_crit <- crit
      if (discount_function == 'dual-systems-exponential') {
        # Ensure k1 < k2
        if (optimized$par['k1'] > optimized$par['k2']) {
          tmp <- optimized$par['k1']
          optimized$par['k1'] <- optimized$par['k2']
          optimized$par['k2'] <- tmp
          optimized$par['w'] <- -optimized$par['w']
        }
      }
      optimized$discount_function <- discount_function
      best_output <- optimized
    }
  }
  best_output$data <- data
  
  # Get untransformed parameters
  best_output$untransformed_parameters <- untransform(best_output$par)
  
  # Compute ED50
  best_output$ED50 <- get_ED50(best_output)
  
  return(best_output)
}

#' Predict indifference points
#'
#' Predict the indifference point at a given delay
#' @param mod A delay discounting model. See `dd_prob_model` and `dd_det_model`
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
  indiffs <- get_discount_function(mod$discount_function)(del, mod$par)
  names(indiffs) <- NULL
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
predict_prob_imm <- function(mod, data = NULL, laplace = T) {
  if (is.null(data)) {
    data <- mod$data
  }
  frame_args <- c('discount_function',
                  'fixed.ends',
                  'choice.rule',
                  'absval')
  p <- do.call(get_prob_mod_frame, mod[frame_args])(data, mod$par)
  if (laplace) {
    p <- laplace_smooth(p)
  }
  names(p) <- NULL
  return(p)
}

invert_decision_function <- function(mod, prob, del) {
  # Given some model and some delay, get the relative value of the immediate 
  # reward at which its probability of being chosen is some desired value
  if (mod$choice.rule == 'exponential') {
    if (mod$fixed.ends) {
      R <- logistic(
        logit(prob)/exp(mod$par['gamma']) +
          logit(predict_indiffs(mod, del)))
    } else {
      R <- logit(prob)/exp(mod$par['gamma']) + predict_indiffs(mod, del)
    }
  } else if (mod$choice.rule == 'power') {
    if (mod$fixed.ends) {
      R <- o2p( p2o(predict_indiffs(mod, del)) * (1/prob - 1) ** (-1/exp(mod$par['gamma'])) )
    } else {
      R <- predict_indiffs(mod, del) * (1/prob - 1) ** (-1/exp(mod$par['gamma']))
    }
  }
  return(R)
}

#' Plot models
#'
#' Plot delay discounting models
#' @param mod A delay discounting model. See `dd_prob_model` and `dd_det_model`
#' @param p_range In addition to the indifference points (where the probability of choosing the immediate reward is 0.5), other "preference points" can also be plotted (e.g., points where the probability of choosing the immediate reward is 0.8). This allows visualization of how stochastic decision making is. Defaults to `c(0.4, 0.6)`
#' @examples
#' # Generate data
#' df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
#' logistic <- function(x) 1 / (1 + exp(-x))
#' logit <- function(x) log(x / (1 - x))
#' prob <- logistic(logit(df$val_imm / df$val_del) - logit(1 / (1 + 0.001*df$del)))
#' df$imm_chosen <- runif(nrow(df)) < prob
#' # Fit model
#' mod <- dd_prob_model(df)
#' plot_dd(mod)
#' @export
plot_dd <- function(mod, p_range = c(0.4, 0.6)) {
  max_del <- max(mod$data$del)
  plotting_delays <- seq(0, max_del, length.out = 1000)
  pred_indiffs <- predict_indiffs(mod, del = plotting_delays)
  if ('choice.rule' %in% names(mod)) {
    # Plot probabilistic model
    if (mod$absval == 'none') {
      lower <- invert_decision_function(mod, prob = p_range[1], del = plotting_delays)
      upper <- invert_decision_function(mod, prob = p_range[2], del = plotting_delays)
    }
    plot(NA, NA,
         xlim = c(0, max_del), ylim = c(0, 1),
         xlab = 'Delay',
         ylab = 'Rel. val. imm. rew.')
    lines(pred_indiffs ~ plotting_delays)
    if (mod$absval == 'none') {
      lines(lower ~ plotting_delays, lty = 'dashed')
      lines(upper ~ plotting_delays, lty = 'dashed')
    }
    mod$data$rel_val <- mod$data$val_imm / mod$data$val_del
    points(rel_val ~ del, col = 'red',
           data = subset(mod$data, imm_chosen))
    points(rel_val ~ del, col = 'blue',
           data = subset(mod$data, !imm_chosen))
  } else {
    # Plot deterministic model
    plot(NA, NA, xlim = c(0, max_del), ylim = c(0, 1),
         xlab = 'Delay',
         ylab = 'Indifference point')
    points(indiff ~ del, data = mod$data)
    lines(pred_indiffs ~ plotting_delays)
  }
  title(mod$discount_function)
}
