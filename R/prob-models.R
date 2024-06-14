
get_score_func_frame <- function(...) {
  # Get the function for computing scores without parameter values
  # specified, but with "structural" aspects (such as the discount
  # function) specified
  
  args <- list(...)
  
  # What is the discount function?
  discount_func <- get_discount_function(args$discount_function)
  
  # Get the transform applied to v_i/v_d and f(t)
  transform <- get_transform(args)
  delta <- function(data, par) transform(data$val_imm/data$val_del) - transform(discount_func(data$del, par))

  # Get the factor by which gamma is scaled
  gamma_scale <- switch(args$gamma_scale,
                        'none' = function(data, par) 1,
                        'linear' = function(data, par) data$val_del)
  
  # Get the final frame
  frame <- function(data, par) {
    exp(par['gamma']) * gamma_scale(data, par) * delta(data, par)}
  
  return(frame)
  
}

get_prob_func_frame <- function(...) {
  # Get probabilistic model without parameter values specified, but with
  # "structural" aspects (such as the CDF) specified
  
  args <- list(...)
  
  # What is the cdf?
  cdf <- get(sprintf('p%s', args$noise_dist))
  
  # Are we fitting error rate?
  if (args$fit_err_rate) {
    err_rate <- function(e, p) 0.5*logistic(e) * (1 - 2*p) + p
  } else {
    err_rate <- function(e, p) p
  }
  
  frame <- function(scores, par) err_rate(par['eps'], cdf( scores ))
  
  return(frame)
}

get_prob_mod_frame <- function(...) {
  # Get decision model without parameter values specified, but with
  # "structural" aspects (such as the discount function) specified
  score_func <- get_score_func_frame(...)
  prob_func <- get_prob_func_frame(...)
  frame <- function(data, par) {
    scores <- score_func(data, par)
    probs <- prob_func(scores, par)
    return(probs)
  }
}

get_nll_fn <- function(data, prob_mod_frame) {
  # Get negative log-likelihood function, given a set of data and a model
  # "frame" with structural aspects specified but parameters unspecified
  
  nll_fn <- function(par) {
    p <- laplace_smooth(prob_mod_frame(data, par))
    return(sum(-ll(p, data$imm_chosen)))
  }
  return(nll_fn)
}

huber <- function(t, c) {
  idx <- t > c
  t[idx] <- 2*sqrt(t[idx]*c) - c
  # t[idx] <- c*log(c*t[idx]) - c*(2*log(c) - 1) # Same idea but with logarithm
  return(t)
}

get_rob_fn <- function(data, prob_mod_frame) {
  # Get robust loss function, given a set of data and a model
  # "frame" with structural aspects specified but parameters unspecified
  
  rob_fn <- function(par) {
    p <- laplace_smooth(prob_mod_frame(data, par))
    nll <- -ll(p, data$imm_chosen)
    return(sum(huber(nll, 1)))
  }
  return(rob_fn)
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
dd_prob_model <- function(data,
                          discount_function = 'all',
                          fixed_ends = 'none',
                          gamma_scale = 'none', # or val_del
                          noise_dist = 'logis',
                          fit_err_rate = F,
                          robust = F,
                          param_ranges = NULL) {
  
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
  
  # if (fixed.ends != 'neither') {
  #   R0 <- subset(data, val_imm == 0)
  #   if (any(R0$imm_chosen)) {
  #     stop('Participant chose an immediate reward with a value of 0. For fixed-endpoint models, this makes the negative log likelihood function impossible to optimize.')
  #   }
  #   R1 <- subset(data, val_imm == val_del)
  #   if (any(!R1$imm_chosen)) {
  #     stop('Participant chose a delayed reward of equal value to an immediate reward. When desid=True, this makes the negative log likelihood function impossible to optimize.')
  #   }
  # }
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
  # if (!(choice.rule %in% c('exponential', 'power', 'probit'))) {
  #   stop(sprintf('choice.rule must be either "exponential", "power", or "probit" (currently %s)', choice.rule))
  # }
  
  # Get a table of permutations of model configurations
  args <- as.list(environment())
  args <- args[names(formals(dd_prob_model))]
  args[c('data', 'param_ranges')] <- NULL
  
  # Run optimization for each discount function
  best_crit <- Inf
  best_output <- list()
  cand_output <- list(
    data = data
  )
  class(cand_output) <- 'td_gnlm'
  for (curr_discount_function in args$discount_function) {
    config <- args
    config$discount_function <- curr_discount_function
    
    cand_output$config <- config
    
    # Get prob. model with the given settings but parameter values unspecified
    prob_mod_frame <- do.call(get_prob_mod_frame, config)
    if (robust) {
      nll_fn <- get_rob_fn(data, prob_mod_frame)
    } else {
      nll_fn <- get_nll_fn(data, prob_mod_frame)
    }
    # Get parameter ranges
    if (config$discount_function == 'none') {
      unique_delays <- unique(data$del)
      curr_param_ranges <- as.list(rep(0.5, length(unique_delays)))
      names(curr_param_ranges) <- sprintf('%s', unique_delays)
    } else {
      curr_param_ranges <- param_ranges[[config$discount_function]]
    }
    curr_param_ranges <- c(curr_param_ranges, param_ranges$gamma)
    if (config$fit_err_rate) {
      curr_param_ranges <- c(curr_param_ranges, param_ranges$err.rate)
    }
    
    # Run optimization
    optimized <- run_optimization(nll_fn, curr_param_ranges, silent = F)
    if (length(optimized) == 0) {
      stop('Optimization failed; optim() returned an error for every choice of initial parameter values')
    }
    
    cand_output$optim <- optimized
    
    if (robust) {
      curr_crit <- optimized$value
    } else {
      curr_crit <- BIC(cand_output)
    }
    if (curr_crit < best_crit) {
      best_crit <- curr_crit
      if (config$discount_function == 'dual-systems-exponential') {
        # Ensure k1 < k2
        if (optimized$par['k1'] > optimized$par['k2']) {
          tmp <- optimized$par['k1']
          optimized$par['k1'] <- optimized$par['k2']
          optimized$par['k2'] <- tmp
          optimized$par['w'] <- -optimized$par['w']
        }
      }
      best_output <- cand_output
    }
  }
  best_output$data <- data
  
  # Compute ED50
  best_output$ED50 <- get_ED50(best_output)
  
  return(best_output)
}
