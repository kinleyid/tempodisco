
get_score_func_frame <- function(discount_function, fixed.ends, absval, choice.rule) {
  # Get the function for computing scores without parameter values
  # specified, but with "structural" aspects (such as the discount
  # function) specified
  
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
    frame <- function(data, par) exp(par['gamma']) * absval(data, par) * rel_diff(data, par)
  } else if (choice.rule == 'power') {
    if (fixed.ends) {
      R <- function(data) p2o(data$val_imm/data$val_del)
      f <- function(data, par) p2o(discount_func(data$del, par))
    } else {
      R <- function(data) data$val_imm/data$val_del
      f <- function(data, par) discount_func(data$del, par)
    }
    frame <- function(data, par) -exp(par['gamma']) * log(f(data, par)/R(data))
  }
  
  return(frame)
  
}

get_prob_func_frame <- function(fit.err.rate, choice.rule) {
  # Get probabilistic model without parameter values specified, but with
  # "structural" aspects (such as the choice) specified
  
  # Are we fitting error rate?
  if (fit.err.rate) {
    err.rate <- function(e, p) 0.5*logistic(e) * (1 - 2*p) + p
  } else {
    err.rate <- function(e, p) p
  }
  
  # What is the choice rule?
  if (choice.rule == 'exponential') {
    frame <- function(scores, par) err.rate(par['eps'], logistic( scores ))
  } else if (choice.rule == 'power') {
    frame <- function(scores, par) err.rate(par['eps'], 1 - 1 / (1 + exp(scores)))
  }
  
  return(frame)
}

get_prob_mod_frame <- function(discount_function, fixed.ends, fit.err.rate, choice.rule, absval) {
  # Get decision model without parameter values specified, but with
  # "structural" aspects (such as the discount function) specified
  score_func <- get_score_func_frame(discount_function, fixed.ends, absval, choice.rule)
  prob_func <- get_prob_func_frame(fit.err.rate, choice.rule)
  frame <- function(data, par) {
    scores <- score_func(data, par)
    probs <- prob_func(scores, par)
    return(probs)
  }
}

# get_prob_mod_frame <- function(discount_function, fixed.ends, fit.err.rate, choice.rule, absval) {
#   # Get probabilistic model without parameter values specified, but with
#   # "structural" aspects (such as the discount function) specified
#   
#   # What is the discount function?
#   discount_func <- get_discount_function(discount_function)
#   
#   # Are we fitting error rate?
#   if (fit.err.rate) {
#     err.rate <- function(e, p) 0.5*logistic(e) * (1 - 2*p) + p
#   } else {
#     err.rate <- function(e, p) p
#   }
#   
#   # What is the choice rule?
#   if (choice.rule == 'exponential') {
#     # Are we using the logit function?
#     if (fixed.ends) {
#       rel_diff <- function(data, par) logit(data$val_imm/data$val_del) - logit(discount_func(data$del, par))
#     } else {
#       rel_diff <- function(data, par) data$val_imm/data$val_del - discount_func(data$del, par)
#     }
#     # Are we accounting for the absolute values of the rewards in any way?
#     absval <- switch(absval,
#                      'none' = function(data, par) 1,
#                      'linear' = function(data, par) data$val_del,
#                      'nonlinear' = function(data, par) varsigma(data$val_del, alpha = exp(par['alpha']), lambda = par['lambda'])
#     )
#     frame <- function(data, par) err.rate(par['eps'], logistic( exp(par['gamma']) * absval(data, par) * rel_diff(data, par) ) )
#   } else if (choice.rule == 'power') {
#     if (fixed.ends) {
#       R <- function(data) p2o(data$val_imm/data$val_del)
#       f <- function(data, par) p2o(discount_func(data$del, par))
#     } else {
#       R <- function(data) data$val_imm/data$val_del
#       f <- function(data, par) discount_func(data$del, par)
#     }
#     frame <- function(data, par) err.rate(par['eps'], (1 + (f(data, par)/R(data))**exp(par['gamma']))**-1 )
#   }
#   return(frame)
# }

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
  # Get negative log-likelihood function, given a set of data and a model
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
dd_prob_model <- function(data, discount_function = 'all', absval = 'none', choice.rule = 'exponential', fixed.ends = T, fit.err.rate = F, robust = F, param_ranges = NULL, silent = T) {
  
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
      c(as.list(environment()))[names(formals(get_prob_mod_frame))],
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
    if (robust) {
      nll_fn <- get_rob_fn(data, prob_mod_frame)
    } else {
      nll_fn <- get_nll_fn(data, prob_mod_frame)
    }
    # Get parameter ranges
    if (prob_mod_args$discount_function == 'none') {
      unique_delays <- unique(data$del)
      curr_param_ranges <- as.list(rep(0.5, length(unique_delays)))
      names(curr_param_ranges) <- sprintf('%s', unique_delays)
    } else {
      curr_param_ranges <- param_ranges[[prob_mod_args$discount_function]]
    }
    curr_param_ranges <- c(curr_param_ranges, param_ranges$gamma)
    if (prob_mod_args$fit.err.rate) {
      curr_param_ranges <- c(curr_param_ranges, param_ranges$err.rate)
    }
    if (prob_mod_args$absval == 'nonlinear') {
      curr_param_ranges <- c(curr_param_ranges, param_ranges$varsigma)
    }
    
    # Run optimization
    optimized <- run_optimization(nll_fn, curr_param_ranges, silent)
    if (length(optimized) == 0) {
      stop('Optimization failed; optim() returned an error for every choice of initial parameter values')
    }
    if (robust) {
      curr_aic <- optimized$value
      curr_bic <- optimized$value
    } else {
      curr_aic <- 2*length(optimized$par) + 2*optimized$value
      curr_bic <- log(nrow(data))*length(optimized$par) + 2*optimized$value
    }
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
      optimized$BIC <- curr_bic
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
  best_output$fit.err.rate <- fit.err.rate
  best_output$absval <- absval
  
  return(best_output)
}
