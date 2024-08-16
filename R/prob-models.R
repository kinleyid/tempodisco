
get_score_func_frame <- function(...) {
  # Get the function for computing scores without parameter values
  # specified, but with "structural" aspects (such as the discount
  # function) specified
  
  args <- list(...)
  
  # What is the discount function?
  discount_function <- args$discount_function$fn
  
  # Get the transform applied to v_i/v_d and f(t)
  transform <- get_transform(args)
  delta <- function(data, par) transform(data$val_imm/data$val_del) - transform(discount_function(data$del, par))

  # Get the factor by which gamma is scaled
  gamma_scale <- switch(args$gamma_scale,
                        'none' = function(data, par) 1,
                        'linear' = function(data, par) data$val_del)
  
  # Get the final frame
  frame <- function(data, par) {
    par['gamma'] * gamma_scale(data, par) * delta(data, par)}
  
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

#' Probabilistic temporal discounting model
#'
#' Compute a probabilistic model for a single subject's temporal discounting
#' @param data A data frame with columns `val_imm` and `val_del` for the values of the immediate and delayed rewards, `del` for the delay, and `imm_chosen` (Boolean) for whether the immediate reward was chosen
#' @param discount_function A vector of strings specifying the name of the discount functions to use.
#' @param choice_rule A string specifying whether the `'exponential'` (default) or `'power'` choice rule should be used.
#' @param fixed_ends A Boolean specifying whether the model should satisfy the desiderata that subjects should always prefer something over nothing (i.e., nonzero delayed reward over nothing) and the same reward sooner rather than later
#' @param param_ranges A list containing the starting values to try for each parameter. Defaults to `c(-5, 0, 5)` for most parameters
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
dd_prob_model <- tdbcm <- function(
    data,
    discount_function = c('all',
                          'hyperbolic',
                          'exponential',
                          'inverse-q-exponential',
                          'nonlinear-time-hyperbolic',
                          'scaled-exponential',
                          'dual-systems-exponential',
                          'nonlinear-time-exponential',
                          'model-free',
                          'noise'),
    choice_rule = c('logistic', 'probit', 'power'),
    fixed_ends = F,
    fit_err_rate = F,
    robust = F,
    param_ranges = NULL,
    silent = T,
    ...) {
  
  # From a user's POV, it's easier to specify `choice_rule` and `fixed_ends`
  # Internally, it makes more sense to use `noise_dist`, `gamma_scale`, and `transform`
  # The user can control the latter through the `...` argument
  config <- list(...)
  if (length(config) == 0) {
    choice_rule = match.arg(choice_rule)
    config <- list()
    # From `choice_rule` and `fixed_ends`, get `noise_dist`, `gamma_scale`, and `transform`
    if (choice_rule == 'logistic') {
      config$noise_dist <- 'logis'
      config$gamma_scale <- 'linear'
      if (fixed_ends) {
        config$transform <- 'noise_dist_quantile'
      } else {
        config$transform <- 'identity'
      }
    } else if (choice_rule == 'probit') {
      config$noise_dist <- 'norm'
      config$gamma_scale <- 'linear'
      if (fixed_ends) {
        config$transform <- 'noise_dist_quantile'
      } else {
        config$transform <- 'identity'
      }
    } else if (choice_rule == 'power') {
      config$noise_dist = 'logis'
      config$gamma_scale = 'none'
      if (fixed_ends) {
        config$transform = 'noise_dist_quantile'
      } else {
        config$transform = 'log'
      }
    } else {
      stop(sprintf('choice_rule must be one of "logistic", "probit", or "power" (currently "%s")', choice_rule))
    }
  } else {
    req_args <- c('noise_dist', 'gamma_scale', 'transform')
    if (!setequal(req_args, names(config))) {
      stop(sprintf('The following must all be specified:\n%s', paste('- ', req_args, collapse = '\n')))
    }
  }
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
  
  # Set discount function(s)
  if ((discount_function %||% 'all') == 'all') {
    discount_function <- eval(formals(dd_prob_model)$discount_function)
    discount_function[discount_function != 'all']
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
  
  # Attention checks
  endpoint_warning_boilerplate <- 'For fixed-endpoint models, this makes the negative log likelihood function impossible to optimize (it also suggests inattention from the participant).'
  R0 <- subset(data, val_imm == 0)
  if (any(R0$imm_chosen)) {
    warning(sprintf('Participant chose an immediate reward with a value of 0. %s', endpoint_warning_boilerplate))
  }
  R1 <- subset(data, val_imm == val_del)
  if (any(!R1$imm_chosen)) {
    warning(sprintf('Participant chose a delayed reward of equal value to an immediate reward. %s', endpoint_warning_boilerplate))
  }
  # While we're at it, more validation
  if (any(data$val_imm > data$val_del)) {
    stop('The data contains cases where val_imm exceeds val_del')
  }
  # Valid discount function
  for (d_f in discount_function) {
    if (!(d_f %in% c(names(all_discount_functions)))) {
      valid_opts <- paste(sprintf('\n- "%s"', names(all_discount_functions)), collapse = '')
      stop(sprintf('"%s" is not a recognized discount function. Valid options are: %s', d_f, valid_opts))
    }
  }
  
  # Get arguments as a list
  args <- c(
    config,
    list(
      fit_err_rate = fit_err_rate
    )
  )
  
  # Get a list of discount functions to test
  if (is.list(discount_function)) {
    cand_fns <- list(discount_function)
  } else {
    cand_fns <- list()
    for (fn_name in discount_function) {
      cand_fns <- c(cand_fns, list(tdfn(fn_name)))
    }
  }
  
  # Run optimization for each discount function
  best_crit <- Inf
  best_mod <- list()
  cand_mod <- list( # Initialize 
    data = data
  )
  class(cand_mod) <- 'td_gnlm'
  for (cand_fn in cand_fns) {
    config <- args
    config$discount_function <- cand_fn
    
    cand_mod$config <- config
    
    # Get prob. model with the given settings but parameter values unspecified
    prob_mod_frame <- do.call(get_prob_mod_frame, config)
    # Get function to compute negative log likelihood
    if (robust) {
      nll_fn <- get_rob_fn(data, prob_mod_frame)
    } else {
      nll_fn <- get_nll_fn(data, prob_mod_frame)
    }
    
    # Get parameter starting values
    if (is.function(config$discount_function$par_starts)) {
      par_starts <- config$discount_function$par_starts(data)
    } else {
      par_starts <- config$discount_function$par_starts
    }
    # Add gamma start values
    par_starts <- c(
      par_starts,
      list(
        gamma = exp(seq(-5, 5, length.out = 3))
      )
    )
    # Add epsilon start values, if fitting error rate
    if (config$fit_err_rate) {
      par_starts <- c(
        par_starts,
        list(
          eps = exp(c(-5, 0))
        )
      )
    }
    
    # Get function to transform parameters so they're bounded
    par_trf <- function(par) coef(cand_mod, bounded = T, par = par)
    
    # Get parameter starting values
    if (is.function(config$discount_function$par_lims)) {
      par_lims <- config$discount_function$par_lims(data)
    } else {
      par_lims <- config$discount_function$par_lims
    }
    # Add gamma limits
    par_lims <- c(
      par_lims,
      list(
        gamma = c(0, Inf)
      )
    )
    # Add epsilon limits
    if (config$fit_err_rate) {
      par_kims <- c(
        par_lims,
        list(
          eps = c(0, 0.5)
        )
      )
    }
    
    # Run optimization
    optimized <- run_optimization(nll_fn,
                                  par_starts,
                                  par_lims,
                                  silent = silent)
    if (length(optimized) == 0) {
      stop('Optimization failed; optim() returned an error for every choice of initial parameter values')
    }
    
    cand_mod$optim <- optimized
    
    if (robust) {
      curr_crit <- optimized$value
    } else {
      curr_crit <- logLik(cand_mod)
    }
    if (curr_crit < best_crit) {
      best_crit <- curr_crit
      if ('par_chk' %in% names(config$discount_function)) {
        optimized$par <- config$discount_function$par_chk(optimized$par)
      }
      best_mod <- cand_mod
    }
  }
  best_mod$data <- data
  
  return(best_mod)
}

default_par_trf <- function(par) {
  par['gamma'] <- exp(par['gamma'])
  par['eps'] <- 0.5*plogis(par['eps'])
  return(par)
}

#' @export
td_glm <- function(data,
                   discount_function = c('hyperbolic.1',
                                         'hyperbolic.2',
                                         'exponential.1',
                                         'exponential.2',
                                         'scaled-exponential.1',
                                         'nonlinear-time-hyperbolic.2',
                                         'nonlinear-time-exponential.2')) {
  discount_function <- match.arg(discount_function)
  data <- add_beta_terms(data, discount_function)
  if ('B3' %in% names(data)) {
    fml <- imm_chosen ~ B1 + B2 + B3 + 0
  } else {
    fml <- imm_chosen ~ B1 + B2 + 0
  }
  mod <- glm(formula = fml, data = data, family = binomial(link = 'logit'))
  mod$discount_function <- discount_function
  class(mod) <- c('td_glm', 'glm', 'lm')
  return(mod)
}

add_beta_terms <- function(data, discount_function) {
  if (discount_function == 'hyperbolic.1') {
    data$B1 <- 1 - data$val_del / data$val_imm
    data$B2 <- data$del
  } else if (discount_function == 'hyperbolic.2') {
    data$B1 <- qlogis(data$val_imm / data$val_del) + log(data$del)
    data$B2 <- 1
  } else if (discount_function == 'exponential.1') {
    data$B1 <- log(data$val_imm / data$val_del)
    data$B2 <- data$del
  } else if (discount_function == 'exponential.2') {
    data$B1 <- varphi(data$val_imm / data$val_del) + data$del
    data$B2 <- 1
  } else if (discount_function == 'scaled-exponential.1') {
    data$B1 <- log(data$val_imm / data$val_del)
    data$B2 <- data$del
    data$B3 <- 1
  } else if (discount_function == 'nonlinear-time-hyperbolic.2') {
    data$B1 <- log(data$val_imm / data$val_del)
    data$B2 <- log(data$del)
    data$B3 <- 1
  } else if (discount_function == 'nonlinear-time-exponential.2') {
    data$B1 <- varphi(data$val_imm / data$val_del)
    data$B2 <- log(data$del)
    data$B3 <- 1
  }
  return(data)
}