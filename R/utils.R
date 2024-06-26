# Utility functions
p2o <- function(p) (1 / (1/p - 1)) # proportion/probability to odds
o2p <- function(o) (1 / (1/o + 1)) # odds to proportion/probability
logit <- function(x) log(1 / (1/x - 1))
clog <- function(x) -log(1 - x)
varphi <- function(x) -log(-log(x))
logistic <- function(x) 1 / (1 + exp(-x))
ll <- function(p, x) { # log-likelihood
  x*log(p) + (1 - x)*log(1 - p)
  # log(c(p[x], (1-p)[!x]))
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

get_transform <- function(config, inverse = F) {
  if (inverse) {
    out <- switch(
      config$fixed_ends,
      'none' = identity,
      'neither' = identity,
      'left' = exp,
      'right' = function(x) 1 - exp(-x),
      'both' = get(sprintf('p%s', config$noise_dist))
    )
  } else {
    out <- switch(
      config$fixed_ends,
      'none' = identity,
      'neither' = identity,
      'left' = log,
      'right' = function(x) -log(1 - x),
      'both' = get(sprintf('q%s', config$noise_dist)) # quantile function of current CDF
    )
  }
  return(out)
}

invert_decision_function <- function(mod, prob, del) {
  # Given some model and some delay, get the relative value of the immediate 
  # reward at which its probability of being chosen is some desired value
  
  indiffs <- predict(mod, newdata = data.frame(del = del), type = 'indiff')
  
  qfunc <- get(sprintf('q%s', mod$config$noise_dist))
  
  transform <- get_transform(mod$config, inverse = F)
  inverse_transform <- get_transform(mod$config, inverse = T)
  
  R <- inverse_transform(qfunc(prob) / coef(mod)['gamma'] + transform(indiffs))
  
  return(R)
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
          control = list(
            'warn.1d.NelderMead' = F, # For finding ED50 with dual-systems exponential discount function
            maxit = 5000 # For when discount function is "none"; this can take a long time to converge
          )
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

#' @export
predict.td_gnlm <- function(mod, newdata = NULL, type = 'link') {
  if (is.null(newdata)) {
    newdata <- mod$data
  }
  
  if (type == 'link') {
    
    score_func <- do.call(get_score_func_frame, mod$config)
    scores <- score_func(newdata, coef(mod, bounded = F))
    return(scores)
    
  } else if (type == 'response') {
    
    prob_mod <- do.call(get_prob_mod_frame, mod$config)
    probs <- prob_mod(newdata, coef(mod, bounded = F))
    return(probs)
    
  } else if (type == 'indiff') {
    
    indiff_func <- get_discount_function(mod$config$discount_function)
    indiffs <- indiff_func(newdata$del, coef(mod, bounded = F))
    names(indiffs) <- NULL
    return(indiffs)
    
  }
}

#' @export
predict.td_glm <- function(mod, newdata = NULL, type = 'link') {
  if (is.null(newdata)) {
    newdata <- mod$glm$data
  }
  
  newdata <- newvars(newdata, discount_function = mod$config$discount_function)
  
  if (type == 'indiff') {
    
    indiff_func <- get_discount_function(mod$config$discount_function)
    indiffs <- indiff_func(newdata$del, coef(mod, bounded = F))
    names(indiffs) <- NULL
    return(indiffs)
    
  } else {
    return(predict.glm(mod$glm, newdata = newdata, type = type))
  }
}

#' @export
AIC.td_gnlm <- function(mod, k = 2) {
  return(-2*as.numeric(logLik(mod)) + k*length(coef(mod)))
}

#' @export
BIC.td_gnlm <- function(mod) {
  return(AIC(mod, k = log(nrow(mod$data))))
}

#' @export
logLik.td_gnlm <- function(mod) {
  p <- laplace_smooth(predict(mod, type = 'response'))
  x <- mod$data$imm_chosen
  val <- sum(ll(p, x))
  attr(val, "nobs") <- nrow(mod$data)
  attr(val, "df") <- length(coef(mod))
  class(val) <- "logLik"
  return(val)
}

#' @export
print.td_gnlm <- function(mod) {
  cat(sprintf('Probabilistic temporal discounting model with config:\n'))
  for (k in names(mod$config)) {
    cat(sprintf(' %s: %s\n', k, mod$config[k]))
  }
}

#' @export
coef.td_gnlm <- function(mod, bounded = T) {
  if (bounded) {
    # For viewing
    cf <- untransform(mod$optim$par)
  } else {
    # For using in internal functions
    cf <- mod$optim$par
  }
  return(cf)
}

#' @export
coef.td_glm <- function(mod, df_par = T) {
  if (df_par) {
    # In terms of discount function parameters
    p <- mod$glm$coefficients
    B <- unname(c(p['B1'], p['B2'], p['B3']))
    d <- mod$config$discount_function
    if (d == 'hyperbolic.1') {
      cf <- c('k' = B[2]/B[1])
    } else if (d == 'hyperbolic.2') {
      cf <- c('k' = exp(B[2]/B[1]))
    } else if (d == 'exponential.1') {
      cf <- c('k' = B[2]/B[1])
    } else if (d == 'exponential.2') {
      cf <- c('k' = exp(B[2]/B[1]))
    } else if (d == 'scaled-exponential.1') {
      cf <- c('k' = B[2]/B[1],
              'w' = exp(-B[3]/B[1]))
    } else if (d == 'nonlinear-time-hyperbolic.2') {
      cf <- c('k' = exp(B[3]/B[1]),
              's' = B[2]/B[1])
    } else if (d == 'nonlinear-time-exponential.2') {
      cf <- c('k' = exp(B[3]/B[1]),
              's' = B[2]/B[1])
    }
  } else {
    # For using in internal functions
    cf <- mod$coefficients
  }
  return(cf)
}