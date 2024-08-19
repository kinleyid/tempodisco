
# Utility functions
p2o <- function(p) (1 / (1/p - 1)) # proportion/probability to odds
o2p <- function(o) (1 / (1/o + 1)) # odds to proportion/probability
logit <- function(x) log(1 / (1/x - 1))
clog <- function(x) -log(1 - x)
varphi <- function(x) -log(-log(x))
logistic <- function(x) 1 / (1 + exp(-x))
ll <- function(p, x) { # log-likelihood
  x*log(p) + (1 - x)*log(1 - p)
}
ln_lambda <- function(x, lambda) { # box-cox transform
  if (lambda == 0) {
    return(log(x))
  } else {
    return((x**lambda - 1) / lambda)
  }
}
varsigma <- function(x, alpha, lambda) alpha*ln_lambda(x + 1, lambda) + 1
laplace_smooth <- function(p, eps = .Machine$double.eps) eps + (1 - 2*eps)*p

get_transform <- function(config, inverse = F) {
  if (inverse) {
    if (config$transform == 'noise_dist_quantile') {
      transform <- get(sprintf('p%s', config$noise_dist))
    } else {
      if (config$transform == 'log') {
        transform <- exp
      } else {
        cat(sprintf('Do not know how to invert transform %s\n', config$transform))
        transform <- NULL
      }
    }
    # out <- switch(
    #   config$transform,
    #   'none' = identity,
    #   'neither' = identity,
    #   'left' = exp,
    #   'right' = function(x) 1 - exp(-x),
    #   'both' = get(sprintf('p%s', config$noise_dist))
    # )
  } else {
    if (config$transform == 'noise_dist_quantile') {
      transform <- get(sprintf('q%s', config$noise_dist))
    } else {
      transform <- get(config$transform)
    }
    # out <- switch(
    #   config$fixed_ends,
    #   'none' = identity,
    #   'neither' = identity,
    #   'left' = log,
    #   'right' = function(x) -log(1 - x),
    #   'both' = get(sprintf('q%s', config$noise_dist)) # quantile function of current CDF
    # )
  }
  return(transform)
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

run_optimization <- function(fn, par_starts, par_lims, silent) {
  # Get the best-fitting optim() object
 
  # Get a table of possible combinations of parameter starting values
  par_start_combos <- as.matrix(do.call(expand.grid, par_starts))
  
  # Get the bounds on each parameter
  # This is a belt-and-suspenders way of ensuring the `par`, `lower`, and `upper` args to `optim` are all in the same, right order
  n_par <- length(par_starts)
  par_names <- colnames(par_start_combos)
  lower <- numeric(n_par)
  upper <- numeric(n_par)
  for (par_idx in 1:n_par) {
    lower[par_idx] <- par_lims[[par_names[par_idx]]][1]
    upper[par_idx] <- par_lims[[par_names[par_idx]]][2]
  }
  names(lower) <- par_names
  names(upper) <- par_names
  
  # Try each combination of parameter starting values
  best_value <- Inf
  best_optimized <- list()
  for (combo_idx in 1:nrow(par_start_combos)) {
    try( # Optimization may fail
      {
        optimized <- optim(
          fn = fn,
          par = par_start_combos[combo_idx, ],
          method = 'L-BFGS-B',
          lower = lower,
          upper = upper
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

#' Model Predictions
#'
#' Generate predictions from a temporal discounting binary choice model
#' @param mod A temporal discounting model. See `td_gnlm`.
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. As in predict.glm, `"link"` (default) and `"response"` give predictions on the scales of the linear predictors and response variable, respectively. `"indiff"` gives predicted indifference points. In this case, `newdata` needs only a `del` column.
#' @return A vector of predictions
#' @export
predict.td_gnlm <- function(mod, newdata = NULL, type = c('link', 'response', 'indiff')) {
  if (is.null(newdata)) {
    newdata <- mod$data
  }
  
  type <- match.arg(type)
  
  if (type == 'link') {
    
    score_func <- do.call(get_score_func_frame, mod$config)
    scores <- score_func(newdata, coef(mod, bounded = F))
    return(scores)
    
  } else if (type == 'response') {
    
    prob_mod <- do.call(get_prob_mod_frame, mod$config)
    probs <- prob_mod(newdata, coef(mod))
    return(probs)
    
  } else if (type == 'indiff') {
    
    indiff_func <- mod$config$discount_function$fn
    indiffs <- indiff_func(newdata, coef(mod))
    names(indiffs) <- NULL
    return(indiffs)
    
  }
}

#' Model Predictions
#'
#' Generate predictions from a temporal discounting indifference point model
#' @param mod A temporal discounting model. See `td_gnlm`.
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. As in predict.glm, `"link"` (default) and `"response"` give predictions on the scales of the linear predictors and response variable, respectively. `"indiff"` gives predicted indifference points. In this case, `newdata` needs only a `del` column.
#' @return A vector of predictions
#' @export
predict.tdipm <- function(mod, del = NULL, newdata = NULL, type = 'indiff') {
  if (is.null(del)) {
    if (is.null(newdata)) {
      newdata <- mod$data
    }
  } else {
    newdata <- data.frame(del = del)
  }
  
  indiff_func <- mod$config$discount_function$fn
  indiffs <- indiff_func(newdata, coef(mod))
  names(indiffs) <- NULL
  return(indiffs)
  
}

#' Residuals from temporal discounting model
#'
#' Get residuals from a temporal discounting model
#' @param mod A temporal discounting model. See `td_gnlm`.
#' @param type The type of residuals to be returned. See `residuals.glm`.
#' @return A vector of residuals
#' @export
residuals.td_gnlm <- function(mod,
                              type = c('deviance', 'pearson', 'response')) {
  type <- match.arg(type)
  y <- mod$data$imm_chosen
  yhat <- fitted(mod)
  e <- y - yhat
  r <- switch (type,
    'deviance' = sign(e)*sqrt(-2*(y*log(yhat) + (1 - y)*log(1 - yhat))),
    'pearson' = e / sqrt(yhat[1]*(1 - yhat[1])),
    'response' = e
  )
  return(r)
}

#' Residuals from temporal discounting model
#'
#' Get residuals from a temporal discounting indifference point model
#' @param mod A temporal discounting model. See `td_gnlm`.
#' @param type The type of residuals to be returned. See `residuals.glm`.
#' @return A vector of residuals
#' @export
residuals.tdipm <- function(mod, type = 'response') {
  y <- mod$data$indiff
  yhat <- fitted(mod)
  e <- y - yhat
  
  return(e)
}

#' @export
fitted.tdm <- function(mod) {predict(mod, type = 'response')}

#' @export
predict.td_glm <- function(mod, newdata = NULL, type = 'link') {
  if (is.null(newdata)) {
    newdata <- mod$data
  }
  newdata <- add_beta_terms(newdata, discount_function = mod$config$discount_function)
  
  if (type == 'indiff') {
    
    discount_function_name <- strsplit(mod$config$discount_function, '\\.')[[1]][1]
    indiff_func <- get_discount_function(discount_function_name)
    indiffs <- indiff_func(newdata$del, coef(mod, df_par = T))
    names(indiffs) <- NULL
    return(indiffs)
    
  } else {
    return(predict.glm(mod, newdata = newdata, type = type))
  }
}

#' @export
AIC.tdm <- function(mod, k = 2) {
  return(-2*as.numeric(logLik(mod)) + k*length(coef(mod)))
}

#' @export
BIC.tdm <- function(mod) {
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
logLik.tdipm <- function(mod) {
  
  # Copied from logLik.nls
  
  res <- residuals(mod)
  N <- length(res)
  w <- rep_len(1, N) # Always unweighted
  ## Note the trick for zero weights
  zw <- w == 0
  val <-  -N * (log(2 * pi) + 1 - log(N) - sum(log(w + zw)) + log(sum(w*res^2)))/2
  ## the formula here corresponds to estimating sigma^2.
  attr(val, "df") <- 1L + length(coef(mod))
  attr(val, "nobs") <- attr(val, "nall") <- sum(!zw)
  class(val) <- "logLik"
  return(val)
}

#' @export
print.td_gnlm <- function(mod) {
  cat(sprintf('\nTemporal discounting binary choice model\n\n'))
  cat(sprintf('Discount function: %s, with coefficients:\n\n', mod$config$discount_function$name))
  print(coef(mod))
  cat(sprintf('\nConfig:\n'))
  for (comp_name in c('noise_dist', 'gamma_scale', 'transform')) {
    cat(sprintf(' %s: %s\n', comp_name, mod$config[[comp_name]]))
  }
  cat(sprintf('\nED50: %s\n', ED50(mod)))
  cat(sprintf('AUC: %s\n', AUC(mod, verbose = F)))
  cat(sprintf('BIC: %s', BIC(mod)))
}

#' @export
print.tdipm <- function(mod) {
  cat(sprintf('\nTemporal discounting indifference point model\n\n'))
  cat(sprintf('Discount function: %s, with coefficients:\n\n', mod$config$discount_function$name))
  print(coef(mod))
  cat(sprintf('\nED50: %s\n', ED50(mod)))
  cat(sprintf('AUC: %s\n', AUC(mod, verbose = F)))
}

#' Extract coefficients
#' 
#' Get coefficients of delay discounting function
#' @param bounded Boolean specifying whether parameters should be bounded (e.g., k > 0) or should be unbounded. Boended and unbounded coefficients are related by some transform. Bounded coefficients can be plugged directly into discount functions; optimization is performed on unbounded coefficients.
#' @return A names vector of coefficients
#' @export
coef.tdm <- function(mod, bounded = T, par = NULL) {
  mod$optim$par
}

#' @export
coef.td_glm <- function(mod, df_par = T) {
  if (df_par) {
    # In terms of discount function parameters
    p <- mod$coefficients
    B <- unname(c(p['B1'], p['B2'], p['B3']))
    d <- mod$discount_function
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
    cf <- coef.glm(mod)
  }
  return(cf)
}
