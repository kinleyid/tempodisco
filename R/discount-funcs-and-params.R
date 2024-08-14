
tdfn <- function(fn_name) {
  fn <- switch (fn_name,
                'noise' = function(D, p) rep(p['k'], length(D)),
                'hyperbolic' = function(D, p) 1 / (1 + p['k']*D),
                'exponential' = function(D, p) exp(-p['k']*D),
                'inverse-q-exponential' = function(D, p) 1 / (1 + p['k']*D)**p['s'],
                'nonlinear-time-hyperbolic' = function(D, p) 1 / (1 + p['k']*D**p['s']),
                'scaled-exponential' = function(D, p) p['w']*exp(-p['k']*D),
                'dual-systems-exponential' = function(D, p) p['w']*exp(-exp(p['k1'])*D) + (1 - p['w'])*exp(-(exp(p['k2']) + exp(p['k1']))*D),
                'nonlinear-time-exponential' = function(D, p) exp(-p['k']*D**p['s']),
                'model-free' = function(D, p) {
                  p <- p[grep('del_', names(p))]
                  # Round parameters and delays to 10 decimal points for comparison
                  dels <- round(as.numeric(gsub('del_', '', names(p))), 10)
                  a <- approx(x = dels, y = logistic(p), xout = round(D, 10))
                  return(a$y)
                }
  )
  par <- switch (fn_name,
                 'noise' = list(
                   k = seq(-5, 5, length.out = 3)
                 ),
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
                 'err.rate' = list(
                   eps = c(-5, 0)
                 ),
                 'varsigma' = list(
                   alpha = seq(-5, 5, length.out = 3),
                   lambda = seq(-1, 1, length.out = 3)
                 ),
                 'model-free' = function(data) {
                   unique_delays <- unique(data$del)
                   curr_param_ranges <- as.list(rep(0.5, length(unique_delays)))
                   # Round to 10 decimal points to be able to align delay values 
                   names(curr_param_ranges) <- sprintf('del_%.10f', unique_delays)
                 }
  )
  par_trf <- function(par) {
    # Function to transform the parameters so they're bounded
    b_p <- par

    # p > 0 -> exp(p)
    idx <- names(b_p) %in% c('k', 's', 'gamma', 'alpha')
    b_p[idx] <- exp(b_p[idx])
    
    # 0 < p < 1 -> plogis(p)
    idx <- grepl('del_', names(b_p)) | names(b_p) == 'w'
    b_p[idx] <- plogis(b_p[idx])
    
    return(b_p)
  }
  
  ED50 <- switch (fn_name,
                  "noise" = function(p)  NA,
                  "hyperbolic" = function(p)  1/p['k'],
                  "exponential" = function(p)  log(2)/p['k'],
                  "inverse-q-exponential" = function(p)  (2^(1/p['s']) - 1) / p['k'],
                  "nonlinear-time-hyperbolic" = function(p)  (1/p['k']) ^ (1/p['s']),
                  "nonlinear-time-exponential" = function(p)  (log(2)/p['k'])^(1/p['s']),
                  "scaled-exponential" = function(p)  log(2*p['w'])/p['k'],
                  "dual-systems-exponential" = function(p)  'non-analytic',
                  "model-free" = function(p)  NA
  )

  out <- list(
    name = fn_name,
    fn = fn,
    par = par,
    par_trf = par_trf,
    ED50 = ED50
  )
  
  if (fn_name == 'dual-systems-exponential') {
    out$par_chk <- function(p) {
      # Ensure k1 < k2
      if (p['k1'] > p['k2']) {
        tmp <- p['k1']
        p['k1'] <- p['k2']
        p['k2'] <- tmp
        p['w'] <- -p['w']
      }
    }
  }
  
  class(out) <- 'tdfn'
  return(out)
}

get_discount_function <- function(mod) mod$config$discount_function$fn

# Discount functions
all_discount_functions <- list(
  'noise' = function(D, p) rep(logistic(p['k']), length(D)),
  'hyperbolic' = function(D, p) 1 / (1 + exp(p['k'])*D),
  'exponential' = function(D, p) exp(-exp(p['k'])*D),
  'inverse-q-exponential' = function(D, p) 1 / (1 + exp(p['k'])*D)**exp(p['s']),
  'nonlinear-time-hyperbolic' = function(D, p) 1 / (1 + exp(p['k'])*D**exp(p['s'])),
  'scaled-exponential' = function(D, p) logistic(p['w'])*exp(-exp(p['k'])*D),
  'dual-systems-exponential' = function(D, p) logistic(p['w'])*exp(-p['k1']*D) + (1 - logistic(p['w']))*exp(-(p['k2'] + p['k1'])*D),
  'nonlinear-time-exponential' = function(D, p) exp(-exp(p['k'])*D**exp(p['s'])),
  'model-free' = function(D, p) {
    p <- p[grep('del_', names(p))]
    # Round parameters and delays to 10 decimal points for comparison
    dels <- round(as.numeric(gsub('del_', '', names(p))), 10)
    a <- approx(x = dels, y = logistic(p), xout = round(D, 10))
    return(a$y)
  }
)

# Plausible parameter ranges
default_param_ranges <- list(
  'noise' = list(
    k = seq(-5, 5, length.out = 3)
  ),
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
  'err.rate' = list(
    eps = c(-5, 0)
  ),
  'varsigma' = list(
    alpha = seq(-5, 5, length.out = 3),
    lambda = seq(-1, 1, length.out = 3)
  ),
  'model-free' = NA
)

untransform <- function(par) {
  # Get untransformed parameters
  u_p <- par
  # Log-transformed
  idx <- names(u_p) %in% c('k', 's', 'gamma', 'alpha')
  u_p[idx] <- exp(u_p[idx])
  # Logit-transformed
  idx <- grepl('\\d', names(u_p)) | names(u_p) == 'w'
  u_p[idx] <- logistic(u_p[idx])
  # 0.5*logistic
  idx <- names(u_p) == 'eps'
  u_p[idx] <- 0.5*logistic(u_p[idx])
  return(u_p)
}

#' Effective delay 50
#'
#' Compute the effective delay 50
#' @param mod A temporal discounting model. See `td_gnlm`
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. As in predict.glm, `"link"` (default) and `"response"` give predictions on the scales of the linear predictors and response variable, respectively. `"indiff"` gives predicted indifference points. In this case, `newdata` needs only a `del` column.
#' @return A vector of predictions
#' @export
ED50 <- function(mod) {
  out <- mod$config$discount_function$ED50(coef(mod))
  if (out == 'non-analytic') {
    # No analytic solution, therefore optimize
    f <- function(t) mod$config$discount_function$fn(t, coef(mod))
    optim_func <- function(t) {
      ((f(exp(t))) - 0.5)**2
    }
    optimized <- optim(fn = optim_func, par = 0, method = 'BFGS')
    out <- exp(optimized$par)
  }
  names(out) <- NULL
  
  return(out)
}

#' Area under the curve (AUC)
#'
#' Compute the area under the curve using numerical integration
#' @param mod A temporal discounting model. See `td_gnlm`
#' @param min_del Lower limit to use for integration
#' @param max_del Upper limit to use for integration
#' @param ... Further arguments passed to `integrate()`
#' @return AUC value
#' @export
AUC <- function(mod, min_del = 0, max_del = NULL, verbose = T, ...) {
  if (is.null(max_del)) {
    max_del <- max(mod$data$del)
    if (verbose) {
      cat(sprintf('Defaulting to max_del = %s\n', max_del))
    }
  }
  disc_func <- mod$config$discount_function$fn
  p <- coef(mod)
  if (mod$config$discount_function$name == 'model-free') {
    cat(sprintf('Assuming an indifference point of 1 at delay 0\n'))
    p <- c(c('del_0' = Inf), p)
  }
  integrate(function(t) disc_func(t, p),
            lower = min_del,
            upper = max_del,
            ...)
}
