
# Discount functions
all_discount_functions <- list(
  'noise' = function(D, p) rep(logistic(p['k']), length(D)),
  'hyperbolic' = function(D, p) 1 / (1 + exp(p['k'])*D),
  'exponential' = function(D, p) exp(-exp(p['k'])*D),
  'inverse-q-exponential' = function(D, p) 1 / (1 + exp(p['k'])*D)**exp(p['s']),
  'nonlinear-time-hyperbolic' = function(D, p) 1 / (1 + exp(p['k'])*D**exp(p['s'])),
  'scaled-exponential' = function(D, p) logistic(p['w'])*exp(-exp(p['k'])*D),
  'dual-systems-exponential' = function(D, p) logistic(p['w'])*exp(-exp(p['k1'])*D) + (1 - logistic(p['w']))*exp(-(exp(p['k2']) + exp(p['k1']))*D),
  'nonlinear-time-exponential' = function(D, p) exp(-exp(p['k'])*D**exp(p['s'])),
  'model-free' = function(D, p) {
    p <- p[grep('del_', names(p))]
    # Round parameters and delays to 10 decimal points for comparison
    dels <- round(as.numeric(gsub('del_', '', names(p))), 10)
    a <- approx(x = dels, y = logistic(p), xout = round(D, 10))
    return(a$y)
  }
)

get_discount_function <- function(func_name) {
  all_discount_functions[[func_name]]
}

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
#' Generate predictions from a temporal discounting model
#' @param mod A temporal discounting model. See `td_gnlm`
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. As in predict.glm, `"link"` (default) and `"response"` give predictions on the scales of the linear predictors and response variable, respectively. `"indiff"` gives predicted indifference points. In this case, `newdata` needs only a `del` column.
#' @return A vector of predictions
#' @export
ED50 <- function(mod) {
  u_p <- coef(mod) # untransformed parameters
  out <- switch(
    mod$config$discount_function,
    "noise" = NA,
    "hyperbolic"= 1/u_p['k'],
    "exponential"= log(2)/u_p['k'],
    "inverse-q-exponential" = (2^(1/u_p['s']) - 1) / u_p['k'],
    "nonlinear-time-hyperbolic" = (1/u_p['k']) ^ (1/u_p['s']),
    "nonlinear-time-exponential" = (log(2)/u_p['k'])^(1/u_p['s']),
    "scaled-exponential" = log(2*u_p['w'])/u_p['k'],
    "dual-systems-exponential" = NA,
    "none" = NA
  )
  if (mod$config$discount_function == 'dual-systems-exponential') {
    # No analytic solution, therefore optimize
    f_tp <- get_discount_function(mod$config$discount_function)
    f_t <- function(del) f_tp(del, coef(mod, bounded = F)) # parameterized
    optim_func <- function(t) {
      ((f_t(exp(t))) - 0.5)**2
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
  disc_func <- get_discount_function(mod$config$discount_function)
  p <- coef(mod, bounded = F)
  if (mod$config$discount_function == 'model-free') {
    cat(sprintf('Assuming an indifference point of 1 at delay 0\n'))
    p <- c(c('del_0' = Inf), p)
  }
  integrate(function(t) disc_func(t, p),
            lower = min_del,
            upper = max_del,
            ...)
}
