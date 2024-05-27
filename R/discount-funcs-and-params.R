
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
  'err.rate' = list(
    eps = c(-5, 0)
  ),
  'varsigma' = list(
    alpha = seq(-5, 5, length.out = 3),
    lambda = seq(-1, 1, length.out = 3)
  ),
  'none' = NA
)

untransform <- function(par) {
  # Get untransformed parameters
  u_p <- par
  idx <- names(u_p) %in% c('k', 's', 'gamma', 'alpha')
  u_p[idx] <- exp(u_p[idx])
  idx <- grepl('\\d', names(u_p)) | names(u_p) == 'w'
  u_p[idx] <- logistic(u_p[idx])
  idx <- names(u_p) == 'eps'
  u_p[idx] <- 0.5*logistic(u_p[idx])
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