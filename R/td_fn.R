
#' Predefined or custom discount function
#'
#' Get a predefined discount function or create a custom discount function.
#' @param predefined A string specifying one of the pre-defined discount functions.
#' @param name Name of custom discount function.
#' @param fn Function that takes a data.frame and a vector of named parameters and returns a vector of values between 0 and 1.
#' @param par_starts A named list of vectors, each specifying possible starting values for a parameter to try when running optimization.
#' @param par_lims A named list of vectors, each specifying the bounds to impose of a parameters. Any parameter for which bounds are unspecified are assumed to be unbounded.
#' @param init A function to initialize the td_fn object. It should take 2 arguments: "self" (the td_fn object being initialized) and "data" (the data used for initialization).
#' @param ED50 A function which, given a named vector of parameters \code{p} and optionally a value of \code{del_val}, computes the ED50. If there is no closed-form solution, this should return the string "non-analytic". If the ED50 is not well-defined, this should return the string "none". As a shortcut for these latter 2 cases, the strings "non-analytic" and "none" can be directly supplied as arguments.
#' @param par_chk Optionally, this is a function that checks the parameters to ensure they meet some criteria. E.g., for the dual-systems-exponential discount function, we require k1 < k2.
#' @return An object of class `td_fn`.
#' @examples 
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcnm(td_bc_single_ptpt, discount_function = "hyperbolic", fixed_ends = T)
#' # Custom discount function
#' custom_discount_function <- td_fn(
#'   name = 'custom',
#'   fn = function(data, p) (1 - p['b'])*exp(-p['k']*data$del) + p['b'],
#'   par_starts = list(k = c(0.001, 0.1), b = c(0.001, 0.1)),
#'   par_lims = list(k = c(0, Inf), b = c(0, 1)),
#'   ED50 = 'non-analytic'
#' )
#' mod <- td_bcnm(td_bc_single_ptpt, discount_function = custom_discount_function, fit_err_rate = T)
#' }
#' @export
td_fn <- function(predefined = c('hyperbolic',
                                 'exponential',
                                 'power',
                                 'inverse-q-exponential',
                                 'nonlinear-time-hyperbolic',
                                 'scaled-exponential',
                                 'dual-systems-exponential',
                                 'nonlinear-time-exponential',
                                 'model-free',
                                 'constant'),
                  name = 'unnamed',
                  fn = NULL,
                  par_starts = NULL,
                  par_lims = NULL,
                  init = NULL,
                  ED50 = NULL,
                  par_chk = NULL) {
  
  out <- list()
  class(out) <- 'td_fn'
  
  if (missing(predefined)) {
    
    stopifnot(
      is.character(name),
      length(name) == 1
    )
    out$name <- name
    
    if (missing(fn)) {
      if (missing(init)) {
        stop('fn must be supplied if it will not be created by init')
      } else {
        fn <- function(data, p) NA
      }
    } else {
      stopifnot(is.function(fn))
      if (!all(names(formals(fn)) == c('data', 'p')))
        stop('fn must take 2 arguments: data (a dataframe) and p (a vector of named parameters)')
    }
    out$fn <- fn
    
    if (missing(par_starts)) {
      if (missing(init)) {
        stop('par_starts must be supplied if it will not be created by init')
      } else {
        par_starts <- list(placeholder = NA)
      }
    } else {
      stopifnot(
        is.list(par_starts),
        !is.null(names(par_starts)),
        all(vapply(par_starts,
                   function(x) is.numeric(x) || is.integer(x),
                   logical(1))))
    }
    out$par_starts <- par_starts
    
    if (missing(par_lims)) {
      par_lims <- list()
    } else {
      stopifnot(
        is.list(par_lims),
        !is.null(names(par_lims))
      )
      extra_names <- setdiff(names(par_lims), names(par_starts))
      if (length(extra_names) > 0) {
        stop(sprintf('parameter(s) %s exist in par_lims but not par_starts',
                     paste(extra_names, collapse = ' and ')))
      }
      
    }
    for (par_name in names(par_starts)) {
      if (!(par_name %in% names(par_lims))) {
        par_lims[[par_name]] <- c(-Inf, Inf)
      }
    }
    out$par_lims <- par_lims
    
    if (!missing(init)) {
      stopifnot(is.function(init))
      if (!all(names(formals(init)) == c('self', 'data')))
        stop('init must take 2 arguments: "self" (the td_fn object being initialized) and "data" (the data being used to initialize)')
      out$init <- init
    }
    
    if (is.null(ED50)) {
      out$ED50 <- function(...) 'non-analytic'
    } else if (is.function(ED50)) {
      out$ED50 <- ED50
    } else if (is.character(ED50)) {
      if (ED50 == 'non-analytic') {
        out$ED50 <- function(...) 'non-analytic'
      } else if (ED50 == 'none') {
        out$ED50 <- function(...) 'none'
      } else {
        stop('Only "non-analytic" and "none" are allowed')
      }
    } else {
      stop('ED50 must be a function, "none", or "non-analytic"')
    }
    
    if (!is.null(par_chk)) {
      out$par_chk <- par_chk
    }
    
  } else {
    
    name <- match.arg(predefined)
    
    if (name == 'constant') {
      
      out <- td_fn(name = name,
                   fn = function(data, p) rep(p['c'], length(data$del)),
                   par_starts = list(
                     c = 0.5),
                   par_lims = list(
                     c = c(0, 1)),
                   ED50 = 'none')
      
    } else if (name == 'hyperbolic') {
      
      out <- td_fn(name = name,
                   fn = function(data, p) 1 / (1 + p['k']*data$del),
                   par_starts = list(
                     k = c(0.001, 0.01, 0.1)),
                   par_lims = list(
                     k = c(0, Inf)),
                   ED50 = function(p, ...) 1/p['k'])
      
    } else if (name == 'exponential') {
      
      out <- td_fn(name = name,
                   fn = function(data, p) exp(-p['k']*data$del),
                   par_starts = list(
                     k = c(0.001, 0.01, 0.1)),
                   par_lims = list(
                     k = c(0, Inf)),
                   ED50 = function(p, ...) log(2)/p['k'])
      
    } else if (name == 'power') {
      
      out <- td_fn(name = name,
                   fn = function(data, p) 1 / (1 + data$del)**p['k'],
                   par_starts = list(
                     k = c(0.001, 0.01, 0.1)),
                   par_lims = list(
                     k = c(0, Inf)),
                   ED50 = function(p, ...) 2**p['k'] - 1)
    
    } else if (name == 'inverse-q-exponential') {
      
      out <- td_fn(name = name,
                   fn = function(data, p) 1 / (1 + p['k']*data$del)**p['s'],
                   par_starts = list(
                     k = c(0.001, 0.01, 0.1),
                     s = c(0.001, 0.01, 0.1)),
                   par_lims = list(
                     k = c(0, Inf),
                     s = c(0, Inf)),
                   ED50 = function(p, ...) (2^(1/p['s']) - 1) / p['k'])
      
    } else if (name == 'nonlinear-time-hyperbolic') {
      
      out <- td_fn(name = name,
                   fn = function(data, p) 1 / (1 + p['k']*data$del**p['s']),
                   par_starts = list(
                     k = c(0.001, 0.01, 0.1),
                     s = c(0.001, 0.01, 0.1)),
                   par_lims = list(
                     k = c(0, Inf),
                     s = c(0, Inf)),
                   ED50 = function(p, ...) (1/p['k']) ^ (1/p['s']))
      
    } else if (name == 'scaled-exponential') {
      
      out <- td_fn(name = name,
                   fn = function(data, p) p['w']*exp(-p['k']*data$del),
                   par_starts = list(
                     w = c(0.1, 0.5, 0.9),
                     k = c(0.001, 0.01, 0.1)),
                   par_lims = list(
                     w = c(0, 1),
                     k = c(0, Inf)),
                   ED50 = function(p, ...) log(2*p['w'])/p['k'])
      
    } else if (name == 'dual-systems-exponential') {
      
      out <- td_fn(name = name,
                   fn = function(data, p) p['w']*exp(-p['k1']*data$del) + (1 - p['w'])*exp(-p['k2']*data$del),
                   par_starts = list(
                     w = c(0.1, 0.5, 0.9),
                     k1 = c(0.001, 0.01, 0.1),
                     k2 = c(0.001, 0.01, 0.1)),
                   par_lims = list(
                     w = c(0, 1),
                     k1 = c(0, Inf),
                     k2 = c(0, Inf)),
                   ED50 = 'non-analytic',
                   par_chk = function(p) {
                     # Ensure k1 <= k2
                     if (p['k1'] > p['k2']) {
                       # Switch k1 and k2
                       k1 <- p['k1']
                       k2 <- p['k2']
                       p['k1'] <- k2
                       p['k2'] <- k1
                       # Complement of w
                       p['w'] <- 1 - p['w']
                     }
                     return(p)})
      
    } else if (name == 'nonlinear-time-exponential') {
      
      out <- td_fn(name = name,
                   fn = function(data, p) exp(-p['k']*data$del**p['s']),
                   par_starts = list(
                     s = c(0.001, 0.01, 0.1),
                     k = c(0.001, 0.01, 0.1)),
                   par_lims = list(
                     s = c(0, Inf),
                     k = c(0, Inf)),
                   ED50 = function(p, ...) (log(2)/p['k'])^(1/p['s']))
      
    } else if (name == 'model-free') {
      
      out <- td_fn(name = name,
                   fn = function(data, p) 'placeholder',
                   par_starts = list(placeholder = 0),
                   par_lims = list(placeholder = c(0, 0)),
                   init = function(self, data) {
                     # Get unique delays
                     delays <- sort(unique(data$del))
                     # Get starts and limits for free parameters
                     par_names <- sprintf('indiff_%s', seq_along(delays))
                     par_starts <- rep(list(0.5), length(delays))
                     names(par_starts) <- par_names
                     par_lims <- rep(list(c(0, 1)), length(delays))
                     names(par_lims) <- par_names
                     # Add to self
                     self$par_starts <- par_starts
                     self$par_lims <- par_lims
                     # Get interpolation function
                     self$fn <- function(data, p) {
                       approx(x = c(0, delays),
                              y = c(1, p[sprintf('indiff_%s', seq_along(delays))]),
                              xout = data$del)[['y']]
                     }
                     return(self)},
                   ED50 = 'none')
                   
      
    }
    
  }
  
  return(out)
}
