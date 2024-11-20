
#' Predefined or custom discount function
#'
#' Get a predefined discount function or create a custom discount function.
#' @param predefined A string specifying one of the pre-defined discount functions.
#' @param name Name of custom discount function.
#' @param fn Function that takes a data.frame and a vector of named parameters and returns a vector of values between 0 and 1.
#' @param par_starts A named list of vectors, each specifying possible starting values for a parameter to try when running optimization.
#' @param par_lims A named list of vectors, each specifying the bounds to impose of a parameters. Any parameter for which bounds are unspecified are assumed to be unbounded.
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
#'   ED50 = function(...) 'non-analytic'
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
                  name = NULL,
                  fn = NULL,
                  par_starts = NULL,
                  par_lims = NULL,
                  ED50 = NULL,
                  par_chk = NULL) {
  
  out <- list()
  class(out) <- 'td_fn'
  
  if (missing(predefined)) {
    
    if (missing(name) | missing(fn) | missing(par_starts)) {
      stop('To create a custom discount funciton, "name", "fn", and "par_starts" must all be provided to td_fn')
    }
    out$name <- name
    if (!all(names(formals(fn)) == c('data', 'p'))) {
      stop('fn must take 2 arguments: data (a dataframe) and p (a vector of named parameters)')
    } else {
      out$fn <- fn
    }
    
    if (is.function(par_starts)) {
      out$par_starts <- par_starts
    } else {
      if (is.null(names(par_starts))) {
        stop('par_starts must be a named list')
      } else {
        out$par_starts <- as.list(par_starts)
      }
    }
    
    if (is.null(par_lims)) {
      par_lims <- list()
    } else {
      if (is.function(par_lims)) {
        out$par_lims <- par_lims
      } else {
        if (is.null(names(par_lims)) | any(names(par_lims) == '')) {
          stop('Every element of par_lims must have a name corresponding to a different parameter')
        } else if (!all(vapply(par_lims, length, numeric(1)) == 2)) {
          stop('par_lims must be a list of 2-element vectors')
        }
        
        for (par_name in names(par_starts)) {
          if (!(par_name %in% names(par_lims))) {
            par_lims[[par_name]] <- c(-Inf, Inf)
          }
        }
        
        out$par_lims <- par_lims
      }
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
                   ED50 = function(p, ...) function(p, ...) (1/p['k']) ^ (1/p['s']))
      
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
                   fn = function(data, p) function(data, p) p['w']*exp(-p['k1']*data$del) + (1 - p['w'])*exp(-p['k2']*data$del),
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
                   fn = function(data, p) {
                     p <- p[grep('del_', names(p))]
                     # Round parameters and delays to 10 decimal points for comparison
                     dels <- round(as.numeric(gsub('del_', '', names(p))), 10)
                     xout <- round(data$del, 10)
                     get_yout <- function(xout_value) {
                       if (xout_value %in% dels) {
                         return(p[which(dels == xout_value)])
                       } else {
                         interp_result <- approx(x = dels, y = p, xout = xout_value)
                         return(interp_result$y)
                       }
                     }
                     yout <- vapply(xout, get_yout, numeric(1))
                     return(yout)},
                   par_starts = function(data) {
                     unique_delays <- unique(data$del)
                     out <- as.list(rep(0.5, length(unique_delays)))
                     # Round to 10 decimal points to be able to align delay values 
                     names(out) <- sprintf('del_%.10f', unique_delays)
                     return(out)},
                   par_lims = function(data) {
                     unique_delays <- unique(data$del)
                     out <- rep(list(c(0, 1)), length(unique_delays))
                     # Round to 10 decimal points to be able to align delay values 
                     names(out) <- sprintf('del_%.10f', unique_delays)
                     return(out)},
                   ED50 = 'none')
                   
      
    }
    
  }
  
  return(out)
}
