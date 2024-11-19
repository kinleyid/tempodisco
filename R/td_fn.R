
#' Temporal discount function
#'
#' Get or create a discount function
#' @param predefined A string specifying one of the pre-defined discount functions
#' @param name Name of custom discount function
#' @param fn Function that takes a data.frame and a vector of named parameters and returns a vector of values between 0 and 1
#' @param par_starts A named list of vectors, each specifying possible starting values for a parameter to try when running optimization
#' @param par_lims A named list of vectors, each specifying the bounds to impose of a parameter
#' @param ED50 A function which, given a named vector of parameters \code{p} and optionally a value of \code{del_val}, computes the ED50. If there is no closed-form solution, this should return the string "non-analytic". If the ED50 is not well-defined, this should return the string "none"
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
    
    out$name <- name
    out$fn <- fn
    out$par_starts <- as.list(par_starts)
    
    if (is.null(par_lims)) {
      par_lims <- list()
      par_lims[names(par_starts)] <- c(-Inf, Inf)
    } else {
      for (par_name in names(par_starts)) {
        if (!(par_name %in% names(par_lims))) {
          par_lims[[par_name]] <- c(-Inf, Inf)
        }
      }
    }
    
    out$par_lims <- par_lims
    
    if (is.null(ED50)) {
      out$ED50 <- function(...) 'non-analytic'
    } else {
      out$ED50 <- ED50
    }
    
    if (!is.null(par_chk)) {
      out$par_chk <- par_chk
    }
    
  } else {
    
    name <- match.arg(predefined)
    out$name <- name
    out$fn <- switch (name,
                      'constant' = function(data, p) rep(p['c'], length(data$del)),
                      'hyperbolic' = function(data, p) 1 / (1 + p['k']*data$del),
                      'exponential' = function(data, p) exp(-p['k']*data$del),
                      'inverse-q-exponential' = function(data, p) 1 / (1 + p['k']*data$del)**p['s'],
                      'nonlinear-time-hyperbolic' = function(data, p) 1 / (1 + p['k']*data$del**p['s']),
                      'scaled-exponential' = function(data, p) p['w']*exp(-p['k']*data$del),
                      'dual-systems-exponential' = function(data, p) p['w']*exp(-p['k1']*data$del) + (1 - p['w'])*exp(-p['k2']*data$del),
                      'nonlinear-time-exponential' = function(data, p) exp(-p['k']*data$del**p['s']),
                      'model-free' = function(data, p) {
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
                        yout <- vapply(xout, get_yout)
                        return(yout)
                      }
    )
    out$par_starts <- switch(name,
                             'constant' = list(
                               c = exp(seq(-8, 0, length.out = 3))
                             ),
                             'hyperbolic' = list(
                               k = exp(seq(-8, 0, length.out = 3))
                             ),
                             'exponential' = list(
                               k = exp(seq(-8, 0, length.out = 3))
                             ),
                             'inverse-q-exponential' = list(
                               k = exp(seq(-8, 0, length.out = 3)),
                               s = exp(seq(-8, 0, length.out = 3))
                             ),
                             'nonlinear-time-hyperbolic' = list(
                               k = exp(seq(-8, 0, length.out = 3)),
                               s = exp(seq(-8, 0, length.out = 3))
                             ),
                             'scaled-exponential' = list(
                               w = exp(seq(-8, 0, length.out = 3)),
                               k = exp(seq(-8, 0, length.out = 3))
                             ),
                             'dual-systems-exponential' = list(
                               w = exp(seq(-8, 0, length.out = 3)),
                               k1 = exp(seq(-8, 0, length.out = 3)),
                               k2 = exp(seq(-8, 0, length.out = 3))
                             ),
                             'nonlinear-time-exponential' = list(
                               k = exp(seq(-8, 0, length.out = 3)),
                               s = exp(seq(-8, 0, length.out = 3))
                             ),
                             'model-free' = function(data) {
                               unique_delays <- unique(data$del)
                               out <- as.list(rep(0.5, length(unique_delays)))
                               # Round to 10 decimal points to be able to align delay values 
                               names(out) <- sprintf('del_%.10f', unique_delays)
                               return(out)
                             }
    )
    out$par_lims <- switch(name,
                           'constant' = list(
                             c = c(0, 1)
                           ),
                           'hyperbolic' = list(
                             k = c(0, Inf)
                           ),
                           'exponential' = list(
                             k = c(0, Inf)
                           ),
                           'inverse-q-exponential' = list(
                             k = c(0, Inf),
                             s = c(0, Inf)
                           ),
                           'nonlinear-time-hyperbolic' = list(
                             k = c(0, Inf),
                             s = c(0, Inf)
                           ),
                           'scaled-exponential' = list(
                             w = c(0, 1),
                             k = c(0, Inf)
                           ),
                           'dual-systems-exponential' = list(
                             w = c(0, 1),
                             k1 = c(0, Inf),
                             k2 = c(0, Inf)
                           ),
                           'nonlinear-time-exponential' = list(
                             k = c(0, Inf),
                             s = c(0, Inf)
                           ),
                           'model-free' = function(data) {
                             unique_delays <- unique(data$del)
                             out <- rep(list(c(0, 1)), length(unique_delays))
                             # Round to 10 decimal points to be able to align delay values 
                             names(out) <- sprintf('del_%.10f', unique_delays)
                             return(out)
                           }
    )
    out$ED50 <- switch (name,
                        "constant" = function(p, ...) 'none',
                        "hyperbolic" = function(p, ...) 1/p['k'],
                        "exponential" = function(p, ...) log(2)/p['k'],
                        "inverse-q-exponential" = function(p, ...) (2^(1/p['s']) - 1) / p['k'],
                        "nonlinear-time-hyperbolic" = function(p, ...) (1/p['k']) ^ (1/p['s']),
                        "nonlinear-time-exponential" = function(p, ...) (log(2)/p['k'])^(1/p['s']),
                        "scaled-exponential" = function(p, ...) log(2*p['w'])/p['k'],
                        "dual-systems-exponential" = function(...) 'non-analytic',
                        "model-free" = function(...) 'none'
    )
    if (name == 'dual-systems-exponential') {
      out$par_chk <- function(p) {
        # Ensure k1 < k2
        if (p['k1'] > p['k2']) {
          # Switch k1 and k2
          k1 <- p['k1']
          k2 <- p['k2']
          p['k1'] <- k2
          p['k2'] <- k1
          # Complement of w
          p['w'] <- 1 - p['w']
        }
        return(p)
      }
    }
  }
  
  return(out)
}
