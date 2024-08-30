
get_rss_fn <- function(data, discount_function) {
  # Get residual sum of squares function
  
  rss_fn <- function(par) {
    pred <- discount_function(data, par)
    return(sum((data$indiff - pred)**2))
  }
  return(rss_fn)
}

#' Indifference point model
#'
#' Compute a model of a single subject's indifference points
#' @param data A data frame with columns \code{indiff} for the pre-computed indifference points and \code{del} for the delay
#' @param discount_function A vector of strings specifying the name of the discount functions to use, or an object of class \code{td_fn}.
#' @param optim_args A list of additional args to pass to \code{optim}
#' @param silent A Boolean specifying whether the call to \code{optim} (which occurs in a \code{try} block) should be silent on error
#' @return A list from \code{optim} with additional components specifying the AIC, the ED50, the discount function, and the probabilistic model
#' @examples
#' \dontrun{
#' # Basic usage
#' data("td_ip_simulated_ptpt")
#' mod <- td_ipm(td_ip_simulated_ptpt, discount_function = "hyperbolic")
#' # Custom discount function
#' custom_discount_function <- td_fn(
#'   name = 'custom',
#'   fn = function(data, p) (1 - p['b'])*exp(-p['k']*data$del) + p['b'],
#'   par_starts = list(k = c(0.001, 0.1), b = c(0.001, 0.1)),
#'   par_lims = list(k = c(0, Inf), b = c(0, 1)),
#'   ED50 = function(p) 'non-analytic'
#' )
#' mod <- td_ipm(td_ip_simulated_ptpt, discount_function = custom_discount_function)
#' }
#' @export
td_ipm <- function(
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
    optim_args = list(),
    silent = T) {
  
  # Set discount function(s)
  if ('all' %in% discount_function) {
    # If "all" is used, replace discount_function with a vector of all the options
    discount_function <- eval(formals(td_fn)$predefined)
  }
  
  # Required data columns
  require_columns(data, c('indiff', 'del'))
  
  # Valid discount function name
  validate_discount_function(discount_function)
  
  # Get a list of discount functions to test
  if (is.list(discount_function)) {
    cand_fns <- list(discount_function)
  } else {
    cand_fns <- list()
    for (fn_name in discount_function) {
      cand_fns <- c(cand_fns, list(td_fn(fn_name)))
    }
  }
  
  # Run optimization on each candidate discount function
  best_crit <- Inf
  cand_mod <- list(data = data, config = list(), optim = NULL)
  class(cand_mod) <- c('td_ipm', 'td_um')
  for (cand_fn in cand_fns) {
   
    # Get residual sum of squares function
    rss_fn <- get_rss_fn(data, cand_fn$fn)
    
    # Get parameter starting values
    if (is.function(cand_fn$par_starts)) {
      par_starts <- cand_fn$par_starts(data)
    } else {
      par_starts <- cand_fn$par_starts
    }
    
    # Get parameter bounds
    if (is.function(cand_fn$par_lims)) {
      par_lims <- cand_fn$par_lims(data)
    } else {
      par_lims <- cand_fn$par_lims
    }
    
    # Run optimization
    optimized <- run_optimization(rss_fn, par_starts, par_lims, optim_args = optim_args, silent = silent)
    if (length(optimized) == 0) {
      stop('Optimization failed; optim() returned an error for every choice of initial parameter values')
    }
    if ('par_chk' %in% names(cand_fn)) {
      optimized$par <- cand_fn$par_chk(optimized$par)
    }
    cand_mod$optim <- optimized
    cand_mod$config$discount_function <- cand_fn
    # Compare by BIC
    crit <- BIC(cand_mod)
    if (crit < best_crit) {
      best_crit <- crit
      best_output <- cand_mod
    }
    
  }
  return(best_output)
}
