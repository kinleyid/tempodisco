
get_rss_fn <- function(data, discount_function) {
  # Get residual sum of squares function
  
  rss_fn <- function(par) {
    pred <- discount_function(data$del, par)
    return(sum((data$indiff - pred)**2))
  }
  return(rss_fn)
}

#' Indifference point model
#'
#' Compute a model of a single subject's indifference points
#' @param data A data frame with columns `indiff` for the pre-computed indifference points and `del` for the delay
#' @param discount_function A vector of strings specifying the name of the discount functions to use. Options are `'hyperbolic'`, `'exponential'`, `'inverse-q-exponential'`, `'nonlinear-time-hyperbolic'`, `'scaled-exponential'`, `'dual-systems-exponential'`, and `'nonlinear-time-exponential'`. Default is `'all'`, meaning every discount function is tested and the one with the best AIC is selected.
#' @param param_ranges A list containing the starting values to try for each parameter. Defaults to `c(-5, 0, 5)` for most parameters
#' @param silent A Boolean specifying whether the call to `optim` (which occurs in a `try` block) should be silent on error
#' @return A list from `optim` with additional components specifying the AIC, the ED50, the discount function, and the probabilistic model
#' @note The `par` component of the output list is for internal use. For statistical analyses, use the `untransformed_parameters`. `par` contains the parameters after various transformations intended to keep them within certain bounds (e.g., k parameters should never be negative)
#' @examples 
#' # Generate data
#' df <- data.frame(del = exp(1:10), indiff = 1 / (1 + 0.001*exp(1:10)))
#' # Fit model
#' mod <- dd_det_model(df)
#' print(mod$discount_function)
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
    silent = T) {
  
  # Set discount function(s)
  if ((discount_function %||% 'all') == 'all') {
    discount_function <- eval(formals(td_ipm)$discount_function)
    discount_function[discount_function != 'all']
  }
  
  # Check args
  if (missing(data)) {
    stop('data unspecified')
  }
  # Required data columns
  req_cols <- c('indiff', 'del')
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
  # Valid discount function
  for (d_f in discount_function) {
    if (!(d_f %in% names(all_discount_functions))) {
      valid_opts <- paste(sprintf('\n- "%s"', names(all_discount_functions)), collapse = '')
      stop(sprintf('"%s" is not a recognized discount function. Valid options are: %s', d_f, valid_opts))
    }
  }
  
  # Terms for computing AIC
  N <- nrow(data)
  
  # Get a list of discount functions to test
  if (is.list(discount_function)) {
    cand_fns <- list(discount_function)
  } else {
    cand_fns <- list()
    for (fn_name in discount_function) {
      cand_fns <- c(cand_fns, list(tdfn(fn_name)))
    }
  }
  
  # Run optimization on each candidate discount function
  args <- data.frame(discount_function = discount_function)
  best_crit <- Inf
  best_output <- list()
  for (cand_fn in cand_fns) {
   
    # Get residual sum of squares function
    rss_fn <- get_rss_fn(data, cand_fn$fn)
    # Get parameter starting values
    if (is.function(cand_fn$par)) {
      par_starts <- cand_fn$par()
    } else {
      par_starts <- cand_fn$par
    }
    
    # Run optimization
    optimized <- run_optimization(rss_fn, par_starts, cand_fn$par_trf, silent)
    # Compute AIC
    # crit <- N*log(optimized$value/N) + 2*(length(optimized$par) + 1)
    # Use RSS
    crit <- optimized$value
    if (crit < best_crit) {
      best_crit <- crit
      if ('par_chk' %in% names(cand_fn)) {
        optimized$par <- cand_fn$par_chk(optimized$par)
      }
      optimized$discount_function <- discount_function
      best_output <- optimized
    }
  }
  best_output$data <- data
  
  return(best_output)
}