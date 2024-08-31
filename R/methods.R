
#' Median effective delay
#'
#' Compute the median effective delay
#' @param mod A temporal discounting model.
#' @param val_del Delayed value, if applicable (i.e., if magnitude effects are accounted for)
#' @return A vector of predictions
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcm(td_bc_single_ptpt)
#' print(ED50(mod))
#' }
#' @export
ED50 <- function(mod, val_del = NULL) {
  if (is.null(val_del)) {
    if ('val_del' %in% names(mod$data)) {
      val_del <- mean(mod$data$val_del)
    }
  }
  out <- mod$config$discount_function$ED50(coef(mod), val_del)
  if (out == 'non-analytic') {
    # No analytic solution, therefore optimize
    f <- function(t) predict(mod,
                             newdata = data.frame(del = t,
                                                  val_del = val_del %def% NA),
                             type = 'indiff')
    optim_func <- function(t) {
      ((f(t)) - 0.5)**2
    }
    optimized <- optim(fn = optim_func, par = 0, lower = 0, method = 'L-BFGS-B')
    out <- optimized$par
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
#' @param verbose Specifies whether to provide extra detail, if applicable
#' @param ... Further arguments passed to `integrate()`
#' @return AUC value
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcm(td_bc_single_ptpt)
#' print(AUC(mod))
#' }
#' @export
AUC <- function(mod, min_del = 0, max_del = NULL, verbose = T, ...) {
  
  if (is.null(max_del)) {
    max_del <- max(mod$data$del)
    if (verbose) {
      cat(sprintf('Defaulting to max_del = %s\n', max_del))
    }
  }
  if (mod$config$discount_function$name == 'model-free') {
    mod$optim$par <- c(c('del_0' = 1), mod$optim$par)
    if (verbose) {
      cat(sprintf('Assuming an indifference point of 1 at delay 0\n'))
    }
  }
  disc_func <- function(t) predict(mod, newdata = data.frame(del = t), type = 'indiff')
  out <- tryCatch(
    expr = {
      integrate(function(t) disc_func(t),
                lower = min_del,
                upper = max_del,
                ...)$value
    },
    error = function(e) {
      return(sprintf('integrate() failed to compute AUC with error: "%s"', e$message))
    }
  )
  return(out)
}

#' Check for non-systematic discounting
#' 
#' Check for non-systematic discounting, per the Johnson & Bickel (2008) criteria. These are:
#' \itemize{
#'  \item C1: No indifference point can exceed the previous by more than 0.2
#'  \item C2: Last indifference point must be lower than first by at least 0.1
#' }
#' @param obj Either a \code{data.frame} with columns \code{indiff} and \code{del}, or a discounting model of class \code{td_bcm} or \code{td_ipm}, fit using the \code{"model-free"} discount function.
#' @returns Named logical vector specifying whether nonsystematic discounting is exhibited according to C1/C2.
#' @examples
#' \dontrun{
#' # On a model
#' data("td_bc_single_ptpt")
#' mod <- td_bcm(td_bc_single_ptpt, discount_function = 'model-free')
#' any(nonsys(mod))
#' 
#' # On a dataframe
#' data("td_ip_simulated_ptpt")
#' any(nonsys(td_ip_simulated_ptpt))
#' 
#' # Artificial case of nonsystematic discounting
#' nonsys(data.frame(del = 1:3, indiff = c(0.5, 0.8, 0.6))) # Both TRUE
#' }
#' @export
nonsys <- function(obj) {
  
  if (is(obj, 'data.frame')) {
    require_columns(obj, c('indiff', 'del'))
    indiffs <- obj$indiff
    delays <- obj$del
  } else if (inherits(obj, c('td_bcm', 'td_ipm'))) {
    if (obj$config$discount_function$name != 'model-free') {
      stop('Discount function must be "model-free" to check for non-systematic discounting.')
    } else {
      cf <- coef(obj)
      cf <- cf[grep('del_', names(cf))]
      indiffs <- unname(cf)
      delays <- as.numeric(gsub('del_', '', names(cf)))
    }
  } else {
    stop('Input must be a data.frame or a model of class td_bcm or td_ipm.')
  }
  
  idx <- order(delays)
  indiffs <- indiffs[idx]
  delays <- delays[idx]
  
  # Criterion 1: monotonicity
  # No indifference point can exceed the previous by more than 0.2
  C1 <- any(diff(indiffs) > 0.2)
  
  # Criterion 2: minimal discounting
  # Last indifference point must be lower than first by at least 0.1
  C2 <- (indiffs[1] - indiffs[length(indiffs)]) < 0.1
  
  return(c(C1 = C1, C2 = C2))
  
}