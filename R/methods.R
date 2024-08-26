
#' Median effective delay
#'
#' Compute the median effective delay
#' @param mod A temporal discounting model. See `td_gnlm`
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. As in predict.glm, `"link"` (default) and `"response"` give predictions on the scales of the linear predictors and response variable, respectively. `"indiff"` gives predicted indifference points. In this case, `newdata` needs only a `del` column.
#' @return A vector of predictions
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcm(td_bc_single_ptpt)
#' print(ED50(mod))
#' }
#' @export
ED50 <- function(mod) {
  out <- mod$config$discount_function$ED50(coef(mod))
  if (out == 'non-analytic') {
    # No analytic solution, therefore optimize
    f <- function(t) predict(mod, newdata = data.frame(del = t), type = 'indiff')
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
    cat(sprintf('Assuming an indifference point of 1 at delay 0\n'))
    mod$optim$par <- c(c('del_0' = 1), mod$optim$par)
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
