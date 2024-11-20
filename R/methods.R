
#' Median effective delay
#'
#' Compute the median effective delay.
#' @param mod A temporal discounting model.
#' @param val_del Delayed value, if applicable (i.e., if magnitude effects are accounted for).
#' @return A vector of predictions.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcnm(td_bc_single_ptpt)
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
    optimized <- optim(fn = optim_func, par = 1, lower = 0, method = 'L-BFGS-B')
    out <- optimized$par
  }
  names(out) <- NULL
  
  return(out)
}

#' Area under the curve (AUC)
#'
#' Compute either the model-based or model-free area under the curve.
#' @param obj A temporal discounting model or a dataframe with columns \code{indiff} (indifference point) and \code{del} (delay).
#' @param min_del Lower limit to use for integration. Defaults to 0.
#' @param max_del Upper limit to use for integration. Defaults to the maximum delay in the data.
#' @param val_del Delayed value to use for computing the indifference curve, if applicable. Defaults to the average \code{del_val} in the data.
#' @param del_transform String specifying transformation to apply to the delays (e.g., log10 + 1 transform or ordinal scaling transform; Borges et al., 2016, \doi{10.1002/jeab.219}). Default is no transform.
#' @param ... Further arguments passed to `integrate()`.
#' @return AUC value.
#' @note
#' Calculation of the area always begins from delay 0, where an indifference point of 1 is assumed.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcnm(td_bc_single_ptpt)
#' print(AUC(mod))
#' data("td_ip_simulated_ptpt")
#' }
#' @export
AUC <- function(obj, min_del = 0, max_del = NULL, val_del = NULL, del_transform = c('none', 'log', 'ordinal-scaling'), ...) {
  
  del_transform <- match.arg(del_transform)
  
  if (is.data.frame(obj)) {
    
    # Model-free AUC
    validate_td_data(obj, required_columns = c('indiff', 'del'))
    obj <- obj[c('indiff', 'del')]
    # Get max del
    max_del <- max_del %def% max(obj$del)
    if (max_del > max(obj$del)) {
      stop(sprintf('max_del (%s) exceeds the maximum delay in the dataset (%s)',
                   max_del,
                   max(obj$del)))
    }
    # Assume indiff = 1 at del = 0
    obj <- rbind(obj, data.frame(del = 0, indiff = 1))
    # Sort by delay
    obj <- obj[order(obj$del), ]
    # Transform delays
    if (del_transform == 'none') {
      trf <- identity
    } else if (del_transform == 'log') {
      trf <- function(x) log10(x + 1)
    } else if (del_transform == 'ordinal-scaling') {
      trf <- approxfun(obj$del, seq_along(obj$del))
    }
    # Integrate
    integrand <- approxfun(trf(obj$del), obj$indiff)
    lims <- trf(c(0, max_del))
    
    # # Transform delay
    # if (del_transform == 'log') {
    #   obj$del <- log10(obj$del + 1)
    # } else if (del_transform == 'ordinal-scaling') {
    #   obj$del <- as.numeric(as.factor(obj$del))
    # }
    # # Exact solution
    # trap_areas <- vapply(seq_len(nrow(obj) - 1),
    #                function(idx) {
    #                  diff(obj$del[idx:(idx + 1)]) * mean(obj$indiff[idx:(idx + 1)])
    #                },
    #                numeric(1))
    # out <- sum(trap_areas) / (max(obj$del) - min(obj$del))
    
  } else {
    
    # Model-based AUC
    stopifnot(inherits(obj, 'td_um'))
    max_del <- max_del %def% max(obj$data$del)
    if (obj$config$discount_function$name == 'model-free') {
      # Assume indiff = 1 at del = 0
      obj$optim$par <- c(c('del_0' = 1), obj$optim$par)
    }
    if (is.null(val_del)) {
      if ('val_del' %in% names(obj$data)) {
        val_del <- mean(obj$data$val_del)
      } else {
        val_del <- NA
      }
    }
    # Get transform to apply to data
    if (del_transform == 'none') {
      trf <- identity
      invtrf <- identity
    } else if (del_transform == 'log') {
      trf <- function(x) log10(x + 1)
      invtrf <- function(x) 10**x - 1
    } else {
      stop('For model-based AUC, del_transform must be "none" or "log"')
    }
    integrand <- function(t) {
      predict(obj,
              newdata = data.frame(del = invtrf(t),
                                   val_del = val_del),
              type = 'indiff')
    }
    lims <- trf(c(0, max_del))
  }
  auc <- tryCatch(
    expr = {
      unnormed_auc <- integrate(f = integrand,
                                lower = lims[1],
                                upper = lims[2],
                                ...)[['value']]
      auc <- unnormed_auc / diff(lims)
    },
    error = function(e) {
      return(sprintf('integrate() failed to compute AUC with error: "%s"', e$message))
    }
  )
  return(auc)
}

#' Check for non-systematic discounting
#' 
#' Check for non-systematic discounting, per the Johnson & Bickel (2008) criteria. These are:
#' \itemize{
#'  \item C1: No indifference point can exceed the previous by more than 0.2
#'  \item C2: Last indifference point must be lower than first by at least 0.1
#' }
#' @param obj Either a \code{data.frame} with columns \code{indiff} and \code{del}, or a discounting model of class \code{td_bcnm} or \code{td_ipm}, fit using the \code{"model-free"} discount function.
#' @returns Named logical vector specifying whether nonsystematic discounting is exhibited according to C1/C2.
#' @examples
#' \dontrun{
#' # On a model
#' data("td_bc_single_ptpt")
#' mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'model-free')
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
    validate_td_data(obj,
                     required_columns = c('indiff', 'del'))
    indiffs <- obj$indiff
    delays <- obj$del
  } else if (inherits(obj, c('td_bcnm', 'td_ipm', 'td_ddm'))) {
    if (obj$config$discount_function$name != 'model-free') {
      stop('Discount function must be "model-free" to check for non-systematic discounting.')
    } else {
      cf <- coef(obj)
      cf <- cf[grep('del_', names(cf))]
      indiffs <- unname(cf)
      delays <- as.numeric(gsub('del_', '', names(cf)))
    }
  } else {
    stop('Input must be a data.frame or a model of class td_bcnm, td_ipm, or td_ddm.')
  }
  
  idx <- order(delays)
  indiffs <- indiffs[idx]
  delays <- delays[idx]
  
  # Criterion 1: monotonicity
  # No indifference point can exceed the previous by more than 0.2
  C1 <- any(diff(indiffs) > 0.2)
  
  # Criterion 2: minimal delay sensitivity
  # Last indifference point must be lower than first by at least 0.1
  C2 <- (indiffs[1] - indiffs[length(indiffs)]) < 0.1
  
  return(c(C1 = C1, C2 = C2))
  
}

#' Get discount function from model
#' 
#' Access the name of the discount function of a model.
#' @param mod A temporal discounting model.
#' @returns The name of the discount function.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcnm(td_bc_single_ptpt)
#' discount_function(mod)
#' }
#' @export
discount_function <- function(mod) mod$config$discount_function$name