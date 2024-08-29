
#' @export
print.td_bcm <- function(x, ...) {
  cat(sprintf('\nTemporal discounting binary choice model\n\n'))
  cat(sprintf('Discount function: %s, with coefficients:\n\n', x$config$discount_function$name))
  print(coef(x))
  cat(sprintf('\nConfig:\n'))
  for (comp_name in c('noise_dist', 'gamma_scale', 'transform')) {
    cat(sprintf(' %s: %s\n', comp_name, x$config[[comp_name]]))
  }
  cat(sprintf('\nED50: %s\n', ED50(x)))
  cat(sprintf('AUC: %s\n', AUC(x, verbose = F)))
  cat(sprintf('BIC: %s', BIC(x)))
}

#' @export
print.td_ipm <- function(x, ...) {
  cat(sprintf('\nTemporal discounting indifference point model\n\n'))
  cat(sprintf('Discount function: %s, with coefficients:\n\n', x$config$discount_function$name))
  print(coef(x))
  cat(sprintf('\nED50: %s\n', ED50(x)))
  cat(sprintf('AUC: %s\n', AUC(x, verbose = F)))
}

#' @export
print.td_fn <- function(x, ...) {
  obj <- x
  cat(sprintf('\nTemporal discounting function: "%s"\n\n', obj$name))
  print(obj$fn)
  cat(sprintf('\n'))
  for (par in names(obj$par_lims)) {
    cat(sprintf('%s < %s < %s\n', obj$par_lims[[par]][1], par, obj$par_lims[[par]][2]))
  }
}

#' Model Predictions
#'
#' Generate predictions from a temporal discounting binary choice model
#' @param object A temporal discounting binary choice model. See \code{td_bcm}.
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. As in predict.glm, \code{"link"} (default) and \code{"response"} give predictions on the scales of the linear predictors and response variable, respectively. \code{"indiff"} gives predicted indifference points. In this case, \code{newdata} needs only a \code{del} column.
#' @param ... Additional arguments currently not used.
#' @return A vector of predictions
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcm(td_bc_single_ptpt, discount_function = 'hyperbolic')
#' indiffs <- predict(mod, newdata = data.frame(del = 1:100), type = 'indiff')
#' }
#' @export
predict.td_bcm <- function(object, ...) {
  
  args <- list(...)
  newdata <- args$newdata
  if (is.null(newdata)) {
    newdata <- object$data
  }
  
  type <- match.arg(args$type, choices = c('link', 'response', 'indiff'))
  
  if (type == 'link') {
    
    score_func <- do.call(get_score_func_frame, object$config)
    scores <- score_func(newdata, coef(object))
    return(scores)
    
  } else if (type == 'response') {
    
    prob_mod <- do.call(get_prob_mod_frame, object$config)
    probs <- prob_mod(newdata, coef(object))
    return(probs)
    
  } else if (type == 'indiff') {
    
    indiff_func <- object$config$discount_function$fn
    indiffs <- indiff_func(newdata, coef(object))
    names(indiffs) <- NULL
    return(indiffs)
    
  }
}

#' Model Predictions
#'
#' Generate predictions from a temporal discounting binary choice linear model
#' @param object A temporal discounting binary choice linear model. See \code{td_bclm}.
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. For \code{'indiff'} (default) gives predicted indifference points. In this case, \code{newdata} needs only a \code{del} column. For all other values (e.g. \code{"link"}, \code{"response"}), this function is just a wrapper to \code{predict.glm()}
#' @param ... Additional arguments passed to predict.glm if type != \code{'indiff'}
#' @return A vector of predictions
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bclm(td_bc_single_ptpt, model = 'hyperbolic.1')
#' indiffs <- predict(mod, newdata = data.frame(del = 1:100), type = 'indiff')
#' }
#' @export
predict.td_bclm <- function(object, ...) {
  args <- list(...)
  newdata <- args$newdata
  if (is.null(newdata)) {
    newdata <- object$data
  }
  
  type <- args$type
  if (type == 'indiff') {
    
    return(predict.td_bcm(object, newdata = newdata, type = type))
    
  } else {
    
    args <- list(...)
    args[c('newdata', 'type')] <- NULL
    newdata <- add_beta_terms(newdata, model = object$config$model)
    preds <- do.call(
      predict.glm,
      c(
        list(
          object,
          newdata = newdata,
          type = type
        ),
        args
      )
    )
    return(preds)
    
  }
}

#' Model Predictions
#'
#' Generate predictions from a temporal discounting indifference point model
#' @param object A temporal discounting indifference point model. See \code{td_ipm}.
#' @param del Vector of delays for which to predict indifference points. If omitted, the data used to fit the model will be used for prediction.
#' @param newdata Optionally, a data frame to use for prediction. This overrides the \code{del} argument.
#' @param ... Additional arguments currently not used.
#' @return A vector of predictions
#' @examples
#' \dontrun{
#' data("td_ip_simulated_ptpt")
#' mod <- td_ipm(td_ip_simulated_ptpt, discount_function = 'hyperbolic')
#' indiffs <- predict(mod, newdata = data.frame(del = 1:100), type = 'indiff')
#' }
#' @export
predict.td_ipm <- function(object, ...) {
  
  args <- list(...)
  newdata <- args$newdata
  del <- args$del
  if (is.null(newdata)) {
    if (is.null(del)) {
      newdata <- object$data
    } else {
      newdata <- data.frame(del = del)
    }
  }
  
  indiff_func <- object$config$discount_function$fn
  indiffs <- indiff_func(newdata, coef(object))
  names(indiffs) <- NULL
  return(indiffs)
  
}

#' Get fitted values
#' 
#' Get fitted values of a temporal discounting binary choice model
#' @param object An object of class \code{td_bcm}
#' @param ... Additional arguments currently not used.
#' @return A named vector of fitted values
#' @export
fitted.td_bcm <- function(object, ...) {predict(object, type = 'response')}

#' Get fitted values
#' 
#' Get fitted values of a temporal discounting indifference point model
#' @param object An object of class \code{td_ipm}
#' @param ... Additional arguments currently not used.
#' @return A named vector of fitted values
#' @export
fitted.td_ipm <- function(object, ...) {predict(object)}

#' Extract model coefficients
#' 
#' Get coefficients of a temporal discounting binary choice model
#' @param object An object of class \code{td_bcm}
#' @param ... Additional arguments currently not used.
#' @return A named vector of coefficients
#' @export
coef.td_bcm <- function(object, ...) {object$optim$par}

#' Extract model coefficients
#' 
#' Get coefficients of a temporal discounting binary choice model
#' @param object An object of class \code{td_bcm}
#' @param df_par Boolean specifying whether the coefficients returned should be the parameters of a discount function (versus the beta parameters from the regression)
#' @param ... Additional arguments currently not used.
#' @return A named vector of coefficients
#' @export
coef.td_bclm <- function(object, ...) {
  args <- list(...)
  df_par <- args$df_par %def% T
  if (df_par) {
    # In terms of discount function parameters
    p <- object$coefficients
    B <- unname(c(p['B1'], p['B2'], p['B3']))
    d <- object$config$model
    if (d == 'hyperbolic.1') {
      cf <- c('k' = B[2]/B[1])
    } else if (d == 'hyperbolic.2') {
      cf <- c('k' = exp(B[2]/B[1]))
    } else if (d == 'exponential.1') {
      cf <- c('k' = B[2]/B[1])
    } else if (d == 'exponential.2') {
      cf <- c('k' = exp(B[2]/B[1]))
    } else if (d == 'scaled-exponential') {
      cf <- c('k' = B[2]/B[1],
              'w' = exp(-B[3]/B[1]))
    } else if (d == 'nonlinear-time-hyperbolic') {
      cf <- c('k' = exp(B[3]/B[1]),
              's' = B[2]/B[1])
    } else if (d == 'nonlinear-time-exponential') {
      cf <- c('k' = exp(B[3]/B[1]),
              's' = B[2]/B[1])
    }
  } else {
    cf <- object$coefficients
  }
  return(cf)
}

#' Extract model coefficients
#' 
#' Get coefficients of a temporal discounting indifference point model
#' @param mod An object of class \code{td_ipm}
#' @param ... Additional arguments currently not used.
#' @return A named vector of coefficients
#' @export
coef.td_ipm <- function(object, ...) {object$optim$par}

#' Residuals from temporal discounting model
#'
#' Get residuals from a temporal discounting binary choice model
#' @param object A temporal discounting binary choice model. See \code{td_bcm}.
#' @param type The type of residuals to be returned. See \code{residuals.glm}.
#' @param ... Additional arguments currently not used.
#' @return A vector of residuals
#' @export
residuals.td_bcm <- function(object, ...) {
  
  args <- list(...)
  type <- args$type
  type <- match.arg(type, choices = c('deviance', 'pearson', 'response'))
  
  y <- object$data$imm_chosen
  yhat <- fitted(object)
  e <- y - yhat
  r <- switch (type,
               'deviance' = sign(e)*sqrt(-2*(y*log(yhat) + (1 - y)*log(1 - yhat))),
               'pearson' = e / sqrt(yhat[1]*(1 - yhat[1])),
               'response' = e
  )
  
  return(r)
}

#' Residuals from temporal discounting model
#'
#' Get residuals from a temporal discounting indifference point model
#' @param object A temporal discounting model. See \code{td_bcm}.
#' @param type The type of residuals to be returned. See \code{residuals.nls}.
#' @param ... Additional arguments currently not used.
#' @return A vector of residuals
#' @export
residuals.td_ipm <- function(object, ...) {
  
  args <- list(...)
  type <- match.arg(args$type, choices = c('response', 'pearson'))
  
  if (type == 'response') {
    y <- object$data$indiff
    yhat <- fitted(object)
    val <- y - yhat
  } else if (type == 'pearson') {
    # From residuals.nls
    val <- residuals(object, type = 'response')
    std <- sqrt(sum(val^2)/(length(val) - length(coef(object))))
    val <- val/std
  }
  
  return(val)
}

#' Extract log-likelihood
#' 
#' Compute log-likelihood for a temporal discounting binary choice model.
#' @param mod An object of class \code{td_bcm}
#' @export
logLik.td_bcm <- function(mod) {
  p <- laplace_smooth(predict(mod, type = 'response'))
  x <- mod$data$imm_chosen
  val <- sum(ll(p, x))
  attr(val, "nobs") <- nrow(mod$data)
  attr(val, "df") <- length(coef(mod))
  class(val) <- "logLik"
  return(val)
}

#' Extract log-likelihood
#' 
#' Compute log-likelihood for a temporal discounting indifference point model.
#' @param mod An object of class \code{td_ipm}
#' @export
logLik.td_ipm <- function(mod) {
  # From logLik.nls
  res <- residuals(mod)
  N <- length(res)
  w <- rep_len(1, N) # Always unweighted
  ## Note the trick for zero weights
  zw <- w == 0
  val <-  -N * (log(2 * pi) + 1 - log(N) - sum(log(w + zw)) + log(sum(w*res^2)))/2
  ## the formula here corresponds to estimating sigma^2.
  attr(val, "df") <- 1L + length(coef(mod))
  attr(val, "nobs") <- attr(val, "nall") <- sum(!zw)
  class(val) <- "logLik"
  return(val)
}


#' Plot models
#'
#' Plot delay discounting models
#' @param x A delay discounting model. See \code{dd_prob_model} and \code{dd_det_model}
#' @param type Type of plot to generate
#' @param ... Additional arguments to \code{plot()}
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bclm(td_bc_single_ptpt, model = 'hyperbolic.1')
#' plot(mod, type = 'summary')
#' plot(mod, type = 'endpoints')
#' }
#' @export
plot.td_um <- function(x, ...) {
  
  args <- list(...)
  type <- match.arg(args$type, choices = c('summary', 'endpoints', 'link'))
  verbose <- args$verbose %def% T
  # Remove args we used
  args[c('type', 'verbose')] <- NULL
  
  if (type == 'summary') {
    
    # Plot of binary choices or indifference points overlaid with discount function
    
    data <- x$data
    max_del <- max(data$del)
    min_del <- min(data$del)
    plotting_delays <- seq(min_del, max_del, length.out = 1000)
    pred_indiffs <- predict(x, newdata = data.frame(del = plotting_delays), type = 'indiff')
    
    # Set up axes
    do.call(
      plot,
      c(
        list(NA, NA,
             xlim = c(min_del, max_del), ylim = c(0, 1),
             xlab = 'Delay',
             ylab = 'val_imm / val_del'),
        args
      )
    )
    
    # Plot indifference curve
    lines(pred_indiffs ~ plotting_delays)
    
    # Visualize stochasticity---goal for later. For now, don't know how to do this for td_bclm
    # if (x$config$gamma_scale != 'none') {
    #   if (verbose) {
    #     cat(sprintf('gamma parameter (steepness of curve) is scaled by val_del.\nThus, the curve will have different steepness for a different value of val_del.\nDefaulting to val_del = %s (mean of val_del from data used to fit model).\nUse the `val_del` argument to specify a custom value.\n\n', val_del))
    #   }
    # }
    # p_range <- args$p_range %def% c(0.4, 0.6)
    # lower <- invert_decision_function(x, prob = p_range[1], del = plotting_delays)
    # upper <- invert_decision_function(x, prob = p_range[2], del = plotting_delays)
    # lines(lower ~ plotting_delays, lty = 'dashed')
    # lines(upper ~ plotting_delays, lty = 'dashed')
    
    if ('indiff' %in% colnames(data)) {
      # Plot empirical indifference points
      points(indiff ~ del, data = data)
    } else {
      # Plot binary choices, immediate in red, delayed in blue
      data$rel_val <- data$val_imm / data$val_del
      points(rel_val ~ del, col = 'red',
             data = subset(data, imm_chosen))
      points(rel_val ~ del, col = 'blue',
             data = subset(data, !imm_chosen))
    }
    title(x$config$discount_function$name)
    
  } else {
    if (is(x, 'td_ipm')) {
      
      stop('Only the "summary" plot type is applicable for models of type td_ipm.')
      
    } else {
      if (type == 'endpoints') {
        
        # Plot of psychometric curve
        
        args <- list(...)
        
        if ('val_del' %in% names(args)) {
          val_del <- args$val_del
        } else {
          val_del <- mean(x$data$val_del)
          if (x$config$gamma_scale %def% 'none' != 'none') {
            if (verbose) {
              cat(sprintf('gamma parameter (steepness of curve) is scaled by val_del.\nThus, the curve will have different steepness for a different value of val_del.\nDefaulting to val_del = %s (mean of val_del from data used to fit model).\nUse the `val_del` argument to specify a custom value.\n\n', val_del))
            }
          }
        }
        
        if ('del' %in% names(args)) {
          del <- args$del
        } else {
          del <- ED50(x)
          if (del == 'none') {
            del <- mean(c(min(x$data$del), max(c(x$data$del))))
            if (verbose) {
              cat(sprintf('ED50 is undefined. Therefore setting del=%s (halfway between min. delay and max. delay from fitting data).\nThis can be specified manually with the `del` argument.\n\n', del))
            }
          } else {
            if (verbose) {
              cat(sprintf('Setting del = %s (ED50) to center the curve.\nThis can be changed using the `del` argument.\n\n', del))
            }
          }
        }
        
        plotting_rs <- seq(0, 1, length.out = 1000)
        newdata <- data.frame(
          del = del,
          val_del = val_del,
          val_imm = plotting_rs*val_del
        )
        p <- predict(x, newdata = newdata, type = 'response')
        plot(p ~ plotting_rs, type = 'l',
             ylim = c(0, 1),
             xlab = 'val_imm/val_del',
             ylab = 'Prob. Imm')
        
        # If applicable, plot the choices at the given delay
        if (del %in% x$data$del) {
          sdf <- x$data[x$data$del == del, ]
          sdf$R <-sdf$val_imm / sdf$val_del
          points(imm_chosen ~ R, data = sdf)
        }
        
        title(sprintf('del = %s, val_del = %s', del, val_del))
        
      } else if (type == 'link') {
        
        # Plot of probabilities and data against linear predictors
        
        # Get score range
        if (is(x, 'td_bcm')) {
          score_func <- do.call(get_score_func_frame, x$config)
          scores <- score_func(x$data, coef(x))
        } else if (is(x, 'td_bclm')) {
          scores <- x$linear.predictors
        }
        lim <- max(abs(min(scores)), abs(max(scores)))
        # Plot choices
        plot(x$data$imm_chosen ~ scores,
             ylim = c(0, 1),
             xlim = c(-lim, lim),
             ylab = 'imm_chosen',
             xlab = 'Linear predictor')
        # Plot probabilities
        plotting_scores <- seq(-lim, lim, length.out = 1000)
        if (is(x, 'td_bcm')) {
          prob_func <- do.call(get_prob_func_frame, x$config)
          p <- prob_func(plotting_scores, coef(x))
        } else if (is(x, 'td_bclm')) {
          p <- x$family$linkinv(plotting_scores)
        }
        lines(p ~ plotting_scores)
        
      }
    }
  }
}

