
#' @export
print.td_bcm <- function(mod) {
  cat(sprintf('\nTemporal discounting binary choice model\n\n'))
  cat(sprintf('Discount function: %s, with coefficients:\n\n', mod$config$discount_function$name))
  print(coef(mod))
  cat(sprintf('\nConfig:\n'))
  for (comp_name in c('noise_dist', 'gamma_scale', 'transform')) {
    cat(sprintf(' %s: %s\n', comp_name, mod$config[[comp_name]]))
  }
  cat(sprintf('\nED50: %s\n', ED50(mod)))
  cat(sprintf('AUC: %s\n', AUC(mod, verbose = F)))
  cat(sprintf('BIC: %s', BIC(mod)))
}

#' @export
print.td_ipm <- function(mod) {
  cat(sprintf('\nTemporal discounting indifference point model\n\n'))
  cat(sprintf('Discount function: %s, with coefficients:\n\n', mod$config$discount_function$name))
  print(coef(mod))
  cat(sprintf('\nED50: %s\n', ED50(mod)))
  cat(sprintf('AUC: %s\n', AUC(mod, verbose = F)))
}

#' @export
print.td_fn <- function(obj) {
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
#' @param mod A temporal discounting binary choice model. See `td_bcm`.
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. As in predict.glm, `"link"` (default) and `"response"` give predictions on the scales of the linear predictors and response variable, respectively. `"indiff"` gives predicted indifference points. In this case, `newdata` needs only a `del` column.
#' @return A vector of predictions
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcm(td_bc_single_ptpt, discount_function = 'hyperbolic')
#' indiffs <- predict(mod, newdata = data.frame(del = 1:100), type = 'indiff')
#' }
#' @export
predict.td_bcm <- function(mod, newdata = NULL, type = c('link', 'response', 'indiff')) {
  if (is.null(newdata)) {
    newdata <- mod$data
  }
  
  type <- match.arg(type)
  
  if (type == 'link') {
    
    score_func <- do.call(get_score_func_frame, mod$config)
    scores <- score_func(newdata, coef(mod))
    return(scores)
    
  } else if (type == 'response') {
    
    prob_mod <- do.call(get_prob_mod_frame, mod$config)
    probs <- prob_mod(newdata, coef(mod))
    return(probs)
    
  } else if (type == 'indiff') {
    
    indiff_func <- mod$config$discount_function$fn
    indiffs <- indiff_func(newdata, coef(mod))
    names(indiffs) <- NULL
    return(indiffs)
    
  }
}

#' Model Predictions
#'
#' Generate predictions from a temporal discounting binary choice linear model
#' @param mod A temporal discounting binary choice linear model. See `td_bclm`.
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. For `"indiff"` (default) gives predicted indifference points. In this case, `newdata` needs only a `del` column. For all other values (e.g. `"link"`, `"response"`), this function is just a wrapper to `predict.glm()`
#' @param ... Additional arguments passed to predict.glm
#' @return A vector of predictions
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bclm(td_bc_single_ptpt, model = 'hyperbolic.1')
#' indiffs <- predict(mod, newdata = data.frame(del = 1:100), type = 'indiff')
#' }
#' @export
predict.td_bclm <- function(mod, newdata = NULL, type = 'indiff', ...) {
  if (is.null(newdata)) {
    newdata <- mod$data
  }
  
  if (type == 'indiff') {
    
    return(predict.td_bcm(mod, newdata = newdata, type = type))
    
  } else {
    
    newdata <- add_beta_terms(newdata, model = mod$config$model)
    return(predict.glm(mod, newdata = newdata, type = type, ...))
    
  }
}

#' Model Predictions
#'
#' Generate predictions from a temporal discounting indifference point model
#' @param mod A temporal discounting indifference point model. See `td_ipm`.
#' @param del Vector of delays for which to predict indifference points. If omitted, the data used to fit the model will be used for prediction.
#' @param newdata Optionally, a data frame to use for prediction. This overrides the `del` argument.
#' @param type The type of prediction required. As in predict.glm, `"link"` (default) and `"response"` give predictions on the scales of the linear predictors and response variable, respectively. `"indiff"` gives predicted indifference points. In this case, `newdata` needs only a `del` column.
#' @return A vector of predictions
#' @examples
#' \dontrun{
#' data("td_ip_simulated_ptpt")
#' mod <- td_ipm(td_ip_simulated_ptpt, discount_function = 'hyperbolic')
#' indiffs <- predict(mod, newdata = data.frame(del = 1:100), type = 'indiff')
#' }
#' @export
predict.td_ipm <- function(mod, del = NULL, newdata = NULL, ...) {
  if (is.null(newdata)) {
    if (is.null(del)) {
      newdata <- mod$data
    } else {
      newdata <- data.frame(del = del)
    }
  }
  
  indiff_func <- mod$config$discount_function$fn
  indiffs <- indiff_func(newdata, coef(mod))
  names(indiffs) <- NULL
  return(indiffs)
  
}

#' Get fitted values
#' 
#' Get fitted values of a temporal discounting binary choice model
#' @return A named vector of fitted values
#' @export
fitted.td_bcm <- function(mod) {predict(mod, type = 'response')}

#' Get fitted values
#' 
#' Get fitted values of a temporal discounting indifference point model
#' @return A named vector of fitted values
#' @export
fitted.td_ipm <- function(mod) {predict(mod)}

#' Extract model coefficients
#' 
#' Get coefficients of a temporal discounting binary choice model
#' @return A named vector of coefficients
#' @export
coef.td_bcm <- function(mod) {mod$optim$par}

#' Extract model coefficients
#' 
#' Get coefficients of a temporal discounting indifference point model
#' @return A named vector of coefficients
#' @export
coef.td_ipm <- function(mod) {mod$optim$par}

#' Residuals from temporal discounting model
#'
#' Get residuals from a temporal discounting binary choice model
#' @param mod A temporal discounting binary choice model. See `td_bcm`.
#' @param type The type of residuals to be returned. See `residuals.glm`.
#' @return A vector of residuals
#' @export
residuals.td_bcm <- function(mod,
                             type = c('deviance', 'pearson', 'response')) {
  
  type <- match.arg(type)
  
  y <- mod$data$imm_chosen
  yhat <- fitted(mod)
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
#' @param mod A temporal discounting model. See `td_bcm`.
#' @param type The type of residuals to be returned. See `residuals.nls`.
#' @return A vector of residuals
#' @export
residuals.td_ipm <- function(mod, type = c('response', 'pearson')) {
  
  type <- match.arg(type)
  
  if (type == 'response') {
    y <- mod$data$indiff
    yhat <- fitted(mod)
    val <- y - yhat
  } else if (type == 'pearson') {
    # From residuals.nls
    val <- residuals(mod, type = 'response')
    std <- sqrt(sum(val^2)/(length(val) - length(coef(mod))))
    val <- val/std
  }
  
  return(val)
}

#' Extract log-likelihood
#' 
#' Compute log-likelihood for a temporal discounting binary choice model.
#' @param mod An object of class `td_bcm`
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
#' @param mod An object of class `td_ipm`
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
#' @param mod A delay discounting model. See `dd_prob_model` and `dd_det_model`
#' @param type Type of plot to generate
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bclm(td_bc_single_ptpt, model = 'hyperbolic.1')
#' plot(mod, type = 'summary')
#' plot(mod, type = 'endpoints')
#' }
#' @export
plot.td_um <- function(mod, type = c('summary', 'endpoints', 'link'), ...) {
  
  type <- match.arg(type)
  
  if (type == 'summary') {
    
    # Plot of binary choices or indifference points overlaid with discount function
    
    data <- mod$data
    max_del <- max(data$del)
    min_del <- min(data$del)
    plotting_delays <- seq(min_del, max_del, length.out = 1000)
    pred_indiffs <- predict(mod, newdata = data.frame(del = plotting_delays), type = 'indiff')
    # Set up axes
    plot(NA, NA,
         xlim = c(min_del, max_del), ylim = c(0, 1),
         xlab = 'Delay',
         ylab = 'val_imm / val_del',
         ...)
    lines(pred_indiffs ~ plotting_delays)
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
    title(mod$config$discount_function$name)
    
  } else {
    if (is(mod, 'td_ipm')) {
      
      stop('Only the "summary" plot type is applicable for models of type td_ipm.')
      
    } else {
      if (type == 'endpoints') {
        
        # Plot of psychometric curve
        
        args <- list(...)
        
        if ('val_del' %in% names(args)) {
          val_del <- args$val_del
        } else {
          val_del <- mean(mod$data$val_del)
          if (mod$config$gamma_scale %||% 'none' != 'none') {
            cat(sprintf('gamma parameter (steepness of curve) is scaled by val_del.\nThus, the curve will have different steepness for a different value of val_del.\nDefaulting to val_del = %s (mean of val_del from data used to fit model).\nUse the `val_del` argument to specify a custom value.\n\n', val_del))
          }
        }
        
        if ('del' %in% names(args)) {
          del <- args$del
        } else {
          del <- ED50(mod)
          if (del == 'none') {
            del <- mean(c(min(mod$data$del), max(c(mod$data$del))))
            cat(sprintf('ED50 is undefined. Therefore setting del=%s (halfway between min. delay and max. delay from fitting data).\nThis can be specified manually with the `del` argument.\n\n', del))
          } else {
            cat(sprintf('Setting del = %s (ED50) to center the curve.\nThis can be changed using the `del` argument.\n\n', del))
          }
        }
        
        plotting_rs <- seq(0, 1, length.out = 1000)
        newdata <- data.frame(
          del = del,
          val_del = val_del,
          val_imm = plotting_rs*val_del
        )
        p <- predict(mod, newdata = newdata, type = 'response')
        plot(p ~ plotting_rs, type = 'l',
             ylim = c(0, 1),
             xlab = 'val_imm/val_del',
             ylab = 'Prob. Imm')
        
        # If applicable, plot the choices at the given delay
        if (del %in% mod$data$del) {
          sdf <- mod$data[mod$data$del == del, ]
          sdf$R <-sdf$val_imm / sdf$val_del
          points(imm_chosen ~ R, data = sdf)
        }
        
        title(sprintf('del = %s, val_del = %s', del, val_del))
        
      } else if (type == 'link') {
        
        # Plot of probabilities and data against linear predictors
        
        # Get score range
        if (is(mod, 'td_bcm')) {
          score_func <- do.call(get_score_func_frame, mod$config)
          scores <- score_func(mod$data, coef(mod))
        } else if (is(mod, 'td_bclm')) {
          scores <- mod$linear.predictors
        }
        lim <- max(abs(min(scores)), abs(max(scores)))
        # Plot choices
        plot(mod$data$imm_chosen ~ scores,
             ylim = c(0, 1),
             xlim = c(-lim, lim),
             ylab = 'imm_chosen',
             xlab = 'Linear predictor')
        # Plot probabilities
        plotting_scores <- seq(-lim, lim, length.out = 1000)
        if (is(mod, 'td_bcm')) {
          prob_func <- do.call(get_prob_func_frame, mod$config)
          p <- prob_func(plotting_scores, coef(mod))
        } else if (is(mod, 'td_bclm')) {
          p <- mod$family$linkinv(plotting_scores)
        }
        lines(p ~ plotting_scores)
        
      }
    }
  }
}

