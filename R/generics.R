
#' @export
print.td_bcnm <- function(x, ...) {
  cat(sprintf('\nTemporal discounting binary choice model\n\n'))
  cat(sprintf('Discount function: %s, with coefficients:\n\n', x$config$discount_function$name))
  print(coef(x))
  cat(sprintf('\nConfig:\n'))
  for (comp_name in c('noise_dist', 'gamma_scale', 'transform')) {
    cat(sprintf(' %s: %s\n', comp_name, x$config[[comp_name]]))
  }
  cat(sprintf('\nED50: %s\n', ED50(x)))
  cat(sprintf('AUC: %s\n', AUC(x, verbose = F)))
  cat(sprintf('BIC: %s\n', BIC(x)))
}

#' @export
print.td_bclm <- function(x, ...) {
  cat(sprintf('\nTemporal discounting binary choice linear model\n\n'))
  cat(sprintf('Discount function: %s from model %s, with coefficients:\n\n',
              x$config$discount_function$name,
              x$config$model))
  print(coef(x))
  NextMethod()
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
print.td_ddm <- function(x, ...) {
  cat(sprintf('\nTemporal discounting drift diffusion model\n\n'))
  cat(sprintf('Discount function: %s\nCoefficients:\n\n', x$config$discount_function$name))
  print(coef(x))
  cat(sprintf('\n"%s" transform applied to drift rates.\n', x$config$drift_transform$name))
  cat(sprintf('\nED50: %s\n', ED50(x)))
  cat(sprintf('AUC: %s\n', AUC(x, verbose = F)))
  cat(sprintf('BIC: %s\n', BIC(x)))
}


#' @export
print.td_fn <- function(x, ...) {
  obj <- x
  cat(sprintf('\n"%s" temporal discounting function\n\n', obj$name))
  
  code <- deparse(body(obj$fn), width.cutoff = 500)
  code <- gsub('p\\["([^"]+)"\\]', '\\1', code)
  code <- gsub('data\\$', '', code)
  cat(sprintf('Indifference points:\n'))
  cat(paste(code, collapse = "\n"))
  
  cat(sprintf('\n\nParameter limits:\n'))
  for (par in names(obj$par_lims)) {
    cat(sprintf('%s < %s < %s\n', obj$par_lims[[par]][1], par, obj$par_lims[[par]][2]))
  }
}

predict_indiffs <- function(object, newdata) {
  
  # Predict indiffs for an object that has:
  #   object$config$discount_function$fn
  #   coef(object)
  
  indiff_func <- object$config$discount_function$fn
  indiffs <- indiff_func(newdata, coef(object))
  names(indiffs) <- NULL
  return(indiffs)
  
}

#' Model Predictions
#'
#' Generate predictions from a temporal discounting binary choice model.
#' @param object A temporal discounting binary choice model. See \code{\link{td_bcnm}}.
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. As in predict.glm, \code{"link"} (default) and \code{"response"} give predictions on the scales of the linear predictors and response variable, respectively. \code{"indiff"} gives predicted indifference points. For predicting indifference points, \code{newdata} needs only a \code{del} column.
#' @param ... Additional arguments currently not used.
#' @family nonlinear binary choice model functions
#' @return A vector of predictions.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'hyperbolic')
#' indiffs <- predict(mod, newdata = data.frame(del = 1:100), type = 'indiff')
#' }
#' @export
predict.td_bcnm <- function(object, newdata = NULL, type = c('link', 'response', 'indiff'), ...) {
  
  if (is.null(newdata)) {
    newdata <- object$data
  }
  
  type <- match.arg(type)
  
  if (type == 'link') {
    
    score_func <- do.call(get_score_func_frame, object$config)
    scores <- score_func(newdata, coef(object))
    return(scores)
    
  } else if (type == 'response') {
    
    prob_mod <- do.call(get_prob_mod_frame, object$config)
    probs <- prob_mod(newdata, coef(object))
    return(probs)
    
  } else if (type == 'indiff') {
    
    return(predict_indiffs(object, newdata))
    
    # indiff_func <- object$config$discount_function$fn
    # indiffs <- indiff_func(newdata, coef(object))
    # names(indiffs) <- NULL
    # return(indiffs)
    
  }
}

#' Model Predictions
#'
#' Generate predictions from a temporal discounting binary choice linear model.
#' @param object A temporal discounting binary choice linear model. See \code{td_bclm}.
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. For \code{'indiff'} (default) gives predicted indifference points. In this case, \code{newdata} needs only a \code{del} column. For all other values (e.g. \code{"link"}, \code{"response"}), this function is just a wrapper to \code{predict.glm()}.
#' @param ... Additional arguments passed to predict.glm if type != \code{'indiff'}.
#' @family linear binary choice model functions
#' @return A vector of predictions.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bclm(td_bc_single_ptpt, model = 'hyperbolic.1')
#' indiffs <- predict(mod, newdata = data.frame(del = 1:100), type = 'indiff')
#' }
#' @export
predict.td_bclm <- function(object, newdata = NULL, type = c('indiff', 'link', 'response', 'terms'), ...) {

  if (is.null(newdata)) {
    newdata <- object$data
  }
  
  type <- match.arg(type)
  if (type == 'indiff') {
    
    return(predict_indiffs(object, newdata))
    
  } else {
    
    newdata <- add_beta_terms(newdata, model = object$config$model)
    preds <- predict.glm(object, newdata = newdata, type = type, ...)
    return(preds)
    
  }
}

#' Model Predictions
#'
#' Generate predictions from a temporal discounting indifference point model
#' @param object A temporal discounting indifference point model. See \code{td_ipm}.
#' @param newdata A data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type Type of prediction, either \code{'indiff'} (indifference points) or \code{'response'} (whether the participants would is predicted to choose the immediate (1) or delayed reward (0)).
#' @param ... Additional arguments currently not used.
#' @family indifference point model functions
#' @return A vector of predictions.
#' @examples
#' \dontrun{
#' data("td_ip_simulated_ptpt")
#' mod <- td_ipm(td_ip_simulated_ptpt, discount_function = 'hyperbolic')
#' indiffs <- predict(mod, del = 1:100)
#' indiffs <- predict(mod, newdata = data.frame(del = 1:100))
#' }
#' @export
predict.td_ipm <- function(object, newdata = NULL, type = c('indiff', 'response'), ...) {
  
  if (is.null(newdata)) {
    if (length(list(...)) > 0) {
      newdata <- data.frame(...) # to enable predict(mod, del = 1:100) type syntax
    } else {
      newdata <- object$data
    }
  }
  
  indiff_func <- object$config$discount_function$fn
  indiffs <- indiff_func(newdata, coef(object))
  names(indiffs) <- NULL
  
  type <- match.arg(type)
  if (type == 'indiff') {
    out <- indiffs
  } else if (type == 'response') {
    validate_td_data(newdata,
                     required_columns = c('val_imm', 'val_del'))
    out <- as.numeric((newdata$val_imm / newdata$val_del) > indiffs)
  }
  
  return(out)
  
}

#' Model Predictions
#'
#' Generate predictions from a temporal discounting drift diffusion model.
#' @param object A temporal discounting drift diffusion model. See \code{\link{td_ddm}}.
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. As in predict.glm, \code{"link"} (default) and \code{"response"} give predictions on the scales of the linear predictors and response variable, respectively. \code{"indiff"} gives predicted indifference points. For predicting indifference points, \code{newdata} needs only a \code{del} column. \code{"rt"} gives predicted reaction times.
#' @param ... Additional arguments currently not used.
#' @family drift diffusion model functions
#' @return A vector of predictions.
#' @note
#' When \code{type = 'rt'}, expected RTs are computed irrespective of which reward was selected, per equation 5 in \href{https://doi.org/10.1016/j.jmp.2009.01.006}{Grasman, Wagenmakers, & van der Maas (2009)}.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_ddm(td_bc_single_ptpt, discount_function = 'exponential')
#' pred_rts <- predict(mod, type = 'rt')
#' }
#' @export
predict.td_ddm <- function(object, newdata = NULL, type = c('indiff', 'link', 'response', 'rt'), ...) {
  
  if (is.null(newdata)) {
    newdata <- object$data
  }
  
  type <- match.arg(type)
  if (type == 'indiff') {
    
    return(predict_indiffs(object, newdata))
    
  } else if (type == 'link') {
    
    linpred_func <- do.call(get_linpred_func_ddm, object$config)
    return(linpred_func(newdata, coef(object)))
    
  } else if (type == 'response') {
    
    # Compute drift rates
    linpred_func <- do.call(get_linpred_func_ddm, object$config)
    drifts <- linpred_func(newdata, coef(object))
    # Compute probability of immediate reward (i.e., absorption at upper barrier)
    return(pimm_ddm(drifts, coef(object)))
    
  } else if (type == 'rt') {

    # Compute drift rates
    linpred_func <- do.call(get_linpred_func_ddm, object$config)
    cf <- coef(object)
    drifts <- linpred_func(newdata, cf)
    
    # Compute expected reaction times
    return(ddm_predicted_rts(drifts, cf))
    
  }
}

#' Get fitted values
#' 
#' Get fitted values of a temporal discounting binary choice model.
#' @param object An object of class \code{\link{td_bcnm}}.
#' @param ... Additional arguments currently not used.
#' @family nonlinear binary choice model functions
#' @return A named vector of fitted values.
#' @export
fitted.td_bcnm <- function(object, ...) {predict(object, type = 'response')}

#' Get fitted values
#' 
#' Get fitted values of a temporal discounting indifference point model.
#' @param object An object of class \code{\link{td_ipm}}.
#' @param ... Additional arguments currently not used.
#' @family indifference point model functions
#' @return A named vector of fitted values.
#' @export
fitted.td_ipm <- function(object, ...) {predict(object)}

#' Get fitted values
#' 
#' Get fitted values of a temporal discounting drift diffusion model.
#' @param object An object of class \code{\link{td_ddm}}.
#' @param ... Additional arguments currently not used.
#' @family drift diffusion model functions
#' @return A named vector of fitted values.
#' @export
fitted.td_ddm <- function(object, ...) {predict(object, type = 'response')}

#' Extract model coefficients
#' 
#' Get coefficients of a temporal discounting drift diffusion model.
#' @param object An object of class \code{\link{td_ddm}}.
#' @param type A string specifying which coefficients to extract. \code{'all'} extracts them all, \code{'ddm'} extracts only DDM-specific parameters, and \code{'df'} extracts only discount function parameters.
#' @param ... Additional arguments currently not used.
#' @family drift diffusion model functions
#' @return A named vector of coefficients.
#' @export
coef.td_ddm <- function(object, type = 'all', ...) {object$optim$par}

#' Extract model coefficients
#' 
#' Get coefficients of a temporal discounting binary choice nonlinear model.
#' @param object An object of class \code{td_bcnm}.
#' @param ... Additional arguments currently not used.
#' @family nonlinear binary choice model functions
#' @return A named vector of coefficients.
#' @export
coef.td_bcnm <- function(object, ...) {object$optim$par}

#' Extract model coefficients
#' 
#' Get coefficients of a temporal discounting binary choice linear model.
#' @param object An object of class \code{td_bcnm}.
#' @param df_par Boolean specifying whether the coefficients returned should be the parameters of a discount function (versus the beta parameters from the regression).
#' @param ... Additional arguments currently not used.
#' @family linear binary choice model functions.
#' @return A named vector of coefficients.
#' @export
coef.td_bclm <- function(object, df_par = TRUE, ...) {
  if (df_par) {
    # In terms of discount function parameters
    p <- object$coefficients
    d <- object$config$model
    if (d == 'hyperbolic.1') {
      cf <- c('k' = unname(p['.B2']/p['.B1']))
    } else if (d == 'hyperbolic.2') {
      cf <- c('k' = unname(exp(p['.B2']/p['.B1'])))
    } else if (d == 'exponential.1') {
      cf <- c('k' = unname(p['.B2']/p['.B1']))
    } else if (d == 'exponential.2') {
      cf <- c('k' = unname(exp(p['.B2']/p['.B1'])))
    } else if (d == 'scaled-exponential') {
      cf <- c('k' = unname(p['.B2']/p['.B1']),
              'w' = unname(exp(-p['.B3']/p['.B1'])))
    } else if (d == 'nonlinear-time-hyperbolic') {
      cf <- c('k' = unname(exp(p['.B3']/p['.B1'])),
              's' = unname(p['.B2']/p['.B1']))
    } else if (d == 'nonlinear-time-exponential') {
      cf <- c('k' = unname(exp(p['.B3']/p['.B1'])),
              's' = unname(p['.B2']/p['.B1']))
    } else if (d == 'itch') {
      cf <- object$coefficients
    } else if (d == 'naive') {
      cf <- object$coefficients
    }
  } else {
    cf <- object$coefficients
  }
  return(cf)
}

#' Extract model coefficients
#' 
#' Get coefficients of a temporal discounting indifference point model.
#' @param object An object of class \code{td_ipm}
#' @param ... Additional arguments currently not used.
#' @family indifference point model functions
#' @return A named vector of coefficients.
#' @export
coef.td_ipm <- function(object, ...) {object$optim$par}

#' Residuals from temporal discounting model
#'
#' Get residuals from a temporal discounting binary choice nonlinear model.
#' @param object A temporal discounting binary choice model. See \code{td_bcnm}.
#' @param type The type of residuals to be returned. See \code{residuals.glm}.
#' @param ... Additional arguments currently not used.
#' @family nonlinear binary choice model functions
#' @return A vector of residuals.
#' @export
residuals.td_bcnm <- function(object, type = c('deviance', 'pearson', 'response'), ...) {
  
  # args <- list(...)
  # type <- args$type
  # type <- match.arg(type, choices = c('deviance', 'pearson', 'response'))
  type <- match.arg(type)
  
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
#' Get residuals from a temporal discounting indifference point model.
#' @param object A temporal discounting model. See \code{td_bcnm}.
#' @param type The type of residuals to be returned. See \code{residuals.nls}.
#' @param ... Additional arguments currently not used.
#' @family indifference point model functions
#' @return A vector of residuals.
#' @export
residuals.td_ipm <- function(object, type = c('response', 'pearson'), ...) {
  
  # args <- list(...)
  # type <- match.arg(args$type, choices = c('response', 'pearson'))
  type <- match.arg(type)
  
  data <- object$data
  if ('indiff' %in% names(data)) {
    if (type == 'response') {
      y <- data$indiff
      yhat <- fitted(object)
      val <- y - yhat
    } else if (type == 'pearson') {
      # From residuals.nls
      val <- residuals(object, type = 'response')
      std <- sqrt(sum(val^2)/(length(val) - length(coef(object))))
      val <- val/std
    }
  } else {
    stop('Data was not fit directly on indifference points, so residuals cannot be computed.')
  }
  
  return(val)
}

#' Extract log-likelihood
#' 
#' Compute log-likelihood for a temporal discounting binary choice nonlinear model.
#' @param mod An object of class \code{td_bcnm}
#' @param ... Additional arguments currently not used.
#' @family nonlinear binary choice model functions
#' @export
logLik.td_bcnm <- function(mod, ...) {
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
#' @param ... Additional arguments currently not used.
#' @family indifference point model functions
#' @export
logLik.td_ipm <- function(mod, ...) {
  
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

#' Extract log-likelihood
#' 
#' Compute log-likelihood for a temporal discounting drift diffusion model.
#' @param mod An object of class \code{td_bcnm}.
#' @param type Should probabilities /probability densities be computed for responses and RTs (\code{'resp_rt'}, default) or responses only (\code{'resp'})?
#' @param ... Additional arguments currently not used.
#' @family drift diffusion model functions
#' @export
logLik.td_ddm <- function(mod, type = c('resp_rt', 'resp', 'rt'), ...) {
  type <- match.arg(type)
  if (type == 'resp_rt') {
    prob_func <- do.call(get_prob_func_ddm, mod$config)
    p <- prob_func(mod$data, coef(mod))
    val <- sum(log(p))
  } else if (type == 'resp') {
    p <- laplace_smooth(predict(mod, type = 'response'))
    x <- mod$data$imm_chosen
    val <- sum(ll(p, x))
  }
  attr(val, "nobs") <- nrow(mod$data)
  attr(val, "df") <- length(coef(mod))
  class(val) <- "logLik"
  return(val)
}

#' Model deviance
#' 
#' Compute deviance for a temporal discounting binary choice model.
#' @param mod An object of class \code{td_bcnm}.
#' @param ... Additional arguments currently not used.
#' @family nonlinear binary choice model functions
#' @export
deviance.td_bcnm <- function(mod, ...) return(-2*logLik.td_bcnm(mod))

#' Model deviance
#' 
#' Compute deviance for a temporal discounting drift diffusion model.
#' @param mod An object of class \code{td_ddm}.
#' @param ... Additional arguments currently not used.
#' @family drift diffusion model functions
#' @export
deviance.td_ddm <- function(mod, ...) return(-2*logLik.td_ddm(mod))

#' Plot models
#'
#' Plot delay discounting models.
#' @param x A delay discounting model. See \code{dd_prob_model} and \code{dd_det_model}.
#' @param type Type of plot to generate.
#' @param del Plots data for a particular delay.
#' @param val_del Plots data for a particular delayed value.
#' @param legend Logical: display a legend? Only relevant for \code{type = 'summary'} and \code{type = 'rt'}.
#' @param p_lines Numerical vector. When \code{type = 'summary'} the discount curve, where the probability of selecting the immediate reward is 0.5, is plotted. \code{p_lines} allows you to specify other probabilities for which similar curves should be plotted (only applicable for probabilistic models, e.g. \code{td_bcnm}, \code{td_bclm} and \code{td_ddm}).
#' @param p_tol If \code{p_lines} is not \code{NULL}, what is the maximum distance that estimated probabilities can be from their true values? Smaller values results in slower plot generation.
#' @param verbose Whether to print info about, e.g., setting del = ED50 when \code{type = 'endpoints'}.
#' @param q_lines When \code{type = 'rt'}, plot dashed lines for which quantiles of the predicted RT distribution? Default is 0.025 and 0.975 (i.e., a quantile-based 95\% confidence interval).
#' @param ... Additional arguments to \code{plot()}.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bclm(td_bc_single_ptpt, model = 'hyperbolic.1')
#' plot(mod, type = 'summary', p_lines = c(0.25, 0.75), log = 'x')
#' plot(mod, type = 'endpoints')
#' }
#' @export
plot.td_um <- function(x,
                       type = c('summary', 'endpoints', 'link', 'rt'),
                       legend = TRUE,
                       p_lines = NULL,
                       p_tol = 0.001,
                       verbose = TRUE,
                       del = NULL,
                       val_del = NULL,
                       q_lines = c(0.025, 0.975),
                       ...) {
  
  type <- match.arg(type)

  if (type == 'summary') {
    
    # Plot of binary choices or indifference points overlaid with discount function
    
    data <- x$data
    max_del <- max(data$del)
    min_del <- min(data$del)
    plotting_delays <- seq(min_del, max_del, length.out = 1000)
    if (is.null(val_del) & ('val_del' %in% names(x$data))) {
      val_del <- mean(x$data$val_del)
      if (verbose) {
        cat(sprintf('Plotting indifference curve for val_del = %s (mean of val_del from data used to fit model). Override this behaviour by setting the `val_del` argument to plot() or set verbose = F to suppress this message.\n', val_del))
      }
    }
    pred_indiffs <- predict(x,
                            newdata = data.frame(del = plotting_delays,
                                                 val_del = val_del %def% NA),
                            type = 'indiff')
    
    # Set up axes
    plot(NA, NA,
         xlim = c(min_del, max_del), ylim = c(0, 1),
         xlab = 'Delay',
         ylab = 'val_imm / val_del',
         ...)
    
    # Plot indifference curve
    lines(pred_indiffs ~ plotting_delays)
    
    if (!is.null(p_lines)) {
      classes <- c('td_bcnm', 'td_bclm', 'td_ddm')
      if (!inherits(x, classes)) {
        stop(sprintf('p_lines can only be used for models of the following classes:\n%s',
                     paste('-', classes, collapse = '\n')))
      }
      # Plot curves for other probabilities
      
      # Because of the overhead of individual calls to predict(), exhaustive grid search
      # is faster than uniroot() or similar to invert predict()
      
      # Call predict() once on a big grid
      val_imm_cands <- seq(0, val_del, length.out = ceiling(1/p_tol + 1))
      grid <- data.frame(val_del = val_del,
                         del = rep(plotting_delays, each = length(val_imm_cands)),
                         val_imm = rep(val_imm_cands, times = length(plotting_delays)))
      grid$p <- predict(x, grid, type = 'response')
      
      # Split the grid by delay
      # Using split() with a numerical index is faster than calling tapply() or similar
      split_idx <- rep(seq_along(plotting_delays), each = length(val_imm_cands))
      subgrid_list <- split(grid, split_idx)
      for (p in p_lines) {
        # Get the val_imm producing (close to) the desired p at each delay
        val_imm <- vapply(subgrid_list, function(subgrid) {
          if (max(subgrid$p) < p | min(subgrid$p) > p) {
            return(NA)
          } else {
            return(subgrid$val_imm[which.min((subgrid$p - p)**2)])
          }
        }, numeric(1))
        # Plot
        y <- val_imm / val_del
        lines(y ~ plotting_delays, lty = 'dashed')
      }
    }
    
    if ('indiff' %in% colnames(data)) {
      # Plot empirical indifference points
      points(indiff ~ del, data = data)
    } else {
      # Plot binary choices, immediate in red, delayed in blue
      data$rel_val <- data$val_imm / data$val_del
      points(rel_val ~ del, col = "#F8766D",
             data = data[data$imm_chosen, ])
      points(rel_val ~ del, col = "#00BFC4",
             data = data[!data$imm_chosen, ])
      if (legend) {
        legend("topright",
               inset = 0.02,
               title = 'Choices',
               legend = c("Imm.", "Del."),
               col = c("#F8766D", "#00BFC4"),
               pch = 1,
               box.lty = 0, # No border
               bg = rgb(1, 1, 1, 0.5)) # Background color with transparency
      }
    }
    
    title(x$config$discount_function$name)
    
  } else {
    if (is(x, 'td_ipm')) {
      
      stop('Only the "summary" plot type is applicable for models of type td_ipm.')
      
    } else {
      if (type == 'endpoints') {
        
        # Plot of psychometric curve
        
        if (is.null(val_del)) {
          val_del <- mean(x$data$val_del)
          if (x$config$gamma_scale %def% 'none' != 'none') {
            if (verbose) {
              cat(sprintf('gamma parameter (steepness of psychometric curve curve) is scaled by val_del.\nThus, the curve will have different steepness for a different value of val_del.\nDefaulting to val_del = %s (mean of val_del from data used to fit model).\nUse the `val_del` argument to specify a custom value or use verbose = F to suppress this message.\n', val_del))
            }
          }
        }
        
        if (is.null(del)) {
          del <- ED50(x, val_del = val_del)
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
             ylab = 'Prob. Imm',
             ...)
        
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
        if (is(x, 'td_bcnm')) {
          score_func <- do.call(get_score_func_frame, x$config)
          scores <- score_func(x$data, coef(x))
        } else if (is(x, 'td_bclm')) {
          scores <- x$linear.predictors
        } else if (is(x, 'td_ddm')) {
          linpred_func <- do.call(get_linpred_func_ddm, x$config)
          scores <- linpred_func(x$data, coef(x))
        }
        lim <- max(abs(min(scores)), abs(max(scores)))
        # Plot choices
        plot(x$data$imm_chosen ~ scores,
             ylim = c(0, 1),
             xlim = c(-lim, lim),
             ylab = 'Reward',
             xlab = 'Linear predictor',
             yaxt = "n",
             ...)
        # Custom y axis ticks
        n_yticks <- 5
        yticks <- seq(0, 1, length.out = n_yticks)
        ytick_labs <- sprintf('%s', round(yticks, 2))
        ytick_labs[1] <- sprintf('Del.\n0')
        ytick_labs[n_yticks] <- sprintf('Imm.\n1')
        axis(2, at = yticks, labels = ytick_labs)
        # Plot probabilities
        plotting_scores <- seq(-lim, lim, length.out = 1000)
        pimm <- switch(class(x)[1],
                       'td_bcnm' = {
                         prob_func <- do.call(get_prob_func_frame, x$config)
                         prob_func(plotting_scores, coef(x))
                       },
                       'td_bclm' = {
                         x$family$linkinv(plotting_scores)
                       },
                       'td_ddm' = {
                         pimm_ddm(plotting_scores, coef(x))
                       }
        )
        lines(pimm ~ plotting_scores)
        
      } else if (type == 'rt') {
        
        if (!is(x, 'td_ddm')) {
          stop('type = "rt" is only applicable for models of class td_ddm')
        }
        
        # Get range of linear predictors
        linpred_func <- do.call(get_linpred_func_ddm, x$config)
        linpreds <- linpred_func(x$data, coef(x))
        x$data$linpreds <- linpreds
        linpred_lim <- max(abs(min(linpreds)), abs(max(linpreds)))
        
        # Plot RTs against linear predictors
        plot(rt ~ linpreds, data = x$data,
             type = 'n', # Don't show for now, just setting up axes
             # ylim = c(min(x$data$rt), max(x$data$rt)),
             xlim = c(-linpred_lim, linpred_lim),
             xlab = 'Drift rate',
             ylab = 'RT (s)',
             ...)
        
        # Plot model predictions
        plotting_linpreds <- seq(-linpred_lim, linpred_lim, length.out = 1000)
        
        # Plot predicted RTs
        cf <- coef(x)
        pred_rts <- ddm_predicted_rts(plotting_linpreds, cf)
        lines(pred_rts ~ plotting_linpreds)
        
        # Plot quantile lines
        for (p in q_lines) {
          bounds <- vapply(plotting_linpreds, function(drift) {
            RWiener::qwiener(p = p, delta = drift,
                             alpha = cf['alpha'], tau = cf['tau'], beta = cf['beta'],
                             resp = 'both')
          }, numeric(1))
          lines(bounds ~ plotting_linpreds, lty = 'dashed')
        }
        
        # Overlay actual data
        points(rt ~ linpreds, col = 'red',
               data = x$data[x$data$imm_chosen, ])
        points(rt ~ linpreds, col = 'blue',
               data = x$data[!x$data$imm_chosen, ])
        
        if (legend) {
          legend("topright",
                 inset = 0.02,
                 title = 'Choices',
                 legend = c("Imm.", "Del."),
                 col = c("#F8766D", "#00BFC4"),
                 pch = 1,
                 box.lty = 0, # No border
                 bg = rgb(1, 1, 1, 0.5)) # Background color with transparency
        }
        
      }
    }
  }
}
