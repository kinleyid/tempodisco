
tdfn <- function(fn_name) {
  fn <- switch (fn_name,
                'noise' = function(data, p) rep(p['i'], length(data$del)),
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
                  a <- approx(x = dels, y = p, xout = round(data$del, 10))
                  return(a$y)
                }
  )
  par_starts <- switch (fn_name,
                        'noise' = list(
                          i = exp(seq(-8, 0, length.out = 3))
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
  par_lims <- switch (fn_name,
                      'noise' = list(
                        i = c(0, 1)
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
  
  ED50 <- switch (fn_name,
                  "noise" = function(p) 'none',
                  "hyperbolic" = function(p) 1/p['k'],
                  "exponential" = function(p) log(2)/p['k'],
                  "inverse-q-exponential" = function(p) (2^(1/p['s']) - 1) / p['k'],
                  "nonlinear-time-hyperbolic" = function(p) (1/p['k']) ^ (1/p['s']),
                  "nonlinear-time-exponential" = function(p) (log(2)/p['k'])^(1/p['s']),
                  "scaled-exponential" = function(p) log(2*p['w'])/p['k'],
                  "dual-systems-exponential" = function(p) 'non-analytic',
                  "model-free" = function(p) 'none'
  )
  
  out <- list(
    name = fn_name, # Function name
    fn = fn, # Actual function to evaluate
    par_starts = par_starts,
    par_lims = par_lims,
    ED50 = ED50
  )
  
  if (fn_name == 'dual-systems-exponential') {
    out$par_chk <- function(p) {
      # Ensure k1 < k2
      if (p['k1'] > p['k2']) {
        # Switch k1 and k2
        k2 <- p['k1']
        k1 <- p['k2']
        p['k1'] <- k1
        p['k2'] <- k2
        # Complement of 2
        p['w'] <- 1 - p['w']
      }
      return(p)
    }
  }
  
  class(out) <- 'tdfn'
  return(out)
}
get_discount_function <- function(mod) mod$config$discount_function$fn

# Discount functions
all_discount_functions <- list(
  'noise' = function(D, p) rep(logistic(p['k']), length(D)),
  'hyperbolic' = function(D, p) 1 / (1 + exp(p['k'])*D),
  'exponential' = function(D, p) exp(-exp(p['k'])*D),
  'inverse-q-exponential' = function(D, p) 1 / (1 + exp(p['k'])*D)**exp(p['s']),
  'nonlinear-time-hyperbolic' = function(D, p) 1 / (1 + exp(p['k'])*D**exp(p['s'])),
  'scaled-exponential' = function(D, p) logistic(p['w'])*exp(-exp(p['k'])*D),
  'dual-systems-exponential' = function(D, p) logistic(p['w'])*exp(-p['k1']*D) + (1 - logistic(p['w']))*exp(-(p['k2'] + p['k1'])*D),
  'nonlinear-time-exponential' = function(D, p) exp(-exp(p['k'])*D**exp(p['s'])),
  'model-free' = function(D, p) {
    p <- p[grep('del_', names(p))]
    # Round parameters and delays to 10 decimal points for comparison
    dels <- round(as.numeric(gsub('del_', '', names(p))), 10)
    a <- approx(x = dels, y = logistic(p), xout = round(D, 10))
    return(a$y)
  }
)

# Plausible parameter ranges
default_param_ranges <- list(
  'noise' = list(
    k = exp(seq(-8, 0, length.out = 3))
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
  'gamma' = list(
    gamma = exp(seq(-8, 0, length.out = 3))
  ),
  'err.rate' = list(
    eps = c(-5, 0)
  ),
  'varsigma' = list(
    alpha = exp(seq(-8, 0, length.out = 3)),
    lambda = seq(-1, 1, length.out = 3)
  ),
  'model-free' = NA
)

untransform <- function(par) {
  # Get untransformed parameters
  u_p <- par
  # Log-transformed
  idx <- names(u_p) %in% c('k', 's', 'gamma', 'alpha')
  u_p[idx] <- exp(u_p[idx])
  # Logit-transformed
  idx <- grepl('\\d', names(u_p)) | names(u_p) == 'w'
  u_p[idx] <- logistic(u_p[idx])
  # 0.5*logistic
  idx <- names(u_p) == 'eps'
  u_p[idx] <- 0.5*logistic(u_p[idx])
  return(u_p)
}

#' Effective delay 50
#'
#' Compute the effective delay 50
#' @param mod A temporal discounting model. See `td_gnlm`
#' @param newdata Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.
#' @param type The type of prediction required. As in predict.glm, `"link"` (default) and `"response"` give predictions on the scales of the linear predictors and response variable, respectively. `"indiff"` gives predicted indifference points. In this case, `newdata` needs only a `del` column.
#' @return A vector of predictions
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
