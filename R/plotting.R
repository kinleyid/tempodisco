#' Plot models
#'
#' Plot delay discounting models
#' @param mod A delay discounting model. See `dd_prob_model` and `dd_det_model`
#' @param type Type of plot to generate
#' @param p_range In addition to the indifference points (where the probability of choosing the immediate reward is 0.5), other "preference points" can also be plotted (e.g., points where the probability of choosing the immediate reward is 0.8). This allows visualization of how stochastic decision making is. Defaults to `c(0.4, 0.6)`
#' @examples
#' # Generate data
#' df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
#' logistic <- function(x) 1 / (1 + exp(-x))
#' logit <- function(x) log(x / (1 - x))
#' prob <- logistic(logit(df$val_imm / df$val_del) - logit(1 / (1 + 0.001*df$del)))
#' df$imm_chosen <- runif(nrow(df)) < prob
#' # Fit model
#' mod <- dd_prob_model(df)
#' plot_dd(mod)
#' @export
plot.td_gnlm <- function(mod, type = c('summary', 'endpoints', 'scores'), ...) {
  type <- match.arg(type)
  if (type == 'summary') {
    plot_summary(mod, ...)
  } else if (type == 'endpoints') {
    plot_endpoints(mod, ...)
  } else if (type == 'scores') {
    plot_scores(mod, ...)
  }
}

#' @export
plot.td_glm <- function(mod, type = 'summary', ...) {
  if (type == 'summary') {
    plot_summary(mod, ...)
  } else if (type == 'endpoints') {
    plot_endpoints(mod, ...)
  } else if (type == 'scores') {
    plot_scores(mod, ...)
  }
}

plot_summary <- function(mod, p_range = c(0.4, 0.6)) {
  data <- mod$data
  max_del <- max(data$del)
  plotting_delays <- seq(0, max_del, length.out = 1000)
  pred_indiffs <- predict(mod, newdata = data.frame(del = plotting_delays), type = 'indiff')
  # Plot probabilistic model
  if (mod$config$gamma_scale == 'none') {
    lower <- invert_decision_function(mod, prob = p_range[1], del = plotting_delays)
    upper <- invert_decision_function(mod, prob = p_range[2], del = plotting_delays)
  }
  plot(NA, NA,
       xlim = c(0, max_del), ylim = c(0, 1),
       xlab = 'Delay',
       ylab = 'Rel. val. imm. rew.')
  lines(pred_indiffs ~ plotting_delays)
  if (mod$config$gamma_scale == 'none') {
    lines(lower ~ plotting_delays, lty = 'dashed')
    lines(upper ~ plotting_delays, lty = 'dashed')
  }
  data$rel_val <- data$val_imm / data$val_del
  points(rel_val ~ del, col = 'red',
         data = subset(data, imm_chosen))
  points(rel_val ~ del, col = 'blue',
         data = subset(data, !imm_chosen))
  title(mod$config$discount_function)
}

# else {
#     # Plot deterministic model
#     plot(NA, NA, xlim = c(0, max_del), ylim = c(0, 1),
#          xlab = 'Delay',
#          ylab = 'Indifference point')
#     points(indiff ~ del, data = mod$data)
#     lines(pred_indiffs ~ plotting_delays)
#   }
#   title(mod$discount_function)
# }

plot_scores <- function(mod, outlier.idx = NULL) {
  if (is.null(outlier.idx)) {
    outlier.idx <- rep(F, nrow(mod$data))
  }
  # Plot scores
  score_func <- do.call(get_score_func_frame, mod$config)
  scores <- score_func(mod$data, coef(mod))
  lim <- max(abs(min(scores)), abs(max(scores)))
  plot(mod$data$imm_chosen[!outlier.idx] ~ scores[!outlier.idx],
       ylim = c(0, 1),
       xlim = c(-lim, lim),
       ylab = 'Prob. imm.',
       xlab = 'Score')
  points(mod$data$imm_chosen[outlier.idx] ~ scores[outlier.idx], col = 'red')
  # Plot probabilities
  prob_func <- do.call(get_prob_func_frame, mod$config)
  score_range <- seq(-lim, lim, length.out = 1000)
  p <- prob_func(score_range, coef(mod))
  lines(p ~ score_range)
}

plot_endpoints <- function(mod, del=NULL, val_del=NULL) {
  if (is.null(val_del)) {
    val_del <- mean(mod$data$val_del)
    if (mod$config$gamma_scale != 'none') {
      warning(sprintf('
        Gamma (steepness of curve) is scaled by val_del
        Thus, the curve will have different steepness for a different value of val_del
        Defaulting to val_del = %s (mean of val_del from data used to fit model)
        Use the `val_del` argument to specify a custom value
      ', val_del))
    }
  }
  if (is.null(del)) {
    if (mod$config$discount_function == 'none') {
      del <- mean(c(min(mod$data$del), max(c(mod$data$del))))
      warning(sprintf('
        No ED50 for the "none" discount function.
        Setting del=%s (halfway between min delay and max delay).
        Consider setting this manually.
      ', del))
    } else if (mod$config$discount_function == 'noise') {
      del <- 1
      warning(sprintf('
        No ED50 for the "noise" discount function.
        Setting del=%s.
      ', del))
    } else {
      del <- ED50(mod)
      cat(sprintf('
        Detting del = %s (ED50) to center the curve.
        This can be changed using the `del` argument.
      ', del))
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
  if (del %in% mod$data$del) {
    sdf <- mod$data[mod$data$del == del, ]
    sdf$R <-sdf$val_imm / sdf$val_del
    points(imm_chosen ~ R, data = sdf)
  }
}