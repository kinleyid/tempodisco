#' Plot models
#'
#' Plot delay discounting models
#' @param mod A delay discounting model. See `dd_prob_model` and `dd_det_model`
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
plot_dd <- function(mod, p_range = c(0.4, 0.6)) {
  max_del <- max(mod$data$del)
  plotting_delays <- seq(0, max_del, length.out = 1000)
  pred_indiffs <- predict_indiffs(mod, del = plotting_delays)
  if ('choice.rule' %in% names(mod)) {
    # Plot probabilistic model
    if (mod$absval == 'none') {
      lower <- invert_decision_function(mod, prob = p_range[1], del = plotting_delays)
      upper <- invert_decision_function(mod, prob = p_range[2], del = plotting_delays)
    }
    plot(NA, NA,
         xlim = c(0, max_del), ylim = c(0, 1),
         xlab = 'Delay',
         ylab = 'Rel. val. imm. rew.')
    lines(pred_indiffs ~ plotting_delays)
    if (mod$absval == 'none') {
      lines(lower ~ plotting_delays, lty = 'dashed')
      lines(upper ~ plotting_delays, lty = 'dashed')
    }
    mod$data$rel_val <- mod$data$val_imm / mod$data$val_del
    points(rel_val ~ del, col = 'red',
           data = subset(mod$data, imm_chosen))
    points(rel_val ~ del, col = 'blue',
           data = subset(mod$data, !imm_chosen))
  } else {
    # Plot deterministic model
    plot(NA, NA, xlim = c(0, max_del), ylim = c(0, 1),
         xlab = 'Delay',
         ylab = 'Indifference point')
    points(indiff ~ del, data = mod$data)
    lines(pred_indiffs ~ plotting_delays)
  }
  title(mod$discount_function)
}

#' @export
plot_scores <- function(mod, outlier.idx = NULL) {
  if (is.null(outlier.idx)) {
    outlier.idx <- rep(F, nrow(mod$data))
  }
  # Get scores
  score_func <- do.call(get_score_func_frame,
                        mod[names(formals(get_score_func_frame))])
  scores <- do.call(score_func,
                    mod[names(formals(score_func))])
  # Get probabilities
  prob_func <- do.call(get_prob_func_frame,
                       mod[names(formals(get_prob_func_frame))])
  p <- prob_func(scores, mod$par)
  lim <- max(abs(min(scores)), abs(max(scores)))
  plot(mod$data$imm_chosen[!outlier.idx] ~ scores[!outlier.idx],
       ylim = c(0, 1),
       xlim = c(-lim, lim),
       ylab = 'Prob. imm.',
       xlab = 'Score')
  points(mod$data$imm_chosen[outlier.idx] ~ scores[outlier.idx], col = 'red')
  smooth_scores <- seq(-lim, lim, length.out = 1000)
  lines(prob_func(smooth_scores, mod$par) ~ smooth_scores)
}

#' @export
plot_rs <- function(mod) {
  ED50 <- get_ED50(mod)
  plotting_rs <- seq(0, 1, length.out = 1000)
  newdat <- data.frame(
    del = ED50,
    val_del = 1,
    val_imm = plotting_rs
  )
  p <- predict_prob_imm(mod, newdat)
  plot(p ~ plotting_rs, type = 'l',
       ylim = c(0, 1),
       xlab = 'R',
       ylab = 'Prob. Imm')
}