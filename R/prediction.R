
#' Predict indifference points
#'
#' Predict the indifference point at a given delay
#' @param mod A delay discounting model. See `dd_prob_model` and `dd_det_model`
#' @param del A vector of delays. Defaults to the delays from the data used to fit the model
#' @return A vector of indifference points
#' @examples
#' # Generate data
#' df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
#' logistic <- function(x) 1 / (1 + exp(-x))
#' logit <- function(x) log(x / (1 - x))
#' prob <- logistic(logit(df$val_imm / df$val_del) - logit(1 / (1 + 0.001*df$del)))
#' df$imm_chosen <- runif(nrow(df)) < prob
#' # Fit model
#' mod <- dd_prob_model(df)
#' indiffs <- predict_indiffs(mod)
#' plot(indiffs ~ log(mod$data$del), type = 'l')
#' @export
predict_indiffs <- function(mod, del = NULL) {
  if (is.null(del)) {
    del <- mod$data$del
  }
  indiffs <- get_discount_function(mod$discount_function)(del, mod$par)
  names(indiffs) <- NULL
  return(indiffs)
}

#' Predict decision probabilities
#'
#' Predict the probability of selecting the immediate reward
#' @param mod A probabilistic delay discounting model. See `dd_prob_model`
#' @param data A data frame with columns `val_imm`, `val_del`, and `del`, specifying the immediate and delayed rewards, and the delay of the delayed reward. Defaults to the data used to fit the model
#' @return A vector of probabilities
#' @examples 
#' # Generate data
#' df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
#' logistic <- function(x) 1 / (1 + exp(-x))
#' logit <- function(x) log(x / (1 - x))
#' prob <- logistic(logit(df$val_imm / df$val_del) - logit(1 / (1 + 0.001*df$del)))
#' df$imm_chosen <- runif(nrow(df)) < prob
#' # Fit model
#' mod <- dd_prob_model(df)
#' prob_imm <- predict_prob_imm(mod)
#' boxplot(prob_imm ~ mod$data$imm_chosen)
#' @export
predict_prob_imm <- function(mod, data = NULL, laplace = T) {
  if (is.null(data)) {
    data <- mod$data
  }
  frame_args <- c('discount_function',
                  'fixed.ends',
                  'fit.err.rate',
                  'choice.rule',
                  'absval')
  p <- do.call(get_prob_mod_frame, mod[frame_args])(data, mod$par)
  if (laplace) {
    p <- laplace_smooth(p)
  }
  names(p) <- NULL
  return(p)
}

