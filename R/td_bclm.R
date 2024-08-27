
#' Temporal discounting binary choice linear model
#' 
#' Compute a binary choice linear model for a single subject. In these models, we can recover the parameters of a discount function from the weights of a standard logistic regression.
#' \eqn{\beta_1}
#' @param model A string specifying which model to use. Below is a list of these models' linear predictors and the means by which we can recover discount function parameters. \cr
#' \code{'hyperbolic.1'}: \eqn{\beta_1(1 - v_D/v_I) + \beta_2 t}; \eqn{k = \beta_2/\beta_1} \cr
#' \code{'hyperbolic.2'}: \eqn{\beta_1(\sigma^{-1}[v_I/v_D] + \log t) + \beta_2}; \eqn{k = \exp[\beta_2/\beta_1]} \cr
#' \code{'exponential.1'}: \eqn{\beta_1 \log (v_I/v_D) + \beta_2 t}; \eqn{k = \beta_2/\beta_1} \cr
#' \code{'exponential.2'}: \eqn{\beta_1(G^{-1}[v_I/v_D] + \log t) + \beta_2}; \eqn{k = \exp[\beta_2/\beta_1]} \cr
#' \code{'scaled-exponential'}: \eqn{\beta_1 \log (v_I/v_D) + \beta_2 t + \beta_3}; \eqn{k = \beta_2/\beta_1}, \eqn{w = \exp[-\beta_3/\beta_1]}
#' \code{'nonlinear-time-hyperbolic'}: \eqn{\beta_1(\sigma^{-1}[v_I/v_D]) + \beta_2\log t + \beta_3}; \eqn{k = \exp[\beta_3/\beta_1]}, \eqn{s = \beta_2/\beta_1} \cr
#' \code{'nonlinear-time-hyperbolic'}: \eqn{\beta_1(G^{-1}[v_I/v_D]) + \beta_2\log t + \beta_3}; \eqn{k = \exp[\beta_3/\beta_1]}, \eqn{s = \beta_2/\beta_1} \cr
#' where \eqn{\sigma^{-1}[\cdot]} is the quantile function of the standard logistic distribution \eqn{G^{-1}[\cdot]} is the quantile function of the standard Gumbel distribution
#' @param ... Additional arguments passed to \code{glm}
#' @return An object of class \code{td_bclm}, nearly identical to a \code{glm} but with an additional \code{config} component.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bclm(td_bc_single_ptpt, model = 'hyperbolic.1')
#' print(coef(mod))
#' }
#' @export
td_bclm <- function(data,
                    model = c('hyperbolic.1',
                              'hyperbolic.2',
                              'exponential.1',
                              'exponential.2',
                              'scaled-exponential',
                              'nonlinear-time-hyperbolic',
                              'nonlinear-time-exponential'),
                    ...) {
  model <- match.arg(model)
  data <- add_beta_terms(data, model)
  if ('B3' %in% names(data)) {
    fml <- imm_chosen ~ B1 + B2 + B3 + 0
  } else {
    fml <- imm_chosen ~ B1 + B2 + 0
  }
  mod <- glm(formula = fml, data = data, family = binomial(link = 'logit'), ...)
  
  disc_func_name <- strsplit(model, '\\.')[[1]][1]
  mod$config <- list(
    discount_function = td_fn(disc_func_name),
    model = model
  )
  
  class(mod) <- c('td_bclm', 'td_um', class(mod))
  
  return(mod)
}

add_beta_terms <- function(data, model) {
  if (model == 'hyperbolic.1') {
    data$B1 <- 1 - data$val_del / data$val_imm
    data$B2 <- data$del
  } else if (model == 'hyperbolic.2') {
    data$B1 <- qlogis(data$val_imm / data$val_del) + log(data$del)
    data$B2 <- 1
  } else if (model == 'exponential.1') {
    data$B1 <- log(data$val_imm / data$val_del)
    data$B2 <- data$del
  } else if (model == 'exponential.2') {
    data$B1 <- qgumbel(data$val_imm / data$val_del) + log(data$del)
    data$B2 <- 1
  } else if (model == 'scaled-exponential') {
    data$B1 <- log(data$val_imm / data$val_del)
    data$B2 <- data$del
    data$B3 <- 1
  } else if (model == 'nonlinear-time-hyperbolic') {
    data$B1 <- log(data$val_imm / data$val_del)
    data$B2 <- log(data$del)
    data$B3 <- 1
  } else if (model == 'nonlinear-time-exponential') {
    data$B1 <- qgumbel(data$val_imm / data$val_del)
    data$B2 <- log(data$del)
    data$B3 <- 1
  }
  return(data)
}

#' @export
coef.td_bclm <- function(mod, df_par = T) {
  if (df_par) {
    # In terms of discount function parameters
    p <- mod$coefficients
    B <- unname(c(p['B1'], p['B2'], p['B3']))
    d <- mod$config$model
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
    cf <- mod$coefficients
  }
  return(cf)
}
