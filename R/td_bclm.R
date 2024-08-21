
#' @export
td_bclm <- function(data,
                   model = c('hyperbolic.1',
                             'hyperbolic.2',
                             'exponential.1',
                             'exponential.2',
                             'scaled-exponential.1',
                             'nonlinear-time-hyperbolic.2',
                             'nonlinear-time-exponential.2'),
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
    discount_function = tdfn(disc_func_name),
    model = model
  )
  
  class(mod) <- c('td_bclm', 'td_um' class(mod))
  
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
    data$B1 <- varphi(data$val_imm / data$val_del) + data$del
    data$B2 <- 1
  } else if (model == 'scaled-exponential.1') {
    data$B1 <- log(data$val_imm / data$val_del)
    data$B2 <- data$del
    data$B3 <- 1
  } else if (model == 'nonlinear-time-hyperbolic.2') {
    data$B1 <- log(data$val_imm / data$val_del)
    data$B2 <- log(data$del)
    data$B3 <- 1
  } else if (model == 'nonlinear-time-exponential.2') {
    data$B1 <- varphi(data$val_imm / data$val_del)
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
    } else if (d == 'scaled-exponential.1') {
      cf <- c('k' = B[2]/B[1],
              'w' = exp(-B[3]/B[1]))
    } else if (d == 'nonlinear-time-hyperbolic.2') {
      cf <- c('k' = exp(B[3]/B[1]),
              's' = B[2]/B[1])
    } else if (d == 'nonlinear-time-exponential.2') {
      cf <- c('k' = exp(B[3]/B[1]),
              's' = B[2]/B[1])
    }
  } else {
    cf <- coef.glm(mod)
  }
  return(cf)
}
