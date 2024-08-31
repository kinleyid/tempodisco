
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
#' @param data A data frame with columns \code{val_imm} and \code{val_del} for the values of the immediate and delayed rewards, \code{del} for the delay, and \code{imm_chosen} (Boolean) for whether the immediate reward was chosen. Other columns can also be present but will be ignored.
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
                              'nonlinear-time-exponential',
                              'itch',
                              'naive'),
                    ...) {
  
  # Validate data
  require_columns(data, c('val_imm', 'val_del', 'del', 'imm_chosen'))
  data$imm_chosen <- as.logical(data$imm_chosen)
  attention_checks(data)
  invariance_checks(data)
  if (length(grep('\\.B', names(data))) > 1) {
    stop('No columns can have a name that begins with ".B"')
  }
  
  model <- match.arg(model)
  data <- add_beta_terms(data, model)
  beta_terms <- names(data)[grep('\\.B', names(data))]
  fml <- sprintf('imm_chosen ~ %s', paste(c(beta_terms, '0'), collapse = ' + '))
  mod <- glm(formula = fml, data = data, family = binomial(link = 'logit'), ...)
  
  if (model == 'itch') {
    discount_function = td_fn(name = 'itch-implied',
                              fn = function(data, p) {
                                ## Sage commands for this:
                                # df = var('df') # Discount function
                                # B_I, B_xA, B_xR, B_t = var('B_I', 'B_xA', 'B_xR', 'B_tA')
                                # val_del, del_ = var('val_del', 'del_')
                                # sol = solve(B_I + B_xA*val_del*(1 - df) + 2*B_xR*(1 - df)/(1 + df) + B_tA*del_, df)
                                # print(sol)
                                # print(derivative(sol[0], del_))
                                # print(derivative(sol[1], del_))
                                ## This gives
                                # [df == 1/2*(B_t*del_ + B_I - 2*B_xR - sqrt(B_t^2*del_^2 + 4*B_xA^2*val_del^2 + B_I^2 - 4*B_I*B_xR + 4*B_xR^2 + 2*(B_I*B_t - 2*B_t*B_xR)*del_ + 4*(B_t*B_xA*del_ + B_I*B_xA + 2*B_xA*B_xR)*val_del))/(B_xA*val_del), df == 1/2*(B_t*del_ + B_I - 2*B_xR + sqrt(B_t^2*del_^2 + 4*B_xA^2*val_del^2 + B_I^2 - 4*B_I*B_xR + 4*B_xR^2 + 2*(B_I*B_t - 2*B_t*B_xR)*del_ + 4*(B_t*B_xA*del_ + B_I*B_xA + 2*B_xA*B_xR)*val_del))/(B_xA*val_del)]
                                ## Followed by the appropriate finds and replaces:
                                # (B_[a-zA-Z]+) -> p['.\1']
                                # del_ -> data$del
                                # val_del -> data$val_del

                                out_1 <- 1/2*(p['.B_tA']*data$del + p['.B_I'] - 2*p['.B_xR'] - sqrt(p['.B_tA']^2*data$del^2 + 4*p['.B_xA']^2*data$val_del^2 + p['.B_I']^2 - 4*p['.B_I']*p['.B_xR'] + 4*p['.B_xR']^2 + 2*(p['.B_I']*p['.B_tA'] - 2*p['.B_tA']*p['.B_xR'])*data$del + 4*(p['.B_tA']*p['.B_xA']*data$del + p['.B_I']*p['.B_xA'] + 2*p['.B_xA']*p['.B_xR'])*data$val_del))/(p['.B_xA']*data$val_del)
                                out_2 <- 1/2*(p['.B_tA']*data$del + p['.B_I'] - 2*p['.B_xR'] + sqrt(p['.B_tA']^2*data$del^2 + 4*p['.B_xA']^2*data$val_del^2 + p['.B_I']^2 - 4*p['.B_I']*p['.B_xR'] + 4*p['.B_xR']^2 + 2*(p['.B_I']*p['.B_tA'] - 2*p['.B_tA']*p['.B_xR'])*data$del + 4*(p['.B_tA']*p['.B_xA']*data$del + p['.B_I']*p['.B_xA'] + 2*p['.B_xA']*p['.B_xR'])*data$val_del))/(p['.B_xA']*data$val_del)
                                # Which one looks more like a discount function?
                                
                                # By negative derivative
                                derivative_1 <- 1/2*(p['.B_tA'] - (p['.B_tA']^2*data$del + 2*p['.B_tA']*p['.B_xA']*data$val_del + p['.B_I']*p['.B_tA'] - 2*p['.B_tA']*p['.B_xR'])/sqrt(p['.B_tA']^2*data$del^2 + 4*p['.B_xA']^2*data$val_del^2 + p['.B_I']^2 - 4*p['.B_I']*p['.B_xR'] + 4*p['.B_xR']^2 + 2*(p['.B_I']*p['.B_tA'] - 2*p['.B_tA']*p['.B_xR'])*data$del + 4*(p['.B_tA']*p['.B_xA']*data$del + p['.B_I']*p['.B_xA'] + 2*p['.B_xA']*p['.B_xR'])*data$val_del))/(p['.B_xA']*data$val_del)
                                derivative_2 <- 1/2*(p['.B_tA'] + (p['.B_tA']^2*data$del + 2*p['.B_tA']*p['.B_xA']*data$val_del + p['.B_I']*p['.B_tA'] - 2*p['.B_tA']*p['.B_xR'])/sqrt(p['.B_tA']^2*data$del^2 + 4*p['.B_xA']^2*data$val_del^2 + p['.B_I']^2 - 4*p['.B_I']*p['.B_xR'] + 4*p['.B_xR']^2 + 2*(p['.B_I']*p['.B_tA'] - 2*p['.B_tA']*p['.B_xR'])*data$del + 4*(p['.B_tA']*p['.B_xA']*data$del + p['.B_I']*p['.B_xA'] + 2*p['.B_xA']*p['.B_xR'])*data$val_del))/(p['.B_xA']*data$val_del)
                                if (derivative_1[1] < 0 & derivative_2[1] > 0) {
                                  out <- out_1
                                } else if (derivative_1[1] > 0 & derivative_2[1] < 0) {
                                  out <- out_2
                                } else {
                                  # Which starts closest to 1?
                                  first_val_1 <- 1/2*(p['.B_tA']*0 + p['.B_I'] - 2*p['.B_xR'] - sqrt(p['.B_tA']^2*0^2 + 4*p['.B_xA']^2*data$val_del^2 + p['.B_I']^2 - 4*p['.B_I']*p['.B_xR'] + 4*p['.B_xR']^2 + 2*(p['.B_I']*p['.B_tA'] - 2*p['.B_tA']*p['.B_xR'])*0 + 4*(p['.B_tA']*p['.B_xA']*0 + p['.B_I']*p['.B_xA'] + 2*p['.B_xA']*p['.B_xR'])*data$val_del))/(p['.B_xA']*data$val_del)
                                  first_val_2 <- 1/2*(p['.B_tA']*0 + p['.B_I'] - 2*p['.B_xR'] + sqrt(p['.B_tA']^2*0^2 + 4*p['.B_xA']^2*data$val_del^2 + p['.B_I']^2 - 4*p['.B_I']*p['.B_xR'] + 4*p['.B_xR']^2 + 2*(p['.B_I']*p['.B_tA'] - 2*p['.B_tA']*p['.B_xR'])*0 + 4*(p['.B_tA']*p['.B_xA']*0 + p['.B_I']*p['.B_xA'] + 2*p['.B_xA']*p['.B_xR'])*data$val_del))/(p['.B_xA']*data$val_del)
                                  
                                  closest <- which.min(c(abs(first_val_1 - 1) - abs(first_val_2 - 1)))
                                  if (closest == 1) {
                                    out <- out_1
                                  } else {
                                    out <- out_2
                                  }
                                }
                                
                                return(out)
                              },
                              ED50 = function(...) {'non-analytic'})
  } else if (model == 'naive') {
    discount_function = td_fn(name = 'naive-glm-implied',
                              fn = function(data, p) {
                                1/(p['.B1']*data$val_del) * -(p['.B2']*data$val_del + p['.B3']*data$del + p['.B4'])
                              },
                              ED50 = function(...) {'non-analytic'})                    
  } else {
    disc_func_name <- strsplit(model, '\\.')[[1]][1] # Remove period, if necessary
    discount_function <- td_fn(predefined = disc_func_name)
  }
  
  mod$config <- list(
    discount_function = discount_function,
    model = model
  )
  
  class(mod) <- c('td_bclm', 'td_um', class(mod))
  
  return(mod)
}

add_beta_terms <- function(data, model) {
  if (model == 'hyperbolic.1') {
    data$.B1 <- 1 - data$val_del / data$val_imm
    data$.B2 <- data$del
  } else if (model == 'hyperbolic.2') {
    data$.B1 <- qlogis(data$val_imm / data$val_del) + log(data$del)
    data$.B2 <- 1
  } else if (model == 'exponential.1') {
    data$.B1 <- log(data$val_imm / data$val_del)
    data$.B2 <- data$del
  } else if (model == 'exponential.2') {
    data$.B1 <- qgumbel(data$val_imm / data$val_del) + log(data$del)
    data$.B2 <- 1
  } else if (model == 'scaled-exponential') {
    data$.B1 <- log(data$val_imm / data$val_del)
    data$.B2 <- data$del
    data$.B3 <- 1
  } else if (model == 'nonlinear-time-hyperbolic') {
    data$.B1 <- log(data$val_imm / data$val_del)
    data$.B2 <- log(data$del)
    data$.B3 <- 1
  } else if (model == 'nonlinear-time-exponential') {
    data$.B1 <- qgumbel(data$val_imm / data$val_del)
    data$.B2 <- log(data$del)
    data$.B3 <- 1
  } else if (model == 'itch') {
    data$.B_I <- 1
    data$.B_xA <- data$val_del - data$val_imm
    x_star <- (data$val_del + data$val_imm) / 2
    data$.B_xR <- (data$val_del - data$val_imm) / x_star
    data$.B_tA <- data$del
    # BtR deliberately excluded because it introduces colinearity when one reward is immediate
  } else if (model == 'naive') {
    data$.B1 <- data$val_imm
    data$.B2 <- data$val_del
    data$.B3 <- data$del
    data$.B4 <- 1
  }
  return(data)
}
