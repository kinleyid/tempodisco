
# Test each discount function
all_discount_functions <- c(
  "hyperbolic",
  "exponential",
  "inverse-q-exponential",
  "nonlinear-time-hyperbolic",
  "scaled-exponential",
  "dual-systems-exponential",
  "nonlinear-time-exponential"
)
# Generate data
df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
logistic <- function(x) 1 / (1 + exp(-x))
logit <- function(x) log(x / (1 - x))
indiffs <- function(x) 1 / (1 + 0.001*x)
prob <- logistic(logit(df$val_imm / df$val_del) - logit(indiffs(df$del)))
df$imm_chosen <- runif(nrow(df)) < prob
# Fit models
for (curr_discount_function in all_discount_functions) {
  mod <- dd_prob_model(df, discount_function = curr_discount_function)
  mod_indiffs <- predict_indiffs(mod)
  dels <- mod$data$del
  plot(mod_indiffs ~ dels, type = 'l', ylim = c(0, 1))
  lines(indiffs(dels) ~ dels, col = 'red')
  print(sprintf('%s: %s', curr_discount_function, predict_indiffs(mod, del = mod$ED50)))
}

curr_discount_function <- "dual-systems-exponential"
