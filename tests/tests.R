
lib_path <- 'C:/Users/isaac/Projects/ddDesidModels'
roxygen2::roxygenise(lib_path)

# Generate data
df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
indiffs <- function(x) 1 / (1 + 0.0001*x)
prob <- plogis(qlogis(df$val_imm / df$val_del) - qlogis(indiffs(df$del)))
prob <- plogis(50*((df$val_imm / df$val_del) - (indiffs(df$del))))
prob <- plogis( 2*( df$val_imm - df$val_del*indiffs(df$del) ) )
df$imm_chosen <- runif(nrow(df)) < prob

# Default, simple call
devtools::load_all(lib_path)
mod <- dd_prob_model(df, discount_function = 'hyperbolic', choice_rule = 'power', fixed_ends = T)
mod <- dd_prob_model(df, discount_function = 'hyperbolic', choice_rule = 'logistic')
mod <- dd_prob_model(df, discount_function = 'hyperbolic', choice_rule = 'probit')
mod <- dd_prob_model(df, discount_function = 'model-free', choice_rule = 'probit')
print(mod)
ED50(mod)
AUC(mod)

plot(mod)
plot(mod, type = 'summary')
plot(mod, type = 'endpoints')
plot(mod, type = 'endpoints', del = 100, val_del = 50)

mod <- td_gnlm(df, discount_function = 'hyperbolic', choice_rule = 'probit', fixed_ends = T)
mod <- td_gnlm(df, discount_function = 'exponential', choice_rule = 'probit', fixed_ends = T)
mod <- td_gnlm(df, discount_function = 'dual-systems-exponential', choice_rule = 'probit', fixed_ends = T)
mod <- td_gnlm(df, discount_function = 'model-free', choice_rule = 'probit', fixed_ends = T)

# prediction
fitted(mod)
predict(mod, type = 'link')
predict(mod, type = 'response')
predict(mod, newdata = data.frame(del = 0:1000), type = 'indiff')



# With different choice rules
td_gnlm(df, )

# Overriding choice rules and separately specifying noise_dist etc.

# Fit models
discount_function_options <- eval(formals(td_gnlm)$discount_function)
for (discount_function in discount_function_options)
for (curr_discount_function in all_discount_functions) {
  mod <- dd_prob_model(df, discount_function = curr_discount_function)
  mod_indiffs <- predict_indiffs(mod)
  dels <- mod$data$del
  plot(mod_indiffs ~ dels, type = 'l', ylim = c(0, 1))
  lines(indiffs(dels) ~ dels, col = 'red')
  print(sprintf('%s: %s', curr_discount_function, predict_indiffs(mod, del = mod$ED50)))
}

curr_discount_function <- "dual-systems-exponential"

# Residuals of each kind
# Plots of each kind

# Test custom discount function