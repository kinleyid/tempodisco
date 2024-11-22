
# Get data
data("td_bc_single_ptpt")
df <- td_bc_single_ptpt

# Test drift transforms
# Expect warning for long RTs
# Test generics
# Test a few different discount functions

default_args <- list(td_bc_single_ptpt,
                     discount_function = 'exponential',
                     v_par_starts = 0.01,
                     beta_par_starts = 0.5,
                     alpha_par_starts = 3.5,
                     tau_par_starts = 0.9)

# Basic call
mod <- do.call(td_ddm, default_args)
expect_s3_class(mod, 'td_ddm')

# Test generics
test_that('generics', {
  expect_output(print(mod))
  expect_in(class(ED50(mod)), c('numeric', 'character')) # Output might be "none", e.g. for model-free
  expect_in(class(AUC(mod)), c('numeric', 'character'))
  expect_type(BIC(mod), 'double')
  expect_type(AIC(mod), 'double')
  expect_type(AIC(mod, k = 5), 'double')
  expect_type(logLik(mod), 'double')
  expect_type(logLik(mod, type = 'resp'), 'double')
  expect_named(coef(mod))
  expect_type(deviance(mod), 'double')
  # Residuals are not applicable
  # expect_vector(residuals(mod, type = 'deviance'), size = nrow(df))
  # expect_vector(residuals(mod, type = 'pearson'), size = nrow(df))
  # expect_vector(residuals(mod, type = 'response'), size = nrow(df))
})

test_that('plots', {
  pdf(NULL) # Don't actually produce plots
  expect_no_error(plot(mod, type = 'summary', verbose = F))
  expect_no_error(plot(mod, type = 'summary', verbose = F, log = 'x'))
  expect_no_error(plot(mod, type = 'summary', verbose = F, log = 'x', p_lines = 0.1, p_tol = 0.1))
  expect_no_error(plot(mod, type = 'endpoints', verbose = F))
  expect_output(plot(mod, type = 'summary', verbose = T))
  expect_output(plot(mod, type = 'endpoints', verbose = T))
  expect_no_error(plot(mod, type = 'endpoints', verbose = F, del = 100, val_del = 50))
  expect_no_error(plot(mod, type = 'link'))
  expect_no_error(plot(mod, type = 'rt'))
  expect_no_error(plot(mod, type = 'rt', q_lines = 0.5))
  dev.off()
})

test_that('predictions', {
  expect_length(fitted(mod), n = nrow(df))
  expect_length(predict(mod, type = 'link'), n = nrow(df))
  expect_length(predict(mod, type = 'response'), n = nrow(df))
  expect_length(predict(mod, type = 'rt'), n = nrow(df))
  expect_length(predict(mod, newdata = data.frame(del = 0:1000, val_del = 1), type = 'indiff'), n = 1001)
})

test_that('multiple discount functions', {
  custom_discount_function <- td_fn(
    name = 'custom',
    fn = function(data, p) (1 - p['b'])*exp(-p['k']*data$del) + p['b'],
    par_starts = list(k = c(0.001, 0.1), b = c(0.001, 0.1)),
    par_lims = list(k = c(0, Inf), b = c(0, 1)),
    ED50 = 'non-analytic'
  )
  args <- default_args
  args$discount_function <- list('model-free', 'dual-systems-exponential', custom_discount_function)
  expect_no_error(do.call(td_ddm, args))
})

test_that('drift transformations', {
  args <- default_args
  args$drift_transform <- 'sigmoid'
  expect_no_error(do.call(td_ddm, args))
  args$drift_transform <- 'bias-correct'
  expect_no_error(do.call(td_ddm, args))
})

test_that('errors', {
  expect_error(td_ddm(df, discount_function = 'random'))
  expect_error(td_ddm())
  expect_error(td_ddm(df[, 1:2]))
  df$rt <- df$rt*1000 # in ms
  expect_error(td_ddm(df, discount_function = 'hyperbolic'))
})
