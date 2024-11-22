
# Get data
data("td_bc_single_ptpt")
df <- td_bc_single_ptpt

# Get all possible models
models <- eval(formals(td_bclm)$model)

model_idx <- 1
while (model_idx <= length(models)) {
  
  mod <- td_bclm(df, model = models[model_idx])
  model_idx <- model_idx + 1
  
  expect_s3_class(mod, 'td_bclm')
  
  test_that('generics', {
    expect_output(print(mod))
    expect_in(class(ED50(mod)), c('numeric', 'character')) # Output might be "none", e.g. for model-free
    expect_in(class(AUC(mod)), c('numeric', 'character'))
    expect_type(BIC(mod), 'double')
    expect_type(AIC(mod), 'double')
    expect_type(AIC(mod, k = 5), 'double')
    expect_type(logLik(mod), 'double')
    expect_named(coef(mod))
    expect_named(coef(mod, df_par = F))
    expect_length(residuals(mod, type = 'deviance'), n = nrow(df))
    expect_length(residuals(mod, type = 'pearson'), n = nrow(df))
    expect_length(residuals(mod, type = 'response'), n = nrow(df))
  })
  
  pdf(NULL) # Don't actually produce plots
  test_that('plots', {
    expect_no_error(plot(mod, type = 'summary', verbose = F))
    expect_no_error(plot(mod, type = 'summary', verbose = F, log = 'x'))
    expect_no_error(plot(mod, type = 'summary', verbose = F, log = 'x', p_lines = 0.1, p_tol = 0.1))
    expect_no_error(plot(mod, type = 'endpoints', verbose = F))
    expect_output(plot(mod, type = 'summary', verbose = T))
    expect_output(plot(mod, type = 'endpoints', verbose = T))
    expect_no_error(plot(mod, type = 'endpoints', verbose = F, del = 100, val_del = 50))
    expect_no_error(plot(mod, type = 'link'))
  })
  dev.off()
  
  # prediction
  test_that('predictions', {
    expect_length(fitted(mod), n = nrow(df))
    expect_length(predict(mod, type = 'link'), n = nrow(df))
    expect_length(predict(mod, type = 'response'), n = nrow(df))
    expect_length(predict(mod, newdata = data.frame(del = 0:1000, val_del = 1), type = 'indiff'), n = 1001)
  })
}

test_that('errors', {
  expect_error(td_bclm(df, model = 'random'))
  expect_error(td_bclm())
  expect_error(td_bclm(df[, 1:2]))
  df$`.B1` <- 1
  expect_error(td_bclm(df, model = 'hyperbolic.1'))
})