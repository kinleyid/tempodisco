
# Get data
data("td_ip_simulated_ptpt")
df <- td_ip_simulated_ptpt

# Get all possible combinations of inputs
discount_functions <- c('all', 'model-free', 'dual-systems-exponential', 'custom')

custom_discount_function <- td_fn(
  name = 'custom',
  fn = function(data, p) 1 / (1 + log(data$del*p['m'] + 1)),
  par_starts = list(m = c(0.001, 0.1)),
  par_lims = list(m = c(0, Inf)),
  ED50 = function(...) 'non-analytic',
  par_chk = function(p) {
    return(p)
  }
)

test_that('printing discount functions', {
  expect_output(print(custom_discount_function))
})

df_idx <- 1
while (df_idx <= length(discount_functions)) {
  discount_function <- discount_functions[df_idx]
  df_idx <- df_idx + 1
  # cat(sprintf('Arguments:\n'))
  # print(arg_combo)
  
  if (discount_function == 'custom') {
    discount_function <- custom_discount_function
  }
  
  mod <- td_ipm(df, discount_function = discount_function)
  
  expect_s3_class(mod, 'td_ipm')
  
  test_that('generics', {
    expect_output(print(mod))
    expect_in(class(ED50(mod)), c('numeric', 'character')) # Might be "none"
    expect_type(AUC(mod, verbose = F), 'double')
    expect_output(AUC(mod, verbose = T))
    expect_type(BIC(mod), 'double')
    expect_type(AIC(mod), 'double')
    expect_type(AIC(mod, k = 5), 'double')
    expect_type(logLik(mod), 'double')
    expect_named(coef(mod))
    expect_vector(residuals(mod, type = 'pearson'), size = nrow(df))
    expect_vector(residuals(mod, type = 'response'), size = nrow(df))
  })
  
  pdf(NULL) # Don't actually produce plots
  test_that('plots', {
    expect_no_error(plot(mod, type = 'summary', verbose = F))
    expect_no_error(plot(mod, type = 'summary', verbose = F, log = 'x'))
  })
  dev.off()
  
  # prediction
  test_that('predictions', {
    expect_vector(fitted(mod), ptype = numeric(0), size = nrow(df))
    expect_vector(predict(mod, newdata = data.frame(del = 0:1000), type = 'indiff'), ptype = numeric(0), size = 1001)
    expect_vector(predict(mod, del = 0:1000, type = 'indiff'), ptype = numeric(0), size = 1001)
  })
}

test_that('errors', {
  expect_error(td_ipm())
  expect_error(td_ipm(data.frame(del = 1:10)))
  expect_error(td_ipm(df, discount_function = 'new'))
  expect_error(plot(mod, type = 'endpoints'))
})