
require(vctrs)

# Get data
data("td_ip_simulated_ptpt")
df <- td_ip_simulated_ptpt

data("td_bc_single_ptpt") # For Kirby scoring

# Get all possible combinations of inputs
discount_functions <- c('all', 'model-free', 'dual-systems-exponential', 'custom', 'kirby-hyperbolic', 'kirby-exponential')

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
  
  if (discount_function == 'kirby-hyperbolic') {
    mod <- kirby_score(td_bc_single_ptpt, discount_function = 'hyperbolic')
  } else if (discount_function == 'kirby-exponential') {
    mod <- kirby_score(td_bc_single_ptpt, discount_function = 'exponential')
  } else {
    if (discount_function == 'custom') {
      discount_function <- custom_discount_function
    }
    mod <- td_ipm(df, discount_function = discount_function)
  }
  
  expect_s3_class(mod, 'td_ipm')
  
  test_that('generics', {
    expect_output(print(mod))
    expect_in(class(ED50(mod)), c('numeric', 'character')) # Might be "none"
    expect_type(AUC(mod, verbose = F), 'double')
    expect_output(AUC(mod, verbose = T))
    expect_named(coef(mod))
    if ('indiff' %in% names(mod$data)) {
      # Computed directly on indifference points
      expect_type(BIC(mod), 'double')
      expect_type(AIC(mod), 'double')
      expect_type(AIC(mod, k = 5), 'double')
      expect_type(logLik(mod), 'double')
      expect_vector(residuals(mod, type = 'pearson'), size = nrow(df))
      expect_vector(residuals(mod, type = 'response'), size = nrow(df))
    } # Computed using Kirby scoring or similar on binary choices, therefore the above functions don't apply
  })
  
  pdf(NULL) # Don't actually produce plots
  test_that('plots', {
    expect_no_error(plot(mod, type = 'summary', verbose = F))
    expect_no_error(plot(mod, type = 'summary', verbose = F, log = 'x'))
  })
  dev.off()
  
  # prediction
  test_that('predictions', {
    expect_vector(fitted(mod), ptype = numeric(0), size = nrow(mod$data))
    expect_vector(predict(mod, newdata = data.frame(del = 0:1000), type = 'indiff'), ptype = numeric(0), size = 1001)
    expect_vector(predict(mod, del = 0:1000, type = 'indiff'), ptype = numeric(0), size = 1001)
    newdata <- data.frame(del = 100, val_del = 1, val_imm = runif(10))
    expect_vector(predict(mod, newdata = newdata, type = 'response'), ptype = numeric(0), size = nrow(newdata))
  })
}

test_that('errors', {
  expect_error(td_ipm())
  expect_error(td_ipm(data.frame(del = 1:10)))
  expect_error(td_ipm(df, discount_function = 'new'))
  expect_error(plot(mod, type = 'endpoints'))
})