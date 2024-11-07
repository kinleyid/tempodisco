
# Get data
data("td_bc_single_ptpt")
df <- td_bc_single_ptpt

# Get combinations of inputs
arg_vals <- list(
  fixed_ends = c(T, F),
  fit_err_rate = c(T, F),
  choice_rule = c('logistic', 'probit', 'power'),
  discount_function = 'hyperbolic')
arg_combos <- do.call(expand.grid, c(arg_vals, list(stringsAsFactors = F)))
arg_combos <- rbind(
  arg_combos,
  data.frame(
    fixed_ends = F,
    fit_err_rate = F,
    choice_rule = 'logistic',
    discount_function = c('all', 'model-free', 'dual-systems-exponential', 'custom')
  ))

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

arg_combo_idx <- 1
while (arg_combo_idx <= nrow(arg_combos)) {
  arg_combo <- arg_combos[arg_combo_idx, ]
  arg_combo_idx <- arg_combo_idx + 1
  # cat(sprintf('Arguments:\n'))
  # print(arg_combo)
  
  arg_combo <- as.list(unlist(arg_combo))
  if (arg_combo$discount_function == 'all') {
    # We aren't testing the "all" functionality, but the following is basically equivalent
    arg_combo$discount_function <- c('hyperbolic', 'exponential')
  } else if (arg_combo$discount_function == 'custom') {
    arg_combo$discount_function <- custom_discount_function
  }
  
  mod <- do.call(td_bcnm, c(list(data = df), arg_combo))
  
  expect_s3_class(mod, 'td_bcnm')
  
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
    expect_length(residuals(mod, type = 'deviance'), n = nrow(df))
    expect_length(residuals(mod, type = 'pearson'), n = nrow(df))
    expect_length(residuals(mod, type = 'response'), n = nrow(df))
  })
  
  pdf(NULL) # Don't actually produce plots
  test_that('plots', {
    expect_no_error(plot(mod, type = 'summary', verbose = F))
    expect_no_error(plot(mod, type = 'summary', verbose = F, log = 'x'))
    expect_no_error(plot(mod, type = 'endpoints', verbose = F))
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
    expect_length(predict(mod, newdata = data.frame(del = 0:1000), type = 'indiff'), n = 1001)
  })
}

test_that('errors', {
  expect_error(td_bcnm(df, choice_rule = 'random'))
  expect_error(td_bcnm(df, noise_dist = 'norm'))
  expect_error(td_bcnm())
  expect_error(td_bcnm(df[, 1:2]))
  expect_error(td_bcnm(df, discount_function = 'new'))
  expect_error(td_bcnm(df, discount_function = 'new'))
})