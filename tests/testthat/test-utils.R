
### nonsys

# On a model
data("td_bc_single_ptpt")
mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'model-free')
ns <- nonsys(mod)

expect_named(ns)
expect_false(any(ns))
expect_type(ns, 'logical')
expect_length(ns, 2)

# On a dataframe
data("td_ip_simulated_ptpt")
ns <- nonsys(td_ip_simulated_ptpt)

expect_named(ns)
expect_false(any(ns))
expect_type(ns, 'logical')
expect_length(ns, 2)

# Artificial case of nonsystematic discounting
ns <- nonsys(data.frame(del = 1:3, indiff = c(0.5, 0.8, 0.6))) # Both TRUE

expect_named(ns)
expect_true(all(ns))
expect_type(ns, 'logical')
expect_length(ns, 2)


### adj_amt_indiffs


data("adj_amt_sim")
test_that('indifference computation works', {
  indiffs <- adj_amt_indiffs(adj_amt_sim, block_indic = 'del', order_indic = 'trial_idx')
  expect_s3_class(indiffs, 'data.frame')
  expect_error(adj_amt_indiffs(adj_amt_sim, order_indic = 'val_imm')) # Wrong order indicator
})

### kirby_score

data("td_bc_single_ptpt")
test_that('kirby scoring', {
  expect_s3_class(kirby_score(td_bc_single_ptpt), 'td_ipm')
})
test_that('inconsistent responses produce a warning', {
  set.seed(123)
  td_bc_single_ptpt$imm_chosen <- round(runif(nrow(td_bc_single_ptpt)))
  expect_warning(kirby_score(td_bc_single_ptpt))
})
test_that('residuals', {
  mod <- kirby_score(td_bc_single_ptpt)
  expect_length(residuals(mod, type = 'response'), n = nrow(td_bc_single_ptpt))
  expect_error(residuals(mod, type = 'pearson'))
})

test_that('confint stuff', {
  expect_no_error(vcov(mod))
  expect_no_error(confint(mod))
  expect_no_error(summary(mod))
})

### wileyto_score
data("td_bc_single_ptpt")
test_that('wileyto scoring', {
  expect_s3_class(wileyto_score(td_bc_single_ptpt), 'td_bclm')
})