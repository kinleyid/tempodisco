
### nonsys

# On a model
data("td_bc_single_ptpt")
mod <- td_bcm(td_bc_single_ptpt, discount_function = 'model-free')
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

