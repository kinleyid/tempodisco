
data("td_ip_simulated_ptpt")
data("td_bc_single_ptpt")

test_that('AUC on dataframes', {
  expect_no_error(AUC(td_ip_simulated_ptpt))
  expect_no_error(AUC(td_ip_simulated_ptpt, del_transform = 'log'))
  expect_no_error(AUC(td_ip_simulated_ptpt, del_transform = 'ord'))
  expect_error(AUC(td_ip_simulated_ptpt,
                   max_del = max(td_ip_simulated_ptpt) + 1))
  td_ip_simulated_ptpt$indiff <- NULL
  expect_error(AUC(td_ip_simulated_ptpt))
})

test_that('AUC on models', {
  mod <- td_ipm(td_ip_simulated_ptpt, discount_function = 'model-free')
  expect_no_error(AUC(mod, del_transform = 'log'))
  expect_error(AUC(mod, del_transform = 'ord'))
  # make integrate() fail
  mod$config$discount_function$fn <- function(data, p) NA
  expect_type(AUC(mod), 'character')
})

test_that('nonsys', {
  mod <- td_ipm(td_ip_simulated_ptpt, discount_function = 'hyperbolic')
  expect_error(nonsys(mod))
  mod <- td_bclm(td_bc_single_ptpt, model = 'hyperbolic.1')
  expect_error(nonsys(mod))
})
