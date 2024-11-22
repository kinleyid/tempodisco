
data("td_ip_simulated_ptpt")

test_that('AUC on dataframes', {
  expect_no_error(AUC(td_ip_simulated_ptpt))
  expect_no_error(AUC(td_ip_simulated_ptpt, del_transform = 'log'))
  expect_no_error(AUC(td_ip_simulated_ptpt, del_transform = 'ord'))
  expect_error(AUC(td_ip_simulated_ptpt,
                   max_del = max(td_ip_simulated_ptpt) + 1))
  td_ip_simulated_ptpt$indiff <- NULL
  expect_error(AUC(td_ip_simulated_ptpt))
})

test_that('model-based AUC', {
  mod <- td_ipm(td_ip_simulated_ptpt, discount_function = 'model-free')
  expect_no_error(AUC(mod, del_transform = 'log'))
  expect_error(AUC(mod, del_transform = 'ord'))
})
