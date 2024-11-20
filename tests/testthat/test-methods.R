
data("td_ip_simulated_ptpt")

test_that('AUC on dataframes', {
  expect_no_error(AUC(td_ip_simulated_ptpt))
  expect_no_error(AUC(td_ip_simulated_ptpt, del_transform = 'log'))
  expect_no_error(AUC(td_ip_simulated_ptpt, del_transform = 'ord'))
  td_ip_simulated_ptpt$indiff <- NULL
  expect_error(AUC(td_ip_simulated_ptpt))
})
