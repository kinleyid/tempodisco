
# All immediate
all_imm <- data.frame(
  val_imm = 0:10,
  val_del = 10,
  del = 100,
  imm_chosen = T
)

# All delayed
all_del <- data.frame(
  val_imm = 0:10,
  val_del = 10,
  del = 100,
  imm_chosen = F
)

test_that('warnings', {
  # Attention checks
  expect_warning(attention_checks(all_imm, warn = T))
  expect_warning(attention_checks(all_del, warn = T, ppn = T))
  expect_no_warning(attention_checks(all_imm, warn = F))
  expect_no_warning(attention_checks(all_del, warn = F))
  expect_warning(invariance_checks(all_imm, warn = T))
  expect_warning(invariance_checks(all_del, warn = T))
  expect_no_warning(attention_checks(all_imm, warn = F))
  expect_no_warning(attention_checks(all_del, warn = F))
})
