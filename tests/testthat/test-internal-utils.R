
df <- data.frame(
  del = 10,
  val_del = 10,
  val_imm = 5,
  imm_chosen = F,
  rt = 0.5
)

test_that('bad_types', {
  expect_no_error(validate_td_data(df, required_columns = colnames(df)))
  for (colm in colnames(df)) {
    df_copy <- df
    df_copy[[colm]] <- as.character(df[[colm]])
    expect_error(validate_td_data(df_copy, required_columns = colnames(df)))
  }
})