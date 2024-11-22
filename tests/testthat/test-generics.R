
# This is quite short because most of the generics are tested along with the class they're defined for

data("td_bc_single_ptpt")

mod <- kirby_score(td_bc_single_ptpt)
test_that('no residuals for kirby-scored data', {
  expect_error(residuals(mod))
})