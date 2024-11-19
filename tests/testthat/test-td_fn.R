
test_that('normal functioning') {
  expect_no_error(td_fn(name = 'custom',
                        fn = function(data, p) {p['x']},
                        par_starts = list(x = 0.5),
                        par_lims = list(x = c(0, 1))))
}

test_that('errors', {
  expect_error(td_fn('random-discount-function'))
  expect_error(td_fn(fn = function(data, p) {p['x']}))
  expect_error(td_fn(name = 'custom',
                     fn = function(data, p) {p['x']},
                     par_starts = c(x = 0.5),
                     par_lims = c(x = 0)))
  expect_error(td_fn(name = 'custom',
                     fn = function(data, p) {p['x']},
                     par_starts = c(x = 0.5),
                     par_lims = list(c(0, 1))))
})
