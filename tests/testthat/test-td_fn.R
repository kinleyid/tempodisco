
test_that('normal functioning', {
  expect_no_error(td_fn(name = 'custom',
                        fn = function(data, p) {p['x']},
                        par_starts = list(x = 0.5),
                        par_lims = list(x = c(0, 1))))
  expect_no_error(td_fn(name = 'custom',
                        fn = function(data, p) {p['x']},
                        par_starts = list(x = 0.5)))
  expect_no_error(td_fn(init = function(self, data) {
    self$fn <- function(data, p) {p['x']}
    self$par_starts <- list(x = 0.5)
    self$par_lims <- list(x = c(0, 1))
    return(self)
  }))
})

test_that('errors', {
  expect_error(td_fn())
  expect_error(td_fn(predefined = 'random-discount-function'))
  expect_error(td_fn(fn = function(data, p) {p['x']}))
  expect_error(td_fn(fn = function(p) {p['x']}))
  expect_error(td_fn(name = 'custom',
                     fn = function(data, p) {p['x']},
                     par_starts = list(x = 0.5),
                     par_lims = list(y = c(0, 1)))) #!
  expect_error(td_fn(init = function(self) { #!
    self$fn <- function(data, p) {p['x']}
    self$par_starts <- list(x = 0.5)
    self$par_lims <- list(x = c(0, 1))
    return(self)
  }))
  expect_error(td_fn(name = 'custom',
                     fn = function(data, p) {p['x']},
                     par_starts = c(x = 0.5), #!
                     par_lims = c(x = 0))) #!
  expect_error(td_fn(name = 'custom',
                     fn = function(data, p) {p['x']},
                     par_starts = c(x = 0.5), #!
                     par_lims = list(c(0, 1))))
  expect_no_error(td_fn(name = 'custom',
                        fn = function(data, p) {p['x']},
                        par_starts = list(x = 0.5),
                        par_lims = list(x = c(0, 1)),
                        ED50 = 'random')) #!
  expect_no_error(td_fn(name = 'custom',
                        fn = function(data, p) {p['x']},
                        par_starts = list(x = 0.5),
                        par_lims = list(x = c(0, 1)),
                        ED50 = 1)) #!
})
