% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/td_fn.R
\name{td_fn}
\alias{td_fn}
\title{Predefined or custom discount function}
\usage{
td_fn(
  predefined = c("hyperbolic", "exponential", "power", "inverse-q-exponential",
    "nonlinear-time-hyperbolic", "scaled-exponential", "dual-systems-exponential",
    "nonlinear-time-exponential", "additive-utility", "model-free", "constant"),
  name = "unnamed",
  fn = NULL,
  par_starts = NULL,
  par_lims = NULL,
  init = NULL,
  ED50 = NULL,
  par_chk = NULL
)
}
\arguments{
\item{predefined}{A string specifying one of the pre-defined discount functions.}

\item{name}{Name of custom discount function.}

\item{fn}{Function that takes a data.frame called \code{data} (expected to contain the column \code{del} for delays) and a vector of named parameters called \code{p}, and returns a vector of values between 0 and 1 representing the indifference points computed at the given delay.}

\item{par_starts}{A named list of vectors, each specifying possible starting values for a parameter to try when running optimization.}

\item{par_lims}{A named list of vectors, each specifying the bounds to impose of a parameters. Any parameter for which bounds are unspecified are assumed to be unbounded.}

\item{init}{A function to initialize the td_fn object. It should take 2 arguments: "self" (the td_fn object being initialized) and "data" (the data used for initialization).}

\item{ED50}{A function which, given a named vector of parameters \code{p} and optionally a value of \code{del_val}, computes the ED50. If there is no closed-form solution, this should return the string "non-analytic". If the ED50 is not well-defined, this should return the string "none". As a shortcut for these latter 2 cases, the strings "non-analytic" and "none" can be directly supplied as arguments.}

\item{par_chk}{Optionally, this is a function that checks the parameters to ensure they meet some criteria. E.g., for the dual-systems-exponential discount function, we require k1 < k2.}
}
\value{
An object of class \code{td_fn}.
}
\description{
Get a predefined discount function or create a custom discount function.
}
\examples{
\donttest{
data("td_bc_single_ptpt")
# Custom discount function
custom_discount_function <- td_fn(
  name = 'custom',
  fn = function(data, p) (1 - p['b'])*exp(-p['k']*data$del) + p['b'],
  par_starts = list(k = c(0.001, 0.1), b = c(0.001, 0.1)),
  par_lims = list(k = c(0, Inf), b = c(0, 1)),
  ED50 = 'non-analytic'
)
mod <- td_bcnm(td_bc_single_ptpt, discount_function = custom_discount_function, fit_err_rate = TRUE)
}
}
