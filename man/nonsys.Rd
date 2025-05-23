% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{nonsys}
\alias{nonsys}
\title{Check for non-systematic discounting}
\usage{
nonsys(obj)
}
\arguments{
\item{obj}{Either a \code{data.frame} with columns \code{indiff} and \code{del}, or a discounting model of class \code{td_bcnm} or \code{td_ipm}, fit using the \code{"model-free"} discount function.}
}
\value{
Named logical vector specifying whether nonsystematic discounting is exhibited according to C1/C2.
}
\description{
Check for non-systematic discounting, per the Johnson & Bickel (2008) criteria. These are:
\itemize{
 \item C1: No indifference point can exceed the previous by more than 0.2
 \item C2: Last indifference point must be lower than first by at least 0.1
}
}
\examples{
\donttest{
# On a model
data("td_bc_single_ptpt")
mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'model-free')
any(nonsys(mod))

# On a dataframe
data("td_ip_simulated_ptpt")
any(nonsys(td_ip_simulated_ptpt))

# Artificial case of nonsystematic discounting
nonsys(data.frame(del = 1:3, indiff = c(0.5, 0.8, 0.6))) # Both TRUE
}
}
