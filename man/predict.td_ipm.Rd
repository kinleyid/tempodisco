% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{predict.td_ipm}
\alias{predict.td_ipm}
\title{Model Predictions}
\usage{
\method{predict}{td_ipm}(object, newdata = NULL, type = c("indiff", "response"), ...)
}
\arguments{
\item{object}{A temporal discounting indifference point model. See \code{td_ipm}.}

\item{newdata}{A data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.}

\item{type}{Type of prediction, either \code{'indiff'} (indifference points) or \code{'response'} (whether the participants would is predicted to choose the immediate (1) or delayed reward (0)).}

\item{...}{Additional arguments currently not used.}
}
\value{
A vector of predictions.
}
\description{
Generate predictions from a temporal discounting indifference point model
}
\examples{
\donttest{
data("td_ip_simulated_ptpt")
mod <- td_ipm(td_ip_simulated_ptpt, discount_function = 'hyperbolic')
indiffs <- predict(mod, del = 1:100)
indiffs <- predict(mod, newdata = data.frame(del = 1:100))
}
}
\seealso{
Other indifference point model functions: 
\code{\link{coef.td_ipm}()},
\code{\link{fitted.td_ipm}()},
\code{\link{logLik.td_ipm}()},
\code{\link{residuals.td_ipm}()}
}
\concept{indifference point model functions}
