% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{predict.td_ddm}
\alias{predict.td_ddm}
\title{Model Predictions}
\usage{
\method{predict}{td_ddm}(
  object,
  newdata = NULL,
  type = c("indiff", "link", "response", "rt"),
  ...
)
}
\arguments{
\item{object}{A temporal discounting drift diffusion model. See \code{\link{td_ddm}}.}

\item{newdata}{Optionally, a data frame to use for prediction. If omitted, the data used to fit the model will be used for prediction.}

\item{type}{The type of prediction required. As in predict.glm, \code{"link"} (default) and \code{"response"} give predictions on the scales of the linear predictors and response variable, respectively. \code{"indiff"} gives predicted indifference points. For predicting indifference points, \code{newdata} needs only a \code{del} column. \code{"rt"} gives predicted reaction times.}

\item{...}{Additional arguments currently not used.}
}
\value{
A vector of predictions.
}
\description{
Generate predictions from a temporal discounting drift diffusion model.
}
\note{
When \code{type = 'rt'}, expected RTs are computed irrespective of which reward was selected, per equation 5 in Grasman, Wagenmakers, & van der Maas (2009, \doi{10.1016/j.jmp.2009.01.006}).
}
\examples{
\donttest{
data("td_bc_single_ptpt")
ddm <- td_ddm(td_bc_single_ptpt, discount_function = 'exponential',
              gamma_par_starts = 0.01,
              beta_par_starts = 0.5,
              alpha_par_starts = 3.5,
              tau_par_starts = 0.9)
pred_rts <- predict(ddm, type = 'rt')
}
}
\seealso{
Other drift diffusion model functions: 
\code{\link{coef.td_ddm}()},
\code{\link{deviance.td_ddm}()},
\code{\link{fitted.td_ddm}()},
\code{\link{logLik.td_ddm}()},
\code{\link{td_ddm}()}
}
\concept{drift diffusion model functions}
