# Model Predictions

Generate predictions from a temporal discounting binary choice model.

## Usage

``` r
# S3 method for class 'td_bcnm'
predict(object, newdata = NULL, type = c("link", "response", "indiff"), ...)
```

## Arguments

- object:

  A temporal discounting binary choice model. See
  [`td_bcnm`](https://kinleyid.github.io/tempodisco/reference/td_bcnm.md).

- newdata:

  Optionally, a data frame to use for prediction. If omitted, the data
  used to fit the model will be used for prediction.

- type:

  The type of prediction required. As in predict.glm, `"link"` (default)
  and `"response"` give predictions on the scales of the linear
  predictors and response variable, respectively. `"indiff"` gives
  predicted indifference points. For predicting indifference points,
  `newdata` needs only a `del` column.

- ...:

  Additional arguments currently not used.

## Value

A vector of predictions.

## See also

Other nonlinear binary choice model functions:
[`coef.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/coef.td_bcnm.md),
[`deviance.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/deviance.td_bcnm.md),
[`fitted.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_bcnm.md),
[`logLik.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/logLik.td_bcnm.md),
[`residuals.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/residuals.td_bcnm.md),
[`td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/td_bcnm.md)

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'hyperbolic')
indiffs <- predict(mod, newdata = data.frame(del = 1:100), type = 'indiff')
# }
```
