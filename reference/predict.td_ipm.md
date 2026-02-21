# Model Predictions

Generate predictions from a temporal discounting indifference point
model

## Usage

``` r
# S3 method for class 'td_ipm'
predict(object, newdata = NULL, type = c("indiff", "response"), ...)
```

## Arguments

- object:

  A temporal discounting indifference point model. See `td_ipm`.

- newdata:

  A data frame to use for prediction. If omitted, the data used to fit
  the model will be used for prediction.

- type:

  Type of prediction, either `'indiff'` (indifference points) or
  `'response'` (whether the participants would is predicted to choose
  the immediate (1) or delayed reward (0)).

- ...:

  Additional arguments currently not used.

## Value

A vector of predictions.

## See also

Other indifference point model functions:
[`coef.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/coef.td_ipm.md),
[`fitted.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_ipm.md),
[`logLik.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/logLik.td_ipm.md),
[`residuals.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/residuals.td_ipm.md)

## Examples

``` r
# \donttest{
data("td_ip_simulated_ptpt")
mod <- td_ipm(td_ip_simulated_ptpt, discount_function = 'hyperbolic')
indiffs <- predict(mod, del = 1:100)
indiffs <- predict(mod, newdata = data.frame(del = 1:100))
# }
```
