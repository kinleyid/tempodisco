# Residuals from temporal discounting model

Get residuals from a temporal discounting indifference point model.

## Usage

``` r
# S3 method for class 'td_ipm'
residuals(object, type = c("response", "pearson"), ...)
```

## Arguments

- object:

  A temporal discounting model. See `td_bcnm`.

- type:

  The type of residuals to be returned. See `residuals.nls`.

- ...:

  Additional arguments currently not used.

## Value

A vector of residuals.

## See also

Other indifference point model functions:
[`coef.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/coef.td_ipm.md),
[`fitted.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_ipm.md),
[`logLik.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/logLik.td_ipm.md),
[`predict.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/predict.td_ipm.md)
