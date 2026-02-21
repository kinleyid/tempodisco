# Residuals from temporal discounting model

Get residuals from a temporal discounting binary choice nonlinear model.

## Usage

``` r
# S3 method for class 'td_bcnm'
residuals(object, type = c("deviance", "pearson", "response"), ...)
```

## Arguments

- object:

  A temporal discounting binary choice model. See `td_bcnm`.

- type:

  The type of residuals to be returned. See `residuals.glm`.

- ...:

  Additional arguments currently not used.

## Value

A vector of residuals.

## See also

Other nonlinear binary choice model functions:
[`coef.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/coef.td_bcnm.md),
[`deviance.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/deviance.td_bcnm.md),
[`fitted.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_bcnm.md),
[`logLik.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/logLik.td_bcnm.md),
[`predict.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/predict.td_bcnm.md),
[`td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/td_bcnm.md)
