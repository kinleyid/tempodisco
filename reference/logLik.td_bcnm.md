# Extract log-likelihood

Compute log-likelihood for a temporal discounting binary choice
nonlinear model.

## Usage

``` r
# S3 method for class 'td_bcnm'
logLik(object, ...)
```

## Arguments

- object:

  An object of class `td_bcnm`

- ...:

  Additional arguments currently not used.

## Value

Returns an object of class `logLik` with attributed `df` and `nobs`

## See also

Other nonlinear binary choice model functions:
[`coef.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/coef.td_bcnm.md),
[`deviance.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/deviance.td_bcnm.md),
[`fitted.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_bcnm.md),
[`predict.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/predict.td_bcnm.md),
[`residuals.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/residuals.td_bcnm.md),
[`td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/td_bcnm.md)
