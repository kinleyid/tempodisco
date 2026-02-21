# Extract log-likelihood

Compute log-likelihood for a temporal discounting indifference point
model.

## Usage

``` r
# S3 method for class 'td_ipm'
logLik(object, ...)
```

## Arguments

- object:

  An object of class `td_ipm`

- ...:

  Additional arguments currently not used.

## Value

Returns an object of class `logLik` with attributed `df` and `nobs`

## See also

Other indifference point model functions:
[`coef.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/coef.td_ipm.md),
[`fitted.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_ipm.md),
[`predict.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/predict.td_ipm.md),
[`residuals.td_ipm()`](https://kinleyid.github.io/tempodisco/reference/residuals.td_ipm.md)
