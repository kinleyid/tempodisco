# Extract model coefficients

Get coefficients of a temporal discounting drift diffusion model.

## Usage

``` r
# S3 method for class 'td_ddm'
coef(object, type = "all", ...)
```

## Arguments

- object:

  An object of class
  [`td_ddm`](https://kinleyid.github.io/tempodisco/reference/td_ddm.md).

- type:

  A string specifying which coefficients to extract. `'all'` extracts
  them all, `'ddm'` extracts only DDM-specific parameters, and `'df'`
  extracts only discount function parameters.

- ...:

  Additional arguments currently not used.

## Value

A named vector of coefficients.

## See also

Other drift diffusion model functions:
[`deviance.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/deviance.td_ddm.md),
[`fitted.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_ddm.md),
[`logLik.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/logLik.td_ddm.md),
[`predict.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/predict.td_ddm.md),
[`td_ddm()`](https://kinleyid.github.io/tempodisco/reference/td_ddm.md)
