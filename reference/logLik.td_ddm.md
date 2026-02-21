# Extract log-likelihood

Compute log-likelihood for a temporal discounting drift diffusion model.

## Usage

``` r
# S3 method for class 'td_ddm'
logLik(object, type = c("resp_rt", "resp", "rt"), ...)
```

## Arguments

- object:

  An object of class `td_bcnm`.

- type:

  Should probabilities /probability densities be computed for responses
  and RTs (`'resp_rt'`, default) or responses only (`'resp'`)?

- ...:

  Additional arguments currently not used.

## Value

Returns an object of class `logLik` with attributed `df` and `nobs`

## See also

Other drift diffusion model functions:
[`coef.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/coef.td_ddm.md),
[`deviance.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/deviance.td_ddm.md),
[`fitted.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_ddm.md),
[`predict.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/predict.td_ddm.md),
[`td_ddm()`](https://kinleyid.github.io/tempodisco/reference/td_ddm.md)
