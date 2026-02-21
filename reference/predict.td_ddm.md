# Model Predictions

Generate predictions from a temporal discounting drift diffusion model.

## Usage

``` r
# S3 method for class 'td_ddm'
predict(
  object,
  newdata = NULL,
  type = c("indiff", "link", "response", "rt"),
  ...
)
```

## Arguments

- object:

  A temporal discounting drift diffusion model. See
  [`td_ddm`](https://kinleyid.github.io/tempodisco/reference/td_ddm.md).

- newdata:

  Optionally, a data frame to use for prediction. If omitted, the data
  used to fit the model will be used for prediction.

- type:

  The type of prediction required. As in predict.glm, `"link"` (default)
  and `"response"` give predictions on the scales of the linear
  predictors and response variable, respectively. `"indiff"` gives
  predicted indifference points. For predicting indifference points,
  `newdata` needs only a `del` column. `"rt"` gives predicted reaction
  times.

- ...:

  Additional arguments currently not used.

## Value

A vector of predictions.

## Note

When `type = 'rt'`, expected RTs are computed irrespective of which
reward was selected, per equation 5 in Grasman, Wagenmakers, & van der
Maas (2009,
[doi:10.1016/j.jmp.2009.01.006](https://doi.org/10.1016/j.jmp.2009.01.006)
).

## See also

Other drift diffusion model functions:
[`coef.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/coef.td_ddm.md),
[`deviance.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/deviance.td_ddm.md),
[`fitted.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_ddm.md),
[`logLik.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/logLik.td_ddm.md),
[`td_ddm()`](https://kinleyid.github.io/tempodisco/reference/td_ddm.md)

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
ddm <- td_ddm(td_bc_single_ptpt, discount_function = 'exponential',
              gamma_par_starts = 0.01,
              beta_par_starts = 0.5,
              alpha_par_starts = 3.5,
              tau_par_starts = 0.9)
pred_rts <- predict(ddm, type = 'rt')
# }
```
