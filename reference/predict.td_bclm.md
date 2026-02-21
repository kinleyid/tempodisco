# Model Predictions

Generate predictions from a temporal discounting binary choice linear
model.

## Usage

``` r
# S3 method for class 'td_bclm'
predict(
  object,
  newdata = NULL,
  type = c("indiff", "link", "response", "terms"),
  ...
)
```

## Arguments

- object:

  A temporal discounting binary choice linear model. See `td_bclm`.

- newdata:

  Optionally, a data frame to use for prediction. If omitted, the data
  used to fit the model will be used for prediction.

- type:

  The type of prediction required. For `'indiff'` (default) gives
  predicted indifference points. In this case, `newdata` needs only a
  `del` column. For all other values (e.g. `"link"`, `"response"`), this
  function is just a wrapper to
  [`predict.glm()`](https://rdrr.io/r/stats/predict.glm.html).

- ...:

  Additional arguments passed to predict.glm if type != `'indiff'`.

## Value

A vector of predictions.

## See also

Other linear binary choice model functions:
[`td_bclm()`](https://kinleyid.github.io/tempodisco/reference/td_bclm.md)

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
mod <- td_bclm(td_bc_single_ptpt, model = 'hyperbolic.1')
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
indiffs <- predict(mod, newdata = data.frame(del = 1:100), type = 'indiff')
# }
```
