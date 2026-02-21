# Area under the curve (AUC)

Compute either the model-based or model-free area under the curve.

## Usage

``` r
AUC(
  obj,
  min_del = 0,
  max_del = NULL,
  val_del = NULL,
  del_transform = c("none", "log", "ordinal-scaling"),
  ...
)
```

## Arguments

- obj:

  A temporal discounting model or a dataframe with columns `indiff`
  (indifference point) and `del` (delay).

- min_del:

  Lower limit to use for integration. Defaults to 0.

- max_del:

  Upper limit to use for integration. Defaults to the maximum delay in
  the data.

- val_del:

  Delayed value to use for computing the indifference curve, if
  applicable. Defaults to the average `del_val` in the data.

- del_transform:

  String specifying transformation to apply to the delays (e.g., log10 +
  1 transform or ordinal scaling transform; Borges et al., 2016,
  [doi:10.1002/jeab.219](https://doi.org/10.1002/jeab.219) ). Default is
  no transform.

- ...:

  Further arguments passed to \`integrate()\`.

## Value

AUC value.

## Note

An indifference point of 1 is assumed at delay 0.

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
mod <- td_bcnm(td_bc_single_ptpt, discount_function = "exponential")
print(AUC(mod))
#> [1] 0.02527911
# }
```
