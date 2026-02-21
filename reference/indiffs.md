# Get model-free indifference points

Convert a temporal discounting model with the "model-free" discount
function to a dataframe of indifference points.

## Usage

``` r
indiffs(mod)
```

## Arguments

- mod:

  A model of class
  [`td_bcnm`](https://kinleyid.github.io/tempodisco/reference/td_bcnm.md),
  [`td_ipm`](https://kinleyid.github.io/tempodisco/reference/td_ipm.md),
  or
  [`td_ddm`](https://kinleyid.github.io/tempodisco/reference/td_ddm.md)
  for which the "model-free" discount function has been fit.

## Value

A dataframe with columns `del` (delay) and `indiff` (indifference
point).

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'model-free')
indiff_data <- indiffs(mod)
# }
```
