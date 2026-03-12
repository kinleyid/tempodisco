# Get model-free indifference points

Create a dataframe of delays and the corresponding indifference points
predicted by a model.

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
  [`td_ddm`](https://kinleyid.github.io/tempodisco/reference/td_ddm.md).

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
