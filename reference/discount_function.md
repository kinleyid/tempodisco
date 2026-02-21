# Get discount function from model

Access the name of the discount function of a model.

## Usage

``` r
discount_function(mod)
```

## Arguments

- mod:

  A temporal discounting model.

## Value

The name of the discount function.

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
mod <- td_bcnm(td_bc_single_ptpt, discount_function = "exponential")
discount_function(mod)
#> [1] "exponential"
# }
```
