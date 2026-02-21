# Check for non-systematic discounting

Check for non-systematic discounting, per the Johnson & Bickel (2008)
criteria. These are:

- C1: No indifference point can exceed the previous by more than 0.2

- C2: Last indifference point must be lower than first by at least 0.1

## Usage

``` r
nonsys(obj)
```

## Arguments

- obj:

  Either a `data.frame` with columns `indiff` and `del`, or a
  discounting model of class `td_bcnm` or `td_ipm`, fit using the
  `"model-free"` discount function.

## Value

Named logical vector specifying whether nonsystematic discounting is
exhibited according to C1/C2.

## Examples

``` r
# \donttest{
# On a model
data("td_bc_single_ptpt")
mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'model-free')
any(nonsys(mod))
#> [1] FALSE

# On a dataframe
data("td_ip_simulated_ptpt")
any(nonsys(td_ip_simulated_ptpt))
#> [1] FALSE

# Artificial case of nonsystematic discounting
nonsys(data.frame(del = 1:3, indiff = c(0.5, 0.8, 0.6))) # Both TRUE
#>   C1   C2 
#> TRUE TRUE 
# }
```
