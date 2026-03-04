# Temporal discounting indifference point model

Compute a model of a single subject's indifference points.

## Usage

``` r
td_ipm(
  data,
  discount_function = "franck-2015",
  optim_args = list(),
  silent = TRUE
)
```

## Arguments

- data:

  A data frame with columns `indiff` for the pre-computed indifference
  points and `del` for the delay.

- discount_function:

  A vector of strings specifying the name of the discount functions to
  use, or an object of class `td_fn` (used for creating custom discount
  functions), or a list of objects of class `td_fn`. Default is
  `'franck-2015'`, which is the set of widely used discount functions
  from Franck et al., 2015: https://doi.org/10.1002/jeab.128.

- optim_args:

  A list of additional args to pass to `optim`.

- silent:

  A Boolean specifying whether the call to `optim` (which occurs in a
  `try` block) should be silent on error.

## Value

An object of class `td_ipm` with components `data` (containing the data
used for fitting), `config` (containing the internal configuration of
the model, including the `discount_function`), and `optim` (the output
of [`optim()`](https://rdrr.io/r/stats/optim.html)).

## Examples

``` r
# \donttest{
# Basic usage
data("td_ip_simulated_ptpt")
mod <- td_ipm(td_ip_simulated_ptpt, discount_function = "hyperbolic")
# Custom discount function
custom_discount_function <- td_fn(
  name = 'custom',
  fn = function(data, p) (1 - p['b'])*exp(-p['k']*data$del) + p['b'],
  par_starts = list(k = c(0.001, 0.1), b = c(0.001, 0.1)),
  par_lims = list(k = c(0, Inf), b = c(0, 1)),
  ED50 = function(p) 'non-analytic'
)
mod <- td_ipm(td_ip_simulated_ptpt, discount_function = custom_discount_function)
# }
```
