# Temporal discounting binary choice linear model

Compute a binary choice linear model for a single subject. In these
models, we can recover the parameters of a discount function from the
weights of a standard logistic regression. \\\beta_1\\

## Usage

``` r
td_bclm(
  data,
  model = c("all", "hyperbolic.1", "hyperbolic.2", "exponential.1", "exponential.2",
    "scaled-exponential", "nonlinear-time-hyperbolic", "power",
    "nonlinear-time-exponential", "arithmetic.1", "arithmetic.2"),
  ...
)
```

## Arguments

- data:

  A data frame with columns `val_imm` and `val_del` for the values of
  the immediate and delayed rewards, `del` for the delay, and
  `imm_chosen` (Boolean) for whether the immediate reward was chosen.
  Other columns can also be present but will be ignored.

- model:

  A string specifying which model to use. Below is a list of these
  models' linear predictors and the means by which we can recover
  discount function parameters.  
  `'hyperbolic.1'`: \\\beta_1(1 - v_D/v_I) + \beta_2 t\\; \\k =
  \beta_2/\beta_1\\  
  `'hyperbolic.2'`: \\\beta_1(\sigma^{-1}\[v_I/v_D\] + \log t) +
  \beta_2\\; \\k = \exp\[\beta_2/\beta_1\]\\  
  `'exponential.1'`: \\\beta_1 \log (v_I/v_D) + \beta_2 t\\; \\k =
  \beta_2/\beta_1\\  
  `'exponential.2'`: \\\beta_1(G^{-1}\[v_I/v_D\] + \log t) + \beta_2\\;
  \\k = \exp\[\beta_2/\beta_1\]\\  
  `'scaled-exponential'`: \\\beta_1 \log (v_I/v_D) + \beta_2 t +
  \beta_3\\; \\k = \beta_2/\beta_1\\, \\w = \exp\[-\beta_3/\beta_1\]\\
  `'nonlinear-time-hyperbolic'`: \\\beta_1(\sigma^{-1}\[v_I/v_D\]) +
  \beta_2\log t + \beta_3\\; \\k = \exp\[\beta_3/\beta_1\]\\, \\s =
  \beta_2/\beta_1\\  
  `'nonlinear-time-hyperbolic'`: \\\beta_1(G^{-1}\[v_I/v_D\]) +
  \beta_2\log t + \beta_3\\; \\k = \exp\[\beta_3/\beta_1\]\\, \\s =
  \beta_2/\beta_1\\  
  where \\\sigma^{-1}\[\cdot\]\\ is the quantile function of the
  standard logistic distribution \\G^{-1}\[\cdot\]\\ is the quantile
  function of the standard Gumbel distribution.

- ...:

  Additional arguments passed to `glm`.

## Value

An object of class `td_bclm`, nearly identical to a `glm` but with an
additional `config` component.

## See also

Other linear binary choice model functions:
[`predict.td_bclm()`](https://kinleyid.github.io/tempodisco/reference/predict.td_bclm.md)

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
mod <- td_bclm(td_bc_single_ptpt, model = 'hyperbolic.1')
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
print(coef(mod))
#>          k 
#> 0.04372626 
# }
```
