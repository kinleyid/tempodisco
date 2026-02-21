# Temporal discounting drift diffusion model

Fit a drift diffusion model for a single subject using maximum
likelihood estimation.

## Usage

``` r
td_ddm(
  data,
  discount_function,
  gamma_par_starts = c(0.01, 0.1, 1),
  beta_par_starts = c(0.25, 0.5, 0.75),
  alpha_par_starts = c(0.5, 1, 10),
  tau_par_starts = c(0.2, 0.8),
  drift_transform = c("none", "logis"),
  bias_adjust = FALSE,
  silent = TRUE,
  optim_args = list()
)
```

## Arguments

- data:

  A data frame with columns `val_imm` and `val_del` for the values of
  the immediate and delayed rewards, `del` for the delays, `imm_chosen`
  (Boolean) for whether the immediate rewards were chosen, and `rt` for
  the reaction times (in seconds). Other columns can also be present but
  will be ignored.

- discount_function:

  A string specifying the name of the discount functions to use, or an
  object of class `td_fn` (used for creating custom discount functions),
  or a list of objects of class `td_fn`.

- gamma_par_starts:

  A vector of starting values to try for the "gamma" parameter (drift
  rate multiplier or sharpness parameter) during optimization.

- beta_par_starts:

  A vector of starting values to try for the "beta" parameter (bias)
  during optimization.

- alpha_par_starts:

  A vector of starting values to try for the "alpha" parameter (boundary
  separation) during optimization.

- tau_par_starts:

  A vector of starting values to try for the "tau" parameter
  (non-decision time) during optimization.

- drift_transform:

  A transform to apply to drift rates. Either `"none"` (no transform),
  `"logis"` (sigmoidal transform described by Peters & D'Esposito, 2020,
  [doi:10.1371/journal.pcbi.1007615](https://doi.org/10.1371/journal.pcbi.1007615)
  , and Fontanesi et al., 2019,
  [doi:10.3758/s13423-018-1554-2](https://doi.org/10.3758/s13423-018-1554-2)
  ).

- bias_adjust:

  Experimental feature. See not below.

- silent:

  Boolean (true by default). The call to
  [`optim()`](https://rdrr.io/r/stats/optim.html) occurs within a
  [`try()`](https://rdrr.io/r/base/try.html) wrapper. The value of
  `silent` is passed along to
  [`try()`](https://rdrr.io/r/base/try.html).

- optim_args:

  Additional arguments to pass to
  [`optim()`](https://rdrr.io/r/stats/optim.html). Default is
  `list(silent = TRUE)`.

## Value

An object of class `td_bcnm` with components `data` (containing the data
used for fitting), `config` (containing the internal configuration of
the model, including the `discount_function`), and `optim` (the output
of [`optim()`](https://rdrr.io/r/stats/optim.html)).

## Note

Drift rates are computed based on the difference in subjective values
between the immediate and delayed rewards. In theory, when they are
equally valued, they should have equal probability of being chosen.
However, this is only true when the bias parameter of the drift
diffusion model (`beta`) is 0.5 (i.e., no bias). To make sure the
immediate and delayed reward have equal probability of being chosen when
they are equally valued, we can set `bias_adjust = TRUE` to add a bias
correction factor to the drift rate. However, this feature is
experimental and its effects on model fit etc. have not been tested.

## See also

Other drift diffusion model functions:
[`coef.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/coef.td_ddm.md),
[`deviance.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/deviance.td_ddm.md),
[`fitted.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_ddm.md),
[`logLik.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/logLik.td_ddm.md),
[`predict.td_ddm()`](https://kinleyid.github.io/tempodisco/reference/predict.td_ddm.md)

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
ddm <- td_ddm(td_bc_single_ptpt, discount_function = 'exponential',
              gamma_par_starts = 0.01,
              beta_par_starts = 0.5,
              alpha_par_starts = 3.5,
              tau_par_starts = 0.9)
# }
```
