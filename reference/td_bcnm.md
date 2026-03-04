# Temporal discounting binary choice nonlinear model

Compute a binary choice model for a single subject

## Usage

``` r
td_bcnm(
  data,
  discount_function = "franck-2015",
  choice_rule = c("logistic", "probit", "power"),
  fixed_ends = FALSE,
  fit_err_rate = FALSE,
  gamma_par_starts = c(0.01, 1, 100),
  eps_par_starts = c(0.01, 0.25),
  silent = TRUE,
  optim_args = list(),
  ...
)
```

## Arguments

- data:

  A data frame with columns `val_imm` and `val_del` for the values of
  the immediate and delayed rewards, `del` for the delay, and
  `imm_chosen` (Boolean) for whether the immediate reward was chosen.
  Other columns can also be present but will be ignored.

- discount_function:

  A string specifying the name of the discount functions to use, or an
  object of class `td_fn` (used for creating custom discount functions),
  or a list of objects of class `td_fn`.

- choice_rule:

  A string specifying whether the `'logistic'` (default), `'probit'`, or
  `'power'` choice rule should be used.

- fixed_ends:

  A Boolean (false by default) specifying whether the model should
  satisfy the desiderata that subjects should always prefer something
  over nothing (i.e., nonzero delayed reward over nothing) and the same
  reward sooner rather than later. See here:
  https://doi.org/10.1016/j.jmp.2025.102902

- fit_err_rate:

  A Boolean (false by default) specifying whether the model should
  include an error rate (parameterized by "eps"). See Eq. 5 here:
  https://doi.org/10.3758/s13428-015-0672-2.

- gamma_par_starts:

  A vector of starting values to try for the "gamma" parameter (which
  controls the steepness of the choice rule) during optimization.

- eps_par_starts:

  A vector of starting values to try for the "eps" parameter (which
  controls the error rate) during optimization. Ignored if
  \`fit_err_rate = FALSE\`.

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

- ...:

  Additional arguments to provide finer-grained control over the choice
  rule. Note that using a custom choice rule causes the `choice_rule`
  and `fixed_ends` arguments to be ignored.

## Value

An object of class `td_bcnm` with components `data` (containing the data
used for fitting), `config` (containing the internal configuration of
the model, including the `discount_function`), and `optim` (the output
of [`optim()`](https://rdrr.io/r/stats/optim.html)).

## See also

Other nonlinear binary choice model functions:
[`coef.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/coef.td_bcnm.md),
[`deviance.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/deviance.td_bcnm.md),
[`fitted.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/fitted.td_bcnm.md),
[`logLik.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/logLik.td_bcnm.md),
[`predict.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/predict.td_bcnm.md),
[`residuals.td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/residuals.td_bcnm.md)

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
mod <- td_bcnm(td_bc_single_ptpt, discount_function = "hyperbolic", fixed_ends = TRUE)
# Custom discount function
custom_discount_function <- td_fn(
  name = 'custom',
  fn = function(data, p) (1 - p['b'])*exp(-p['k']*data$del) + p['b'],
  par_starts = list(k = c(0.001, 0.1), b = c(0.001, 0.1)),
  par_lims = list(k = c(0, Inf), b = c(0, 1)),
  ED50 = function(...) 'non-analytic'
)
mod <- td_bcnm(td_bc_single_ptpt, discount_function = custom_discount_function, fit_err_rate = TRUE)
# }
```
