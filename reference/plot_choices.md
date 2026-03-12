# Plot choices

Create a plot that displays binary choices at each delay

## Usage

``` r
plot_choices(data, legend = TRUE, ...)
```

## Arguments

- data:

  A data frame with columns `val_imm` and `val_del` for the values of
  the immediate and delayed rewards, `del` for the delay, and
  `imm_chosen` (Boolean) for whether the immediate reward was chosen.

- legend:

  Logical: display a legend?

- ...:

  Additional arguments to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

No return value (called to produce a plot)

## Examples

``` r
# \donttest{
data('td_bc_single_ptpt')
plot_choices(td_bc_single_ptpt)

# }
```
