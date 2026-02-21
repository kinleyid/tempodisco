# Extract model coefficients

Get coefficients of a temporal discounting binary choice linear model.

## Usage

``` r
# S3 method for class 'td_bclm'
coef(object, df_par = TRUE, ...)
```

## Arguments

- object:

  An object of class `td_bcnm`.

- df_par:

  Boolean specifying whether the coefficients returned should be the
  parameters of a discount function (versus the beta parameters from the
  regression).

- ...:

  Additional arguments currently not used.

## Value

A named vector of coefficients.
