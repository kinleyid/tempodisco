# Compute consistency score

Compute the consistency score per the method of Kirby et al. (1999,
[doi:10.1037//0096-3445.128.1.78](https://doi.org/10.1037//0096-3445.128.1.78)
). This is described in detail in Kaplan et al. (2016,
[doi:10.1007/s40614-016-0070-9](https://doi.org/10.1007/s40614-016-0070-9)
), where it's suggested that a consistency score below 0.75 might be a
sign of inattentive responding.

## Usage

``` r
kirby_consistency(
  data,
  discount_function = c("hyperbolic", "exponential", "power", "arithmetic")
)
```

## Arguments

- data:

  Responses to score.

- discount_function:

  Should \\k\\ values be computed according to the hyperbolic,
  exponential, power, or arithmetic discount function? The original
  method uses the hyperbolic, but in principle any single-parameter
  discount function can also be used (though these should be considered
  experimental features).

## Value

A consistency score between 0 and 1.

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
mod <- kirby_consistency(td_bc_single_ptpt)
# }
```
