# Kirby MCQ-style scoring

Score a set of responses according to the method of Kirby et al. (1999,
[doi:10.1037//0096-3445.128.1.78](https://doi.org/10.1037//0096-3445.128.1.78)
). This is described in detail in Kaplan et al. (2016,
[doi:10.1007/s40614-016-0070-9](https://doi.org/10.1007/s40614-016-0070-9)
).

## Usage

``` r
kirby_score(data, discount_function = c("hyperbolic", "exponential", "power"))
```

## Arguments

- data:

  Responses to score.

- discount_function:

  Should \\k\\ values be computed according to the hyperbolic,
  exponential, or power discount function? The original method uses the
  hyperbolic, but in principle the exponential and power are also
  possible (though these should be considered experimental features).

## Value

An object of class
[`td_ipm`](https://kinleyid.github.io/tempodisco/reference/td_ipm.md).

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
mod <- kirby_score(td_bc_single_ptpt)
# }
```
