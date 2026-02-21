# Wileyto score a questionnaire

Score a set of responses according to the method of Wileyto et al.
(2004, [doi:10.3758/BF03195548](https://doi.org/10.3758/BF03195548) ).
This function is a thin wrapper to
[`td_bclm`](https://kinleyid.github.io/tempodisco/reference/td_bclm.md).

## Usage

``` r
wileyto_score(data)
```

## Arguments

- data:

  Responses to score.

## Value

An object of class `td_bclm`.

## Examples

``` r
# \donttest{
data("td_bc_single_ptpt")
mod <- wileyto_score(td_bc_single_ptpt)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
# }
```
