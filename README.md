
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tempodisco

<!-- badges: start -->

[![R-CMD-check](https://github.com/kinleyid/tempodisco/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kinleyid/tempodisco/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/kinleyid/tempodisco/graph/badge.svg?token=CCQXS3SNGB)](https://app.codecov.io/github/kinleyid/tempodisco)

<!-- badges: end -->

[`tempodisco`](https://kinleyid.github.io/tempodisco/index.html) is an R
package for behavioural researchers working with delay discounting data
(also known as temporal discounting intertemporal choice data). It
implements common tasks such as scoring responses (e.g. computing
indifference points from an adjusting amounts procedure, computing the
“area under the curve”, or computing $k$ values as in the Monetary
Choice Questionnaire; [Frye et al.,
2016](https://doi.org/10.3791/53584); [Myerson et al.,
2001](https://doi.org/10.1901/jeab.2001.76-235); [Kirby et al.,
1999](https://doi.org/10.1037//0096-3445.128.1.78)), identifying
poor-quality data (e.g. failed attention checks and non-systematic
responding; [Johnson & Bickel,
2008](https://doi.org/10.1037/1064-1297.16.3.264)), modelling choice
data using multiple discount functions (e.g. hyperbolic, exponential,
etc.—see below; [Franck et al.,
2015](https://doi.org/10.1002/jeab.128)), and modelling reaction times
using drift diffusion models ([Peters & D’Esposito,
2020](https://doi.org/10.1371/journal.pcbi.1007615)).

## Installation

You can install `tempodisco` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("kinleyid/tempodisco")
```

## Getting started

See the [documentation](https://kinleyid.github.io/tempodisco/),
particularly the “[Getting
started](https://kinleyid.github.io/tempodisco/articles/tempodisco.html)”
page, for example usage.

## Overview

A good practice in delay discounting research is to not assume that the
same discount function describes every individual ([Franck et al.,
2015](https://doi.org/10.1002/jeab.128)). `tempodisco` implements the
following discount functions and can automatically select the best one
for a given individual according to the Bayesian information criterion
([Schwartz, 1978](https://doi.org/10.1214/aos/1176344136)):

| Name | Functional form | Other names |
|----|----|----|
| `exponential` ([Samuelson, 1937](https://doi.org/10.2307/2967612)) | $f(t; k) = e^{-k t}$ |  |
| `scaled-exponential` ([Laibson, 1997](https://doi.org/10.1162/003355397555253)) | $f(t; k, w) = w e^{-k t}$ | Quasi-hyperbolic; beta-delta |
| `nonlinear-time-exponential` ([Ebert & Prelec, 2007](https://doi.org/10.1287/mnsc.1060.0671)) | $f(t; k, s) = e^{-k t^s}$ | Constant sensitivity |
| `dual-systems-exponential` ([Ven den Bos & McClure, 2013](https://doi.org/10.1002/jeab.6)) | $f(t; k_1, k_2, w) = w e^{-k_1 t} + (1 - w) e^{-k_2 t}$ |  |
| `inverse-q-exponential` ([Green & Myerson, 2004](https://doi.org/10.1037/0033-2909.130.5.769)) | $f(t; k, s) = \frac{1}{(1 + k t)^s}$ | Generalized hyperbolic ([Loewenstin & Prelec](https://doi.org/10.2307/2118482)); hyperboloid ([Green & Myerson, 2004](https://doi.org/10.1037/0033-2909.130.5.769)); q-exponential ([Han & Takahashi, 2012](https://doi.org/10.1016/j.physa.2012.07.012)) |
| `hyperbolic` ([Mazur, 1987](https://doi.org/10.4324/9781315825502)) | $f(t; k) = \frac{1}{1 + kt}$ |  |
| `nonlinear-time-hyperbolic` ([Rachlin, 2006](https://doi.org/10.1901/jeab.2006.85-05)) | $f(t; k, s) = \frac{1}{1 + k t^s}$ | Power-function ([Rachlin, 2006](https://doi.org/10.1901/jeab.2006.85-05)) |
| `power` ([Harvey, 1986](https://doi.org/10.1287/mnsc.32.9.1123)) | $f(t; k) = \frac{1}{(1 + t)^k}$ |  |

These discount functions can be fit to indifference point data (see
[`td_ipm`](https://kinleyid.github.io/tempodisco/reference/td_ipm.html)),
choice-level data (see
[`td_bcnm`](https://kinleyid.github.io/tempodisco/reference/td_bcnm.html)),
or data including both choices and reaction times (see
[`td_ddm`](https://kinleyid.github.io/tempodisco/reference/td_ddm.html)).

After fitting a model, we can check to see how well it matches the data
using the
[`plot()`](https://kinleyid.github.io/tempodisco/reference/plot.td_um.html)
function:

``` r
library(tempodisco)
data("td_bc_single_ptpt")
mod <- td_bcnm(td_bc_single_ptpt, discount_function = c('hyperbolic', 'exponential'))
plot(mod, p_lines = c(0.1, 0.9), log = 'x', verbose = F)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Note that the discount curve contains an inflection point because the
x-axis is on a log scale. See the “[Visualizing
models](https://kinleyid.github.io/tempodisco/articles/visualizing-models.html)”
page of the documentation for more examples.

## Further reading

The “Examples” tab on [the
documentation](https://kinleyid.github.io/tempodisco/) contains a list
of tutorials on solving common problems in delay discounting research.

## Reporting issues and requesting features

If you encounter problems with the software or would like to it to have
additional functionality, please open a new issue on the GitHub
repository. Try to include as much detail as possible, especially how to
reproduce any errors/incorrect results. GitHub has instructions on
opening an issue
[here](https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/creating-an-issue).

## Contributing

If you would like to contribute to `tempodisco`, you’re more than
welcome! Please follow the instructions
[here](https://docs.github.com/en/get-started/exploring-projects-on-github/contributing-to-a-project)
on how to contribute to a project on GitHub. Feel free to [contact
me](https://kinleyid.github.io) if you’d like help with any
contributions.
