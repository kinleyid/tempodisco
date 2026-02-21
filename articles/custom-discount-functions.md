# Creating custom discount functions

``` r
library(tempodisco)
```

## Example 1: hyperbolic function with magnitude effect

The first step to create a custom discount function is to define a
function that computes an indifference point given arguments `data` (a
dataframe) and `p` (a vector of parameters). For example, the following
describes hyperbolic discounting with the magnitude effect accounted for
as in [Vincent (2015)](https://doi.org/10.3758/s13428-015-0672-2):

$$k = \exp\left\lbrack m\log\text{val}_{\text{del}} + c\prime \right\rbrack$$$$k = c \cdot {\text{val}_{\text{del}}}^{m}$$

``` r
indiff_fn <- function(data, p) {
  k <- p['c'] * data$val_del^p['m']
  1 / (1 + k * data$del)
}
```

Next, we specify the range of values each parameter can take on. $k$
should always be positive, meaning $c$ above should be positive. $m$ can
take on any value.

``` r
par_lims <- list(c = c(0, Inf))
```

For optimization, the function must be initially evaluated at some set
of parameter values. We can specify these as a similar list:

``` r
par_starts <- list(m = c(-1, 0, 1),
                   c = c(-10, -5, -1))
```

When we provide more than one possible starting value per parameter in
this way, each combination of starting values will be tried during
optimization, and the best resulting fit will be kept.

Optionally, we can define a function to compute the ED50 (the delay at
which the function returns 0.5). In this case, the ED50 is:

``` r
ED50_fn <- function(p, val_del) {
  k <- p['c'] * val_del^p['m']
  1 / k
}
```

If we do not define such a function, the
[`ED50()`](https://kinleyid.github.io/tempodisco/reference/ED50.md)
method will solve for the ED50 value numerically.

With these ingredients, we can create our custom discount function with
a call to
[`td_fn()`](https://kinleyid.github.io/tempodisco/reference/td_fn.md):

``` r
custom_discount_function <- td_fn(name = 'hyp-mag-eff',
                                  fn = indiff_fn,
                                  par_starts = par_starts,
                                  par_lims = par_lims,
                                  ED50 = ED50_fn)
print(custom_discount_function)
#> 
#> "hyp-mag-eff" temporal discounting function
#> 
#> Indifference points:
#> {
#>     k <- c * val_del^m
#>     1/(1 + k * del)
#> }
#> 
#> Parameter limits:
#> 0 < c < Inf
#> -Inf < m < Inf
```

The next step is to try fitting the model:

``` r
data("td_bc_single_ptpt")
mod <- td_bcnm(td_bc_single_ptpt, discount_function = custom_discount_function)
print(mod)
#> 
#> Temporal discounting binary choice model
#> 
#> Discount function: hyp-mag-eff, with coefficients:
#> 
#>          m          c      gamma 
#> 0.06467234 0.01230133 0.06721089 
#> 
#> Config:
#>  noise_dist: logis
#>  gamma_scale: linear
#>  transform: identity
#> 
#> ED50: 57.7394519246891
#> AUC: 0.0658081315115585
#> BIC: 39.8740195369945
```

## Example 2: dual-systems hyperbolic

Let’s create a discount function similar to the dual-systems function of
(Van den Bos & McClure, (2013))\[<https://doi.org/10.1002/jeab.6>\], but
with hyperbolic discounting in each system rather than exponential.

``` r
dsh <- td_fn(name = 'dual-systems-hyperbolic',
             fn = function(data, p) {
               p['w'] * 1/(1 + p['k1']*data$del) + (1 - p['w']) * 1/(1 + p['k2']*data$del)
             },
             par_starts = list(k1 = c(0.001, 0.0001),
                               k2 = c(0.1, 0.01),
                               w = 0.5),
             par_lims = list(w = c(0, 1),
                             k1 = c(0, Inf),
                             k2 = c(0, Inf)),
             par_chk = function(p) {
               # Ensure k1 < k2
               if (p['k1'] > p['k2']) {
                 # Switch k1 and k2
                 k2 <- p['k1']
                 k1 <- p['k2']
                 p['k1'] <- k1
                 p['k2'] <- k2
                 # Complement of w
                 p['w'] <- 1 - p['w']
               }
               return(p)
             })
print(dsh)
#> 
#> "dual-systems-hyperbolic" temporal discounting function
#> 
#> Indifference points:
#> {
#>     w * 1/(1 + k1 * del) + (1 - w) * 1/(1 + k2 * del)
#> }
#> 
#> Parameter limits:
#> 0 < w < 1
#> 0 < k1 < Inf
#> 0 < k2 < Inf
```

Here, we’ve added a parameter checker function `par_chk` which will
ensure certain conditions are met for our parameters. We’ll define
$k_{1}$ as our patient system, meaning we want to ensure $k_{1} < k2$.
Note that we haven’t specified a function to compute ED50. With the
model defined, we can fit it to our data:

``` r
mod <- td_bcnm(td_bc_single_ptpt, discount_function = dsh)
print(mod)
#> 
#> Temporal discounting binary choice model
#> 
#> Discount function: dual-systems-hyperbolic, with coefficients:
#> 
#>         k1         k2          w      gamma 
#> 0.01726021 0.01729258 0.49511114 0.06749238 
#> 
#> Config:
#>  noise_dist: logis
#>  gamma_scale: linear
#>  transform: identity
#> 
#> ED50: 57.8818909718295
#> AUC: 0.0659320948311096
#> BIC: 44.1751992280079
```
