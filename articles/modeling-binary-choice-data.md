# Modeling binary choice data

``` r
library(tempodisco)
```

## Classic methods

For binary choice data not explicitly designed to [titrate out
indifference
points](https://kinleyid.github.io/tempodisco/articles/adjusting-amounts.html)
(as in an adjusting amount procedure), there are a few widely-used
traditional scoring methods to quantify discounting.

### Kirby scoring

One scoring method is the one designed for the Monetary Choice
Questionnaire [(Kirby,
1999)](https://doi.org/10.1037//0096-3445.128.1.78):

``` r
data("td_bc_single_ptpt")
mod <- kirby_score(td_bc_single_ptpt)
print(mod)
#> 
#> Temporal discounting indifference point model
#> 
#> Discount function: hyperbolic, with coefficients:
#> 
#>          k 
#> 0.02176563 
#> 
#> ED50: 45.9439876371218
#> AUC: 0.0551987542147013
```

Although this method computes $k$ values according to the hyperbolic
discount function, in principle it’s possible to use other
single-parameter discount functions (though this is not an established
practice and should be considered an experimental feature of
`tempodisco`):

``` r
mod_exp <- kirby_score(td_bc_single_ptpt, discount_function = 'exponential')
print(mod_exp)
#> 
#> Temporal discounting indifference point model
#> 
#> Discount function: exponential, with coefficients:
#> 
#>           k 
#> 0.008170247 
#> 
#> ED50: 84.8379742026149
#> AUC: 0.0335100135964859
mod_pow <- kirby_score(td_bc_single_ptpt, discount_function = 'power')
print(mod_pow)
#> 
#> Temporal discounting indifference point model
#> 
#> Discount function: power, with coefficients:
#> 
#>         k 
#> 0.3052023 
#> 
#> ED50: 8.69012421478859
#> AUC: 0.1173431135372
mod_ari <- kirby_score(td_bc_single_ptpt, discount_function = 'arithmetic')
print(mod_ari)
#> 
#> Temporal discounting indifference point model
#> 
#> Discount function: arithmetic, with coefficients:
#> 
#>        k 
#> 1.034244 
#> 
#> ED50: 95.8739950116839
#> AUC: 0.0262488714558682
```

### Wileyto scoring

It is also possible to use the logistic regression method of [Wileyto et
al. (2004)](https://doi.org/10.3758/BF03195548), where we can solve for
the $k$ value of the hyperbolic discount function in terms of the
regression coefficients:

``` r
mod <- wileyto_score(td_bc_single_ptpt)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
print(mod)
#> 
#> Temporal discounting binary choice linear model
#> 
#> Discount function: hyperbolic from model hyperbolic.1, with coefficients:
#> 
#>          k 
#> 0.04372626 
#> 
#> Call:  glm(formula = fml, family = binomial(link = "logit"), data = data)
#> 
#> Coefficients:
#>     .B1      .B2  
#> 0.49900  0.02182  
#> 
#> Degrees of Freedom: 70 Total (i.e. Null);  68 Residual
#> Null Deviance:       97.04 
#> Residual Deviance: 37.47     AIC: 41.47
```

## Newer methods

### Linear models

The [Wileyto et al. (2004)](https://doi.org/10.3758/BF03195548) approach
turns out to be possible for other discount functions as well ([Kinley,
Oluwasola & Becker, 2025](https://doi.org/10.1016/j.jmp.2025.102902).):

[TABLE]

Where $\sigma^{- 1}\lbrack \cdot \rbrack$ is the logit function, or the
quantile function of a standard logistic distribution, and
$G^{- 1}\lbrack \cdot \rbrack$ is the quantile function of a standard
Gumbel distribution.

We can test all of these and select the best according to the Bayesian
Information Criterion as follows:

``` r
mod <- td_bclm(td_bc_single_ptpt, model = 'all')
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
print(mod)
#> 
#> Temporal discounting binary choice linear model
#> 
#> Discount function: exponential from model exponential.2, with coefficients:
#> 
#>          k 
#> 0.01003216 
#> 
#> Call:  glm(formula = fml, family = binomial(link = "logit"), data = data)
#> 
#> Coefficients:
#>     .B1      .B2  
#>   3.597  -16.553  
#> 
#> Degrees of Freedom: 70 Total (i.e. Null);  68 Residual
#> Null Deviance:       97.04 
#> Residual Deviance: 15.7  AIC: 19.7
```

### Nonlinear models

To explore a wider range of discount functions, we can fit a nonlinear
model by calling `td_bcnm`. The full list of built-in discount functions
is as follows:

| Name                                                                                               | Functional form                                                          | Notes                                                                                                                                                                                                                                                                      |
|----------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `exponential` ([Samuelson, 1937](https://doi.org/10.2307/2967612))                                 | $f(t;k) = e^{- kt}$                                                      |                                                                                                                                                                                                                                                                            |
| `hyperbolic` ([Mazur, 1987](https://doi.org/10.4324/9781315825502))                                | $f(t;k) = \frac{1}{1 + kt}$                                              |                                                                                                                                                                                                                                                                            |
| `scaled-exponential` ([Laibson, 1997](https://doi.org/10.1162/003355397555253))                    | $f(t;k,w) = we^{- kt}$                                                   | Also known as quasi-hyperbolic or beta-delta and written as $f(t;\beta,\delta) = \beta e^{- \delta t}$                                                                                                                                                                     |
| `nonlinear-time-exponential` ([Ebert & Prelec, 2007](https://doi.org/10.1287/mnsc.1060.0671))      | $f(t;k,s) = e^{- kt^{s}}$                                                | Also known as constant sensitivity                                                                                                                                                                                                                                         |
| `inverse-q-exponential` ([Green & Myerson, 2004](https://doi.org/10.1037/0033-2909.130.5.769))     | $f(t;k,s) = \frac{1}{(1 + kt)^{s}}$                                      | Also known as generalized hyperbolic ([Loewenstin & Prelec](https://doi.org/10.2307/2118482)), hyperboloid ([Green & Myerson, 2004](https://doi.org/10.1037/0033-2909.130.5.769)), or q-exponential ([Han & Takahashi, 2012](https://doi.org/10.1016/j.physa.2012.07.012)) |
| `nonlinear-time-hyperbolic` ([Rachlin, 2006](https://doi.org/10.1901/jeab.2006.85-05))             | $f(t;k,s) = \frac{1}{1 + kt^{s}}$                                        | Also known as power-function ([Rachlin, 2006](https://doi.org/10.1901/jeab.2006.85-05))                                                                                                                                                                                    |
| `dual-systems-exponential` ([Ven den Bos & McClure, 2013](https://doi.org/10.1002/jeab.6))         | $f\left( t;k_{1},k_{2},w \right) = we^{- k_{1}t} + (1 - w)e^{- k_{2}t}$  |                                                                                                                                                                                                                                                                            |
| `additive-utility` ([Killeen, 2009](https://doi.org/10.1037/a0016414))                             | $f(t;k,s,a) = \left( 1 - \frac{k}{V_{D}^{a}}t^{s} \right)^{\frac{1}{a}}$ | $V_{D}$ is the value of the delayed reward. $f(t;k,s,a) = 0$ for $t > \left( V_{D}^{a}/k \right)^{1/s}$.                                                                                                                                                                   |
| `power` ([Harvey, 1986, eq. 2](https://doi.org/10.1287/mnsc.32.9.1123))                            | $f(t;k) = \frac{1}{(1 + t)^{k}}$                                         | In equation 2 of the reference, the discount function is described as $\frac{1}{t^{k}}$, but time begins at $t = 1$.                                                                                                                                                       |
| `arithmetic` ([Doyle & Chen, 2010](https://dx.doi.org/10.2139/ssrn.1609594))                       | $f(t;k) = 1 - \frac{kt}{V_{D}}$                                          | $V_{D}$ is the value of the delayed reward. $f(t;k) = 0$ for $kt > V_{D}$.                                                                                                                                                                                                 |
| `fixed-cost` ([Benhabib, Bisin, & Schotter, 2010](https://doi.org/10.1016/j.geb.2009.11.003))      | $f(t;w) = e^{- kt} - \frac{w}{V_{D}}$                                    | $V_{D}$ is the value of the delayed reward. $f(t;w) = 0$ for $\frac{w}{V_{D}} > e^{- kt}$.                                                                                                                                                                                 |
| `absolute-stationarity` ([Blavatskyy, 2024, eq. 3](https://doi.org/10.1016/j.econlet.2024.111559)) | $f(t;k,s) = \exp\left\{ - k\frac{ts}{ts + 1} \right\}$                   | The original paper uses $t$ rather than $ts$. However, a scale factor appears necessary to account for different time units.                                                                                                                                               |
| `relative-stationarity` ([Blavatskyy, 2024, eq. 7](https://doi.org/10.1016/j.econlet.2024.111559)) | $f(t;k,s) = \left( \frac{ts + 1}{2ts + 1} \right)^{k}$                   | The original paper uses $t$ rather than $ts$. However, a scale factor appears necessary to account for different time units.                                                                                                                                               |
| `constant` ([Franck et al., 2015](https://doi.org/10.1002/jeab.128))                               | $f(t;k) = k$                                                             | Null model; participants can be excluded if this model provides the best fit ([Franck et al., 2015](https://doi.org/10.1002/jeab.128))                                                                                                                                     |
| `nonlinear-time-power`                                                                             | $f(t;k) = \frac{1}{\left( 1 + t^{s} \right)^{k}}$                        | Experimental extension of the `power` discount function along the lines of the `nonlinear-time-hyperbolic` and `nonlinear-time-exponential` functions.                                                                                                                     |
| `nonlinear-time-arithmetic`                                                                        | $f(t;k) = 1 - \frac{kt^{s}}{V_{D}}$                                      | Experimental extension of the `arithmetic` discount function along the lines of the `nonlinear-time-hyperbolic` and `nonlinear-time-exponential` functions.                                                                                                                |
| `scaled-hyperbolic`                                                                                | $f(t;k,w) = \frac{w}{1 + kt}$                                            | Experimental extension of the `hyperbolic` discount function along the lines of the `scaled-exponential` function.                                                                                                                                                         |

``` r
mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'all')
print(mod)
#> 
#> Temporal discounting binary choice model
#> 
#> Discount function: arithmetic, with coefficients:
#> 
#>          k      gamma 
#> 1.05682221 0.08007441 
#> 
#> Config:
#>  noise_dist: logis
#>  gamma_scale: linear
#>  transform: identity
#> 
#> ED50: 93.8257558942611
#> AUC: 0.025688096036785
#> BIC: 25.5896625868426
```

#### Choice rules

Several additional arguments can be used to customize the model. For
example, we can use different choice rules—the “logistic” choice rule is
the default, but the “probit” and “power” choice rules are also
available (see [this
tutorial](https://kinleyid.github.io/tempodisco/articles/choice-rules.html)
for more details):

``` r
# Probit choice rule:
mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'exponential', choice_rule = 'probit')
# Power choice rule:
mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'exponential', choice_rule = 'power')
```

#### Error rates

It is also possible to fit an error rate $\epsilon$ that describes the
probability of the participant making a response error (see [Vincent,
2015](https://doi.org/10.3758/s13428-015-0672-2)). I.e.:
$$P\left( \text{imm} \right) = \epsilon + (1 - 2\epsilon)g^{- 1}\lbrack\eta\rbrack$$
where $P\left( \text{imm} \right)$ is the probability of choosing the
immediate reward, $g$ is the link function, and $\eta$ is the linear
predictor.

``` r
data("td_bc_study")
# Select the second participant
second_ptpt_id <- unique(td_bc_study$id)[2]
df <- subset(td_bc_study, id == second_ptpt_id)
mod <- td_bcnm(df, discount_function = 'exponential', fit_err_rate = T)
plot(mod, type = 'endpoints', verbose = F)
lines(c(0, 1), c(0, 0), lty = 2)
lines(c(0, 1), c(1, 1), lty = 2)
```

![](modeling-binary-choice-data_files/figure-html/unnamed-chunk-10-1.png)

``` r
cat(sprintf("epsilon = %.2f\n", coef(mod)['eps']))
#> epsilon = 0.11
```

We can see that the probability of choosing the immediate reward doesn’t
approach 0 or 1 but instead approaches a value of
$\epsilon \approx 0.11$.

#### Fixed endpoints

Alternatively, we might expect that participants should never choose an
immediate reward worth 0 and should never choose a delayed reward worth
the same face amount as an immediate reward ([Kinley, Oluwasola &
Becker, 2025](https://doi.org/10.1016/j.jmp.2025.102902); see
[here](https://kinleyid.github.io/tempodisco/articles/choice-rules.html#fixed-endpoint-choice-rules)
for more details). We can control this by setting `fixed_ends = T`,
which “fixes” the endpoints of the psychometric curve, where
`val_imm = 0` and `val_imm = val_del`, at 0 and 1, respectively:

``` r
mod <- td_bcnm(df, discount_function = 'exponential', fixed_ends = T)
plot(mod, type = 'endpoints', verbose = F, del = 50, val_del = 200)
```

![](modeling-binary-choice-data_files/figure-html/unnamed-chunk-11-1.png)
