
# tempodisco

[![R-CMD-check](https://github.com/kinleyid/tempodisco/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kinleyid/tempodisco/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/kinleyid/tempodisco/graph/badge.svg?token=CCQXS3SNGB)](https://codecov.io/github/kinleyid/tempodisco)

An R package for fitting models of temporal discounting, as described in the paper "[Probabilistic models of delay discounting: improving plausibility and performance](https://doi.org/10.31234/osf.io/y2fdh)."

## Installation
```
require(devtools)
install_github("kinleyid/tempodisco");
```

## Description
This package allows you to fit 2 types of models: **binary choice models** (BCMs), which are probabilistic models of individual binary choices, and **indifference point models** (IPMs), which are models of indifference points. In both cases, this package tests 7 candidate discount functions and identifies the best fit to an individual's data (the one yielding the lowest BIC):

| Name | Functional form |
|------|-----------------|
| Exponential (Samuelson, 1937) |	$f(t; k) = e^{-k t}$ |
| Scaled exponential (beta-delta; Laibson, 1997) | $f(t; k, w) = w e^{-k t}$ |
| Nonlinear-time exponential (Ebert & Prelec, 2007) | $f(t; k, s) = e^{-k t^s}$ |
| Dual-systems exponential (Ven den Bos & McClure, 2013) | $f(t; k_1, k_2, w) = w e^{-k_1 t} + (1 - w) e^{-k_2 t}$ |
| Inverse q-exponential (Green & Myerson, 2004) | $f(t; k, s) = \frac{1}{(1 + k t)^s}$ |
| Hyperbolic (Mazur, 1987) | $f(t; k) = \frac{1}{1 + kt}$ |
| Nonlinear-time hyperbolic (Rachlin, 2006) | $f(t; k, s) = \frac{1}{1 + k t^s}$ |

Additionally, 2 other discount functions are tested: a "model-free" function in which each indifference point is a different parameter, and a "noise" function that only specifies an intercept (i.e., $f(t; c) = c$).

## Example usage

### Fitting binary choice models: `td_bcm` and `td_bclm`

To fit a binary choice model, we need data from a single participant formatted as follows:

```R
data("td_bc_single_ptpt")
```

| val_imm | val_del | del | imm_chosen |
|--|--|--|--|
|112 |    187 |   30.4167 |      FALSE |
|37 |     186 | 3652.5000  |      TRUE |
| ... | ... | ... | ... |

Here, each row corresponds to a difference decision. *val_imm* specifies the value of the immediate reward, *val_del* specifies the value of the delayed reward, *del* speifies the delay of the delayed reward, and *imm_chosen* specifies whether the participant selected the immediate reward. There can be additional columns (e.g., containing reaction times or a participant identifier), but at least these ones are required.

From here, we can fit a binary choice model:

```R
mod <- td_bcm(td_bc_single_ptpt)
```

By default, all of the discount functions above are tested. If we are interested in only a subset of these, we can specify them as follows:

```R
mod <- td_bcm(td_bc_single_ptpt, discount_function = c('hyperbolic', 'exponential'))
```

From here, we can plot the model and get various useful pieces of information:

```R
plot(mod, log = "x") # Plot a summary of the model
coef(mod) # Extract coefficients
AUC(mod) # Compute the area under the curve
ED50(mod) # Compute the median effective delay ([Yoon & Higgins, 2008](https://doi.org/10.1016/j.drugalcdep.2007.12.011))
BIC(mod) # Bayesian information criterion
```

<img src="https://github.com/user-attachments/assets/bf997df2-b110-42c8-aaa2-7f8973bd9d18" width="400">

In the above, red points are those where the individual chose the immediate reward, blue the later reward.

Alternatively, we can fit a generalized linear model with terms chosen so that we can recover a discount function parameterized by the coefficients from max. likelihood estomation. This approach was introduced for the hyperbolic discount function by [Wileyto et al.](https://doi.org/10.3758/BF03195548) and we extended it to other discount functions [here](https://doi.org/10.31234/osf.io/y2fdh).

| Name | Discount function | Linear predictor | Parameters |
|--|--|--|--|
| `hyperbolic.1` | $\frac{1}{1 + kt}$ | $\beta_1 \left(1 - \frac{v_D}{v_I} \right) + \beta_2 t$ | $k = \frac{\beta_2}{\beta_1}$ | 
| `hyperbolic.2` | $\frac{1}{1 + kt}$ | $\beta_1\left( \sigma^{-1}\left[\frac{v_\mathcal{I}}{v_\mathcal{D}}\right] + \log t \right) + \beta_2$ | $k = e^\frac{\beta_2}{\beta_1}$ |
| `exponential.1` | $e^{-kt}$ | $\beta_1 \log \frac{v_I}{v_D} + \beta_2 t$ | $k = \frac{\beta_2}{\beta_1}$ |
| `exponential.2` | $e^{-kt}$ | $\beta_1\left( G^{-1}\left[\frac{v_\mathcal{I}}{v_\mathcal{D}}\right] + \log t \right) + \beta_2$ | $k = e^\frac{\beta_2}{\beta_1}$ |
| `scaled.exponential` | $w e^{-kt}$ | $\beta_1\log\frac{v_{I}}{v_{D}} + \beta_2 t + \beta_3$ | $k = \frac{\beta_2}{\beta_1}$, $w = e^{-\frac{\beta_3}{\beta_1}}$ |
| `nonlinear-time-hyperbolic` | $\frac{1}{1 + k t^s}$ | $\beta_1 \sigma^{-1}\left[\frac{v_{I}}{v_{D}}\right] + \beta_2\log t + \beta_3$ | $k = e^\frac{\beta_3}{\beta_1}$, $s = \frac{\beta_2}{\beta_1}$ |
| `nonlinear-time-exponential` | $e^{-kt^s}$ | $\beta_1 G^{-1}\left[\frac{v_\mathcal{I}}{v_\mathcal{D}}\right] + \beta_2\log t + \beta_3$ | $k = e^\frac{\beta_3}{\beta_1}$, $s = \frac{\beta_2}{\beta_1}$ |

where $\sigma^{-1}[\cdot]$ is the logit function, or the quantile function of a standard logistic distribution, and $G^{-1}[\cdot]$ is the quantile function of a standard Gumbel distribution. `td_bclm` objects are just `glm`s and all of the `glm`-specific generic functions will work.

### Fitting indifference point models: `td_ipm()`

If you have precomputed indifference points (e.g., from an adjusting amounts procedure), you can fit a discounting curve to these as long as they are formatted as a table with columns for the delay (`del`) and corresponding indifference point (`indiff`):

```R
data("td_ip_simulated_ptpt")
```

| del | indiff |
| -- | -- |
| 3 | 0.99 |
| 7 | 0.98 |
| ... | ... |

From here, we can fit model the indifference points as follows:

```R
mod <- td_ipm(td_ip_simulated_ptpt, discount_function = c('hyperbolic', 'exponential'))
plot(mod, log = 'x')
coef(mod) # Extract coefficients
```

<img src="https://github.com/user-attachments/assets/20b81461-48fa-40a5-990c-89808e3b8e6d" width="400">


