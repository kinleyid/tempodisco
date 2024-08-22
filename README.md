# tempodisco
An R package for fitting models of delay discounting, as described in the paper "[Probabilistic models of delay discounting: improving plausibility and performance](https://doi.org/10.31234/osf.io/y2fdh)."

## Installation
```
require(devtools)
install_github("kinleyid/tempodisco");
```

## Description
This package tests 7 candidate discount functions and identifies the best fit to an individual's data (the one yielding the lowest AIC):

| Name | Functional form |
|------|-----------------|
| Exponential (Samuelson, 1937) |	$f(t; k) = e^{-k t}$ |
| Scaled exponential (beta-delta; Laibson, 1997) | $f(t; k, w) = w e^{-k t}$ |
| Nonlinear-time exponential (Ebert & Prelec, 2007) | $f(t; k, s) = e^{-k t^s}$ |
| Dual-systems exponential (Ven den Bos & McClure, 2013) | $f(t; k_1, k_2, w) = w e^{-k_1 t} + (1 - w) e^{-k_2 t}$ |
| Inverse q-exponential (Green & Myerson, 2004) | $f(t; k, s) = \frac{1}{(1 + k t)^s}$ |
| Hyperbolic (Mazur, 1987) | $f(t; k) = \frac{1}{1 + kt}$ |
| Nonlinear-time hyperbolic (Rachlin, 2006) | $f(t; k, s) = \frac{1}{1 + k t^s}$ |


This package also allows you to fit probabilistic models that satisfy two desiderata (hence dd**Desid**Models) specified in the above-mentioned paper: that individuals should always prefer something over nothing (i.e., should never choose \$0 over \$100 later) and should always prefer sooner rather than later for equal reward values (i.e., should never choose \$100 later over \$100 now). As shown in the paper, this improves the performance of these models. You can also fit conventional models that don't satisfy these desiderata.

## Example usage

### Fitting probabilistic models: `dd_prob_model()`

`dd_prob_model` fits a probabilistic model of a given individual's delay discounting:

```R
library(tempodisco)
# Generate data
df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
logistic <- function(x) 1 / (1 + exp(-x))
logit <- function(x) log(x / (1 - x))
gamma <- 2
prob <- logistic(gamma*(logit(df$val_imm / df$val_del) - logit(1 / (1 + 0.001*df$del)))) # hyperbolic discounting
df$imm_chosen <- runif(nrow(df)) < prob
# Fit model
mod <- dd_prob_model(df)
print(mod$discount_function_name) # should usually be "hyperbolic"
```

Use `?dd_prob_model` to read the full documentation

### Fitting deterministic models: `dd_det_model()`

If you simply want to fit a discounting curve to a set of indifference points, you can use `dd_det_model` for this:

```R
df <- data.frame(del = exp(1:10), indiff = 1 / (1 + 0.001*exp(1:10)))
# Fit model
mod <- dd_det_model(df)
print(mod$discount_function_name)
```

### Plotting models: `plot_dd()`

To double check that the results make sense, you can plot them using `plot_dd`. For a probabilistic model, this produces the following plot:

<img src="https://github.com/kinleyid/tempodisco/assets/18541620/58efbe67-acc7-4993-81e0-2d896448fb5d" width="400">

It's not publication-ready, but it is a good way of quickly checking the model fit. On the x-axis is the delay and on the y-axis is the relative value of the immediate reward. Each point is a different decision. Red means the immediate reward was chosen and blue means the delayed reward was chosen. The bold line is the best fitting discount curve, and the plot's title tells you which type of discount function it's from. The dashed lines give a sense of how stochastic the decisions were: the wider apart they are, the more stochastic. Just as the discount curve shows all the points where the probability of selecting the immediate reward is 0.5, the dotted lines show where this probability would be 0.4 and 0.6 (these values can be adjusted using the `pr_range` argument to `plot_dd`).

### Predicting indifference points and decision probabilities: `predict_indiffs()` and `predict_prob_imm()`

Given the output of either of the above functions, you can use the function `predict_indiffs` to predict an individual's indifference points:

```R
predict_indiffs(mod) # Predict the indifference points for the data the model was fit on
predict_indiffs(mod, del = 1:100) # Predict indifference points for a new set of delays (e.g., for plotting)
```

Given a probabilistic model (the output of `dd_prob_model`), you can also predict an individual's probability of selecting the immediate reward:

```R
predict_prob_imm(mod) # Predicted probabilities for the data the model was fit on
predict_prob_imm(mod, data = data.frame(del = 1000, val_imm = 1:99, val_del = 100)) # Predicted probabilities for new data
```

### Using ED50 values to measure discounting between different discount functions

To compare discounting between individuals with different discount curves, you can use the "ED50" measure (the delay at which a delayed reward's subjective value is reduced to 50% of its objective value; [Yoon & Higgins, 2008](https://doi.org/10.1016/j.drugalcdep.2007.12.011)). This can be done as follows:

```R
# Generate data
df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
dels <- seq(0, max(df$del), length.out = 1000)
logistic <- function(x) 1 / (1 + exp(-x))
logit <- function(x) log(x / (1 - x))

# Indifference points for 2 different discounters
indiffs_1 <- function(x) 1 / (1 + 0.0005*x)
indiffs_2 <- function(x) exp(-0.0001*x)

# Fit a model for each discounter
prob_1 <- logistic(logit(df$val_imm / df$val_del) - logit(indiffs_1(df$del)))
prob_2 <- logistic(logit(df$val_imm / df$val_del) - logit(indiffs_2(df$del)))
df_1 <- df
df_1$imm_chosen <- runif(nrow(df)) < prob_1
mod_1 <- tempodisco::dd_prob_model(df_1, discount_function = 'hyperbolic')
df_2 <- df
df_2$imm_chosen <- runif(nrow(df)) < prob_2
mod_2 <- tempodisco::dd_prob_model(df_2, discount_function = 'exponential')

# Plot discount functions with ED50 values
plot(1, type = "n", xlab = "delay", ylab = "indifference",
     xlim = c(0, max(df$del)), ylim = c(0, 1))
abline(h = 0.5, col = 'gray')
lines(tempodisco::predict_indiffs(mod_1, dels) ~ dels)
abline(v = mod_1$ED50)
lines(tempodisco::predict_indiffs(mod_2, dels) ~ dels, col = 'red')
abline(v = mod_2$ED50, col = 'red')
```
This results in the following plot (where ED50 values are shown as vertical lines):

![image](https://github.com/kinleyid/tempodisco/assets/18541620/e793b520-063e-4880-84c3-bc586e8530a4)


