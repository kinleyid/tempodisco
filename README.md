# ddDesidModels
An R package for fitting probabilistic models of delay discounting, as described in the paper "[Probabilistic models of delay discounting: improving plausibility and performance](https://doi.org/10.31234/osf.io/y2fdh)."

## Installation
```
require(devtools)
install_github("kinleyid/ddDesidModels");
```

## Description
What's unique about this package is the option to fit models that satisfy two desiderata (hence dd**Desid**Models) specified in the above-mentioned paper: that individuals should always prefer something over nothing (i.e., should never choose \$0 over \$100 later) and should always prefer sooner rather than later for equal reward values (i.e., should never choose \$100 later over \$100 now). As shown in the paper, this improves the performance of these models. You can also fit conventional models that don't satisfy these desiderata. By default, the package tests 7 candidate discount functions and identifies the one yielding the lowest AIC:

| Name | Functional form |
|------|-----------------|
| Exponential (Samuelson, 1937) |	$f(t; k) = e^{-k t}$ |
| Scaled exponential (beta-delta; Laibson, 1997) | $f(t; k, w) = w e^{-k t}$ |
| Nonlinear-time exponential (Ebert & Prelec, 2007) | $f(t; k, s) = e^{-k t^s}$ |
| Dual-systems exponential (Ven den Bos & McClure, 2013) | $f(t; k_1, k_2, w) = w e^{-k_1 t} + (1 - w) e^{-k_2 t}$ |
| Inverse q-exponential (Green & Myerson, 2004) | $f(t; k, s) = \frac{1}{(1 + k t)^s}$ |
| Hyperbolic (Mazur, 1987) | $f(t; k) = \frac{1}{1 + kt}$ |
| Nonlinear-time hyperbolic (Rachlin, 2006) | $f(t; k, s) = \frac{1}{1 + k t^s}$ |

## Example usage

The main function is `dd_prob_model`, which fits a probabilistic model of a given individual's delay discounting:

```R
library(ddDesidModels)
# Generate data
df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
logistic <- function(x) 1 / (1 + exp(-x))
logit <- function(x) log(x / (1 - x))
gamma <- 2
prob <- logistic(gamma*(logit(df$val_imm / df$val_del) - logit(1 / (1 + 0.001*df$del)))) # hyperbolic discounting
df$imm_chosen <- runif(nrow(df)) < prob
# Fit model
mod <- ddDesidModels::dd_prob_model(df)
print(mod$discount_function_name) # should usually be "hyperbolic"
```

Given such a model, you can predict both an individual's indifference points at a given set of delays and the probabilities that they will select the immediate reward in a given set of intertemporal choices:

```R
# Plot predicted indifference points
delays <- exp(seq(0, 9, length.out = 100))
indiffs <- ddDesidModels::predict_indiffs(mod, del = delays)
plot(indiffs ~ delays, type = 'l')
# Plot predicted probabilities of selecting immediate reward
df <- data.frame(val_imm = 0:100, val_del = 100, del = 500)
p_imm <- ddDesidModels::predict_prob_imm(mod, data = df)
plot(p_imm ~ seq(0, 1, length.out = 101), type = 'l')
```

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
mod_1 <- ddDesidModels::dd_prob_model(df_1, discount_function = 'hyperbolic')
df_2 <- df
df_2$imm_chosen <- runif(nrow(df)) < prob_2
mod_2 <- ddDesidModels::dd_prob_model(df_2, discount_function = 'exponential')

# Plot discount functions with ED50 values
plot(1, type = "n", xlab = "delay", ylab = "indifference",
     xlim = c(0, max(df$del)), ylim = c(0, 1))
abline(h = 0.5, col = 'gray')
lines(ddDesidModels::predict_indiffs(mod_1, dels) ~ dels)
abline(v = mod_1$ED50)
lines(ddDesidModels::predict_indiffs(mod_2, dels) ~ dels, col = 'red')
abline(v = mod_2$ED50, col = 'red')
```
This results in the following plot (where ED50 values are shown as vertical lines):

![image](https://github.com/kinleyid/ddDesidModels/assets/18541620/e793b520-063e-4880-84c3-bc586e8530a4)


