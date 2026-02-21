# Analyzing data from multiple participants

``` r
library(tempodisco)
```

The other vignettes examine how we can analyze data at the
single-subject level. What about analyzing data from a whole study, from
multiple participants? This vignette will give some quick example
analyses of this kind.

``` r
data("td_bc_study")
# For speed, only look at the first 10 participants
ids <- unique(td_bc_study$id)[1:10]
study <- subset(td_bc_study, id %in% ids)
```

As a first step, we might want to remove cases of non-systematic
discounting:

``` r
# Check for nonsystematic discounting
is_nonsys <- by(study, study$id, function(ptpt) { # Stratify by participant ID
  mod <- td_bcnm(ptpt, discount_function = 'model-free') # Fit a "model-free" discount function to the participant-level data
  any(nonsys(mod)) # Check whether either non-systematic criterion is met
}, simplify = T)
# Keep data from systematic discounters
keep_ids <- rownames(is_nonsys)[!is_nonsys]
study <- subset(study, id %in% keep_ids)
```

Next, we can quantify discounting for each participant:

``` r
rows <- by(study, study$id, function(ptpt) { # Stratify by participant ID
  # Test several discount functions and use the best-fitting one
  mod <- td_bcnm(ptpt, discount_function = c('hyperbolic', 'exponential'))
  # Return a one-row dataframe containing model-agnostic measures of discounting
  data.frame(
    id = ptpt$id[1],
    AUC = AUC(mod),
    ED50 = ED50(mod)
  )
}, simplify = F)
discount_measures <- do.call(rbind, rows)
cor.test(discount_measures$AUC, log(discount_measures$ED50))
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  discount_measures$AUC and log(discount_measures$ED50)
#> t = 14.939, df = 6, p-value = 5.663e-06
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.9262591 0.9977046
#> sample estimates:
#>      cor 
#> 0.986823
```

The resulting table could be merged with another table containing other
measures of interest (e.g., from other questionnaires).

In the absence of other measures of interest, we can ask interesting
questions about discounting per se. For example, are similar estimates
of $k$ produced by Kirby MCQ-style scoring vs. nonlinear model fitting?

``` r
# Compare Kirby scoring to nonlinear modeling
k_vals <- by(study, study$id, function(ptpt) { # Stratify by participant ID
  # Get k from Kirby MCQ scoring
  mod <- kirby_score(ptpt)
  k_kirby <- coef(mod)['k']
  # Get k from nonlinear model fitting
  mod <- td_bcnm(ptpt, discount_function = 'hyperbolic')
  k_nm <- coef(mod)['k']
  # Return a one-row dataframe containing both
  data.frame(
    k_nm = k_nm,
    k_kirby = k_kirby
  )
}, simplify = F)
# Stack all the rows to produce a single dataframe
df <- do.call(rbind, k_vals)
# What's the correlation?
cor.test(log(df$k_nm), log(df$k_kirby))
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  log(df$k_nm) and log(df$k_kirby)
#> t = 31.388, df = 6, p-value = 6.947e-08
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.9826287 0.9994742
#> sample estimates:
#>       cor 
#> 0.9969687
```

Or: how prevalent is hyperbolic versus exponential discounting?

``` r
best_mods <- by(study, study$id, function(ptpt) {
  # Test both the exponential and hyperbolic discount functions
  mod <- td_bcnm(ptpt, discount_function = c('hyperbolic', 'exponential'))
  # Return the name of the best-fitting function
  mod$config$discount_function$name
}, simplify = T)
table(best_mods)
#> best_mods
#> exponential  hyperbolic 
#>           5           3
```
