# Package index

## Temporal discounting models

- [`td_ipm()`](https://kinleyid.github.io/tempodisco/reference/td_ipm.md)
  : Temporal discounting indifference point model
- [`td_bclm()`](https://kinleyid.github.io/tempodisco/reference/td_bclm.md)
  : Temporal discounting binary choice linear model
- [`td_bcnm()`](https://kinleyid.github.io/tempodisco/reference/td_bcnm.md)
  : Temporal discounting binary choice nonlinear model
- [`td_ddm()`](https://kinleyid.github.io/tempodisco/reference/td_ddm.md)
  : Temporal discounting drift diffusion model

## Discount functions

- [`td_fn()`](https://kinleyid.github.io/tempodisco/reference/td_fn.md)
  : Predefined or custom discount function
- [`discount_function()`](https://kinleyid.github.io/tempodisco/reference/discount_function.md)
  : Get discount function from model
- [`get_available_discount_functions()`](https://kinleyid.github.io/tempodisco/reference/get_available_discount_functions.md)
  : Get all available pre-defined discount functions

## Scoring response data

- [`adj_amt_indiffs()`](https://kinleyid.github.io/tempodisco/reference/adj_amt_indiffs.md)
  : Indifference points from adjusting amount procedure
- [`indiffs()`](https://kinleyid.github.io/tempodisco/reference/indiffs.md)
  : Get model-free indifference points
- [`kirby_score()`](https://kinleyid.github.io/tempodisco/reference/kirby_score.md)
  : Kirby MCQ-style scoring
- [`wileyto_score()`](https://kinleyid.github.io/tempodisco/reference/wileyto_score.md)
  : Wileyto score a questionnaire
- [`most_consistent_indiffs()`](https://kinleyid.github.io/tempodisco/reference/most_consistent_indiffs.md)
  : Experimental method for computing indifference points

## Data quality checks

- [`nonsys()`](https://kinleyid.github.io/tempodisco/reference/nonsys.md)
  : Check for non-systematic discounting
- [`attention_checks()`](https://kinleyid.github.io/tempodisco/reference/attention_checks.md)
  : Test for failed attention checks
- [`kirby_consistency()`](https://kinleyid.github.io/tempodisco/reference/kirby_consistency.md)
  : Compute consistency score
- [`invariance_checks()`](https://kinleyid.github.io/tempodisco/reference/invariance_checks.md)
  : Check for invariant responding

## Model-agnostic measures of discounting

- [`AUC()`](https://kinleyid.github.io/tempodisco/reference/AUC.md) :
  Area under the curve (AUC)
- [`ED50()`](https://kinleyid.github.io/tempodisco/reference/ED50.md) :
  Median effective delay

## Methods

- [`plot(`*`<td_um>`*`)`](https://kinleyid.github.io/tempodisco/reference/plot.td_um.md)
  : Plot models
- [`coef(`*`<td_bclm>`*`)`](https://kinleyid.github.io/tempodisco/reference/coef.td_bclm.md)
  : Extract model coefficients
- [`coef(`*`<td_bcnm>`*`)`](https://kinleyid.github.io/tempodisco/reference/coef.td_bcnm.md)
  : Extract model coefficients
- [`coef(`*`<td_ddm>`*`)`](https://kinleyid.github.io/tempodisco/reference/coef.td_ddm.md)
  : Extract model coefficients
- [`coef(`*`<td_ipm>`*`)`](https://kinleyid.github.io/tempodisco/reference/coef.td_ipm.md)
  : Extract model coefficients
- [`deviance(`*`<td_bcnm>`*`)`](https://kinleyid.github.io/tempodisco/reference/deviance.td_bcnm.md)
  : Model deviance
- [`deviance(`*`<td_ddm>`*`)`](https://kinleyid.github.io/tempodisco/reference/deviance.td_ddm.md)
  : Model deviance
- [`fitted(`*`<td_bcnm>`*`)`](https://kinleyid.github.io/tempodisco/reference/fitted.td_bcnm.md)
  : Get fitted values
- [`fitted(`*`<td_ddm>`*`)`](https://kinleyid.github.io/tempodisco/reference/fitted.td_ddm.md)
  : Get fitted values
- [`fitted(`*`<td_ipm>`*`)`](https://kinleyid.github.io/tempodisco/reference/fitted.td_ipm.md)
  : Get fitted values
- [`logLik(`*`<td_bcnm>`*`)`](https://kinleyid.github.io/tempodisco/reference/logLik.td_bcnm.md)
  : Extract log-likelihood
- [`logLik(`*`<td_ddm>`*`)`](https://kinleyid.github.io/tempodisco/reference/logLik.td_ddm.md)
  : Extract log-likelihood
- [`logLik(`*`<td_ipm>`*`)`](https://kinleyid.github.io/tempodisco/reference/logLik.td_ipm.md)
  : Extract log-likelihood
- [`predict(`*`<td_bclm>`*`)`](https://kinleyid.github.io/tempodisco/reference/predict.td_bclm.md)
  : Model Predictions
- [`predict(`*`<td_bcnm>`*`)`](https://kinleyid.github.io/tempodisco/reference/predict.td_bcnm.md)
  : Model Predictions
- [`predict(`*`<td_ddm>`*`)`](https://kinleyid.github.io/tempodisco/reference/predict.td_ddm.md)
  : Model Predictions
- [`predict(`*`<td_ipm>`*`)`](https://kinleyid.github.io/tempodisco/reference/predict.td_ipm.md)
  : Model Predictions
- [`residuals(`*`<td_bcnm>`*`)`](https://kinleyid.github.io/tempodisco/reference/residuals.td_bcnm.md)
  : Residuals from temporal discounting model
- [`residuals(`*`<td_ipm>`*`)`](https://kinleyid.github.io/tempodisco/reference/residuals.td_ipm.md)
  : Residuals from temporal discounting model

## Datasets

- [`td_bc_single_ptpt`](https://kinleyid.github.io/tempodisco/reference/td_bc_single_ptpt.md)
  : Binary choice data for a single participant
- [`adj_amt_sim`](https://kinleyid.github.io/tempodisco/reference/adj_amt_sim.md)
  : Simulated adjusting amount procedure
- [`td_ip_simulated_ptpt`](https://kinleyid.github.io/tempodisco/reference/td_ip_simulated_ptpt.md)
  : Simulated indifference point data for a single participant
- [`td_bc_study`](https://kinleyid.github.io/tempodisco/reference/td_bc_study.md)
  : Binary choice data for a study
