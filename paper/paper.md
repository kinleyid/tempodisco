---
title: 'tempodisco: an R package for temporal discounting'
tags:
  - R
  - psychology
  - economics
  - behaviour
  - decision making
  - temporal discounting
  - delay discounting
  - intertemporal choice
authors:
  - name: Isaac Kinley
    orcid: 0000-0003-2057-9606
    corresponding: true
    affiliation: 1
affiliations:
 - name: Postdoctoral Fellow, Rotman Research Institute, Canada
   index: 1
date: 2 September 2024
bibliography: paper.bib

---

# Summary

"Temporal discounting" or "delay discounting" refers to the near-universal phenomenon in which humans (and other animals) undervalue future rewards as a function of their delay [@odum2011delay]. The degree to which future rewards are discounted in this way is an important individual difference that is related to a range of real-life outcomes [@story2014does]. Many methods exist for modeling and quantifying discounting, and `tempodisco` is designed to provide easy access to these.

# Statement of need

`tempodisco` is an R package for working with temporal discounting data aimed at behavioural researchers in psychology, economics, and related fields. It is designed to simplify the analysis steps of data cleaning, model fitting, and quantifying discounting. The package implements widely used methods such as computing indifference points from adjusting amount task [@frye2016measuring], testing for non-systematic discounting per the criteria of @johnson2008algorithm, scoring questionnaires according to the methods of @kirby1999heroin and @wileyto2004using, and computing model-agnostic measures of discounting such as area under the curve [@myerson2001area] and ED50 [@yoon2008turning]. The package can also perform approximate Bayesian model selection using a range of discount functions [@franck2015accurate] for models of both choice-level data and indifference points, and implements more recently proposed methods such as drift diffusion models of discounting [@peters2020drift] and "fixed-endpoint" psychometric curves [@kinley2025probabilistic]. Users can also produce simple diagnostic plots to examine model fits. Moreover, the package provides a framework within which users can specify custom discount functions (e.g., discount functions to account for the "magnitude effect"; @green1997rate).

So far, there are relatively few software tools dedicated to working with temporal discounting data: a widely-used Excel-based spreadsheet tool [@kaplan2016automating] implements the @kirby1999heroin scoring method; a Matlab toolbox [@vincent2016hierarchical] implements hierarchical Bayesian modelling of choice-level data; the R package `discountingtools` [@discountingtools] fits a range of discount functions to indifference point data; and the R package `discAUC` [@discAUC] computes the model-free area under the curve for indifference point data. `tempodisco` provides several important functions not available in these tools, such as modeling choice-level data using a range of discount functions, fitting drift diffusion models, and implementing multiple "choice rules" to link subjective valuations to choice probabilities [@wulff2018modeling]. However, `tempodisco` currently fits only individual-level models (vs hierarchical models), only models choices between immediate and delayed rewards (vs choices between two delayed rewards), and only performs maximum-likelihood (vs Bayesian) parameter estimation.

The package is based around a few basic objects such as `td_fn` for discount functions, `td_ipm` for models of indifference points, `td_bclm`/`td_bcnm` for linear/nonlinear models of binary choice data, and `td_ddm` for drift diffusion models. Objects containing models are designed to feel much like R's `lm` and `glm` objects, with methods for generic functions such as `coef`, `predict`,  `residuals`, and `A/BIC`. Thus, users familiar with basic R functionality should find it straightforward to use `tempodisco`. With the growing recognition that temporal discounting may be described by a range of functions beyond the standard hyperbolic [@franck2015accurate; @bailey2021problems] and that model fit should be assessed by measures other than $R^2$ [@gelinor], `tempodisco` will allow researchers to easily implement these best practices and will continue to evolve along with best practices in temporal discounting research.

# References