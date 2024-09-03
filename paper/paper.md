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

"Temporal discounting" refers to the near-universal phenomenon in which future rewards are under-valued as a function of their delay. The degree to which future rewards are discounted is an important individual difference that is related to a range of real-life outcomes. Many methods exist for modeling and quantifying discounting, and `tempodisco` is designed to provide easy access to these.

# Statement of need

`tempodisco` is an R package for working with temporal discounting data. It is designed to simplify the analysis steps of data cleaning, model fitting, and quantifying discounting. The package implements widely used methods such as computing indifference points from adjusting amount task [@frye2016measuring], testing for non-systematic discounting per the criteria of @johnson2008algorithm, scoring questionnaires according to the methods of @kirby1999heroin and @wileyto2004using, Bayesian model selection using a range of discount functions [@franck2015accurate], and model-agnostic measures of discounting such as area under the curve (AUC; @myerson2001area) and ED50 [@yoon2008turning]. Users can also produce simple diagnostic plots to examine model fits. Moreover, the package provides a framework within which users can specify custom discount functions.

The package is based around a few basic objects such as `td_fn` for discount functions, `td_ipm` for models of indifference points, and `td_bclm`/`td_bcnm` for linear and nonlinear models of binary choice data, respectively. Objects containing models are designed to feel much like R's `lm` and `glm` objects, with methods for generic functions such as `coef`, `predict`,  `residuals`, and `A/BIC`. Thus, users familiar with basic R functionality should find it straightforward to use `tempodisco`. With the growing recognition that temporal discounting may be described by a range of functions beyond the standard hyperbolic [@franck2015accurate; @bailey2021problems] and that model fit should be assessed by measures other than $R^2$ [@gelinor], `tempodisco` will allow researchers to easily implement these best practices and will continue to evolve as best practices in temporal discounting research do.

# References