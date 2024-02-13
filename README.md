# ddDesidModels
An R package for fitting probabilistic models of delay discounting, as described in the paper "Probabilistic models of delay discounting: improving plausibility and performance."

## Installation
```
require(devtools)
install_github("kinleyid/ddDesidModels");
```

## Usage
The main function is `dd_prob_model`, which fits a probabilistic model of a given individual's delay discounting. By default, the function tests 7 candidate discount functions and identifies the one yielding the lowest AIC.
