# Utility functions
p2o <- function(p) (1 / (1/p - 1)) # proportion/probability to odds
o2p <- function(o) (1 / (1/o + 1)) # odds to proportion/probability
logit <- function(x) log(1 / (1/x - 1))
logistic <- function(x) 1 / (1 + exp(-x))
ll <- function(p, x) { # log-likelihood
  log(c(p[x], (1-p)[!x]))
}
ln_lambda <- function(x, lambda) { # box-cox transform
  if (lambda == 0) {
    return(log(x))
  } else {
    return((x**lambda - 1) / lambda)
  }
}
varsigma <- function(x, alpha, lambda) alpha*ln_lambda(x + 1, lambda) + 1
eps <- .Machine$double.eps # For later use in Laplace smoothing
laplace_smooth <- function(p) p <- eps + (1 - 2*eps)*p

invert_decision_function <- function(mod, prob, del) {
  # Given some model and some delay, get the relative value of the immediate 
  # reward at which its probability of being chosen is some desired value
  if (mod$choice.rule == 'exponential') {
    if (mod$fixed.ends) {
      R <- logistic(
        logit(prob)/exp(mod$par['gamma']) +
          logit(predict_indiffs(mod, del)))
    } else {
      R <- logit(prob)/exp(mod$par['gamma']) + predict_indiffs(mod, del)
    }
  } else if (mod$choice.rule == 'power') {
    if (mod$fixed.ends) {
      R <- o2p( p2o(predict_indiffs(mod, del)) * (1/prob - 1) ** (-1/exp(mod$par['gamma'])) )
    } else {
      R <- predict_indiffs(mod, del) * (1/prob - 1) ** (-1/exp(mod$par['gamma']))
    }
  }
  return(R)
}

run_optimization <- function(fn, param_ranges, silent) {
  # Get the best-fitting optim() object
  # Convert to a table of all possible combinations
  param_vals <- as.matrix(do.call(expand.grid, param_ranges))
  # Try each combination
  best_value <- Inf
  best_optimized <- list()
  for (val_idx in 1:nrow(param_vals)) {
    try( # Optimization may fail
      {
        optimized <- optim(
          fn = fn,
          par = param_vals[val_idx, ],
          control = list(
            'warn.1d.NelderMead' = F, # For finding ED50 with dual-systems exponential discount function
            maxit = 5000 # For when discount function is "none"; this can take a long time to converge
          )
        )
        if (optimized$value < best_value) {
          best_value <- optimized$value
          best_optimized <- optimized
        }
      },
      silent = silent
    )
  }
  return(best_optimized)
}