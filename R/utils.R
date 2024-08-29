
# Utility functions

`%def%` <- function(x, y) { # To avoid importing rlang
  if (is.null(x)) {
    y
  } else {
    x
  }
}

# Gumbel quantile distribution
qgumbel <- function(p, location = 0, scale = 1) {
  location - scale*log(-log(p))
}

# Log-likelihood
ll <- function(p, x) {
  x*log(p) + (1 - x)*log(1 - p)
}

# Laplace smoothing for probabilities
laplace_smooth <- function(p, eps = 1e-10) eps + (1 - 2*eps)*p

get_transform <- function(config, inverse = F) {
  
  # From a string, get the transform applied to val_imm/val_del and to the value of the discount function
  # Optionally, get the inverse of this transform
  
  if (inverse) {
    # if (config$transform == 'noise_dist_quantile') {
    #   transform <- get(sprintf('p%s', config$noise_dist))
    # } else if (config$transform == 'log') {
    #   transform <- exp
    # } else if (config$transform == 'identity') {
    #   transform <- identity
    # } else {
    #   cat(sprintf('Do not know how to invert transform "%s"\n', config$transform))
    #   transform <- NULL
    # }
  } else {
    if (config$transform == 'noise_dist_quantile') {
      transform <- get(sprintf('q%s', config$noise_dist))
    } else {
      transform <- get(config$transform)
    }
  }
  return(transform)
}
# 
# invert_decision_function <- function(mod, data, prob) {
#   
#   # Given some model and some delay, get the relative value of the immediate 
#   # reward at which its probability of being chosen is some desired value
#   
#   browser()
#   
#   indiffs <- predict(mod, newdata = data, type = 'indiff')
#   
#   qfunc <- get(sprintf('q%s', mod$config$noise_dist))
#   
#   transform <- get_transform(mod$config, inverse = F)
#   inverse_transform <- get_transform(mod$config, inverse = T)
#   
#   gamma <- coef(mod)['gamma']
#   if (mod$config$gamma_scale == 'linear') {
#     val_del <- mean(mod$data$val_del)
#     gamma <- gamma * val_del
#   }
#   
#   R <- inverse_transform(qfunc(prob) / gamma + transform(indiffs))
#   
#   return(R)
# }

run_optimization <- function(fn, par_starts, par_lims, optim_args, silent) {
  
  # Get the best-fitting optim() object
 
  # Get a table of possible combinations of parameter starting values
  par_start_combos <- as.matrix(do.call(expand.grid, par_starts))
  
  # Get the bounds on each parameter
  # This is a belt-and-suspenders way of ensuring the `par`, `lower`, and `upper` args to `optim` are all in the same, right order
  n_par <- length(par_starts)
  par_names <- colnames(par_start_combos)
  lower <- numeric(n_par)
  upper <- numeric(n_par)
  for (par_idx in 1:n_par) {
    lower[par_idx] <- par_lims[[par_names[par_idx]]][1]
    upper[par_idx] <- par_lims[[par_names[par_idx]]][2]
  }
  names(lower) <- par_names
  names(upper) <- par_names
  
  # Try each combination of parameter starting values
  best_value <- Inf
  best_optimized <- list()
  for (combo_idx in 1:nrow(par_start_combos)) {
    try( # Optimization may fail
      {
        args <- c(
          list(
            fn = fn,
            par = par_start_combos[combo_idx, ],
            method = 'L-BFGS-B',
            lower = lower,
            upper = upper
          ),
          optim_args
        )
        optimized <- do.call(optim, args)
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
