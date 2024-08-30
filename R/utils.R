
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

#' Indifference points from adjusting amount procedure
#' 
#' Compute indifference points for data from an adjusting amount procedure (also called a "titrating procedure"). 
#' @param data A dataframe where each row corresponds to a binary choice, with at least columns \code{val_imm}, \code{val_del}, and \code{imm_chosen}, along with a block indicator and (if applicable) an order indicator.
#' @param block_indic Column name of the block indicator---i.e., the column that will identify a block of trials for which an indifference point should be computed. If unspecified, defaults to \code{'del'}, which assumes that each block corresponds to a different delay.
#' @param order_indic Column name of the order indicator---i.e., the column that specifies the order in which trials were completed. Sorting by this column within a block should sort the rows in chronological order. If unspecified, the rows are assumed to already be in chronological order.
#' @returns A dataframe with two columns: one for the block indicator and another for the corresponding indifference point
#' @examples
#' \dontrun{
#' data("adj_amt_sim")
#' adj_amt_indiffs(adj_amt_sim)
#' adj_amt_indiffs(adj_amt_sim, block_indic = 'del', order_indic = 'trial_idx')
#' }
#' @export
adj_amt_indiffs <- function(data, block_indic = 'del', order_indic = NULL) {

  require_columns(data, c('val_imm', 'val_del', 'imm_chosen',
                          block_indic, order_indic))
                  
  rows <- by(data, INDICES = data[[block_indic]], simplify = F, FUN = function(block) {

    block_id <- block[[block_indic]][1]
    
    # Ensure correct order
    if (!is.null(order_indic)) {
      block <- block[order(block[[order_indic]]), ]
    }
    
    # Validation
    adj_amts <- abs(diff(block$val_imm))
    if (!all(diff(adj_amts) < 0)) {
      stop(sprintf('For block %s = %s, amount adjustments are not monotonically decreasing in magnitude. Consider specifying/checking the order_indic argument.', block_indic, block_id))
    }
    
    final_imm_chosen <- block$imm_chosen[nrow(block)]
    adj_amt <- 2^-(nrow(block) + 1)
    adj_dir <- ifelse(final_imm_chosen, -1, 1)
    indiff <- (block$val_imm/block$val_del)[nrow(block)] + adj_amt*adj_dir
    
    row <- data.frame(block_id, indiff)
    names(row) <- c(block_indic, 'indiff')
    
    return(row)
  })
  
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  
  return(out)
}
