
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

# Dirac CDF
# pdirac <- function(q, location = 0) {
#   ifelse(q < location, 0, 1)
# }

# Log-likelihood
ll <- function(p, x) {
  x*log(p) + (1 - x)*log(1 - p)
}

# Laplace smoothing for probabilities
laplace_smooth <- function(p, eps = 1e-10) eps + (1 - 2*eps)*p

# Geometric mean
geomean <- function(x, ...) {
  exp(mean(log(x), ...))
}

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

run_optimization <- function(fn, par_starts, par_lims, optim_args, silent = F) {
  
  # Get the best-fitting optim() object
 
  # Get a table of possible combinations of parameter starting values
  par_start_combos <- as.matrix(do.call(expand.grid, par_starts))
  
  # Get the bounds on each parameter
  # This is a belt-and-suspenders way of ensuring the `par`, `lower`, and `upper` args to `optim` are all in the same, right order
  n_par <- length(par_starts)
  par_names <- colnames(par_start_combos)
  lower <- numeric(n_par)
  upper <- numeric(n_par)
  for (par_idx in seq_len(n_par)) {
    lower[par_idx] <- par_lims[[par_names[par_idx]]][1]
    upper[par_idx] <- par_lims[[par_names[par_idx]]][2]
  }
  names(lower) <- par_names
  names(upper) <- par_names
  
  # Try each combination of parameter starting values
  best_value <- Inf
  best_optimized <- list()
  for (combo_idx in seq_len(nrow(par_start_combos))) {
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
#' @returns A dataframe with two columns: one for the block indicator and another for the corresponding indifference point.
#' @examples
#' \dontrun{
#' data("adj_amt_sim")
#' adj_amt_indiffs(adj_amt_sim)
#' adj_amt_indiffs(adj_amt_sim, block_indic = 'del', order_indic = 'trial_idx')
#' }
#' @export
adj_amt_indiffs <- function(data, block_indic = 'del', order_indic = NULL) {

  validate_td_data(data,
                   required_columns = c('val_imm', 'val_del', 'imm_chosen',
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

#' Kirby MCQ-style scoring
#'
#' Score a set of responses according to the method of Kirby et al. (1999, \doi{10.1037//0096-3445.128.1.78}). This is described in detail in Kaplan et al. (2016, \doi{10.1007/s40614-016-0070-9}).
#' @param data Responses to score.
#' @param discount_function Should \eqn{k} values be computed according to the hyperbolic or exponential discount function? The original method uses the hyperbolic, but in principle the exponential is also possible.
#' @return An object of class \code{\link{td_ipm}}.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- kirby_score(td_bc_single_ptpt)
#' }
#' @export
kirby_score <- function(data, discount_function = c('hyperbolic', 'exponential')) {
  
  discount_function <- match.arg(discount_function)
  
  data <- kirby_preproc(data, discount_function)
  
  max_consistency <- max(data$consistency)
  if (max_consistency < 0.75) {
    warning('Maximum consistency score is below 0.75. Inattentive responding?')
  }
  most_consistent_idx <- which(data$consistency == max_consistency)
  
  cands <- vapply(most_consistent_idx, function(cand) {
    geomean(data$k[(cand-1) : cand])
  }, numeric(1))
  if (length(cands) > 1) {
    best_k <- geomean(cands)
  } else {
    best_k <- cands
  }
  
  # Construct dummy td_ipm
  mod <- list(
    data = data,
    config = list(discount_function = td_fn(predefined = discount_function)),
    optim = list(par = c(k = best_k))
  )
  class(mod) <- c('td_ipm', 'td_um')
  
  return(mod)
  
}

#' Compute consistency score
#'
#' Compute the consistency score per the method of Kirby et al. (1999, \doi{10.1037//0096-3445.128.1.78}). This is described in detail in Kaplan et al. (2016, \doi{10.1007/s40614-016-0070-9}), where it's suggested that a consistency score below 0.75 might be a sign of inattentive responding.
#' @param data Responses to score.
#' @param discount_function Should \eqn{k} values be computed according to the hyperbolic or exponential discount function? The original method uses the hyperbolic, but in principle the exponential is also possible.
#' @return A consistency score between 0 and 1.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- kirby_consistency(td_bc_single_ptpt)
#' }
#' @export
kirby_consistency <- function(data, discount_function = c('hyperbolic', 'exponential')) {
  
  data <- kirby_preproc(data, discount_function)
  return(max(data$consistency))
  
}

kirby_preproc <- function(data, discount_function = c('hyperbolic', 'exponential')) {
  # Compute k and consistency scores
  discount_function <- match.arg(discount_function)
  validate_td_data(data,
                   required_columns = c('val_imm', 'val_del', 'del', 'imm_chosen'))
  
  data$k <- switch (discount_function,
                    'hyperbolic' = (data$val_del/data$val_imm - 1) / data$del,
                    'exponential' = -log(data$val_imm/data$val_del) / data$del
  )
  
  data <- data[order(data$k), ]
  
  data$consistency <- vapply(seq_len(nrow(data)), function(idx) {
    mean(c(data$imm_chosen[0:(idx-1)],
           !data$imm_chosen[(idx):(nrow(data)+1)]),
         na.rm = T)
  }, numeric(1))
  
  return(data)
  
}

#' Wileyto score a questionnaire
#' 
#' Score a set of responses according to the method of Wileyto et al. (2004, \doi{10.3758/BF03195548}). This function is a thin wrapper to \code{\link{td_bclm}}.
#' @param data Responses to score.
#' @returns An object of class \code{td_bclm}.
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- wileyto_score(td_bc_single_ptpt)
#' }
#' @export
wileyto_score <- function(data) {
  
  mod <- td_bclm(data, model = 'hyperbolic.1')
  return(mod)
  
}

#' Experimental method for computing indifference points
#' 
#' @param data Responses to score.
#' @returns A dataframe with two columns: one for the block indicator and another for the corresponding indifference point.
#' @export
most_consistent_indiffs <- function(data) {
  
  dc <- delwise_consistencies(data)
  rows <- list()
  for (sdf in dc) {
    max_consistency <- max(sdf$consistency)
    most_consistent <- which(sdf$consistency == max_consistency)
    rows[[length(rows) + 1]] <- data.frame(
      del = sdf$del[1],
      indiff = mean(sdf$indiff[most_consistent]),
      consistency = max_consistency
    )
  }
  out <- do.call(rbind, rows)
  return(out)
  
}

delwise_consistencies <- function(data) {
  
  validate_td_data(data,
                   required_columns = c('val_del', 'val_imm', 'imm_chosen', 'del'))
  
  data$val_rel <- data$val_imm / data$val_del
  
  rows <- by(data, data$del, function(sdf) {
    # Get candidate indifference points
    cand_indiffs <- filter(c(0, sort(sdf$val_rel), 1), rep(0.5, 2))[1:(nrow(sdf) + 1)]
    consistencies <- vapply(cand_indiffs, function(ci) {
      mean(
        c(!sdf[sdf$val_rel <= ci, 'imm_chosen'],
          sdf[sdf$val_rel >= ci, 'imm_chosen'])
      )
    }, numeric(1))
    return(
      data.frame(
        del = sdf$del[1],
        indiff = cand_indiffs,
        consistency = consistencies
      )
    )
    # filter(sdf$val_rel, rep(0.5, 0.5))
    # sdf <- sdf[order(sdf$val_rel), ]
    # if (!(0 %in% sdf$val_rel)) {
    #   
    # }
    # sdf$consistency <- sapply(1:nrow(sdf), function(idx) {
    #   mean(c(!sdf$imm_chosen[0:(idx-1)],
    #          sdf$imm_chosen[(idx):(nrow(sdf)+1)]),
    #        na.rm = T)
    # })
    # return(sdf)
    # browser()
    # data.frame(
    #   del = sdf$del[1],
    #   consistency = max(sdf$consistency)
    # )
  }, simplify = F)
  
  return(rows)
  # do.call(rbind, rows)
  
}

#' Get model-free indifference points
#' 
#' Convert a temporal discounting model with the "model-free" discount function to a dataframe of indifference points.
#' @param mod A model of class \code{\link{td_bcnm}}, \code{\link{td_ipm}}, or \code{\link{td_ddm}} for which the "model-free" discount function has been fit.
#' @returns A dataframe with columns \code{del} (delay) and \code{indiff} (indifference point).
#' @examples
#' \dontrun{
#' data("td_bc_single_ptpt")
#' mod <- td_bcnm(td_bc_single_ptpt, discount_function = 'model-free')
#' indiff_data <- indiffs(mod)
#' }
#' @export
indiffs <- function(mod) {
  stopifnot(inherits(mod, 'td_um'))
  stopifnot(discount_function(mod) == 'model-free')
  out <- data.frame(del = sort(unique(mod$data$del)))
  out$indiff <- predict(mod, type = 'indiff', newdata = out)
  return(out)
}