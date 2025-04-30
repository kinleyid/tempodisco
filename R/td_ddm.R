
# TODO: Use the same terminology (get_linpred_func_bcnm) for td_bcnm as here
# TODO: Custom criterion functions, both here and td_bcnm

# A note on conventions:
# UPPER boundary represents immediate reward
# LOWER boundary represents delayed reward
# Densities are by default computed at upper boundary
# To compute at lower boundary, use -v and (1 - w)
# Subjective value difference should be positive if favouring imm., negative if favouring del.
# 
# v is a drift rate *multiplier*. I.e., not the drift rate itself
# alpha = boundary separation
# tau = non-decision-time
# beta = bias


#' Temporal discounting drift diffusion model
#'
#' Fit a drift diffusion model for a single subject using maximum likelihood estimation.
#' @param data A data frame with columns \code{val_imm} and \code{val_del} for the values of the immediate and delayed rewards, \code{del} for the delays, \code{imm_chosen} (Boolean) for whether the immediate rewards were chosen, and \code{rt} for the reaction times (in seconds). Other columns can also be present but will be ignored.
#' @param discount_function A string specifying the name of the discount functions to use, or an object of class \code{td_fn} (used for creating custom discount functions), or a list of objects of class \code{td_fn}.
#' @param gamma_par_starts A vector of starting values to try for the "gamma" parameter (drift rate multiplier or sharpness parameter) during optimization.
#' @param beta_par_starts A vector of starting values to try for the "beta" parameter (bias) during optimization.
#' @param alpha_par_starts A vector of starting values to try for the "alpha" parameter (boundary separation) during optimization.
#' @param tau_par_starts A vector of starting values to try for the "tau" parameter (non-decision time) during optimization.
#' @param drift_transform A transform to apply to drift rates. Either \code{"none"} (no transform), \code{"sigmoid"} (sigmoidal transform described by Peters & D'Esposito, 2020, \doi{10.1371/journal.pcbi.1007615}, and Fontanesi et al., 2019, \doi{10.3758/s13423-018-1554-2}), or \code{"bias-correct"} (experimental; see note below).
#' @param optim_args Additional arguments to pass to \code{optim()}. Default is \code{list(silent = TRUE)}.
#' @param silent Boolean (true by default). The call to \code{optim()} occurs within a \code{try()} wrapper. The value of \code{silent} is passed along to \code{try()}.
#' @family drift diffusion model functions
#' @return An object of class \code{td_bcnm} with components \code{data} (containing the data used for fitting), \code{config} (containing the internal configuration of the model, including the \code{discount_function}), and \code{optim} (the output of \code{optim()}).
#' @note
#' Drift rates are computed based on the difference in subjective values between the immediate and delayed rewards. In theory, when they are equally valued, they should have equal probability of being chosen. However, this is only true when the bias parameter of the drift diffusion model (\code{beta}) is 0.5 (i.e., no bias). To make sure the immediate and delayed reward have equal probability of being chosen when they are equally valued, we can set \code{drift_transform = "bias-correct"} to add a bias correction factor to the drift rate. However, this feature is experimental and its effects on model fit etc. have not been tested.
#' @examples
#' \donttest{
#' data("td_bc_single_ptpt")
#' ddm <- td_ddm(td_bc_single_ptpt, discount_function = 'exponential',
#'               gamma_par_starts = 0.01,
#'               beta_par_starts = 0.5,
#'               alpha_par_starts = 3.5,
#'               tau_par_starts = 0.9)
#' }
#' @export
td_ddm <- function(
    data,
    discount_function,
    gamma_par_starts = c(0.01, 0.1, 1),
    beta_par_starts = c(0.25, 0.5, 0.75),
    alpha_par_starts = c(0.5, 1, 10),
    tau_par_starts = c(0.2, 0.8),
    drift_transform = c('none', 'sigmoid', 'bias-correct'),
    silent = TRUE,
    optim_args = list()) {
  # Input validation--------------------------
  
  # Required data columns
  data <- validate_td_data(data,
                           required_columns = c('val_imm', 'val_del', 'del', 'imm_chosen', 'rt'))
  
  # Check that RTs are in seconds vs milliseconds
  if (median(data$rt) > 100) {
    stop('Median RT is greater than 100, meaning RTs are likely in units of milliseconds (or smaller). They should be in units of seconds.')
  }
  
  # Attention checks
  attention_checks(data, warn = TRUE)
  
  # All immediate chosen or all delayed chosen?
  invariance_checks(data, warn = TRUE)
  
  # End input validation----------------------
  
  # Parse drift transform (produce an object called drift_trans)
  drift_transform <- match.arg(drift_transform)
  if (drift_transform == 'none') {
    drift_trans <- list(
      fn = function(drift, par) identity(drift),
      par_lims = NULL,
      par_starts = NULL)
  } else if (drift_transform == 'sigmoid') {
    drift_trans <- list(
      fn = function(drift, par) par['max_abs_drift']*( 2*plogis(drift) - 1 ),
      par_lims = list(max_abs_drift = c(0, Inf)),
      par_starts = list(max_abs_drift = c(0.5, 1, 2)))
  } else if (drift_transform == 'bias-correct') {
    drift_trans <- list(
      fn = function(drift, par) {
        mdn <- median_pimm_ddm(par)
        return(drift + mdn)
      },
      par_lims = NULL,
      par_starts = NULL)
  } else if (drift_transform == 'bias-correct-sigmoid') {
    drift_trans <- list(
      fn = function(drift, par) {
        # mdn <- get_median(<func>)
        par['max_abs_drift']*( 2*plogis(drift) - 1 ) + median_pimm_ddm(par)
      },
      par_lims = list(max_abs_drift = c(0, Inf)),
      par_starts = list(max_abs_drift = c(0.1, 1, 10)))
  }
  drift_trans$name <- drift_transform

  # Get a list of candidate discount functions
  disc_func_cands <- get_candidate_discount_functions(arg = discount_function)
  
  # Run optimization for each candidate discount function
  best_crit <- Inf
  best_mod <- list()
  for (disc_func in disc_func_cands) {
    
    disc_func <- initialize_discount_function(disc_func, data)
    
    # Candidate model
    cand_mod <- list(data = data,
                     config = list(discount_function = disc_func,
                                   drift_transform = drift_trans))
    class(cand_mod) <- c('td_ddm', 'td_um')
    
    # Get parameter bounds and starts for
    #   discount function
    #   DDM
    #   drift transform
    par_lims <- c(
      disc_func$par_lims,
      list(
        gamma = c(0, Inf),
        beta = c(0, 1),
        alpha = c(1e-10, Inf), # Not exactly 0 because this throws an error
        tau = c(1e-10, Inf) # Ditto
      ),
      drift_trans$par_lims
    )
    par_starts <- c(
      disc_func$par_starts,
      list(
        gamma = gamma_par_starts,
        beta = beta_par_starts,
        alpha = alpha_par_starts,
        tau = tau_par_starts
      ),
      drift_trans$par_starts
    )
    
    # Get function for computing resp/RT densities
    prob_func <- get_prob_func_ddm(discount_function = disc_func,
                                   drift_transform = drift_trans)
    
    # Get NLL function
    nll_fn <- function(par) {
      d <- prob_func(data, par) # Compute probability densities of observed responses/RTs
      d <- laplace_smooth(d)
      return(-sum(log(d)))
    }
    
    # Run optimization
    optimized <- run_optimization(nll_fn,
                                  par_starts,
                                  par_lims,
                                  optim_args = list(
                                    control = list(maxit = 1000)
                                  ),
                                  silent = silent)
    if (length(optimized) == 0) {
      stop('Optimization failed; optim() returned an error for every choice of initial parameter values')
    }
    cand_mod$optim <- optimized
    
    curr_crit <- BIC(cand_mod)
    
    if (curr_crit < best_crit) {
      best_crit <- curr_crit
      if ('par_chk' %in% names(cand_mod$config$discount_function)) {
        optimized$par <- cand_mod$config$discount_function$par_chk(optimized$par)
      }
      best_mod <- cand_mod
    }
    
  }
  
  return(best_mod)
  
}

get_median <- function(fn) {
  # Get the input to fn that produces 0.5
  o <- optim(0, function(x) {(fn(x) - 0.5)**2}, method = 'L-BFGS-B')
  o$par
}

median_pimm_ddm <- function(par) {
  # Get drift rate producing indifferent (i.e., p = 0.5 for imm. and del.)
  o <- optim(0, function(v) {(pimm_ddm(v, par) - 0.5)**2}, control = list(warn.1d.NelderMead = FALSE))
  o$par
}

# Function to get the median, given some set of w and a
# pddm_median <- function(w = 0.5, a = 1) {
#   o <- optim(0, function(v) {(pddm(v, w, a) - 0.5)**2}, control = list(warn.1d.NelderMead = FALSE))
#   o$par
# }

# # Function to get the optimal bias, given some v and a
# pddm_bias <- function(v, a = 1) {
#   o <- optim(0, function(w) {(pddm(v, w, a) - 0.5)**2}, control = list(warn.1d.NelderMead = FALSE))
#   o$par
# }
# 
# pddm_bias_2 <- function(v, a = 1) {
#   1/(2*v*a)*log(2 / (1 + exp(-2*v*a)))
# }

get_linpred_func_ddm <- function(discount_function, drift_transform) {
  
  # Returns a function to compute linear predictor (i.e., drift rate)
  
  linpred_func <- function(data, par) {
    # Compute subjective value difference
    svd <- data$val_imm - data$val_del*discount_function$fn(data, par)
    # Compute drift rate
    drift <- svd*par['gamma']
    # Transform drift rate
    drift <- drift_transform$fn(drift, par)
    return(drift)
  }
  
  return(linpred_func)
  
}

pimm_ddm <- function(drift, par) {
  # Compute probability of selecting immediate reward (i.e., absorption at upper barrier)
  # Formula from Blurton et al., 2012: https://doi.org/10.1016/j.jmp.2012.09.002
  pimm <- ifelse(drift == 0,
                 {
                   par['beta']
                 },
                 {
                   E1 <- exp(-2*drift*par['alpha']*(1 - par['beta']))
                   E2 <- exp(2*drift*par['alpha']*par['beta'])
                   1 - (1 - E1) / (E2 - E1)
                 })
  return(pimm)
  
}

get_prob_func_ddm <- function(discount_function, drift_transform) {
  # Get function for computing probability density for responses and RTs
  
  # First get function for computing linear predictor (i.e., drift rate)
  linpred_func <- get_linpred_func_ddm(discount_function, drift_transform)
  
  prob_func <- function(data, par) {
    
    # Get drift rate from subjective value difference
    drift <- linpred_func(data, par)
    
    # Compute densities
    q <- data$rt
    resp <- ifelse(data$imm_chosen, 'upper', 'lower')
    d <- vapply(
      seq_len(nrow(data)),
      function(i) {
        RWiener::dwiener(q = data$rt[i], delta = drift[i], resp = resp[i],
                         alpha = par['alpha'], tau = par['tau'], beta = par['beta'])
      },
      numeric(1)
    )

    return(d)

  }

  return(prob_func)

}

ddm_predicted_rts <- function(drifts, par) {
  # Compute the RT expectation, irrespective of boundary (formula from https://doi.org/10.1016/j.jmp.2009.01.006)
  
  z <- par['beta']*par['alpha']
  A <- exp(-2*drifts*par['alpha']) - 1
  Z <- exp(-2*drifts*z) - 1
  E_rt <- -z/drifts + par['alpha']/drifts * Z/A + par['tau']
  
  return(E_rt)
}
