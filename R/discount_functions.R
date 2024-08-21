
tdfn <- function(fn_name) {
  fn <- switch (fn_name,
                'noise' = function(data, p) rep(p['i'], length(data$del)),
                'hyperbolic' = function(data, p) 1 / (1 + p['k']*data$del),
                'exponential' = function(data, p) exp(-p['k']*data$del),
                'inverse-q-exponential' = function(data, p) 1 / (1 + p['k']*data$del)**p['s'],
                'nonlinear-time-hyperbolic' = function(data, p) 1 / (1 + p['k']*data$del**p['s']),
                'scaled-exponential' = function(data, p) p['w']*exp(-p['k']*data$del),
                'dual-systems-exponential' = function(data, p) p['w']*exp(-p['k1']*data$del) + (1 - p['w'])*exp(-p['k2']*data$del),
                'nonlinear-time-exponential' = function(data, p) exp(-p['k']*data$del**p['s']),
                'model-free' = function(data, p) {
                  p <- p[grep('del_', names(p))]
                  # Round parameters and delays to 10 decimal points for comparison
                  dels <- round(as.numeric(gsub('del_', '', names(p))), 10)
                  a <- approx(x = dels, y = p, xout = round(data$del, 10))
                  return(a$y)
                }
  )
  par_starts <- switch (fn_name,
                        'noise' = list(
                          i = exp(seq(-8, 0, length.out = 3))
                        ),
                        'hyperbolic' = list(
                          k = exp(seq(-8, 0, length.out = 3))
                        ),
                        'exponential' = list(
                          k = exp(seq(-8, 0, length.out = 3))
                        ),
                        'inverse-q-exponential' = list(
                          k = exp(seq(-8, 0, length.out = 3)),
                          s = exp(seq(-8, 0, length.out = 3))
                        ),
                        'nonlinear-time-hyperbolic' = list(
                          k = exp(seq(-8, 0, length.out = 3)),
                          s = exp(seq(-8, 0, length.out = 3))
                        ),
                        'scaled-exponential' = list(
                          w = exp(seq(-8, 0, length.out = 3)),
                          k = exp(seq(-8, 0, length.out = 3))
                        ),
                        'dual-systems-exponential' = list(
                          w = exp(seq(-8, 0, length.out = 3)),
                          k1 = exp(seq(-8, 0, length.out = 3)),
                          k2 = exp(seq(-8, 0, length.out = 3))
                        ),
                        'nonlinear-time-exponential' = list(
                          k = exp(seq(-8, 0, length.out = 3)),
                          s = exp(seq(-8, 0, length.out = 3))
                        ),
                        'model-free' = function(data) {
                          unique_delays <- unique(data$del)
                          out <- as.list(rep(0.5, length(unique_delays)))
                          # Round to 10 decimal points to be able to align delay values 
                          names(out) <- sprintf('del_%.10f', unique_delays)
                          return(out)
                        }
  )
  par_lims <- switch (fn_name,
                      'noise' = list(
                        i = c(0, 1)
                      ),
                      'hyperbolic' = list(
                        k = c(0, Inf)
                      ),
                      'exponential' = list(
                        k = c(0, Inf)
                      ),
                      'inverse-q-exponential' = list(
                        k = c(0, Inf),
                        s = c(0, Inf)
                      ),
                      'nonlinear-time-hyperbolic' = list(
                        k = c(0, Inf),
                        s = c(0, Inf)
                      ),
                      'scaled-exponential' = list(
                        w = c(0, 1),
                        k = c(0, Inf)
                      ),
                      'dual-systems-exponential' = list(
                        w = c(0, 1),
                        k1 = c(0, Inf),
                        k2 = c(0, Inf)
                      ),
                      'nonlinear-time-exponential' = list(
                        k = c(0, Inf),
                        s = c(0, Inf)
                      ),
                      'model-free' = function(data) {
                        unique_delays <- unique(data$del)
                        out <- rep(list(c(0, 1)), length(unique_delays))
                        # Round to 10 decimal points to be able to align delay values 
                        names(out) <- sprintf('del_%.10f', unique_delays)
                        return(out)
                      }
  )
  
  ED50 <- switch (fn_name,
                  "noise" = function(p) 'none',
                  "hyperbolic" = function(p) 1/p['k'],
                  "exponential" = function(p) log(2)/p['k'],
                  "inverse-q-exponential" = function(p) (2^(1/p['s']) - 1) / p['k'],
                  "nonlinear-time-hyperbolic" = function(p) (1/p['k']) ^ (1/p['s']),
                  "nonlinear-time-exponential" = function(p) (log(2)/p['k'])^(1/p['s']),
                  "scaled-exponential" = function(p) log(2*p['w'])/p['k'],
                  "dual-systems-exponential" = function(p) 'non-analytic',
                  "model-free" = function(p) 'none'
  )
  
  out <- list(
    name = fn_name, # Function name
    fn = fn, # Actual function to evaluate
    par_starts = par_starts,
    par_lims = par_lims,
    ED50 = ED50
  )
  
  if (fn_name == 'dual-systems-exponential') {
    out$par_chk <- function(p) {
      # Ensure k1 < k2
      if (p['k1'] > p['k2']) {
        # Switch k1 and k2
        k2 <- p['k1']
        k1 <- p['k2']
        p['k1'] <- k1
        p['k2'] <- k2
        # Complement of 2
        p['w'] <- 1 - p['w']
      }
      return(p)
    }
  }
  
  class(out) <- 'tdfn'
  return(out)
}

get_discount_function <- function(mod) mod$config$discount_function$fn
