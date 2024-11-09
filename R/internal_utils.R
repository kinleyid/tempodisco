
get_candidate_discount_functions <- function(arg) {
  # Get a list of candidate td_fn objects to test
  # `arg` can be:
  # - 'all' (test all predefined discount functions)
  # - a character vector of names of predefined discount functions to test
  # - a custom discount function (object of class `td_fn`)
  # - a list containing both custom discount functions and names of predefined discount functions
  
  # First get singletons into a list
  if (is.character(arg)) {
    arg <- as.list(arg)
  } else if (is(arg, 'td_fn')) {
    arg <- list(arg)
  }
  
  candidates <- list()
  for (item in arg) {
    if (is.character(item)) {
      if (item == 'all') {
        # get names of predefined discount functions
        predefined_disc_funcs <- eval(formals(td_fn)$predefined)
        # get corresponding td_fn objects and append them
        curr_cands <- lapply(predefined_disc_funcs, td_fn)
      } else if (is.character(item)) {
        curr_cands <- list(td_fn(item))
      }
    } else if (is(item, 'td_fn')) {
      curr_cands <- list(item)
    }
    candidates <- append(candidates, curr_cands)
  }
  
  return(unique(candidates))
}

get_pimm_func <- function(mod) {
  # Get a function to compute the probability of selecting the immediate reward
  
  if (is(mod, 'td_bcnm')) {
    frame <- do.call(get_prob_mod_frame, mod$config)
  } else if (is(mod, 'td_ddm')) {
    linpred_func <- do.call(get_linpred_func_ddm, mod$config)
    frame <- function(data, par) {
      drift <- linpred_func(data, par)
      return(pimm_ddm(drift, par))
    }
  }
  # Get a function where the parameter values are fixed
  func <- function(data) {
    frame(data, coef(mod))
  }
  
  return(func)
  
}