
# Internal utility functions, e.g., input validation functions that are frequently reused

get_candidate_discount_functions <- function(arg, val_del_avail = TRUE) {
  # Get a list of candidate td_fn objects to test
  # `arg` can be:
  # - 'all' (test all predefined discount functions)
  # - a character vector of names of predefined discount functions to test
  # - a custom discount function (object of class `td_fn`)
  # - a list containing both custom discount functions and names of predefined discount functions
  # Note that, when val_del_avail = FALSE (e.g., when we're fitting indifference point data), the additive-utility function can't be used
  
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
        if (!val_del_avail) {
          # val_del is not a column in the data, therefore additive-utility can't be used
          predefined_disc_funcs <- predefined_disc_funcs[predefined_disc_funcs != 'additive-utility']
        }
        # get corresponding td_fn objects and append them
        curr_cands <- lapply(predefined_disc_funcs, td_fn)
      } else if (is.character(item)) {
        curr_cands <- list(td_fn(predefined = item))
      }
    } else if (is(item, 'td_fn')) {
      curr_cands <- list(item)
    }
    candidates <- append(candidates, curr_cands)
  }
  
  return(unique(candidates))
}

validate_td_data <- function(data, required_columns) {
  
  # Validate columns in dataframe
  
  # Check that all required columns are present
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf('Missing required data column(s): %s', paste(missing_cols, collapse = ', ')))
  }
  
  # Omit NA rows
  pre_nrow <- nrow(data)
  data <- data[complete.cases(data[required_columns]), ]
  n_rm <- pre_nrow - nrow(data)
  if (n_rm > 0) {
    warning(sprintf('Removing %s rows containing missing values', n_rm))
  }
  if (nrow(data) == 0) {
    stop('Dataframe empty after removing missing values')
  }
  
  # Check that each column is of the expected type
  expectations <- list(
    'del' = list(type = c('numeric', 'integer', 'factor'),
                 lims = c(0, Inf)),
    'indiff' = list(type = c('numeric', 'integer'),
                    lims = c(0, 1)),
    'val_imm' = list(type = c('numeric', 'integer'),
                     lims = c(0, Inf)),
    'val_del' = list(type = c('numeric', 'integer'),
                     lims = c(0, Inf)),
    'imm_chosen' = list(type = c('numeric', 'integer', 'logical'),
                        lims = c(0, 1)),
    'rt' = list(type = 'numeric',
                lims = c(0, Inf))
  )
  # Rather than stopping when a column fails validation, run the validation
  # for all columns and print out all the failures so the user can fix them
  # all.
  stop_flag <- FALSE
  for (colname in names(expectations)) {
    if (colname %in% names(data)) {
      
      curr_col <- data[[colname]]
      expected <- expectations[[colname]]
      
      if (!inherits(curr_col, expected$type)) {
        message(sprintf('%s should be of type %s',
                        colname,
                        paste(expected$type, collapse = ' or ')))
        stop_flag <- TRUE
      }
      
      if (!all(curr_col >= expected$lims[1]) || !all(curr_col <= expected$lims[2])) {
        message(sprintf('%s is %s; should be >= %s, <= %s',
                colname,
                curr_col,
                expected$lims[1],
                expected$lims[2]))
        stop_flag <- TRUE
      }
    }
  }
  if (stop_flag) {
    stop('Data failed validation; see messages above.')
  }
  
  # Convert imm_chosen to logical, if applicable
  if ('imm_chosen' %in% names(data)) {
    data$imm_chosen <- as.logical(data$imm_chosen)
  }
  
  return(data)
  
}

initialize_discount_function <- function(disc_func, data) {
  if ('init' %in% names(disc_func)) {
    # Run init() and do some validation
    disc_func <- disc_func$init(disc_func, data)
    stopifnot(
      is.list(disc_func$par_starts),
      is.list(disc_func$par_lims),
      all(names(disc_func$par_starts) == names(disc_func$par_lims)),
      all(vapply(disc_func$par_lims, length, integer(1)) == 2),
      is.function(disc_func$fn),
      all(names(formals(disc_func$fn)) == c('data', 'p'))
    )
  }
  return(disc_func)
}