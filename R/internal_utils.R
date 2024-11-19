
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

validate_td_data <- function(data, required_columns) {
  
  # Validate columns in dataframe
  
  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf('Missing required data column(s): %s', paste(missing_cols, collapse = ', ')))
  }
  
  expectations <- list(
    'del' = list(type = c('numeric', 'factor'),
                 lims = c(0, Inf)),
    'indiff' = list(type = 'numeric',
                    lims = c(0, 1)),
    'val_imm' = list(type = 'numeric',
                     lims = c(0, Inf)),
    'val_del' = list(type = 'numeric',
                     lims = c(0, Inf)),
    'imm_chosen' = list(type = c('numeric', 'logical'),
                        lims = c(0, 1)),
    'rt' = list(type = 'numeric',
                lims = c(0, Inf)),
  )
  # Rather than stopping when a column fails validation, run the validation
  # for all columns and print out all the failures so the user can fix them
  # all.
  stop_flag <- F
  for (colname in expectations) {
    if (colname %in% names(data)) {
      
      curr_col <- data[[colname]]
      expected <- expectations[[colname]]
      
      if (!inherits(curr_col, expected$type)) {
        message('%s should be of type %s',
                colname,
                paste(expected$type, collapse = ' or '))
        stop_flag <- T
      }
      
      if (!(curr_col >= expected$lims[1]) || !(curr_col <= expected$lims[2])) {
        message('%s should be >= %s, <= %s',
                colname,
                expected$lims[1],
                expected$lims[2])
        stop_flag <- T
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
  
}
