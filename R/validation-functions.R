
require_columns <- function(data, columns) {
  
  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf('Missing required data column(s): %s', paste(missing_cols, collapse = ', ')))
  }
  
}

attention_checks <- function(data) {
  
  D1 <- data$val_imm == 0 & data$imm_chosen
  if (any(D1)) {
    warning(sprintf('Participant chose an immediate reward with a value of 0 %s time(s). Failed attention check?', sum(D1)))
  }
  
  D2 <- (data$val_imm == data$val_del) & !data$imm_chosen
  if (any(D2)) {
    warning(sprintf('Participant chose a delayed reward of equal value to an immediate reward %s time(s). Failed attention check?', sum(D2)))
  }
  
}

invariance_checks <- function(data) {
  
  if (all(data$imm_chosen)) {
    warning('Participant chose only immediate rewards.')
  }
  
  if (all(!data$imm_chosen)) {
    warning('Participant chose only delayed rewards.')
  }
  
}

validate_discount_function <- function(discount_function) {

  if (is.character(discount_function)) {
    valids <- eval(formals(td_fn)$predefined)
    invalids <- setdiff(discount_function, valids)
    if (length(invalids) > 0) {
      stop(sprintf('Invalid discount function name(s): %s\n\nValid options are: %s',
                   paste(sprintf('\n- "%s"', invalids), collapse = ''),
                   paste(sprintf('\n- "%s"', valids), collapse = '')))
    }
  } else {
    if (!is(discount_function, 'td_fn')) {
      if (!all(sapply(discount_function, class) == 'td_fn')) {
        stop('Discount function must be an object of class td_fn or a list of such objects.')
      }
    }
  }
  
}