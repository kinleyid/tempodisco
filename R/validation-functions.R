
#' Run attention checks
#' 
#' Check whether participants failed attention checks, either choosing an immediate reward of 0 or choosing a delayed reward equal in face value to an immediate reward. If the participant was never offered either choice, a warning is given.
#' @param data A \code{data.frame} with columns \code{val_imm}, \code{val_del} and \code{del_chosen}, representing data from a single participant.
#' @param warn Logical: give a warning for failed attention checks?
#' @param ppn Logical: return proportions of attention checks participant failed, versus absolute numbers?
#' @returns Named vector counting the number of times the participant chose an immediate reward of 0 (\code{imm_0}) or chose a delayed reward equal in face value to an immediate reward (\code{del_eq_imm}).
#' @examples
#' \dontrun{
#' # On a model
#' data("td_bc_single_ptpt")
#' attention_checks(td_bc_single_ptpt)
#' }
#' @export
attention_checks <- function(data, warn = F, ppn = F) {
  
  validate_td_data(data,
                   required_columns = c('val_imm', 'val_del', 'imm_chosen'))
  
  imm_0 <- data$val_imm == 0 
  D1 <- imm_0 & data$imm_chosen
  if (any(D1) & warn) {
    warning(sprintf('Participant chose an immediate reward with a value of 0 %s time(s). Failed attention check?', sum(D1)))
  }
  
  del_eq_imm <- data$val_imm == data$val_del
  D2 <- del_eq_imm & !data$imm_chosen
  if (any(D2) & warn) {
    warning(sprintf('Participant chose a delayed reward of equal value to an immediate reward %s time(s). Failed attention check?', sum(D2)))
  }
  
  # if (sum(imm_0 | del_eq_imm) == 0) {
  #   warning('Participant was never offered an immediate reward equal to 0 or equal to the delayed reward; no attention checks to fail.')
  # }
  
  if (ppn) {
    f <- mean
  } else {
    f <- sum
  }
  return(c(imm_0 = f(D1), del_eq_imm = f(D2)))
  
}

#' Check for invariant responding
#' 
#' Check whether participants always chose the immediate reward or always chose the delayed reward
#' @param data A \code{data.frame} with columns \code{val_imm}, \code{val_del} and \code{del_chosen}, representing data from a single participant.
#' @param warn Logical: give a warning for invariant responding?
#' @returns Named vector specifying whether the participant chose only immediate rewards (\code{all_imm}) or chose all delayed rewards (\code{all_del}).
#' @examples
#' \dontrun{
#' # On a model
#' data("td_bc_single_ptpt")
#' attention_checks(td_bc_single_ptpt)
#' }
#' @export
invariance_checks <- function(data, warn = F) {
  
  all_imm <- all(data$imm_chosen)
  if (all_imm & warn) {
    warning('Participant chose only immediate rewards.')
  }
  
  all_del <- all(!data$imm_chosen)
  if (all_del & warn) {
    warning('Participant chose only delayed rewards.')
  }
  
  return(c(all_imm = all_imm, all_del = all_del))
  
}
