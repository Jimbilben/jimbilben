#' Make residual correlation formula
#'
#' Generate the brms formula for making pairwise correlations among a set of variables
#'
#' @param my_names A string vector containing the names of the variables to be correlated in your data set
#' @export

make_rescor_formula <- function(my_names) {

  # Create the mvbind part
  mvbind_part <- paste("mvbind(", paste(my_names, collapse = ", "), ")", sep = "")

  # Create the full bf formula
  formula_string <- paste(mvbind_part, "~ 0, sigma ~ 0")

  # Using the formula in a brms model
  full_formula <-
    brms::bf(as.formula(paste(mvbind_part, "~ 0")),
             sigma ~ 0) +
    brms::set_rescor(TRUE)


  return(full_formula)

}
