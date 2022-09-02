#' Prior Code
#'
#' Print out code for defining a custom prior for ease of re-use.
#'
#' @export
prior_code <- function() {

  intercept <- glue::glue('set_prior("normal(0 , 2)", class = "Intercept")')
  specific_coefficient <- glue::glue('set_prior("normal(0, .5)", class = "b", coef = "conditiontreatment:oz", dpar = "muDontKnow")')
  reminder <- "You can just put these into a list/vector with c()"

  glue::glue('{intercept}\n{specific_coefficient}\n{reminder}')

}
