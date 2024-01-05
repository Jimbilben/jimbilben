#' Prior Code
#'
#' Print out code for defining a custom prior for ease of re-use.
#'
#' @export
prior_code <- function() {

  intercept <- glue::glue('c(set_prior("normal(0 , 2)", class = "Intercept"),\nset_prior("normal(0 , 1)", class = "b"))')
  specific_coefficient <- glue::glue('set_prior("normal(0, .5)", class = "b", coef = "conditiontreatment:oz", dpar = "muDontKnow")')
  exponential <- glue::glue('set_prior("exponential(2)", class = "sd")')
  reminder <- "You can just put these into a list/vector with c()"

  l1 <- glue::glue("get_prior(formula = my_form,")
  l2 <- glue::glue("family = bernoulli,")
  l3 <- glue::glue("data = my_data)")

  print(glue::glue('{intercept}\n{specific_coefficient}\n{exponential}\n\n{reminder}\n\n{l1}\n{l2}\n{l3}'))

}
