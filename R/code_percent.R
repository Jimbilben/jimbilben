#' Percent code
#'
#' Print out code for ggplot axis expansion
#'
#' @param axis String, the axis being referenced - defaults to "x" (vs. "y")
#' @export

code_percent <- function(axis = "x") {

  glue::glue('scale_{axis}_continuous(labels = scales::percent_format(scale = 1))')

}
