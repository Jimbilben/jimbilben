#' Expand code
#'
#' Print out code for ggplot axis expansion
#'
#' @param type String, defaults to "mult" (vs. "add")
#' @param low Numeric, expansion at the bottom of the axis - defaults to 0
#' @param high Numeric, expansion at the top of the axis - defaults to .05
#' @export


code_expand <- function(type = "mult", low = 0, high = .05) {

  glue::glue('expand = expansion({type} = c({low}, {high}))')

}
