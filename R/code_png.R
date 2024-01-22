#' PNG Code
#'
#' Print out code for making a png for ease of use.
#'
#' @export
code_png <- function() {

  glue::glue('png("name.png", width = 6, height = 4, units = "in", type = "cairo", res = 1000)')

}
