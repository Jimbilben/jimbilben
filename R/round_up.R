#' Round up
#'
#' Round a number up to the nearest specified number
#'
#' @param number The number to be rounded up
#' @param accuracy What to round up to the nearest multiple of
#'
#' @export
round_up <- function(number, accuracy, f = ceiling) {

  f(number / accuracy) * accuracy

}
