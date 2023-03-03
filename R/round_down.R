#' Round down
#'
#' Round a number down to the nearest specified number
#'
#' @param number The number to be rounded up
#' @param accuracy What to round up to the nearest multiple of
#'
#' @export
round_down <- function(number, accuracy = 10, f = floor) {

  f(number / accuracy) * accuracy

}
