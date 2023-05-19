#' Lottery
#'
#' This function randomly picks 'n' values from the provided options.
#' By default, it simulates a binary lottery with options 0 and 1 and draws a single value.
#'
#' @param options A vector of possible values to be sampled from. Default is c(0, 1).
#' @param n An integer specifying the number of values to be drawn from the options. Default is 1.
#'
#' @return A vector of 'n' values randomly picked from the provided options.
#'
#' @examples
#' lottery()
#' lottery(c(1, 2, 3, 4, 5))
#' lottery(c("a", "b", "c", "d"))
#' lottery(n = 3)
#' lottery(c(1, 2, 3, 4, 5), n = 5)
#'
#' @export

lottery <- function(options = c(0, 1), n = 1) {

  return(sample(options, n, replace = TRUE))

}
