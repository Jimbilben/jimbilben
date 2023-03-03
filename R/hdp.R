#' Highest Density Point
#'
#' Get the highest density point (essentially, the mode) of a distribution of values.
#'
#' @param data A vector of data for which the highest density point (essentially, the mode) will be calculated.
#'
#' @export
hdp <- function(data) {

  hdp <- density(data)$x[which.max(density(data)$y)]

  return(hdp)

}
