#' Highest Density Point
#'
#' Get the highest density point (essentially, the mode) of a distribution of values.
#'
#' @param data The data for which the region variable will be made
#'
#' @export
hdp <- function(data) {

  hdp <- density(data)$x[which.max(density(data)$y)]

  return(hdp)

}
