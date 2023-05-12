#' Squish Ratings
#'
#' This function squishes a vector of ratings over based on the maximum rating value and a specified shift proportion.
#'
#' @param ratings A numeric vector of ratings to be shifted.
#' @param max_rating The maximum rating value. If NULL (default), the maximum value of the input `ratings` vector will be used.
#' @param proportion A numeric value (between 0 and 1) specifying the proportion by which to shift the ratings (default is 0.2).
#'
#' @return A numeric vector of shifted ratings.
#'
#' @examples
#' ratings <- c(30, 50, 90)
#' max_rating <- 100
#' proportion <- 0.1
#'
#' squished_ratings <- squish_ratings(ratings, max_rating, proportion)
#' print(squished_ratings)
#'
#' @export
squish_ratings <- function(ratings, max_rating = NULL, proportion = .2) {

  if(is.null(max_rating)) {
    max_rating <- max(ratings, na.rm = TRUE)
  }

  shifted_ratings <- (ratings * ((max_rating - (proportion * max_rating)) / max_rating)) + (proportion * max_rating)
  return(shifted_ratings)

}
