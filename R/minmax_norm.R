#' Min-max normalization
#'
#' This function performs min-max normalization on a numeric vector, scaling the input numbers
#' to a specified range, defined by the 'low' and 'high' arguments.
#'
#' @param numbers A numeric vector to be normalized.
#' @param low The lower bound of the normalization range (default: 0).
#' @param high The upper bound of the normalization range (default: 1).
#'
#' @return A numeric vector of the same length as the input, with values normalized to the specified range.
#'
#' @examples
#' my_numbers <- c(2, 4, 6, 8, 10)
#' minmax_norm(my_numbers)
#' minmax_norm(my_numbers, low = -1, high = 1)
minmax_norm <- function(numbers, low = 0, high = 1) {
  min_value <- min(numbers, na.rm = TRUE)
  max_value <- max(numbers, na.rm = TRUE)
  normalized_numbers <- (numbers - min_value) / (max_value - min_value)

  my_range = abs(low - high)
  normalized_numbers <- (normalized_numbers * my_range) + low

  return(normalized_numbers)
}
