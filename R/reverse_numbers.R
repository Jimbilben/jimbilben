#' Reverse numbers
#'
#' This function reverses the numeric values within a vector by subtracting each number
#' from the maximum value and then adding the minimum value.
#'
#' @param numbers A numeric vector to be reversed.
#'
#' @return A numeric vector of the same length as the input, with values reversed within the range of the input vector.
#'
#' @examples
#' my_numbers <- c(2, 4, 6, 8, 10)
#' reverse_numbers(my_numbers)
reverse_numbers <- function(numbers) {
  reversed_numbers <- max(numbers, na.rm = TRUE) - numbers + min(numbers, na.rm = TRUE)
  return(reversed_numbers)
}
