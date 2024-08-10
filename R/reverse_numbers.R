#' Reverse numbers
#'
#' This function reverses the numeric values within a vector by subtracting each number
#' from the maximum value and then adding the minimum value.
#'
#' @param numbers A numeric vector to be reversed.
#' @param minmax A numeric vector for the minimum and maximum values - defaults to NULL, in which case the observed min and max from the data are used
#' @return A numeric vector of the same length as the input, with values reversed within the range of the input vector.
#'
#' @examples
#' my_numbers <- c(2, 4, 6, 8, 10)
#' reverse_numbers(my_numbers)
reverse_numbers <- function(numbers, minmax = NULL) {

  if(is.null(minmax) | length(minmax) != 2) {

    print("Using observed min and max from data")
    reversed_numbers <- max(numbers, na.rm = TRUE) - numbers + min(numbers, na.rm = TRUE)

  }

  else {

    reversed_numbers <- minmax[2] - numbers + minmax[1]

  }

  return(reversed_numbers)

}
