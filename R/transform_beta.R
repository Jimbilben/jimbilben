#' Beta Transform Function
#'
#' Transforms a number to fall within the open interval (0,1), via a beta transformation. The transformation is especially useful in preparing
#' data for beta regression models, which require inputs to be in the open interval (0,1).
#'
#' @usage beta_transform(number, sample_size = 886, max = 100)
#'
#' @param number The number to be converted.
#' @param sample_size An integer representing the sample size for the transformation. Default is 2000.
#' @param max An optional numeric value representing the expected maximum value in the data.
#'            All elements in 'number' are divided by this value to initially scale them to a (0,1) range. Default is 100.
#'
#' @details
#' The function works in two main steps. First, it scales the input vector ('number') to a (0,1) range by dividing by 'max'.
#' Then it adjusts the scaled values to fall within the open interval (0,1) using the formula:
#' (number * (sample_size - 1) + 0.5) / sample_size. This ensures that the transformed values are compatible with beta regression requirements.
#'
#' @return A numeric vector of the same length as 'number', with all elements transformed to fall within the open interval (0,1).
#'
transform_beta <- function(number, sample_size = 2000, max = 100) {

  number <- number / max

  number <- (number * (sample_size - 1) + .5) / sample_size

  return(number)

}
