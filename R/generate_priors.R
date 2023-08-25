#' Generate Prior Code
#'
#' This function generates prior code specification for a given set of items. The function has limited uses currently and needs to be made more general.
#'
#' @param items A vector of item names or identifiers for which specific priors will be created.
#' @param term A string that defines the prefix for the coefficients. Default is "item".
#' @param specific_prior A string that defines the prior distribution for specific items. Default is "normal(0, 1.5)".
#' @param other_prior A string that defines the prior distribution for other, non-specific items. Default is "normal(0, 1)".
#' @param sd_prior A string that defines the prior distribution for the standard deviation. Default is "exponential(2)".
#' @param intercept_prior A string that defines the prior distribution for the intercept. Default is "normal(0, 1.5)".
#'
#' @return A list of prior objects.
#'
#' @export
generate_priors <- function(items,
                            term = "item",
                            specific_prior = "normal(0, 1.5)",
                            other_prior = "normal(0, 1)",
                            sd_prior = "exponential(2)",
                            intercept_prior = "normal(0, 1.5)") {
  # Create a list of priors for specific items using the specified distribution
  priors <- purrr::map(items, ~brms::set_prior(glue::glue("{specific_prior}"), class = "b", coef = paste0(term, .x)))

  # Add the other prior (non-specific) to the list
  priors <- c(priors, list(brms::set_prior(other_prior, class = "b")))

  # If sd_prior is not NULL, add the standard deviation prior with the given distribution
  if (!is.null(sd_prior)) {
    priors <- c(priors, list(brms::set_prior(glue::glue("{sd_prior}"), class = "sd")))
  }

  # If intercept_prior is not NULL, add the intercept prior with the given distribution
  if (!is.null(intercept_prior)) {
    priors <- c(priors, list(brms::set_prior(glue::glue("{intercept_prior}"), class = "Intercept")))
  }

  # Return the concatenated list of priors
  return(do.call(c, priors))
}
