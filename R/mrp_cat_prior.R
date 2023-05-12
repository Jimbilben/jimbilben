#' MRP categorical prior generator
#'
#' Generates a list of priors for a given categorical variable in a Multilevel Regression and Poststratification (MRP) model using the brms package.
#'
#' @param categorical_var A factor containing the categorical variable for which priors will be generated
#' @param intercept_prior A string specifying the prior distribution for the intercept (Default: "normal(0, 2)")
#' @param b_prior A string specifying the prior distribution for the fixed effects (Default: "normal(0, 1)")
#' @param sd_prior A string specifying the prior distribution for the standard deviation of the random effects (Default: "exponential(2)")
#'
#' @return A list of prior specifications compatible with the brms package
#' @export
#'
#' @examples
#' # Example with a categorical variable 'region'
#' data <- data.frame(region = as.factor(c("North", "East", "West", "South")))
#' mrp_cat_prior(data$region)
mrp_cat_prior <- function(categorical_var, intercept_prior = "normal(0, 2)", b_prior = "normal(0, 1)", sd_prior = "exponential(2)") {

  ref_level <- levels(categorical_var)[1] # Assumes the reference level is the first level

  non_ref_levels <- levels(categorical_var)[-1] # Excludes the reference level

  priors <- c(brms::set_prior(intercept_prior, class = "Intercept"),
              brms::set_prior(b_prior, class = "b"))

  for (level in non_ref_levels) {
    clean_level <- gsub("[^[:alnum:].]", "", level) # Remove non-alphanumeric characters except periods
    prior_str <- sprintf('brms::set_prior("%s", class = "sd", dpar = "mu%s")', sd_prior, clean_level)
    priors <- c(priors, eval(parse(text = prior_str)))
  }

  return(priors)
}
