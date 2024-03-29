% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrp_cat_prior.R
\name{mrp_cat_prior}
\alias{mrp_cat_prior}
\title{MRP categorical prior generator}
\usage{
mrp_cat_prior(
  categorical_var,
  intercept_prior = "normal(0, 2)",
  b_prior = "normal(0, 1)",
  sd_prior = "exponential(2)",
  sd_lower_bound = NULL
)
}
\arguments{
\item{categorical_var}{A factor containing the categorical variable for which priors will be generated}

\item{intercept_prior}{A string specifying the prior distribution for the intercept (Default: "normal(0, 2)")}

\item{b_prior}{A string specifying the prior distribution for the fixed effects (Default: "normal(0, 1)")}

\item{sd_prior}{A string specifying the prior distribution for the standard deviation of the random effects (Default: "exponential(2)")}
}
\value{
A list of prior specifications compatible with the brms package
}
\description{
Generates a list of priors for a given categorical variable in a Multilevel Regression and Poststratification (MRP) model using the brms package.
}
\examples{
# Example with a categorical variable 'region'
data <- data.frame(region = as.factor(c("North", "East", "West", "South")))
mrp_cat_prior(data$region)
}
