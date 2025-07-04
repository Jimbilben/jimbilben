% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psup_mrp.R
\name{psup_mrp}
\alias{psup_mrp}
\title{Run PSup MRP}
\usage{
psup_mrp(
  variable_name,
  variable_label = NULL,
  my_data = set_my_data,
  save_model = save_my_model,
  save_epred = save_my_epred,
  my_prior = c(brms::set_prior("normal(0 , 1.25)", class = "Intercept"),
    brms::set_prior("normal(0 , 1)", class = "Intercept", dpar = "mu0"),
    brms::set_prior("normal(0 , 1)", class = "Intercept", dpar = "mu1"),
    brms::set_prior("normal(0 , 1)", class = "b"), brms::set_prior("exponential(2)",
    class = "sd", dpar = "mu0"), brms::set_prior("exponential(2)", class = "sd", dpar =
    "mu1")),
  my_init = 0,
  my_iter = set_my_iter,
  my_warmup = set_my_warmup,
  my_refresh = 250,
  my_poststrat = set_my_poststrat,
  my_adapt_delta = set_my_adapt_delta,
  name_addition = ""
)
}
\arguments{
\item{variable_name}{String. The name of the categorical PSup variable to be modeled.}

\item{variable_label}{Optional. A human-readable label for the variable. Defaults to \code{variable_name}.}

\item{my_data}{A data frame containing the data for the regression model. Defaults to \code{set_my_data}.}

\item{save_model}{Logical. If \code{TRUE}, saves the fitted model object to an mrp_models folder in your directory. Defaults to \code{save_my_model}.}

\item{save_epred}{Logical. If \code{TRUE}, saves the posterior epred draws to an mrp_epreds folder in your directory. Defaults to \code{save_my_epred}.}

\item{my_prior}{A list of prior distributions for the model. Defaults to:
\itemize{
  \item \code{Intercept: normal(0, 1.25)}
  \item \code{Intercept (mu0, mu1): normal(0, 1)}
  \item \code{Coefficients: normal(0, 1)}
  \item \code{Random effects (mu0, mu1): exponential(2)}
}}

\item{my_init}{Initial values for the model. Default is \code{0}.}

\item{my_iter}{Total number of iterations per chain. Default is \code{set_my_iter}.}

\item{my_warmup}{Number of warmup iterations for the sampler. Default is \code{set_my_warmup}.}

\item{my_refresh}{Frequency of progress updates during model fitting. Default is \code{250}.}

\item{my_poststrat}{A data frame for poststratification. Default is \code{set_my_poststrat}.}

\item{my_adapt_delta}{Numeric value for the Stan sampler control parameter to improve convergence. Default is \code{set_my_adapt_delta}.}

\item{name_addition}{String, defaults to "" (i.e., nothing). Can provide a string to more uniquely identify what the file will be named as.}
}
\value{
A list containing:
  \itemize{
    \item \code{"model"}: The fitted \code{brms} model object.
    \item \code{"epred"}: Posterior predictions from the fitted model.
  }
}
\description{
Perform the regression and epred steps of a multilevel regression and poststratification (MRP) for a partially supervised (PSup) categorical outcome variable.
}
\details{
This function performs multilevel Bayesian regression using the \code{brms} package.
It constructs a model formula based on a predefined structure, fits the model
using the \code{cmdstanr} backend, and generates posterior epred draws for
a target population. The model and predictions can be optionally saved for future use.

If the variable is numeric (0, 0.5, 1), it will be converted to a factor with 0.5 as the reference category.
}
\note{
Requires the \code{brms} and \code{cmdstanr} packages.
Ensure that the \code{my_data} and \code{my_poststrat} arguments are properly formatted.
}
