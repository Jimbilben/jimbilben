% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rake_weights.R
\name{rake_weights}
\alias{rake_weights}
\title{Rake Weights}
\usage{
rake_weights(
  targets,
  data,
  id_col = "id",
  weight_name = "weights",
  append_to_data = FALSE,
  include_modified_weight = TRUE,
  show_reminder = TRUE,
  max_attempts = 50,
  .weightvec = NULL,
  .cap = NULL,
  .verbose = FALSE,
  .maxit = 3000,
  .type = "pctlim",
  .pctlim = 0.02,
  .iterate = TRUE,
  .convcrit = 0.01,
  .force1 = TRUE,
  .center.baseweights = TRUE
)
}
\arguments{
\item{targets}{A list of weighting targets (see the gen_us() function to generate these easily)}

\item{data}{Data for which you want to generate weights - unlike like anesrake this can be a tibble, and the function will also factor the target variables for you.}

\item{id_col}{A string indicating the name of the column in the data that uniquely identifies each respondent. Defaults to "id"}

\item{weight_name}{An optional name for the weights column that will be appended to your data if you select append_to_data = TRUE. Defaults to "weights"}

\item{append_to_data}{TRUE or FALSE - should the function return the data with the weights attached? Defaults to false.}

\item{include_modified_weight}{Whether or not to return a vector (and append to the data if append_to_data = TRUE) a vector of the weights divided by the design effect, which can be used in pseudo-weighted analyses.}

\item{show_reminder}{The function will, by default (TRUE), show you the names of the objects it has created and tell you what they are.}

\item{max_attempts}{Numeric, the number of attempts that the function should make when increasing the cap to try and get the weighting to converge. Defaults to 50.}

\item{.pctlim}{Same as the pctlim parameter of anesrake: the amount of deviation from the target population proportions that will be allowed. This should be set somewhere between .02 and .05}
}
\description{
Generates weights using the Anesrake procedure, trying to get convergence starting at a low cap and the increasing if convergence is not reached. The function also calculates the design effect.
}
