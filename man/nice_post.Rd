% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nice_post.R
\name{nice_post}
\alias{nice_post}
\title{Nice Posterior}
\usage{
nice_post(
  posterior,
  estimate = vars(proportion),
  interval = 0.95,
  point_est = "mean",
  decimals = 2,
  remove_lead = TRUE,
  percentage = FALSE,
  above = NULL,
  below = NULL,
  between = NULL
)
}
\arguments{
\item{posterior}{A set of posterior draws to be nicely summarised}

\item{interval}{The width of the uncertainty interval to be calculated, defaults to .95}

\item{point_est}{What point estimate would you like to summarise the posterior with, defaults to "median" - other options: "mean", "mode"}

\item{decimals}{The number of decimals to be included in summary labels, defaults to 2}

\item{remove_lead}{Whether the leading 0 should be removed in summary labels, defaults to TRUE}

\item{percentage}{When the posterior represents a proportion, we can represent it as a percentage instead, defaults to FALSE}

\item{above}{A number against which to test the proportion of the posterior equal to or above it}

\item{below}{A number against which to test the proportion of the posterior equal to or below it}

\item{between}{A vector of 2 numbers to test the proportion of the posterior falling between}

\item{uncertainty}{What type of interval would you like to summarise the posterior with, defaults to "hdi" - other options: "eti"}
}
\description{
Generate a character version of a number in a nice format for plotting.
}
