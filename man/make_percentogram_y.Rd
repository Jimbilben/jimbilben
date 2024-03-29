% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_percentogram_y.R
\name{make_percentogram_y}
\alias{make_percentogram_y}
\title{Make percentogram X}
\usage{
make_percentogram_y(
  data,
  percent = 5,
  percentile_range = FALSE,
  variable_name = "y",
  .xmin = 0,
  .xmax = 0.75
)
}
\arguments{
\item{data}{Ideally a single vector of your values to plot as a percentogram. Will also take a dataframe provided you give the name of the variable to 'variable_name'}

\item{percent}{A numeric value indicating the percentage you want each bar to represent. Defaults to 5. Other suggested values as 2.5, 4, and 10. Ideally, but does not necessarily, divide 100 without remainders.}

\item{percentile_range}{A character string or vector of character strings to request whether to also return designations of particular percentiles: 'quartile', 'quintile', 'octile', and 'decile' are supported. These can then be used to color bars according to the percentiles they cover.}

\item{variable_name}{A character string indicating the name of the variable if a dataframe is provided rather than a vector.}

\item{.xmin}{Defaults to 0 - the value on the x axis at which the bottom of the bars will be plotted.}

\item{.xmax}{Defaults to .75 - the highest value on the x axis at which the widest bar will be plotted.}
}
\description{
Takes a vector of values and returns coordinates to make a percentogram (variable bin width histogram with set percentile bars) using a plotting device such as geom_rect. Provides values for making the bins show the values on the Y axis - use make_percentogram_x for making it on the X axis.
}
