% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_xpos.R
\name{get_xpos}
\alias{get_xpos}
\title{get_xpos}
\usage{
get_xpos(
  data,
  variable,
  nearest = 10,
  break_width = NULL,
  number_block_multiple = 1.2
)
}
\arguments{
\item{data}{Copy from call to nice_x() - The data from which the x variable is to be taken}

\item{variable}{Copy from call to nice_x() - A character string naming the x variable}

\item{nearest}{Copy from call to nice_x() - The nearest number to round up to - defaults to 10}

\item{break_width}{Copy from call to nice_x() - Size of the major break, defaults to 'nearest'}

\item{number_block_multiple}{Copy from call to nice_x() - Proportional expansion of the axis to accommodate the number block - defaults to 1.2}
}
\description{
Get the start and middle points for a number/annotation block.
}
