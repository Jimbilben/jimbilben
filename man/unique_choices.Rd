% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unique_choices.R
\name{unique_choices}
\alias{unique_choices}
\title{Unique choices
Returns a vector of strings indicating each possible item in a multiple choice question where people can select more than one thing.}
\usage{
unique_choices(data, variable, sep = ",", add_backslash = TRUE)
}
\arguments{
\item{data}{the target dataframe}

\item{variable}{the variable/column name in which the multiple choice items are stored}

\item{add_backslash}{Logical, defaults to TRUE. If TRUE, parentheses will be preceded by two backslashes e.g., \\(this\\)}

\item{prefix}{= A string. Each variable name should, but does not have to be, preceded by a prefix so that the new variables can be easily identified/selected and e.g., pivot_longer from. Defaults to choice_}
}
\description{
Unique choices
Returns a vector of strings indicating each possible item in a multiple choice question where people can select more than one thing.
}
