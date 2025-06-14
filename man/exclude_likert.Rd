% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exclude_likert.R
\name{exclude_likert}
\alias{exclude_likert}
\title{Exclude Specific Likert Response Categories and Renormalize Posterior Estimates}
\usage{
exclude_likert(
  data_list,
  responses_to_exclude,
  subgroups = c("population", "partyid", "male", "income_ces", "age_fine", "race",
    "education_collapse", "region"),
  .percentage = TRUE,
  .decimals = 1
)
}
\arguments{
\item{data_list}{A list of poststratified results, where each element (e.g., `population`, `partyid`) contains a `$posterior` data frame.}

\item{responses_to_exclude}{A character vector of response options to remove (e.g., `c("Don't know", "Refused")`).}

\item{subgroups}{A character vector of subgroup names to process. Defaults to common groupings such as "population", "partyid", etc.}

\item{.percentage}{Logical. Whether to express summary values as percentages (default is `TRUE`).}

\item{.decimals}{Integer. Number of decimal places to show in the summary (default is `1`).}
}
\value{
The original `data_list` with two new components added for each subgroup:
\describe{
  \item{\code{posterior_excluded}}{The posterior draws with specified responses excluded and remaining proportions renormalized.}
  \item{\code{summary_excluded}}{A formatted summary of the excluded posterior, grouped and labeled using \code{nice_post()} and \code{nice_num()}.}
}
}
\description{
Removes specified response options (e.g., "Don't know", "Refused") from poststratified Likert-style posterior distributions.
After exclusion, remaining response proportions are renormalized within each combination of draw, outcome, grouping_type,
and any subgroup variables so that proportions sum to 1.
}
\examples{
# Remove "Don't know" and "Refused" responses
filtered <- exclude_likert(data_list = eg_agree, responses_to_exclude = c("Don't know", "Refused"))

# Then reduce on the excluded set
reduced <- reduce_likert(filtered, split_type = "agree")

}
