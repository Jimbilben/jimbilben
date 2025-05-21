#' Exclude Specific Likert Response Categories and Renormalize Posterior Estimates
#'
#' Removes specified response options (e.g., "Don't know", "Refused") from poststratified Likert-style posterior distributions.
#' After exclusion, remaining response proportions are renormalized within each combination of draw, outcome, grouping_type,
#' and any subgroup variables so that proportions sum to 1.
#'
#' @param data_list A list of poststratified results, where each element (e.g., `population`, `partyid`) contains a `$posterior` data frame.
#' @param responses_to_exclude A character vector of response options to remove (e.g., `c("Don't know", "Refused")`).
#' @param subgroups A character vector of subgroup names to process. Defaults to common groupings such as "population", "partyid", etc.
#' @param .percentage Logical. Whether to express summary values as percentages (default is `TRUE`).
#' @param .decimals Integer. Number of decimal places to show in the summary (default is `1`).
#'
#' @return The original `data_list` with two new components added for each subgroup:
#' \describe{
#'   \item{\code{posterior_excluded}}{The posterior draws with specified responses excluded and remaining proportions renormalized.}
#'   \item{\code{summary_excluded}}{A formatted summary of the excluded posterior, grouped and labeled using \code{nice_post()} and \code{nice_num()}.}
#' }
#'
#' @examples
#' # Remove "Don't know" and "Refused" responses
#' filtered <- exclude_likert(data_list = eg_agree, responses_to_exclude = c("Don't know", "Refused"))
#'
#' # Then reduce on the excluded set
#' reduced <- reduce_likert(filtered, split_type = "agree")
#'
#' @export
exclude_likert <- function(data_list,
                           responses_to_exclude,
                           subgroups = c("population", "partyid", "male", "income_ces", "age_fine", "race", "education_collapse", "region"),
                           .percentage = TRUE,
                           .decimals = 1) {

  subgroup_group_vars <- list(
    population = character(0),
    partyid = "partyid",
    male = "male",
    income_ces = "income_ces",
    age_fine = "age_fine",
    race = "race",
    education_collapse = "education_collapse",
    region = "region"
  )

  for (sg in subgroups) {
    if (!sg %in% names(data_list)) next
    group_vars <- subgroup_group_vars[[sg]]

    grouping_vars <- c("draw", "outcome", "grouping_type", group_vars)

    posterior_excluded <- data_list[[sg]]$posterior %>%
      dplyr::filter(!choice %in% responses_to_exclude) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("choice", grouping_vars)))) %>%
      dplyr::summarise(proportion = sum(proportion), .groups = "drop") %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
      dplyr::mutate(proportion = proportion / sum(proportion)) %>%
      dplyr::ungroup()

    summary_excluded <- posterior_excluded %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("choice", "outcome", "grouping_type", group_vars)))) %>%
      jimbilben::nice_post(proportion, percentage = .percentage, decimals = .decimals) %>%
      dplyr::mutate(
        nice_mean = dplyr::case_when(mean < 1 ~ "<1",
                                     TRUE ~ jimbilben::nice_num(mean, 0, FALSE)),
        new_label = glue::glue("**{nice_mean}%** [{jimbilben::nice_num(lower, 1, FALSE)}%; {jimbilben::nice_num(upper, 1, FALSE)}%]")
      ) %>%
      dplyr::relocate(new_label)

    data_list[[sg]]$posterior_excluded <- posterior_excluded
    data_list[[sg]]$summary_excluded <- summary_excluded
  }

  return(data_list)
}
