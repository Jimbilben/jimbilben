#' Reduce Likert Scale Posterior Estimates to Collapsed Categories
#'
#' This function processes posterior draws from poststratification estimates,
#' collapsing Likert-scale choices (e.g., from a 7-point scale) into simpler
#' groupings like "Agree", "Neutral", and "Disagree". It returns both a
#' reduced posterior (`posterior_reduced`) and a formatted summary
#' (`summary_reduced`) for each subgroup.
#'
#' @param data_list A list of poststratified results, where each element (e.g., `population`, `partyid`) contains a `$posterior` data frame.
#' @param subgroups A character vector of subgroup names to process. Defaults to common subgroup names.
#' @param split_type Either a string keyword (`"agree"` or `"support"`) to use built-in regex logic, or a named list defining custom category groupings.
#' @param .percentage Logical. Whether to format the summary as percentages (default `TRUE`).
#' @param .decimals Integer. Number of decimal places to show in the summary (default `1`).
#'
#' @return The input `data_list` with additional `$posterior_reduced` and `$summary_reduced` elements appended to each specified subgroup.
#'
#' @examples
#' # Using built-in "agree" split
#' reduced <- reduce_likert(eg_agree, split_type = "agree")
#'
#' # Using a custom split
#' custom_split <- list(
#'   Low = c("Strongly disagree", "Disagree"),
#'   Middle = c("Neutral", "Neither agree nor disagree"),
#'   High = c("Agree", "Strongly agree")
#' )
#' reduced <- reduce_likert(eg_agree, split_type = custom_split)
#'
#' @export
reduce_likert <- function(data_list,
                          subgroups = c("population", "partyid", "male", "income_ces", "age_fine", "race", "education_collapse", "region"),
                          split_type = "agree",  # or list(...)
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

  # Function to categorize using regex-based logic
  get_category_regex <- function(choice_text, type) {
    choice_text <- tolower(choice_text)
    if (stringr::str_detect(choice_text, "neither|neutral")) {
      return("Neutral")
    } else if (type == "agree") {
      if (stringr::str_detect(choice_text, "disagree")) {
        return("Disagree")
      } else {
        return("Agree")
      }
    } else if (type == "support") {
      if (stringr::str_detect(choice_text, "oppose")) {
        return("Oppose")
      } else {
        return("Support")
      }
    } else {
      stop("Unsupported split_type string. Use 'agree', 'support', or a custom list.")
    }
  }

  use_custom_split <- is.list(split_type)

  # Define ordering of choice levels
  if (use_custom_split) {
    choice_levels <- names(split_type)
    split_map <- unlist(lapply(choice_levels, function(name) {
      stats::setNames(rep(name, length(split_type[[name]])), split_type[[name]])
    }))
  } else {
    split_type <- tolower(split_type)
    choice_levels <- switch(
      split_type,
      agree = c("Disagree", "Neutral", "Agree"),
      support = c("Oppose", "Neutral", "Support"),
      stop("Unsupported split_type string. Use 'agree', 'support', or a custom list.")
    )
  }

  for (sg in subgroups) {
    if (!sg %in% names(data_list)) next
    group_vars <- subgroup_group_vars[[sg]]

    posterior <- data_list[[sg]]$posterior

    # Recode choices
    if (use_custom_split) {
      posterior_reduced <- dplyr::mutate(posterior, choice = dplyr::recode(choice, !!!split_map))
    } else {
      posterior_reduced <- dplyr::mutate(posterior, choice = vapply(choice, get_category_regex, character(1), type = split_type))
    }

    posterior_reduced <- posterior_reduced %>%
      dplyr::mutate(choice = factor(choice, levels = choice_levels, ordered = TRUE)) %>%
      dplyr::arrange(choice) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("choice", "draw", "outcome", "grouping_type", group_vars)))) %>%
      dplyr::summarise(proportion = sum(proportion), .groups = "drop")

    # Summary
    summary_reduced <- posterior_reduced %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("choice", "outcome", "grouping_type", group_vars)))) %>%
      jimbilben::nice_post(proportion, percentage = .percentage, decimals = .decimals) %>%
      dplyr::mutate(
        nice_mean = dplyr::case_when(mean < 1 ~ "<1",
                                     TRUE ~ jimbilben::nice_num(mean, 0, FALSE)),
        new_label = glue::glue("**{nice_mean}%** [{jimbilben::nice_num(lower, 1, FALSE)}%; {jimbilben::nice_num(upper, 1, FALSE)}%]")
      ) %>%
      dplyr::relocate(new_label)

    # Append both versions to the list
    data_list[[sg]]$posterior_reduced <- posterior_reduced
    data_list[[sg]]$summary_reduced <- summary_reduced
  }

  return(data_list)
}
