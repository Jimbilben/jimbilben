#' Poststratify Categorical MRP Predictions
#'
#' Perform poststratification on the posterior predictive draws from a categorical MRP model across specified subgroups.
#'
#' @param input The object generated from \code{categorical_mrp}, containing the model and posterior predictions.
#' @param variable_name String. The name of the categorical variable being poststratified.
#' @param outcome_name Optional. A human-readable name for the outcome. Defaults to \code{variable_name}.
#' @param cat_levels A character vector specifying the levels of the categorical variable (e.g., \code{c("Approve", "Disapprove", "DK")}). Must be set for the function to work.
#' @param save_output Logical. If \code{TRUE}, saves the poststratification summaries to a file. Defaults to \code{save_my_poststrat}.
#' @param return_state Logical. Whether to include the "state" subgroup in the returned output. Defaults to \code{set_state}.
#' @param .poststrat_tibble A data frame containing the poststratification tibble. Defaults to \code{set_my_poststrat}.
#' @param .poststrat_epred A data frame containing the poststratification posterior predictions. Defaults to \code{set_my_epred}.
#' @param name_addition String, defaults to "" (i.e., nothing). Can provide a string to more uniquely identify what the file will be named as.
#'
#' @return A list of poststratification summaries, including:
#'   \itemize{
#'     \item \code{"population"}: The population-level summary.
#'     \item Subgroup summaries for each variable in \code{subgroups}.
#'   }
#'   If \code{return_state = FALSE}, the "state" subgroup summary is excluded.
#'
#' @details
#' This function takes the posterior epred draws from \code{categorical_mrp} and performs poststratification
#' at both the population level and across specified subgroups. It applies the function
#' \code{mrp_party_categorical_poststrat} for stratification and formats the output with readable labels.
#'
#' The function requires \code{cat_levels} to be defined to correctly order and arrange categorical choices.
#' Subgroups can be arranged in a logical order (e.g., for \code{"education_collapse"} or \code{"income_ces"}).
#' The output can optionally be saved as an RDS file for future use.
#'
#' @note Requires the \code{brms} and \code{cmdstanr} packages.
#' Ensure that the \code{.poststrat_tibble} and \code{.poststrat_epred} arguments are properly formatted.
#'
#' @export
poststrat_multi_categorical <- function(input,
                                        variable_name,
                                        outcome_name = NULL,
                                        cat_levels = NULL,
                                        save_output = save_my_poststrat,
                                        return_state = set_state,
                                        .poststrat_tibble = set_my_poststrat,
                                        .poststrat_epred = set_my_epred,
                                        name_addition = "") {

  if(is.null(outcome_name)) {
    outcome_name <- variable_name
  }

  # overall population level
  print(glue::glue("getting population level for {outcome_name}"))
  population_summary <-
    jimbilben::mrp_party_categorical_poststrat(input,
                                               outcome = outcome_name,
                                               poststrat_tibble = .poststrat_tibble,
                                               poststrat_epred = .poststrat_epred)

  population_summary$summary <-
    population_summary$summary %>%
    dplyr::mutate(new_mean = dplyr::case_when(mean < 1 ~ "<1",
                                              TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)),
                  new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]")) %>%
    dplyr::relocate(outcome, grouping_type, new_label)

  population_summary$summary <-
    population_summary$summary %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(choice)

  population_summary$posterior <-
    population_summary$posterior %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(choice)

  # subgroups
  ## education_collapse
  print(glue::glue("stratifying by education_collapse for {outcome_name}"))
  education_collapse_summary <-
    jimbilben::mrp_party_categorical_poststrat(input,
                                               outcome = outcome_name,
                                               subgroups = TRUE,
                                               poststrat_tibble = .poststrat_tibble %>% dplyr::group_by(education_collapse),
                                               poststrat_epred = .poststrat_epred)

  education_collapse_summary$summary <-
    education_collapse_summary$summary %>%
    dplyr::mutate(new_mean = dplyr::case_when(mean < 1 ~ "<1",
                                              TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)),
                  new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]"),
                  grouping_type = "Education") %>%
    dplyr::relocate(outcome, grouping_type, new_label) %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(education_collapse, choice)

  education_collapse_summary$posterior <-
    education_collapse_summary$posterior %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(education_collapse, choice)

  ## race
  print(glue::glue("stratifying by race for {outcome_name}"))
  race_summary <-
    jimbilben::mrp_party_categorical_poststrat(input,
                                               outcome = outcome_name,
                                               subgroups = TRUE,
                                               poststrat_tibble = .poststrat_tibble %>% dplyr::group_by(race),
                                               poststrat_epred = .poststrat_epred)

  race_summary$summary <-
    race_summary$summary %>%
    dplyr::mutate(new_mean = dplyr::case_when(mean < 1 ~ "<1",
                                              TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)),
                  new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]"),
                  grouping_type = "Race") %>%
    dplyr::relocate(outcome, grouping_type, new_label) %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(choice)

  race_summary$posterior <-
    race_summary$posterior %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(choice)

  ## income_ces
  print(glue::glue("stratifying by income_ces for {outcome_name}"))
  income_ces_summary <-
    jimbilben::mrp_party_categorical_poststrat(input,
                                               outcome = outcome_name,
                                               subgroups = TRUE,
                                               poststrat_tibble = .poststrat_tibble %>% dplyr::group_by(income_ces),
                                               poststrat_epred = .poststrat_epred)

  income_ces_summary$summary <-
    income_ces_summary$summary %>%
    dplyr::mutate(new_mean = dplyr::case_when(mean < 1 ~ "<1",
                                              TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)),
                  new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]"),
                  grouping_type = "Income") %>%
    dplyr::relocate(outcome, grouping_type, new_label) %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(income_ces, choice)

  income_ces_summary$posterior <-
    income_ces_summary$posterior %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(income_ces, choice)

  ## male
  print(glue::glue("stratifying by male for {outcome_name}"))
  male_summary <-
    jimbilben::mrp_party_categorical_poststrat(input,
                                               outcome = outcome_name,
                                               subgroups = TRUE,
                                               poststrat_tibble = .poststrat_tibble %>% dplyr::group_by(male),
                                               poststrat_epred = .poststrat_epred)

  male_summary$summary <-
    male_summary$summary %>%
    dplyr::mutate(new_mean = dplyr::case_when(mean < 1 ~ "<1",
                                              TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)),
                  new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]"),
                  grouping_type = "Sex") %>%
    dplyr::relocate(outcome, grouping_type, new_label) %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(choice)

  male_summary$posterior <-
    male_summary$posterior %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(choice)

  ## partyid
  print(glue::glue("stratifying by partyid for {outcome_name}"))
  partyid_summary <-
    jimbilben::mrp_party_categorical_poststrat(input,
                                               outcome = outcome_name,
                                               subgroups = TRUE,
                                               poststrat_tibble = .poststrat_tibble %>% dplyr::group_by(partyid),
                                               poststrat_epred = .poststrat_epred)

  partyid_summary$summary <-
    partyid_summary$summary %>%
    dplyr::mutate(new_mean = dplyr::case_when(mean < 1 ~ "<1",
                                              TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)),
                  new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]"),
                  grouping_type = "Party affiliation") %>%
    dplyr::relocate(outcome, grouping_type, new_label) %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(choice)

  partyid_summary$posterior <-
    partyid_summary$posterior %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(choice)

  ## age_fine
  print(glue::glue("stratifying by age_fine for {outcome_name}"))
  age_fine_summary <-
    jimbilben::mrp_party_categorical_poststrat(input,
                                               outcome = outcome_name,
                                               subgroups = TRUE,
                                               poststrat_tibble = .poststrat_tibble %>% dplyr::group_by(age_fine),
                                               poststrat_epred = .poststrat_epred)

  age_fine_summary$summary <-
    age_fine_summary$summary %>%
    dplyr::mutate(new_mean = dplyr::case_when(mean < 1 ~ "<1",
                                              TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)),
                  new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]"),
                  grouping_type = "Age") %>%
    dplyr::relocate(outcome, grouping_type, new_label) %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(age_fine, choice)

  age_fine_summary$posterior <-
    age_fine_summary$posterior %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(age_fine, choice)

  ## state
  print("I'm not going to compute this by state! But you can unhash me")
  # print(glue::glue("stratifying by state for {outcome_name}"))
  # state_summary <-
  #   jimbilben::mrp_party_categorical_poststrat(input,
  #                                              outcome = outcome_name,
  #                                              subgroups = TRUE,
  #                                              poststrat_tibble = .poststrat_tibble %>% dplyr::group_by(state),
  #                                              poststrat_epred = .poststrat_epred)
  #
  # state_summary$summary<-
  #   state_summary$summary %>%
  #   dplyr::mutate(new_mean = dplyr::case_when(mean < 1 ~ "<1",
  #                                             TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)),
  #                 new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]"),
  #                 grouping_type = "State") %>%
  #   dplyr::relocate(outcome, grouping_type, new_label) %>%
  #   dplyr::mutate(choice = factor(choice,
  #                                 levels = cat_levels,
  #                                 ordered = TRUE)) %>%
  #   dplyr::arrange(choice)
  #
  # state_summary$posterior <-
  #   state_summary$posterior %>%
  #   dplyr::mutate(choice = factor(choice,
  #                                 levels = cat_levels,
  #                                 ordered = TRUE)) %>%
  #   dplyr::arrange(choice)

  ## region
  print(glue::glue("stratifying by region for {outcome_name}"))
  region_summary <-
    jimbilben::mrp_party_categorical_poststrat(input,
                                               outcome = outcome_name,
                                               subgroups = TRUE,
                                               poststrat_tibble = .poststrat_tibble %>% dplyr::group_by(region),
                                               poststrat_epred = .poststrat_epred)

  region_summary$summary <-
    region_summary$summary %>%
    dplyr::mutate(new_mean = dplyr::case_when(mean < 1 ~ "<1",
                                              TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)),
                  new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]"),
                  grouping_type = "Region") %>%
    dplyr::relocate(outcome, grouping_type, new_label) %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(choice)

  region_summary$posterior <-
    region_summary$posterior %>%
    dplyr::mutate(choice = factor(choice,
                                  levels = cat_levels,
                                  ordered = TRUE)) %>%
    dplyr::arrange(choice)

  output <-
    list("population" = population_summary,
         "education_collapse" = education_collapse_summary,
         "age_fine" = age_fine_summary,
         "race" = race_summary,
         "income_ces" = income_ces_summary,
         "partyid" = partyid_summary,
         "male" = male_summary,
         "region" = region_summary#,
         #"state" = state_summary
    )

  if(save_output == TRUE) {
    saveRDS(output,
            glue::glue("mrp_poststrats/{variable_name}{name_addition}_poststrat.rds"))
  }

  if(return_state == FALSE) {
    output <-
      list("population" = population_summary,
           "education_collapse" = education_collapse_summary,
           "age_fine" = age_fine_summary,
           "race" = race_summary,
           "income_ces" = income_ces_summary,
           "partyid" = partyid_summary,
           "male" = male_summary,
           "region" = region_summary)
  }

  return(output)

}
