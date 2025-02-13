#' Poststratify Numeric MRP Predictions
#'
#' Perform poststratification on the posterior predictive draws from a numeric MRP model across specified subgroups.
#'
#' @param input The object generated from \code{numeric_mrp}, containing the model and posterior predictions.
#' @param variable_name String. The name of the numeric variable being poststratified.
#' @param outcome_name Optional. A human-readable name for the outcome. Defaults to \code{variable_name}.
#' @param save_output Logical. If \code{TRUE}, saves the poststratification summaries to a file. Defaults to \code{save_my_poststrat}.
#' @param return_state Logical. Whether to include the "state" subgroup in the returned output. Defaults to \code{set_state}.
#' @param .poststrat_tibble A data frame containing the poststratification tibble. Defaults to \code{set_my_poststrat}.
#' @param .poststrat_epred A data frame containing the poststratification posterior predictions. Defaults to \code{set_my_epred}.
#'
#' @return A list of poststratification summaries, including:
#'   \itemize{
#'     \item \code{"population"}: The population-level summary.
#'     \item Subgroup summaries for each variable in \code{subgroups}.
#'   }
#'   If \code{return_state = FALSE}, the "state" subgroup summary is excluded.
#'
#' @details
#' This function takes the posterior epred draws from \code{numeric_mrp} and performs poststratification
#' at both the population level and across specified subgroups. It applies the function
#' \code{mrp_party_normal_poststrat} for stratification and formats the output with readable labels.
#'
#' Subgroups can be arranged in a logical order (e.g., for \code{"education_collapse"} or \code{"income_ces"}).
#' The output can optionally be saved as an RDS file for future use.
#'
#' @note Requires the \code{brms} and \code{cmdstanr} packages.
#' Ensure that the \code{.poststrat_tibble} and \code{.poststrat_epred} arguments are properly formatted.
#'
#' @export
poststrat_multi_numeric <- function(input,
                                    variable_name,
                                    outcome_name,
                                    save_output = save_my_poststrat,
                                    return_state = set_state,
                                    .poststrat_tibble = set_my_poststrat,
                                    .poststrat_epred = set_my_epred,
                                    name_addition = "") {

  # overall population level
  print(glue::glue("getting population level for {outcome_name}"))
  population_summary <-
    mrp_party_normal_poststrat(input$epred,
                               outcome = outcome_name,
                               poststrat_tibble = .poststrat_tibble,
                               poststrat_epred = .poststrat_epred)

  population_summary$summary <-
    population_summary$summary %>%
    mutate(new_label = glue::glue("**{nice_num(mean, 1, FALSE)}** [{nice_num(lower, 2, FALSE)}; {nice_num(upper, 2, FALSE)}]")) %>%
    relocate(outcome, grouping_type, new_label)

  # subgroups
  ## education_collapse
  print(glue::glue("stratifying by education_collapse for {outcome_name}"))
  education_collapse_summary <-
    mrp_party_normal_poststrat(input$epred,
                               outcome = outcome_name,
                               subgroups = TRUE,
                               poststrat_tibble = .poststrat_tibble %>% group_by(education_collapse),
                               poststrat_epred = .poststrat_epred)

  education_collapse_summary$summary <-
    education_collapse_summary$summary %>%
    mutate(new_label = glue::glue("**{nice_num(mean, 1, FALSE)}** [{nice_num(lower, 2, FALSE)}; {nice_num(upper, 2, FALSE)}]"),
           grouping_type = "Education") %>%
    relocate(outcome, grouping_type, new_label) %>%
    arrange(education_collapse)

  ## race
  print(glue::glue("stratifying by race for {outcome_name}"))
  race_summary <-
    mrp_party_normal_poststrat(input$epred,
                               outcome = outcome_name,
                               subgroups = TRUE,
                               poststrat_tibble = .poststrat_tibble %>% group_by(race),
                               poststrat_epred = .poststrat_epred)

  race_summary$summary <-
    race_summary$summary %>%
    mutate(new_label = glue::glue("**{nice_num(mean, 1, FALSE)}** [{nice_num(lower, 2, FALSE)}; {nice_num(upper, 2, FALSE)}]"),
           grouping_type = "Race") %>%
    relocate(outcome, grouping_type, new_label)

  ## income_ces
  print(glue::glue("stratifying by income_ces for {outcome_name}"))
  income_ces_summary <-
    mrp_party_normal_poststrat(input$epred,
                               outcome = outcome_name,
                               subgroups = TRUE,
                               poststrat_tibble = .poststrat_tibble %>% group_by(income_ces),
                               poststrat_epred = .poststrat_epred)

  income_ces_summary$summary <-
    income_ces_summary$summary %>%
    mutate(new_label = glue::glue("**{nice_num(mean, 1, FALSE)}** [{nice_num(lower, 2, FALSE)}; {nice_num(upper, 2, FALSE)}]"),
           grouping_type = "Income") %>%
    relocate(outcome, grouping_type, new_label) %>%
    arrange(income_ces)

  ## male
  print(glue::glue("stratifying by male for {outcome_name}"))
  male_summary <-
    mrp_party_normal_poststrat(input$epred,
                               outcome = outcome_name,
                               subgroups = TRUE,
                               poststrat_tibble = .poststrat_tibble %>% group_by(male),
                               poststrat_epred = .poststrat_epred)

  male_summary$summary <-
    male_summary$summary %>%
    mutate(new_label = glue::glue("**{nice_num(mean, 1, FALSE)}** [{nice_num(lower, 2, FALSE)}; {nice_num(upper, 2, FALSE)}]"),
           grouping_type = "Sex") %>%
    relocate(outcome, grouping_type, new_label)

  ## partyid
  print(glue::glue("stratifying by partyid for {outcome_name}"))
  partyid_summary <-
    mrp_party_normal_poststrat(input$epred,
                               outcome = outcome_name,
                               subgroups = TRUE,
                               poststrat_tibble = .poststrat_tibble %>% group_by(partyid),
                               poststrat_epred = .poststrat_epred)

  partyid_summary$summary <-
    partyid_summary$summary %>%
    mutate(new_label = glue::glue("**{nice_num(mean, 1, FALSE)}** [{nice_num(lower, 2, FALSE)}; {nice_num(upper, 2, FALSE)}]"),
           grouping_type = "Party affiliation") %>%
    relocate(outcome, grouping_type, new_label)

  ## age_fine
  print(glue::glue("stratifying by age_fine for {outcome_name}"))
  age_fine_summary <-
    mrp_party_normal_poststrat(input$epred,
                               outcome = outcome_name,
                               subgroups = TRUE,
                               poststrat_tibble = .poststrat_tibble %>% group_by(age_fine),
                               poststrat_epred = .poststrat_epred)

  age_fine_summary$summary <-
    age_fine_summary$summary %>%
    mutate(new_label = glue::glue("**{nice_num(mean, 1, FALSE)}** [{nice_num(lower, 2, FALSE)}; {nice_num(upper, 2, FALSE)}]"),
           grouping_type = "Age") %>%
    relocate(outcome, grouping_type, new_label)

  ## state
  print(glue::glue("stratifying by state for {outcome_name}"))
  state_summary <-
    mrp_party_normal_poststrat(input$epred,
                               outcome = outcome_name,
                               subgroups = TRUE,
                               poststrat_tibble = .poststrat_tibble %>% group_by(state),
                               poststrat_epred = .poststrat_epred)

  state_summary$summary <-
    state_summary$summary %>%
    mutate(new_label = glue::glue("**{nice_num(mean, 1, FALSE)}** [{nice_num(lower, 2, FALSE)}; {nice_num(upper, 2, FALSE)}]"),
           grouping_type = "State") %>%
    relocate(outcome, grouping_type, new_label)

  ## region
  print(glue::glue("stratifying by region for {outcome_name}"))
  region_summary <-
    mrp_party_normal_poststrat(input$epred,
                               outcome = outcome_name,
                               subgroups = TRUE,
                               poststrat_tibble = .poststrat_tibble %>% group_by(region),
                               poststrat_epred = .poststrat_epred)

  region_summary$summary <-
    region_summary$summary %>%
    mutate(new_label = glue::glue("**{nice_num(mean, 1, FALSE)}** [{nice_num(lower, 2, FALSE)}; {nice_num(upper, 2, FALSE)}]"),
           grouping_type = "Region") %>%
    relocate(outcome, grouping_type, new_label)

  output <-
    list("population" = population_summary,
         "education_collapse" = education_collapse_summary,
         "age_fine" = age_fine_summary,
         "race" = race_summary,
         "income_ces" = income_ces_summary,
         "partyid" = partyid_summary,
         "male" = male_summary,
         "region" = region_summary,
         "state" = state_summary)

  if(save_output == TRUE) {
    saveRDS(output,
            glue::glue("mrp_poststrats/{variable_name}_poststrat.rds"))
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
