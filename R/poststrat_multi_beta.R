#' Poststratify Beta MRP Predictions
#'
#' Perform poststratification on Beta regression model predictions across population and specified subgroups.
#'
#' @param input The object generated from a Beta MRP model, containing the posterior predictions and precision parameter (\code{phi}).
#' @param variable_name String. The name of the variable being poststratified.
#' @param outcome_name Optional. A human-readable name for the outcome. Defaults to \code{variable_name}.
#' @param subgroups A character vector of subgroup variables for stratification. Defaults to:
#'   \itemize{
#'     \item \code{"education_collapse"}
#'     \item \code{"race"}
#'     \item \code{"income_ces"}
#'     \item \code{"male"}
#'     \item \code{"partyid"}
#'     \item \code{"age_fine"}
#'     \item \code{"region"}
#'   }
#' @param subgroup_mapping A named list mapping subgroup variable names to human-readable labels. Defaults to:
#'   \itemize{
#'     \item \code{"education_collapse"} = \code{"Education"}
#'     \item \code{"race"} = \code{"Race"}
#'     \item \code{"income_ces"} = \code{"Income"}
#'     \item \code{"male"} = \code{"Sex"}
#'     \item \code{"partyid"} = \code{"Party affiliation"}
#'     \item \code{"age_fine"} = \code{"Age"}
#'     \item \code{"region"} = \code{"Region"}
#'   }
#' @param save_output Logical. Whether to save the poststratification summaries as an RDS file. Defaults to \code{set_save_output}.
#' @param .nsim Numeric. The number of 'people' from the population to be simulated when generating the mean, median, and mode of the distribution. Defaults to \code{100000}.
#' @param return_state Logical. Whether to include the "state" subgroup in the output. Defaults to \code{set_state}.
#' @param .poststrat_tibble A data frame containing the poststratification tibble. Defaults to \code{set_my_poststrat}.
#' @param .poststrat_epred A data frame containing the poststratification posterior predictions. Defaults to \code{set_my_epred}.
#'
#' @return A list containing poststratification summaries:
#'   \itemize{
#'     \item \code{"population"}: Population-level summary.
#'     \item Subgroup summaries for each variable in \code{subgroups}.
#'   }
#' @export
poststrat_multi_beta <- function(input,
                                      variable_name,
                                      outcome_name = NULL,
                                      subgroups = c("education_collapse", "race", "income_ces",
                                                    "male", "partyid", "age_fine", "region"),
                                      subgroup_mapping = list(
                                        education_collapse = "Education",
                                        race = "Race",
                                        income_ces = "Income",
                                        male = "Sex",
                                        partyid = "Party affiliation",
                                        age_fine = "Age",
                                        region = "Region"
                                      ),
                                      save_output = set_save_output,
                                      .nsim = 100000,
                                      return_state = set_state,
                                      .poststrat_tibble = set_my_poststrat,
                                      .poststrat_epred = set_my_epred) {

  # Set outcome_name to variable_name if not provided
  if (is.null(outcome_name)) {
    outcome_name <- variable_name
  }

  # Helper function to process summaries
  process_summary <- function(summary_df, grouping_type) {
    summary_df %>%
      dplyr::mutate(
        new_label = glue::glue("**{jimbilben::nice_num(mean * 100, 0, FALSE)}%** [{jimbilben::nice_num(lower * 100, 1, FALSE)}%; {jimbilben::nice_num(upper * 100, 1, FALSE)}%]")
      ) %>%
      dplyr::relocate(outcome, grouping_type, new_label)
  }

  # Helper function to perform poststratification and process summaries for a subgroup
  process_subgroup <- function(subgroup) {
    print(glue::glue("Stratifying by {subgroup} for {outcome_name}"))

    # Perform poststratification for the subgroup
    subgroup_summary <- jimbilben::mrp_party_beta_poststrat(
      input$epred,
      current_model_phi = input$phi,
      outcome = outcome_name,
      nsim = .nsim,
      subgroups = TRUE,
      poststrat_tibble = .poststrat_tibble %>% dplyr::group_by(.data[[subgroup]]),
      poststrat_epred = .poststrat_epred
    )

    # Determine the grouping type using the mapping
    grouping_type <- subgroup_mapping[[subgroup]]

    # Process the summary dataframe
    subgroup_summary$summary <- process_summary(
      summary_df = subgroup_summary$summary,
      grouping_type = grouping_type
    )

    # Arrange the summary if necessary (optional, based on subgroup)
    if (subgroup %in% c("education_collapse", "income_ces")) {
      subgroup_summary$summary <- subgroup_summary$summary %>% arrange(.data[[subgroup]])
    }

    return(subgroup_summary)
  }

  # Initialize a list to store summaries for all subgroups
  summary_list <- list()

  # ----------------------------
  # 1. Population Level Summary
  # ----------------------------
  print(glue::glue("Getting population level for {outcome_name}"))

  population_summary <- jimbilben::mrp_party_beta_poststrat(
    input$epred,
    current_model_phi = input$phi,
    outcome = outcome_name,
    nsim = .nsim,
    poststrat_tibble = .poststrat_tibble,
    poststrat_epred = .poststrat_epred
  )

  # Process population summary
  population_summary$summary <- process_summary(
    summary_df = population_summary$summary,
    grouping_type = "Population"
  )

  # Add to summary list
  summary_list[["population"]] <- population_summary

  # ----------------------------
  # 2. Process Each Subgroup
  # ----------------------------
  for (subgroup in subgroups) {
    # Skip 'state' if return_state is FALSE and subgroup is 'state'
    if (!return_state && subgroup == "state") {
      next
    }

    # Process the subgroup and add to the summary list
    summary_list[[subgroup]] <- process_subgroup(subgroup)
  }

  # ----------------------------
  # 3. Compile and Save Output
  # ----------------------------

  # Optionally save the output to an RDS file
  if (save_output == TRUE) {
    file_name <- glue::glue("mrp_poststrats/{variable_name}_poststrat.rds")
    saveRDS(summary_list, file = file_name)
  }

  # Return the summary list, excluding 'state' if return_state is FALSE
  if (return_state == FALSE & "state" %in% subgroups) {
    summary_list[["state"]] <- NULL
  }

  return(summary_list)
}
