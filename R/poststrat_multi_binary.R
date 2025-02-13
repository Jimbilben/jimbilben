#' Poststratify Binary MRP Predictions
#'
#' Perform poststratification on the posterior predictive draws from a binary MRP model across specified subgroups.
#'
#' @param input The object generated from \code{binary_mrp}, containing the model and posterior predictions.
#' @param variable_name String. The name of the binary variable being poststratified.
#' @param outcome_name Optional. A human-readable name for the outcome. Defaults to \code{variable_name}.
#' @param subgroups A character vector of subgroup variables to stratify by. Defaults to:
#'   \itemize{
#'     \item \code{"education_collapse"}
#'     \item \code{"race"}
#'     \item \code{"income_ces"}
#'     \item \code{"male"}
#'     \item \code{"partyid"}
#'     \item \code{"age_fine"}
#'     \item \code{"state"}
#'     \item \code{"region"}
#'   }
#' @param weighted Logical. Whether the poststratification weights should be applied. Defaults to FALSE.
#' @param save_output Logical. If \code{TRUE}, saves the poststratification summaries to a file. Defaults to \code{set_save_output}.
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
#' This function takes the posterior epred draws from \code{binary_mrp} and performs poststratification
#' at both the population level and across specified subgroups. It applies the function
#' \code{mrp_party_bernoulli_poststrat} for stratification and formats the output with readable labels.
#'
#' Subgroups can be arranged in a logical order (e.g., for \code{"education_collapse"} or \code{"income_ces"}).
#' The output can optionally be saved as an RDS file for future use.
poststrat_multi_binary <- function(input, # the object generated from binary_mrp
                                   variable_name,
                                   outcome_name = NULL,
                                   subgroups = c("education_collapse", "race", "income_ces",
                                                 "male", "partyid", "age_fine", "state", "region"),
                                   weighted = FALSE,
                                   save_output = save_my_poststrat,
                                   return_state = set_state,
                                   .poststrat_tibble = set_my_poststrat,
                                   .poststrat_epred = set_my_epred) {

  # Set outcome_name to variable_name if not provided
  if(is.null(outcome_name)) {
    outcome_name <- variable_name
  }

  # Initialize output list
  output <- list()

  # Function to process each subgroup
  process_subgroup <- function(subgroup) {
    print(glue::glue("stratifying by {subgroup} for {outcome_name}"))
    summary_result <- jimbilben::mrp_party_bernoulli_poststrat(
      input$epred,
      outcome = outcome_name,
      subgroups = TRUE,
      poststrat_tibble = .poststrat_tibble %>% group_by(.data[[subgroup]]),
      poststrat_epred = .poststrat_epred
    )

    summary_result$summary <- summary_result$summary %>%
      dplyr::mutate(
        new_mean = case_when(
          mean < 1 ~ "<1",
          TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)
        ),
        new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]")
      ) %>%
      dplyr::relocate(outcome, grouping_type, new_label)

    # dplyr::arrange specific subgroups if necessary
    if(subgroup %in% c("education_collapse", "income_ces")) {
      summary_result$summary <- summary_result$summary %>% dplyr::arrange(.data[[subgroup]])
    }

    return(summary_result)
  }

  # Process population-level summary
  print(glue::glue("getting population level for {outcome_name}"))
  population_summary <- mrp_party_bernoulli_poststrat(
    input$epred,
    outcome = outcome_name,
    poststrat_tibble = .poststrat_tibble,
    poststrat_epred = .poststrat_epred
  )

  population_summary$summary <- population_summary$summary %>%
    dplyr::mutate(
      new_mean = case_when(
        mean < 1 ~ "<1",
        TRUE ~ jimbilben::nice_num(mean, 0, remove_lead = FALSE)
      ),
      new_label = glue::glue("**{new_mean}%** [{nice_num(lower, 1, FALSE)}%; {nice_num(upper, 1, FALSE)}%]")
    ) %>%
    dplyr::relocate(outcome, grouping_type, new_label)

  # Add population summary to output
  output$population <- population_summary

  # Iterate over each subgroup and process
  for(subgroup in subgroups) {
    # Process subgroup
    summary <- process_subgroup(subgroup)

    # Add to output with subgroup name as key
    output[[subgroup]] <- summary
  }

  # Optional: Save output to file
  if(save_output) {
    file_suffix <- if(weighted) "_weighted_poststrat" else "_poststrat"
    saveRDS(output, file = glue::glue("mrp_poststrats/{variable_name}{file_suffix}.rds"))
  }

  # Prepare the output to return to the user
  if(!return_state) {
    # Create a copy of output without the 'state' subgroup
    output_to_return <- output
    output_to_return$state <- NULL
  } else {
    output_to_return <- output
  }

  return(output_to_return)

}
