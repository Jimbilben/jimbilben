#' MRP Party Categorical Poststrat
#'
#' Post-stratify for categorical models.
#'
#' @param current_model_epred posterior prediction for the regression model of interest
#' @param subgroups Indicating whether this is a population-level or subgroup prediction
#' @param outcome Give a name to the outcome being computed (e.g., Support)
#' @param interval The summary interval level
#' @param poststrat_tibble This is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it
#' @param poststrat_epred This is the epred for our party poststrat model containing numbers of people expected to fall into each row of the poststrat tibble

#'
#' @export
mrp_party_categorical_poststrat <- function(current_model_epred, # posterior prediction for the regression model of interest
                                            subgroups = FALSE, # just indicating this is a population-level prediction (FALSE) or a subgrouped version (TRUE)
                                            outcome = "Vaccine support", # give a name to the outcome/response being assessed, e.g., Support for Vaccines
                                            interval = .95, # the summary interval level
                                            poststrat_tibble = acs5_2020_poststrat_with_partyid, # this is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it!
                                            poststrat_epred = acs5_2020_model_expected_n, # this is the epred for our party poststrat model containing proportions expected to fall into each party
                                            .point_est = "median",
                                            .decimals = 1,
                                            .remove_lead = FALSE) {
  if(subgroups == FALSE) {

    # a function that will get the posterior for the probability of ONE of the categorical responses
    posterior_tibble_maker <- function(which_categorical_choice,
                                       name_categorical_choice) {

      single_categorical_choice <-
        tibble(choice = name_categorical_choice,
               proportion = colSums(t(current_model_epred[ , , which_categorical_choice]) * poststrat_epred) / colSums(poststrat_epred),
               draw = 1:1000)

      return(single_categorical_choice)

    }

    # now map that function over EACH OF THE categorical CHOICES
    # the number of categorical choices is the final of the 3 dimensions of the current model epred
    categorical_posterior <-
      map2_df(.x = 1:dim(current_model_epred)[3], # gets the number of separate choice
              .y = names(current_model_epred[1, 1, ]), # gets the names of those choices (e.g., "Ferrari", "Jeep")
              .f = posterior_tibble_maker) %>%
      mutate(outcome = outcome,
             grouping_type = "Population")

    # this summary gives some useful/usable default options, but we also return the full posterior as one might wish to summarise it many ways
    categorical_summary <-
      categorical_posterior %>%
      group_by(choice) %>%
      jimbilben::nice_post(proportion,
                           interval = interval,
                           decimals = .decimals,
                           percentage = TRUE,
                           point_est = .point_est,
                           remove_lead = .remove_lead) %>%
      mutate(outcome = outcome,
             grouping_type = "Population")

  }

  if(subgroups == TRUE) {

    # get the subgroups selected
    which_subgroups <-
      group_vars(poststrat_tibble)

    # split the poststrat tibble according to groupings
    poststrat_tibble_split <-
      poststrat_tibble %>%
      group_split()

    subgroup_posterior_tibble_maker <- function(data,
                                                which_categorical_choice,
                                                name_categorical_choice,
                                                .which_subgroups = which_subgroups) {

      single_categorical_choice <-
        data %>%
        dplyr::select(all_of(.which_subgroups)) %>%
        slice_head()

      single_categorical_choice <-
        sample_n(single_categorical_choice,
                 1000,
                 replace = TRUE) %>%
        mutate(choice = name_categorical_choice,
               proportion = colSums(t(current_model_epred[ , data$case, which_categorical_choice]) * poststrat_epred[data$case, ]) / colSums(poststrat_epred[data$case, ]),
               draw = 1:1000,
               outcome = outcome,
               grouping_type = "Subgrouped")

      return(single_categorical_choice)

    }

    categorical_posterior <- pmap_dfr(list(data = rep(poststrat_tibble_split, dim(current_model_epred)[3]),
                                           which_categorical_choice = rep(1:dim(current_model_epred)[3], each = length(poststrat_tibble_split)),
                                           name_categorical_choice = rep(names(current_model_epred[1, 1, ]), each = length(poststrat_tibble_split))),
                                      .f = subgroup_posterior_tibble_maker)

    which_subgroups <- c("choice", which_subgroups)

    categorical_summary <-
      categorical_posterior %>%
      group_by(across(all_of(which_subgroups))) %>%
      jimbilben::nice_post(proportion,
                           interval = interval,
                           decimals = .decimals,
                           percentage = TRUE,
                           point_est = .point_est,
                           remove_lead = .remove_lead) %>%
      mutate(outcome = outcome,
             grouping_type = "Subgrouped")

    #### Now we just sort out the factor levels ####
    if("partyid" %in% which_subgroups) {
      categorical_posterior <- jimbilben::factor_partyid(categorical_posterior)
      categorical_summary <- jimbilben::factor_partyid(categorical_summary)
    }

    if("male" %in% which_subgroups) {
      categorical_posterior <- jimbilben::factor_male(categorical_posterior)
      categorical_summary <- jimbilben::factor_male(categorical_summary)
    }

    if("income_ces" %in% which_subgroups) {
      categorical_posterior <- jimbilben::factor_income(categorical_posterior)
      categorical_summary <- jimbilben::factor_income(categorical_summary)
    }

    if("education" %in% which_subgroups) {
      categorical_posterior <- jimbilben::factor_education(categorical_posterior)
      categorical_summary <- jimbilben::factor_education(categorical_summary)
    }

    if("education_collapse" %in% which_subgroups) {
      categorical_posterior <- jimbilben::factor_education_collapse(categorical_posterior)
      categorical_summary <- jimbilben::factor_education_collapse(categorical_summary)
    }

    if("race" %in% which_subgroups) {
      categorical_posterior <- jimbilben::factor_race(categorical_posterior)
      categorical_summary <- jimbilben::factor_race(categorical_summary)
    }

    if("age" %in% which_subgroups) {
      categorical_posterior <- jimbilben::factor_age(categorical_posterior)
      categorical_summary <- jimbilben::factor_age(categorical_summary)
    }
    if("age_fine" %in% which_subgroups) {
      categorical_posterior <- jimbilben::factor_age_fine(categorical_posterior)
      categorical_summary <- jimbilben::factor_age_fine(categorical_summary)
    }
    if("region" %in% which_subgroups) {
      reference_choice <-
        categorical_summary %>%
        group_by(choice) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference_choice <- reference_choice$choice[nrow(reference_choice)]

      reference <-
        categorical_summary %>%
        group_by(choice, region) %>%
        filter(choice == reference_choice) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$region

      categorical_posterior <-
        categorical_posterior %>%
        mutate(region = factor(region,
                               levels = reference))
      categorical_summary <-
        categorical_summary %>%
        mutate(region = factor(region,
                               levels = reference))

    }

    if("division" %in% which_subgroups) {
      reference_choice <-
        categorical_summary %>%
        group_by(choice) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference_choice <- reference_choice$choice[nrow(reference_choice)]

      reference <-
        categorical_summary %>%
        group_by(choice, division) %>%
        filter(choice == reference_choice) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$division

      categorical_posterior <-
        categorical_posterior %>%
        mutate(division = factor(division,
                                 levels = reference))
      categorical_summary <-
        categorical_summary %>%
        mutate(division = factor(division,
                                 levels = reference))

    }

    if("state" %in% which_subgroups) {
      reference_choice <-
        categorical_summary %>%
        group_by(choice) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference_choice <- reference_choice$choice[nrow(reference_choice)]

      reference <-
        categorical_summary %>%
        group_by(choice, state) %>%
        filter(choice == reference_choice) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$state

      categorical_posterior <-
        categorical_posterior %>%
        mutate(state = factor(state,
                              levels = reference))
      categorical_summary <-
        categorical_summary %>%
        mutate(state = factor(state,
                              levels = reference))
    }

  }


  return(list("posterior" = categorical_posterior,
              "summary" = categorical_summary))

}
