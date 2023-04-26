#' MRP Party Bernoulli Poststrat
#'
#' Post-stratify for Bernoulli/binary models.
#'
#' @param current_model_epred posterior prediction for the regression model of interest
#' @param subgroups Indicating whether this is a population-level or subgroup prediction
#' @param outcome Give a name to the outcome being computed (e.g., Support)
#' @param interval The summary interval level
#' @param poststrat_tibble This is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it
#' @param poststrat_epred This is the epred for our party poststrat model containing numbers of people expected to fall into each row of the poststrat tibble

#'
#' @export
mrp_party_bernoulli_poststrat <- function(current_model_epred, # posterior prediction for the regression model of interest
                                          subgroups = FALSE, # just indicating this is a population-level prediction
                                          outcome = "yes_no", # give a name to the outcome being computed (e.g., likelihood of answering yes)
                                          interval = .95, # the summary interval level
                                          poststrat_tibble = acs5_2020_poststrat_with_partyid, # this is the tibble - usually an ACS tibble - containing the demographic variable names
                                          poststrat_epred = acs5_2020_model_expected_n, # this is the epred for our party poststrat model containing numbers of people expected to fall into each row of the poststrat tibble
                                          .point_est = "median",
                                          .decimals = 1,
                                          .remove_lead = FALSE) {

  if(subgroups == FALSE) {

    bernoulli_posterior <-
      tibble(outcome = outcome,
             proportion = colSums(t(current_model_epred) * poststrat_epred) / colSums(poststrat_epred), # the t transforms the current_model matrix so it can be multiplied with our poststrat matrix
             draw = 1:1000) %>%
      mutate(outcome = outcome,
             grouping_type = "Population")

    # this summary gives some useful/usable default options, but we also return the full posterior as one might wish to summarise it many ways
    bernoulli_summary <-
      bernoulli_posterior %>%
      jimbilben::nice_post(proportion,
                           interval = interval,
                           decimals = .decimals,
                           remove_lead = .remove_lead,
                           percentage = TRUE,
                           point_est = .point_est) %>%
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
                                                .which_subgroups = which_subgroups) {

      bernoulli_data <-
        data %>%
        dplyr::select(all_of(.which_subgroups)) %>%
        slice_head()

      bernoulli_data <-
        sample_n(bernoulli_data,
                 1000,
                 replace = TRUE) %>%
        tibble(outcome = outcome,
               proportion = colSums(t(current_model_epred[ , data$case]) * poststrat_epred[data$case , ]) / colSums(poststrat_epred[data$case, ]), # the t transforms the current_model matrix so it can be multiplied with our poststrat matrix
               draw = 1:1000) %>%
        mutate(outcome = outcome,
               grouping_type = "Subgrouped")

      return(bernoulli_data)

    }

    bernoulli_posterior <-
      map_dfr(.x = poststrat_tibble_split,
              .f = subgroup_posterior_tibble_maker)

    bernoulli_summary <-
      bernoulli_posterior %>%
      group_by(across(all_of(which_subgroups))) %>%
      jimbilben::nice_post(proportion,
                           interval = interval,
                           decimals = .decimals,
                           remove_lead = .remove_lead,
                           percentage = TRUE,
                           point_est = .point_est) %>%
      mutate(outcome = outcome,
             grouping_type = "Subgrouped")

    #### Now we just sort out the factor levels ####
    if("partyid" %in% which_subgroups) {
      bernoulli_posterior <- jimbilben::factor_partyid(bernoulli_posterior)
      bernoulli_summary <- jimbilben::factor_partyid(bernoulli_summary)
    }

    if("male" %in% which_subgroups) {
      bernoulli_posterior <- jimbilben::factor_male(bernoulli_posterior)
      bernoulli_summary <- jimbilben::factor_male(bernoulli_summary)
    }

    if("income_ces" %in% which_subgroups) {
      bernoulli_posterior <- jimbilben::factor_income(bernoulli_posterior)
      bernoulli_summary <- jimbilben::factor_income(bernoulli_summary)
    }

    if("education" %in% which_subgroups) {
      bernoulli_posterior <- jimbilben::factor_education(bernoulli_posterior)
      bernoulli_summary <- jimbilben::factor_education(bernoulli_summary)
    }

    if("education_collapse" %in% which_subgroups) {
      bernoulli_posterior <- jimbilben::factor_education_collapse(bernoulli_posterior)
      bernoulli_summary <- jimbilben::factor_education_collapse(bernoulli_summary)
    }

    if("race" %in% which_subgroups) {
      bernoulli_posterior <- jimbilben::factor_race(bernoulli_posterior)
      bernoulli_summary <- jimbilben::factor_race(bernoulli_summary)
    }

    if("age" %in% which_subgroups) {
      bernoulli_posterior <- jimbilben::factor_age(bernoulli_posterior)
      bernoulli_summary <- jimbilben::factor_age(bernoulli_summary)
    }

    if("age_fine" %in% which_subgroups) {
      psup_posterior <- jimbilben::factor_age_fine(bernoulli_posterior)
      psup_summary <- jimbilben::factor_age_fine(bernoulli_posterior)
    }

    if("region" %in% which_subgroups) {
      reference <-
        psup_summary %>%
        group_by(region) %>%
        dplyr::summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$region

      psup_posterior <-
        psup_posterior %>%
        mutate(region = factor(region,
                               levels = reference))
      psup_summary <-
        psup_summary %>%
        mutate(region = factor(region,
                               levels = reference))

    }

    if("division" %in% which_subgroups) {
      reference <-
        psup_summary %>%
        group_by(division) %>%
        dplyr::summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$division

      psup_posterior <-
        psup_posterior %>%
        mutate(division = factor(division,
                                 levels = reference))
      psup_summary <-
        psup_summary %>%
        mutate(division = factor(division,
                                 levels = reference))

    }

    if("state" %in% which_subgroups) {
      reference <-
        bernoulli_summary %>%
        group_by(state) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$state

      bernoulli_posterior <-
        bernoulli_posterior %>%
        mutate(state = factor(state,
                              levels = reference))
      bernoulli_summary <-
        bernoulli_summary %>%
        mutate(state = factor(state,
                              levels = reference))

    }

  }

  return(list("posterior" = bernoulli_posterior, "summary" = bernoulli_summary))

}
