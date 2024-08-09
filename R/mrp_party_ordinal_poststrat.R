#' MRP Party Ordinal Poststrat
#'
#' Post-stratify for ordinal models.
#'
#' @param current_model_epred posterior prediction for the regression model of interest
#' @param subgroups Indicating whether this is a population-level or subgroup prediction
#' @param outcome Give a name to the outcome being computed (e.g., Support)
#' @param interval The summary interval level
#' @param poststrat_tibble This is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it
#' @param poststrat_epred This is the epred for our party poststrat model containing numbers of people expected to fall into each row of the poststrat tibble

#'
#' @export
mrp_party_ordinal_poststrat <- function(current_model_epred, # posterior prediction for the regression model of interest
                                        subgroups = FALSE, # just indicating this is a population-level prediction (FALSE) or a subgrouped version (TRUE)
                                        outcome = "Vaccine support", # give a name to the outcome/response being assessed, e.g., Support for Vaccines
                                        interval = .95, # the summary interval level
                                        poststrat_tibble = acs5_2020_poststrat_with_partyid, # this is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it!
                                        poststrat_epred = acs5_2020_model_expected_n, # this is the epred for our party poststrat model containing proportions expected to fall into each party
                                        .point_est = "mean",
                                        .decimals = 1,
                                        .remove_lead = FALSE) {

  if(subgroups == FALSE) {

    # a function that will get the posterior for the probability of ONE of the ordinal responses
    posterior_tibble_maker <- function(which_ordinal_rating) {

      single_ordinal_rating <-
        tibble(rating = which_ordinal_rating,
               proportion = colSums(t(current_model_epred[ , , which_ordinal_rating]) * poststrat_epred) / colSums(poststrat_epred),
               draw = 1:1000)

      return(single_ordinal_rating)

    }

    # now map that function over EACH OF THE ORDINAL RESPONSES
    # the number of ordinal responses is the final of the 3 dimensions of the current model epred
    ordinal_posterior <-
      map_df(.x = 1:dim(current_model_epred)[3],
             .f = posterior_tibble_maker) %>%
      mutate(outcome = outcome,
             grouping_type = "Population")

    # this summary gives some useful/usable default options, but we also return the full posterior as one might wish to summarise it many ways
    ordinal_summary <-
      ordinal_posterior %>%
      group_by(rating) %>%
      jimbilben::nice_post(proportion,
                           interval = interval,
                           decimals = .decimals,
                           point_est = .point_est,
                           remove_lead = .remove_lead,
                           percentage = TRUE) %>%
      mutate(outcome = outcome,
             grouping_type = "Population")

    ordinal_mean_posterior <-
      ordinal_posterior %>%
      group_by(draw) %>%
      summarise(average = sum(proportion * rating)) %>%
      mutate(outcome = outcome,
             grouping_type = "Population")

    ordinal_mean_summary <-
      ordinal_mean_posterior %>%
      jimbilben::nice_post(average,
                           interval = interval,
                           point_est = .point_est,
                           remove_lead = FALSE) %>%
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
                                                which_ordinal_rating,
                                                .which_subgroups = which_subgroups) {

      single_ordinal_rating <-
        data %>%
        dplyr::select(all_of(.which_subgroups)) %>%
        slice_head()

      single_ordinal_rating <-
        sample_n(single_ordinal_rating,
                 1000,
                 replace = TRUE) %>%
        mutate(rating = which_ordinal_rating,
               proportion = colSums(t(current_model_epred[ , data$case, which_ordinal_rating]) * poststrat_epred[data$case, ]) / colSums(poststrat_epred[data$case, ]),
               draw = 1:1000,
               outcome = outcome,
               grouping_type = "Subgrouped")

      return(single_ordinal_rating)

    }

    ordinal_posterior <- pmap_dfr(list(data = rep(poststrat_tibble_split, dim(current_model_epred)[3]),
                                       which_ordinal_rating = rep(1:dim(current_model_epred)[3], each = length(poststrat_tibble_split))),
                                  .f = subgroup_posterior_tibble_maker)

    mean_which_subgroups <- c("draw", which_subgroups)

    ordinal_mean_posterior <-
      ordinal_posterior %>%
      group_by(across(all_of(mean_which_subgroups))) %>%
      summarise(average = sum(proportion * rating)) %>%
      mutate(outcome = outcome,
             grouping_type = "Subgrouped")

    ordinal_mean_summary <-
      ordinal_mean_posterior %>%
      group_by(across(all_of(which_subgroups))) %>%
      jimbilben::nice_post(average,
                           interval = interval,
                           point_est = .point_est,
                           remove_lead = FALSE) %>%
      mutate(outcome = outcome,
             grouping_type = "Subgrouped")

    which_subgroups <- c("rating", which_subgroups)

    ordinal_summary <-
      ordinal_posterior %>%
      group_by(across(all_of(which_subgroups))) %>%
      jimbilben::nice_post(proportion,
                           interval = interval,
                           decimals = .decimals,
                           point_est = .point_est,
                           remove_lead = .remove_lead,
                           percentage = TRUE) %>%
      mutate(outcome = outcome,
             grouping_type = "Subgrouped")

    #### Now we just sort out the factor levels ####
    if("partyid" %in% which_subgroups) {
      ordinal_posterior <- jimbilben::factor_partyid(ordinal_posterior)
      ordinal_summary <- jimbilben::factor_partyid(ordinal_summary)
      ordinal_mean_posterior <- jimbilben::factor_partyid(ordinal_mean_posterior)
      ordinal_mean_summary <- jimbilben::factor_partyid(ordinal_mean_summary)
    }

    if("male" %in% which_subgroups) {
      ordinal_posterior <- jimbilben::factor_male(ordinal_posterior)
      ordinal_summary <- jimbilben::factor_male(ordinal_summary)
      ordinal_mean_posterior <- jimbilben::factor_male(ordinal_mean_posterior)
      ordinal_mean_summary <- jimbilben::factor_male(ordinal_mean_summary)
    }

    if("income_ces" %in% which_subgroups) {
      ordinal_posterior <- jimbilben::factor_income(ordinal_posterior)
      ordinal_summary <- jimbilben::factor_income(ordinal_summary)
      ordinal_mean_posterior <- jimbilben::factor_income(ordinal_mean_posterior)
      ordinal_mean_summary <- jimbilben::factor_income(ordinal_mean_summary)
    }

    if("education" %in% which_subgroups) {
      ordinal_posterior <- jimbilben::factor_education(ordinal_posterior)
      ordinal_summary <- jimbilben::factor_education(ordinal_summary)
      ordinal_mean_posterior <- jimbilben::factor_education(ordinal_mean_posterior)
      ordinal_mean_summary <- jimbilben::factor_education(ordinal_mean_summary)
    }

    if("education_collapse" %in% which_subgroups) {
      ordinal_posterior <- jimbilben::factor_education_collapse(ordinal_posterior)
      ordinal_summary <- jimbilben::factor_education_collapse(ordinal_summary)
      ordinal_mean_posterior <- jimbilben::factor_education_collapse(ordinal_mean_posterior)
      ordinal_mean_summary <- jimbilben::factor_education_collapse(ordinal_mean_summary)
    }

    if("race" %in% which_subgroups) {
      ordinal_posterior <- jimbilben::factor_race(ordinal_posterior)
      ordinal_summary <- jimbilben::factor_race(ordinal_summary)
      ordinal_mean_posterior <- jimbilben::factor_race(ordinal_mean_posterior)
      ordinal_mean_summary <- jimbilben::factor_race(ordinal_mean_summary)
    }

    if("age" %in% which_subgroups) {
      ordinal_posterior <- jimbilben::factor_age(ordinal_posterior)
      ordinal_summary <- jimbilben::factor_age(ordinal_summary)
      ordinal_mean_posterior <- jimbilben::factor_age(ordinal_mean_posterior)
      ordinal_mean_summary <- jimbilben::factor_age(ordinal_mean_summary)
    }

    if("age_fine" %in% which_subgroups) {
      ordinal_posterior <- jimbilben::factor_age_fine(ordinal_posterior)
      ordinal_summary <- jimbilben::factor_age_fine(ordinal_summary)
      ordinal_mean_posterior <- jimbilben::factor_age_fine(ordinal_mean_posterior)
      ordinal_mean_summary <- jimbilben::factor_age_fine(ordinal_mean_summary)
    }

    if("region" %in% which_subgroups) {
      reference <-
        ordinal_mean_summary %>%
        group_by(region) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$region

      ordinal_posterior <-
        ordinal_posterior %>%
        mutate(region = factor(region,
                               levels = reference))
      ordinal_summary <-
        ordinal_summary %>%
        mutate(region = factor(region,
                               levels = reference))

      ordinal_mean_posterior <-
        ordinal_mean_posterior %>%
        mutate(region = factor(region,
                               levels = reference))

      ordinal_mean_summary <-
        ordinal_mean_summary %>%
        mutate(region = factor(region,
                               levels = reference))
    }

    if("division" %in% which_subgroups) {
      reference <-
        ordinal_mean_summary %>%
        group_by(division) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$division

      ordinal_posterior <-
        ordinal_posterior %>%
        mutate(division = factor(division,
                                 levels = reference))
      ordinal_summary <-
        ordinal_summary %>%
        mutate(division = factor(division,
                                 levels = reference))

      ordinal_mean_posterior <-
        ordinal_mean_posterior %>%
        mutate(division = factor(division,
                                 levels = reference))

      ordinal_mean_summary <-
        ordinal_mean_summary %>%
        mutate(division = factor(division,
                                 levels = reference))
    }

    if("state" %in% which_subgroups) {
      reference <-
        ordinal_mean_summary %>%
        group_by(state) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$state

      ordinal_posterior <-
        ordinal_posterior %>%
        mutate(state = factor(state,
                              levels = reference))
      ordinal_summary <-
        ordinal_summary %>%
        mutate(state = factor(state,
                              levels = reference))

      ordinal_mean_posterior <-
        ordinal_mean_posterior %>%
        mutate(state = factor(state,
                              levels = reference))

      ordinal_mean_summary <-
        ordinal_mean_summary %>%
        mutate(state = factor(state,
                              levels = reference))
    }

  }


  return(list("posterior" = ordinal_posterior,
              "summary" = ordinal_summary,
              "posterior_mean" = ordinal_mean_posterior,
              "summary_mean" = ordinal_mean_summary))

}
