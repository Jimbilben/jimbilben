#' MRP Normal Poststrat
#'
#' Post-stratify for normally distributed/gaussian models.
#'
#' @param current_model_epred posterior prediction for the regression model of interest
#' @param subgroups Indicating whether this is a population-level or subgroup prediction
#' @param outcome Give a name to the outcome being computed (e.g., Support)
#' @param interval The summary interval level
#' @param poststrat_tibble This is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it

#'
#' @export
mrp_normal_poststrat <- function(current_model_epred, # posterior prediction for the regression model of interest
                                       subgroups = FALSE, # just indicating this is a population-level prediction
                                       outcome = "vaccines", # give a name to the outcome being computed (e.g., support for vaccines)
                                       interval = .95, # the summary interval level
                                       poststrat_tibble = acs5_2020_poststrat_with_partyid, # this is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it!
                                       .decimals = 2,
                                       .point_est = "mean") { # this is the epred for our party poststrat model containing numbers of people expected to fall into each row of the poststrat tibble

  if(subgroups == FALSE) {

    normal_posterior <-
      tibble(outcome = outcome,
             mean = colSums(t(current_model_epred) * poststrat_tibble$n) / sum(poststrat_tibble$n), # the t transforms the current_model matrix so it can be multiplied with our poststrat matrix
             draw = 1:1000) %>%
      mutate(outcome = outcome,
             grouping_type = "Population")

    # this summary gives some useful/usable default options, but we also return the full posterior as one might wish to summarise it many ways
    normal_summary <-
      normal_posterior %>%
      jimbilben::nice_post(mean,
                           interval = interval,
                           decimals = .decimals,
                           point_est = .point_est,
                           remove_lead = FALSE,
                           percentage = FALSE) %>%
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

      normal_data <-
        data %>%
        dplyr::select(all_of(.which_subgroups)) %>%
        slice_head()

      normal_data <-
        sample_n(normal_data,
                 1000,
                 replace = TRUE) %>%
        tibble(outcome = outcome,
               mean = colSums(t(current_model_epred[ , data$case]) * poststrat_tibble[data$case , ]$n) / sum(poststrat_tibble[data$case, ]$n), # the t transforms the current_model matrix so it can be multiplied with our poststrat matrix
               draw = 1:1000) %>%
        mutate(outcome = outcome,
               grouping_type = "Subgrouped")

      return(normal_data)

    }

    normal_posterior <-
      map_dfr(.x = poststrat_tibble_split,
              .f = subgroup_posterior_tibble_maker)

    normal_summary <-
      normal_posterior %>%
      group_by(across(all_of(which_subgroups))) %>%
      jimbilben::nice_post(mean,
                           interval = interval,
                           decimals = .decimals,
                           point_est = .point_est,
                           remove_lead = FALSE,
                           percentage = FALSE) %>%
      mutate(outcome = outcome,
             grouping_type = "Subgrouped")

    #### Now we just sort out the factor levels ####
    if("partyid" %in% which_subgroups) {
      normal_posterior <- jimbilben::factor_partyid(normal_posterior)
      normal_summary <- jimbilben::factor_partyid(normal_summary)
    }

    if("male" %in% which_subgroups) {
      normal_posterior <- jimbilben::factor_male(normal_posterior)
      normal_summary <- jimbilben::factor_male(normal_summary)
    }

    if("income_ces" %in% which_subgroups) {
      normal_posterior <- jimbilben::factor_income(normal_posterior)
      normal_summary <- jimbilben::factor_income(normal_summary)
    }

    if("education" %in% which_subgroups) {
      normal_posterior <- jimbilben::factor_education(normal_posterior)
      normal_summary <- jimbilben::factor_education(normal_summary)
    }

    if("education_collapse" %in% which_subgroups) {
      normal_posterior <- jimbilben::factor_education_collapse(normal_posterior)
      normal_summary <- jimbilben::factor_education_collapse(normal_summary)
    }

    if("race" %in% which_subgroups) {
      normal_posterior <- jimbilben::factor_race(normal_posterior)
      normal_summary <- jimbilben::factor_race(normal_summary)
    }

    if("age" %in% which_subgroups) {
      normal_posterior <- jimbilben::factor_age(normal_posterior)
      normal_summary <- jimbilben::factor_age(normal_summary)
    }

    if("age_fine" %in% which_subgroups) {
      normal_posterior <- jimbilben::factor_age_fine(normal_posterior)
      normal_summary <- jimbilben::factor_age_fine(normal_summary)
    }

    if("region" %in% which_subgroups) {
      reference <-
        normal_summary %>%
        group_by(region) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$region

      normal_posterior <-
        normal_posterior %>%
        mutate(region = factor(region,
                               levels = reference))
      normal_summary <-
        normal_summary %>%
        mutate(region = factor(region,
                               levels = reference))

    }

    if("division" %in% which_subgroups) {
      reference <-
        normal_summary %>%
        group_by(division) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$division

      normal_posterior <-
        normal_posterior %>%
        mutate(division = factor(division,
                                 levels = reference))
      normal_summary <-
        normal_summary %>%
        mutate(division = factor(division,
                                 levels = reference))

    }

    if("state" %in% which_subgroups) {
      reference <-
        normal_summary %>%
        group_by(state) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$state

      normal_posterior <-
        normal_posterior %>%
        mutate(state = factor(state,
                              levels = reference))
      normal_summary <-
        normal_summary %>%
        mutate(state = factor(state,
                              levels = reference))

    }

  }

  return(list("posterior" = normal_posterior, "summary" = normal_summary))

}
