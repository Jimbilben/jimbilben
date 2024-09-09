#' MRP Party Beta Poststrat
#'
#' Post-stratify for beta distributed models. Provides different parameter summaries for the distribution - the mean, the median, and the mode, as these can vary a lot with a beta distribution depending on the level of precision.
#'
#' @param current_model_epred posterior prediction for the regression model of interest - when making the prediction, specify a thinned epred with specific draws the same as for current_model_phi
#' @param current_model_phi posterior samples of the phi parameter for the model, from as_draws_df - specify a thinned epred with specific draws the same as for current_model_epred
#' @param subgroups Indicating whether this is a population-level or subgroup prediction
#' @param outcome Give a name to the outcome being computed (e.g., Support)
#' @param nsim How many individual responses to simulate for generating the estimated beta distribution. Defaults to 50000
#' @param interval The summary interval level
#' @param poststrat_tibble This is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it
#' @param poststrat_epred This is the epred for our party poststrat model containing numbers of people expected to fall into each row of the poststrat tibble
#' @param .decimals How many decimals for the label that is returned - defaults to 2
#' @param .phi_decimals How many decimals for the phi parameter that is returned - defaults to 2
#' @param .point_est What point estimate to use for the label summarising the posterior
#' @param .remove_lead Whether or not to remove leading zeroes in the label - defaults to FALSE
#' @param .percentage Whether to represent the beta proportions as a percentage rather than from 0-1 (defaults to FALSE)
#'
#' @export
mrp_party_beta_poststrat <- function(current_model_epred, # posterior prediction for the regression model of interest
                                     current_model_phi,
                                     subgroups = FALSE, # just indicating this is a population-level prediction
                                     outcome = "vaccines", # give a name to the outcome being computed (e.g., support for vaccines)
                                     interval = .95, # the summary interval level
                                     nsim = 50000,
                                     poststrat_tibble = acs5_2020_poststrat_with_partyid, # this is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it!
                                     poststrat_epred = acs5_2020_model_expected_n, # this is the epred for our party poststrat model containing numbers of people expected to fall into each row of the poststrat tibble
                                     .decimals = 2,
                                     .phi_decimals = 2,
                                     .point_est = "mean",
                                     .remove_lead = FALSE,
                                     .percentage = FALSE) {


  has_draw <-
    any(str_detect(names(current_model_phi), ".draw"))

  if(has_draw == TRUE) {
    phi_posterior <-
      current_model_phi %>%
      rename(og_draw = .draw) %>%
      mutate(draw = 1:1000,
             parameter = "phi",
             outcome = outcome)
  }
  else {
    phi_posterior <-
      current_model_phi %>%
      mutate(draw = 1:1000,
             parameter = "phi",
             outcome = outcome)
  }


  phi_summary <-
    phi_posterior %>%
    group_by(outcome) %>%
    nice_post(phi,
              interval = interval,
              decimals = .phi_decimals,
              point_est = .point_est) %>%
    mutate(parameter = "phi",
           outcome = outcome)

  beta_average_function <- function(beta_split_data) {

    simulated_beta_vals <- extraDistr::rprop(nsim, size = beta_split_data$phi, mean = beta_split_data$mean)
    beta_split_data$median <- median(simulated_beta_vals)
    beta_split_data$mode <- jimbilben::hdp(simulated_beta_vals)

    beta_split_data <-
      beta_split_data %>%
      pivot_longer(cols = c(mean, median, mode),
                   names_to = "parameter",
                   values_to = "estimate")

  }

  if(subgroups == FALSE) {

    beta_posterior <-
      tibble(outcome = outcome,
             mean = colSums(t(current_model_epred) * poststrat_epred) / colSums(poststrat_epred), # the t transforms the current_model matrix so it can be multiplied with our poststrat matrix
             draw = 1:1000) %>%
      mutate(outcome = outcome,
             phi = phi_posterior$phi)

    beta_posterior_split <-
      beta_posterior %>%
      group_by(draw) %>%
      group_split()

    beta_posterior <-
      map_df(.x = beta_posterior_split,
             .f = beta_average_function) %>%
      mutate(grouping_type = "Population")

    # this summary gives some useful/usable default options, but we also return the full posterior as one might wish to summarise it many ways
    beta_summary <-
      beta_posterior %>%
      group_by(parameter) %>%
      jimbilben::nice_post(estimate,
                           interval = interval,
                           decimals = .decimals,
                           point_est = .point_est,
                           remove_lead = .remove_lead,
                           percentage = .percentage) %>%
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

      beta_data <-
        data %>%
        dplyr::select(all_of(.which_subgroups)) %>%
        slice_head()

      beta_data <-
        sample_n(beta_data,
                 1000,
                 replace = TRUE) %>%
        tibble(outcome = outcome,
               mean = colSums(t(current_model_epred[ , data$case]) * poststrat_epred[data$case , ]) / colSums(poststrat_epred[data$case, ]), # the t transforms the current_model matrix so it can be multiplied with our poststrat matrix
               draw = 1:1000) %>%
        mutate(outcome = outcome,
               phi = phi_posterior$phi)

      beta_posterior_split <-
        beta_data %>%
        group_by(draw) %>%
        group_split()

      beta_data <-
        map_df(.x = beta_posterior_split,
               .f = beta_average_function) %>%
        mutate(grouping_type = "Subgrouped")

      return(beta_data)

    }

    beta_posterior <-
      map_dfr(.x = poststrat_tibble_split,
              .f = subgroup_posterior_tibble_maker)

    beta_summary <-
      beta_posterior %>%
      group_by(across(all_of(which_subgroups)), parameter) %>%
      jimbilben::nice_post(estimate,
                           interval = interval,
                           decimals = .decimals,
                           point_est = .point_est,
                           remove_lead = .remove_lead,
                           percentage = .percentage) %>%
      mutate(outcome = outcome,
             grouping_type = "Subgrouped")

    #### Now we just sort out the factor levels ####
    if("partyid" %in% which_subgroups) {
      beta_posterior <- jimbilben::factor_partyid(beta_posterior)
      beta_summary <- jimbilben::factor_partyid(beta_summary)
    }

    if("male" %in% which_subgroups) {
      beta_posterior <- jimbilben::factor_male(beta_posterior)
      beta_summary <- jimbilben::factor_male(beta_summary)
    }

    if("income_ces" %in% which_subgroups) {
      beta_posterior <- jimbilben::factor_income(beta_posterior)
      beta_summary <- jimbilben::factor_income(beta_summary)
    }

    if("education" %in% which_subgroups) {
      beta_posterior <- jimbilben::factor_education(beta_posterior)
      beta_summary <- jimbilben::factor_education(beta_summary)
    }

    if("education_collapse" %in% which_subgroups) {
      beta_posterior <- jimbilben::factor_education_collapse(beta_posterior)
      beta_summary <- jimbilben::factor_education_collapse(beta_summary)
    }

    if("race" %in% which_subgroups) {
      beta_posterior <- jimbilben::factor_race(beta_posterior)
      beta_summary <- jimbilben::factor_race(beta_summary)
    }

    if("age" %in% which_subgroups) {
      beta_posterior <- jimbilben::factor_age(beta_posterior)
      beta_summary <- jimbilben::factor_age(beta_summary)
    }

    if("age_fine" %in% which_subgroups) {
      beta_posterior <- jimbilben::factor_age_fine(beta_posterior)
      beta_summary <- jimbilben::factor_age_fine(beta_summary)
    }

    if("region" %in% which_subgroups) {
      reference <-
        beta_summary %>%
        group_by(region) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$region

      beta_posterior <-
        beta_posterior %>%
        mutate(region = factor(region,
                               levels = reference))
      beta_summary <-
        beta_summary %>%
        mutate(region = factor(region,
                               levels = reference))

    }

    if("division" %in% which_subgroups) {
      reference <-
        beta_summary %>%
        group_by(division) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$division

      beta_posterior <-
        beta_posterior %>%
        mutate(division = factor(division,
                                 levels = reference))
      beta_summary <-
        beta_summary %>%
        mutate(division = factor(division,
                                 levels = reference))

    }

    if("state" %in% which_subgroups) {
      reference <-
        beta_summary %>%
        group_by(state) %>%
        summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$state

      beta_posterior <-
        beta_posterior %>%
        mutate(state = factor(state,
                              levels = reference))
      beta_summary <-
        beta_summary %>%
        mutate(state = factor(state,
                              levels = reference))

    }

  }

  return(list("posterior" = beta_posterior %>% relocate(outcome, parameter),
              "summary" = beta_summary %>% relocate(outcome, parameter),
              "phi_posterior" = phi_posterior %>% relocate(outcome, parameter),
              "phi_summary" = phi_summary %>% relocate(outcome, parameter)))

}
