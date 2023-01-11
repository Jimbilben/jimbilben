#' MRP Party PSup Poststrat
#'
#' Post-stratify for within subjects probability of superiority.
#'
#' @param current_model_epred posterior prediction for the regression model of interest
#' @param subgroups Indicating whether this is a population-level or subgroup prediction
#' @param comparison Give a name to the comparison being computed (e.g., Vaccines minus Treatments)
#' @param interval The summary interval level
#' @param superior_name Exact name of the outcome indicating superiority - e.g., "Sup"
#' @param inferior_name Exact name of the outcome indicating inferiority - e.g., "Inf"
#' @param equal_name Exact name of the outcome indicating equality - e.g., "Same"
#' @param poststrat_tibble This is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it
#' @param poststrat_epred This is the epred for our party poststrat model containing numbers of people expected to fall into each row of the poststrat tibble

#'
#' @export
mrp_party_psup_poststrat <- function(current_model_epred, # posterior prediction for the regression model of interest
                                     subgroups = FALSE, # just indicating this is a population-level prediction
                                     comparison = "e.g., vax_min_treat", # give a name to the comparison being computed (e.g., Vaccines minus Treatments)
                                     interval = .95, # the summary interval level
                                     superior_name = "Sup", # you might have named your outcomes differently, so you can indicate that here - e.g., "Higher", "Lower", "Equal"
                                     inferior_name = "Inf",
                                     equal_name = "Same",
                                     poststrat_tibble = acs5_2020_poststrat_with_partyid, # this is the tibble - usually an ACS tibble - containing the demographic variable names. You can group it!
                                     poststrat_epred = acs5_2020_model_expected_n) { # this is the epred for our party poststrat model containing numbers of people expected to fall into each row of the poststrat tibble

  if(subgroups == FALSE) {

    psup_posterior <-
      tibble(superior = colSums(t(current_model_epred[ , , superior_name]) * poststrat_epred) / colSums(poststrat_epred), # the t transforms the current_model matrix so it can be multiplied with our poststrat matrix
             inferior = colSums(t(current_model_epred[ , , inferior_name]) * poststrat_epred) / colSums(poststrat_epred),
             equal = colSums(t(current_model_epred[ , , equal_name]) * poststrat_epred) / colSums(poststrat_epred),
             draw = 1:1000) %>%
      pivot_longer(cols = superior:equal, names_to = "superiority", values_to = "proportion") %>%
      mutate(psup = case_when(superiority == "inferior" ~ 0, # inferior scores 0
                              superiority == "equal" ~ .5 * proportion, # same scores half
                              superiority == "superior" ~ proportion)) %>% # superior scores full. # if we stop the pipe here we'd be able to get population proportions of each type of response
      group_by(draw) %>% # we need to ensure that draws stick together - to keep the sup, equal, and inf proportions for that draw and summarise the psup for that specific draw
      dplyr::summarise(psup = sum(psup)) %>%
      mutate(comparison = comparison,
             grouping_type = "Population") # just adding a name so we know which psup comparison is being made.

    # this summary gives some useful/usable default options, but we also return the full posterior as one might wish to summarise it many ways
    psup_summary <-
      psup_posterior %>%
      jimbilben::nice_post(psup,
                           interval = interval,
                           decimals = 2,
                           percentage = FALSE,
                           above = .575,
                           below = .425,
                           between = c(.425, .575)) %>%
      mutate(comparison = comparison,
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

      psup <-
        data %>%
        dplyr::select(all_of(.which_subgroups)) %>%
        slice_head()

      psup <-
        sample_n(psup,
                 1000,
                 replace = TRUE) %>%
        mutate(superior = colSums(t(current_model_epred[ , data$case, superior_name]) * poststrat_epred[data$case, ]) / colSums(poststrat_epred[data$case, ]), # the t transforms the current_model matrix so it can be multiplied with our poststrat matrix
               inferior = colSums(t(current_model_epred[ , data$case, inferior_name]) * poststrat_epred[data$case, ]) / colSums(poststrat_epred[data$case, ]),
               equal = colSums(t(current_model_epred[ , data$case, equal_name]) * poststrat_epred[data$case, ]) / colSums(poststrat_epred[data$case, ]),
               draw = 1:1000) %>%
        pivot_longer(cols = superior:equal, names_to = "superiority", values_to = "proportion") %>%
        mutate(psup = case_when(superiority == "inferior" ~ 0, # inferior scores 0
                                superiority == "equal" ~ .5 * proportion, # same scores half
                                superiority == "superior" ~ proportion)) %>% # superior scores full. # if we stop the pipe here we'd be able to get population proportions of each type of response
        group_by(across(all_of(c("draw", .which_subgroups)))) %>% # we need to ensure that draws stick together - to keep the sup, equal, and inf proportions for that draw and summarise the psup for that specific draw
        dplyr::summarise(psup = sum(psup)) %>%
        mutate(comparison = comparison,
               grouping_type = "Subgrouped")

      return(psup)

    }

    psup_posterior <-
      map_dfr(.x = poststrat_tibble_split,
              .f = subgroup_posterior_tibble_maker)

    psup_summary <-
      psup_posterior %>%
      group_by(across(all_of(which_subgroups))) %>%
      jimbilben::nice_post(psup,
                           interval = interval,
                           decimals = 2,
                           percentage = FALSE,
                           above = .575,
                           below = .425,
                           between = c(.425, .575)) %>%
      mutate(comparison = comparison,
             grouping_type = "Subgrouped")

    #### Now we just sort out the factor levels ####
    if("partyid" %in% which_subgroups) {
      psup_posterior <- jimbilben::factor_partyid(psup_posterior)
      psup_summary <- jimbilben::factor_partyid(psup_summary)
    }

    if("male" %in% which_subgroups) {
      psup_posterior <- jimbilben::factor_male(psup_posterior)
      psup_summary <- jimbilben::factor_male(psup_summary)
    }

    if("income_ces" %in% which_subgroups) {
      psup_posterior <- jimbilben::factor_income(psup_posterior)
      psup_summary <- jimbilben::factor_income(psup_summary)
    }

    if("education" %in% which_subgroups) {
      psup_posterior <- jimbilben::factor_education(psup_posterior)
      psup_summary <- jimbilben::factor_education(psup_summary)
    }

    if("education_collapse" %in% which_subgroups) {
      psup_posterior <- jimbilben::factor_education_collapse(psup_posterior)
      psup_summary <- jimbilben::factor_education_collapse(psup_summary)
    }

    if("race" %in% which_subgroups) {
      psup_posterior <- jimbilben::factor_race(psup_posterior)
      psup_summary <- jimbilben::factor_race(psup_summary)
    }

    if("age" %in% which_subgroups) {
      psup_posterior <- jimbilben::factor_age(psup_posterior)
      psup_summary <- jimbilben::factor_age(psup_summary)
    }

    if("age_fine" %in% which_subgroups) {
      psup_posterior <- jimbilben::factor_age_fine(psup_posterior)
      psup_summary <- jimbilben::factor_age_fine(psup_summary)
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
        psup_summary %>%
        group_by(state) %>%
        dplyr::summarise(for_order = mean(median)) %>%
        arrange(for_order)

      reference <- reference$state

      psup_posterior <-
        psup_posterior %>%
        mutate(state = factor(state,
                              levels = reference))
      psup_summary <-
        psup_summary %>%
        mutate(state = factor(state,
                              levels = reference))

    }

  }

  return(list("posterior" = psup_posterior, "summary" = psup_summary))

}
