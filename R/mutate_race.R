#' Mutate Race
#'
#' Mutate a variables related to racial/ethnic identity into the categories of racial identification that we use in MRP. The mutated variable will be called 'race'.
#'
#' @param data The data for which the mutate will be made
#' @param race_var_name The variable in which racial identities are stored.
#' @param hisp_var_name The in which hispanic identification is stored.

#'
#' @export
mutate_race <- function(data, race_var_name = race_og, hisp_var_name = hispanic_og) {

  data <-
    data %>%
    dplyr::mutate(race = case_when({{race_var_name}} == "American Indian or Alaska Native" ~ "Other",
                                   {{race_var_name}} == "Native Hawaiian or Pacific Islander" ~ "Other",
                                   {{race_var_name}} == "Some other race" ~ "Other",
                                   {{race_var_name}} == "Other race" ~ "Other",
                                   {{race_var_name}} == "Identify with two or more races" ~ "Other",
                                   {{race_var_name}} == "Black" ~ "Black or African American",
                                   {{race_var_name}} == "White" ~ "White or Caucasian",
                                   {{race_var_name}} == "More than one race" ~ "Other",
                                   TRUE ~ {{race_var_name}}),
                  race = case_when({{hisp_var_name}} == "Yes" ~ "Hispanic or Latino",
                                   TRUE ~ race))

  return(data)

}
