#' Mutate Age Alt
#'
#' Mutate a numeric age variable to an age bracket used in MRP or weighting. The new variable will be called 'age_alt'
#'
#' @param data The data for which the mutate will be made
#' @param var_name The target of the mutate - i.e., the variable name in which the numeric ages are stored.
#'
#' @export
mutate_age_alt <- function(data, var_name = age_og) {

  data <-
    data %>%
    dplyr::mutate(age_alt = case_when({{var_name}} < 18 ~ "Under 18",
                                      {{var_name}} < 25 ~ "18-24",
                                      {{var_name}} < 40 ~ "25-39",
                                      {{var_name}} < 55 ~ "40-54",
                                      {{var_name}} >= 55 ~ "55+"))

  return(data)

}
