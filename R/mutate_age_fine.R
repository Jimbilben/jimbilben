#' Mutate Age Fine
#'
#' Mutate a numeric age variable to an age bracket used in MRP or weighting. The new variable will be called 'age_fine'
#'
#' @param data The data for which the mutate will be made
#' @param var_name The target of the mutate - i.e., the variable name in which the numeric ages are stored.
#'
#' @export
mutate_age_fine <- function(data, var_name = age_og) {

  data <-
    data %>%
    dplyr::mutate(age_fine = case_when({{var_name}} < 18 ~ "Under 18",
                                       {{var_name}} < 25 ~ "18-24",
                                       {{var_name}} < 35 ~ "25-34",
                                       {{var_name}} < 45 ~ "35-44",
                                       {{var_name}} < 65 ~ "45-64",
                                       {{var_name}} >= 65 ~ "65+"))

  return(data)

}
