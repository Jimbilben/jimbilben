#' Mutate Male
#'
#' Mutate a sexual identification variable into a numeric female vs. male variable, with 0 being between them for a regression model - as in MRP. The mutated variable will be called 'male', with -.5 being female, and .5 being male.
#'
#' @param data The data for which the mutate will be made
#' @param var_name The variable in which the sexual identity categories are contained.

#'
#' @export
mutate_male <- function(data, var_name = sex_og) {

  data <-
    data %>%
    dplyr::mutate(male = case_when({{var_name}} == "Male" ~ .5,
                                   {{var_name}} == "Female" ~ -.5))

  return(data)

}
