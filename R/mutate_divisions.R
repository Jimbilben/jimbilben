#' Mutate Divisions
#'
#' Assign states to their respective Census divisions with a mutate. The new variable is called 'division'. Use function set_census_divisions() first.
#'
#' @param data The data for which the division variable will be made
#'
#' @export
mutate_divisions <- function(data, var_name = state) {

  data <-
    data %>%
    dplyr::mutate(division = case_when({{var_name}} %in% newengland ~ "New England",
                                       {{var_name}} %in% matlantic ~ "Mid Atlantic",
                                       {{var_name}} %in% enc ~ "East North Central",
                                       {{var_name}} %in% wnc ~ "West North Central",
                                       {{var_name}} %in% satlantic ~ "South Atlantic",
                                       {{var_name}} %in% esc ~ "East South Central",
                                       {{var_name}} %in% wsc ~ "West South Central",
                                       {{var_name}} %in% mountain ~ "Mountain",
                                       {{var_name}} %in% pacific ~ "Pacific"))

  return(data)

}
