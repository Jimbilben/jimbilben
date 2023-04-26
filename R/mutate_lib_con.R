#' Mutate Political Views (Liberal - Conservative)
#'
#' Mutate a Liberal / Conservative Political Views variable with shortened response names used in weighting. The mutated variable will be called 'bible'.
#'
#' @param data The data for which the mutate will be made
#' @param var_name The target of the mutate where the original data is.
#'
#' @export
#'
mutate_lib_con <- function(data, var_name = lib_con_og) {

  data <-
    data %>%
    dplyr::mutate(lib_con = case_when({{var_name}} == "Extremely conservative" ~ "Conservative",
                                      {{var_name}} == "Conservative" ~ "Conservative",
                                      {{var_name}} == "Slightly conservative" ~ "Conservative",
                                      {{var_name}} == "Moderate" ~ "Moderate",
                                      {{var_name}} == "Slightly liberal" ~ "Liberal",
                                      {{var_name}} == "Liberal" ~ "Liberal",
                                      {{var_name}} == "Extremely liberal" ~ "Liberal"))

  return(data)

}
