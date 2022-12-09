#' Mutate Regions
#'
#' Assign states to their respective Census regions with a mutate. The new variable is called 'region'. Use function set_census_regions() first.
#'
#' @param data The data for which the region variable will be made
#'
#' @export
mutate_regions <- function(data, var_name = state) {

  data <-
    data %>%
    dplyr::mutate(region = case_when({{var_name}} %in% south ~ "South",
                                     {{var_name}} %in% mid_west ~ "Midwest",
                                     {{var_name}} %in% northeast ~ "Northeast",
                                     {{var_name}} %in% west ~ "West"))

  return(data)

}
