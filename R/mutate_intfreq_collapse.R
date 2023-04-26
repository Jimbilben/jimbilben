#' Mutate Internet Frequency Collapse
#'
#' Mutate an Internet Frequency variable with 3 collapsed response options used in weighting. The mutated variable will be called 'intfreq_collapse'.
#'
#' @param data The data for which the mutate will be made
#' @param var_name The target of the mutate where the original data is.
#'
#' @export
mutate_intfreq_collapse <- function(data, var_name = intfreq_og) {

  data <-
    data %>%
    dplyr::mutate(intfreq_collapse = case_when({{var_name}} == "Almost constantly" ~ "Almost constantly",
                                               {{var_name}} == "Several times a day" ~ "Several times a day",
                                               {{var_name}} == "About once a day" ~ "Once a day or less",
                                               {{var_name}} == "Several times a week" ~ "Once a day or less",
                                               {{var_name}} == "Less often (less than several times a week)" ~ "Once a day or less",
                                               {{var_name}} == "Never" ~ "Once a day or less"))

  return(data)

}
