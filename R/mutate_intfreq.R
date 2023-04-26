#' Mutate Internet Frequency
#'
#' Mutate an Internet Frequency variable with 4 collapsed response options used in weighting. The mutated variable will be called 'intfreq_collapse'. Source is Pew https://www.pewresearch.org/wp-content/uploads/2021/03/Internet-Frequency-Update-Methodology-Topline.pdf
#'
#' @param data The data for which the mutate will be made
#' @param var_name The target of the mutate where the original data is.
#'
#' @export
mutate_intfreq <- function(data, var_name = intfreq_og) {

  data <-
    data %>%
    dplyr::mutate(intfreq = case_when({{var_name}} == "Almost constantly" ~ "Almost constantly",
                                               {{var_name}} == "Several times a day" ~ "Several times a day",
                                               {{var_name}} == "About once a day" ~ "Once a day or less",
                                               {{var_name}} == "Several times a week" ~ "Once a day or less",
                                               {{var_name}} == "Less often (less than several times a week)" ~ "Less than several times a week",
                                               {{var_name}} == "Never" ~ "Less than several times a week"))

  return(data)

}
