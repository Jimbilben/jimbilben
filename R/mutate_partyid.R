#' Mutate Partyid
#'
#' Mutate a political party variable that includes Other and Not Sure into a simplified version where these become Independent. The mutated variable will be called 'partyid'.
#'
#' @param data The data for which the mutate will be made
#' @param var_name The target of the mutate - i.e., the variable name in which the numeric ages are stored.
#'
#' @export
mutate_partyid <- function(data, var_name = party_raw) {

  data <-
    data %>%
    dplyr::mutate(partyid = case_when({{var_name}} == "Other" ~ "Independent",
                                      {{var_name}} == "Not sure" ~ "Independent",
                                      TRUE ~ {{var_name}}))

  return(data)

}
