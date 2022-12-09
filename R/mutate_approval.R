#' Mutate Approval
#'
#' Mutate a fine-grained presidential approval variable into Approve, Disapprove, and Don't Know. The mutated variable will be called 'pres_approval'.
#'
#' @param data The data for which the mutate will be made
#' @param var_name The variable in which the Presidential approval categories are contained.

#'
#' @export
mutate_approval <- function(data, var_name = pres_approve_nonbin) {

  data <-
    data %>%
    dplyr::mutate(pres_approval = case_when({{var_name}} == "Strongly approve" ~ "Approve",
                                            {{var_name}} == "Somewhat approve" ~ "Approve",
                                            {{var_name}} == "Somewhat disapprove" ~ "Disapprove",
                                            {{var_name}} == "Strongly disapprove" ~ "Disapprove",
                                            {{var_name}} == "Don't know / No opinion" ~ "DK"))

  return(data)

}
