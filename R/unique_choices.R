#' Unique choices
#' Returns a vector of strings indicating each possible item in a multiple choice question where people can select more than one thing.
#'
#' @param data the target dataframe
#' @param variable the variable/column name in which the multiple choice items are stored
#' @param prefix = A string. Each variable name should, but does not have to be, preceded by a prefix so that the new variables can be easily identified/selected and e.g., pivot_longer from. Defaults to choice_

unique_choices <- function(data, variable, sep = ",") {

  variable_quoted <- rlang::enquo(variable)

  target_column <- dplyr::select(data, !!variable_quoted) %>% dplyr::pull()

  unique_choices <- unique(unlist(stringr::str_split(na.omit(target_column), sep)))

  return(unique_choices)

}
