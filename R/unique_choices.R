#' Unique choices
#' Returns a vector of strings indicating each possible item in a multiple choice question where people can select more than one thing.
#'
#' @param data the target dataframe
#' @param variable the variable/column name in which the multiple choice items are stored
#' @param prefix = A string. Each variable name should, but does not have to be, preceded by a prefix so that the new variables can be easily identified/selected and e.g., pivot_longer from. Defaults to choice_
#' @param add_backslash Logical, defaults to TRUE. If TRUE, parentheses will be preceded by two backslashes e.g., \\(this\\)

unique_choices <- function(data, variable, sep = ",", add_backslash = TRUE) {

  variable_quoted <- rlang::enquo(variable)

  target_column <- dplyr::select(data, !!variable_quoted) %>% dplyr::pull()

  unique_choices <- unique(unlist(stringr::str_split(na.omit(target_column), sep)))

  if (add_backslash) {
    unique_choices <- stringr::str_replace_all(unique_choices, "([()])", "\\\\\\1")
  }

  return(unique_choices)
}
