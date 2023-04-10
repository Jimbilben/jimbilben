#' Lazy unique
#'
#' Get unique values from a variable in a data frame with less typing
#'
#' @param variable the variable to assess unique values of
#' @param data What data to draw the unique values from - defaults to ref data

#' @export

lunique <- function(variable, data = ref_data) {

  unique_values <-
    data %>%
    select({{variable}}) %>%
    pull() %>%
    unique()

  return(unique_values)

}
