#' Lazy unique 2
#'
#' Get unique values from a variable in a data frame with less typing, formatted to work in a pipe
#'
#' @param data What data to draw the unique values from - defaults to ref data
#' @param variable the variable to assess unique values of

#' @export

lunique2 <- function(data = ref_data, variable) {

  unique_values <-
    data %>%
    select({{variable}}) %>%
    pull() %>%
    unique()

  return(unique_values)

}
