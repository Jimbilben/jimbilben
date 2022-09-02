#' Nice Number
#'
#' Generate a character version of a number in a nice format for plotting.
#'
#' @param number The number to be converted to a nice number
#' @param decimals The number of decimals to be included, defaults to 2
#' @param remove_lead Whether the leading 0 should be removed, defaults to TRUE
#'
#' @export
nice_num <- function(number, decimals = 2, remove_lead = TRUE) {

  sprintf_string <- glue::glue('%.{decimals}f')

  get_to_dp <- sprintf(sprintf_string, number)

  if(remove_lead == TRUE) {
    output <- sub("^0+", "", get_to_dp)
  }
  else {
    output <- get_to_dp
  }

  return(output)

}
