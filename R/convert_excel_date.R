#' Convert Excel Date to POSIXct
#'
#' Converts an Excel date-time number to a POSIXct date-time object. Excel stores dates as serial numbers
#' where the integer part represents the date and the fractional part represents the time. This function
#' converts such a number into a standard R date-time format.
#'
#' @param excel_date A numeric value representing the Excel date-time number to be converted.
#'
#' @return A POSIXct object representing the date and time.
#'
#' @details The function first converts the input to a numeric value to ensure compatibility.
#' The origin for Excel dates (December 30, 1899) is used to convert the integer part to a date.
#' The fractional part of the number is then converted to seconds and added to the date to get
#' the full date-time. The function assumes the date-time is in the UTC timezone.
#'
#' @examples
#' convert_excel_date(44204.75) # Returns the POSIXct date-time for the Excel number 44204.75
#'
#' @export
convert_excel_date <- function(excel_date) {
  excel_date <- as.numeric(excel_date)
  # The origin for Excel dates is December 30, 1899
  excel_origin <- as.Date("1899-12-30")
  # Convert the integer part to dates
  date_part <- as.Date(floor(excel_date), origin = excel_origin)
  # Convert the fractional part to times
  time_part <- (excel_date %% 1) * 24 * 3600
  # Combine date and time
  full_date_time <- as.POSIXct(date_part, tz = "UTC") + time_part
  return(full_date_time)
}
