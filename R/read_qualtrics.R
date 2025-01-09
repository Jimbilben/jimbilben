#' Read Qualtrics
#'
#' Reads in a qualtrics excel file. The redundant second row is removed, as are preview responses. Columns identified with time are converted to numbers.
#'
#' @param file An xlsx file from qualtrics
#' @param provider The survey or panel provider from which the sample comes, defaults to Prolific
#' @param row_removal Should the second row - usually just bad column names - be removed, defaults to TRUE
#' @param preview_removal Should preview responses be removed, defaults to TRUE
#' @param convert_time Should timer-question-based responses be converted to a number, defaults to TRUE
#' @param extras_removal Should empty columns such as recipient name and email be removed, defaults to TRUE
#'
#' @export
read_qualtrics <- function(file,
                           provider = "Prolific",
                           row_removal = TRUE,
                           preview_removal = TRUE,
                           convert_time = TRUE,
                           extras_removal = TRUE) {

  qual <- readxl::read_xlsx(file)

  if(preview_removal == TRUE) {
    qual <- qual %>%
      filter(grepl('review', Status) == FALSE)
  }

  qual_names <- names(qual)
  qual_names <- str_replace_all(tolower(qual_names), " ", "_")

  names(qual) <- qual_names

  if(row_removal == TRUE) {
    qual <- qual[-1, ]
  }

  if(extras_removal == TRUE) {
    qual <- qual %>%
      select(-c("recipientlastname",
                "recipientfirstname",
                "recipientemail",
                "externalreference"))
  }

  if(convert_time == TRUE) {
    qual <- qual %>%
      mutate(across(.cols = starts_with("time_"),
                    ~ as.numeric(.)))
  }

  qual <- qual %>%
    mutate(provider = provider)

  return(qual)

}
