#' Factor Education
#'
#' factor_xyz functions correctly order demographic variables used in weighting and poststratification and, where necessary, shorten their names.
#' @param data The data to be factored

#'
factor_education <- function(data) {

  data <- data %>%
    mutate(education = factor(education,
                              levels = c("Less than high school",
                                         "Graduated from high school",
                                         "Some college, no degree",
                                         "Graduated from college",
                                         "Completed graduate school"),
                              labels = c("< High school",
                                         "High school grad",
                                         "Some college",
                                         "College grad",
                                         "Completed grad school")))

  return(data)

}
