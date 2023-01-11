#' Factor Education Collapse
#'
#' factor_xyz functions correctly order demographic variables used in weighting and poststratification and, where necessary, shorten their names.
#' @param data The data to be factored

#'
factor_education_collapse <- function(data) {

  data <- data %>%
    mutate(education_collapse = factor(education_collapse,
                                       levels = c("High school or less",
                                                  "Some college, no degree",
                                                  "Graduated from college",
                                                  "Completed graduate school"),
                                       labels = c("High school or less",
                                                  "Some college",
                                                  "College grad",
                                                  "Completed grad school")))

  return(data)

}
