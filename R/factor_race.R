#' Factor Race
#'
#' factor_xyz functions correctly order demographic variables used in weighting and poststratification and, where necessary, shorten their names.
#' @param data The data to be factored

#'
factor_race <- function(data) {

  data <- data %>%
    mutate(race = factor(race,
                         levels = c("Asian or Asian American",
                                    "Black or African American",
                                    "Hispanic or Latino",
                                    "White or Caucasian",
                                    "Other")))

  return(data)

}
