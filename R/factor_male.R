#' Factor Male
#'
#' factor_xyz functions correctly order demographic variables used in weighting and poststratification and, where necessary, shorten their names.
#' @param data The data to be factored

#'
factor_male <- function(data) {

  data <- data %>%
    mutate(male = factor(male,
                         levels = c("0.5",
                                    "-0.5"),
                         labels = c("Male",
                                    "Female")))

  return(data)

}
