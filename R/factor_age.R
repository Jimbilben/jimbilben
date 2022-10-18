#' Factor Age
#'
#' factor_xyz functions correctly order demographic variables used in weighting and poststratification and, where necessary, shorten their names.
#' @param data The data to be factored

#'
factor_age <- function(data) {

  data <- data %>%
    mutate(age = factor(age,
                        levels = c("18-24",
                                   "25-44",
                                   "45-64",
                                   "65+")))

  return(data)

}
