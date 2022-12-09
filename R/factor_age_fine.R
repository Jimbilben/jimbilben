#' Factor Age Fine
#'
#' factor_xyz functions correctly order demographic variables used in weighting and poststratification and, where necessary, shorten their names.
#' @param data The data to be factored

#'
factor_age_fine <- function(data) {

  data <- data %>%
    mutate(age_fine = factor(age_fine,
                        levels = c("18-24",
                                   "25-34",
                                   "35-44",
                                   "45-64",
                                   "65+")))

  return(data)

}
