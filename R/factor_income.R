#' Factor Income
#'
#' factor_xyz functions correctly order demographic variables used in weighting and poststratification and, where necessary, shorten their names.
#' @param data The data to be factored

#'
factor_income <- function(data) {

  data <- data %>%
    mutate(income_ces = factor(income_ces,
                               levels = c("Unclassified",
                                          "Under $20,000",
                                          "Between $20,000 and $49,999",
                                          "Between $50,000 and $79,999",
                                          "Between $80,000 and $99,999",
                                          "Between $100,000 and $150,000",
                                          "Over $150,000"),
                               labels = c("Unclassified",
                                          "<$20k",
                                          "$20k-$49k",
                                          "$50k-$79k",
                                          "$80k-$99k",
                                          "$100k-$150k",
                                          ">$150k")))

  return(data)

}
