#' Factor PartyID
#'
#' factor_xyz functions correctly order demographic variables used in weighting and poststratification and, where necessary, shorten their names.
#' @param data The data to be factored

#'
factor_partyid <- function(data) {

  data <- data %>%
    mutate(partyid = factor(partyid,
                            levels = c("Republican",
                                       "Independent",
                                       "Democrat")))

  return(data)

}
