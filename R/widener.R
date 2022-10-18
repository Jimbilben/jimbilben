#' Widener
#'
#' Only used inside of nice_post in order to incorporate multiple HDIs
#'
#' @param decimals Inherits from the arguments to nice_post
#' @param remove_lead Inherits from the arguments to nice_post

#' @export

widener <- function(for_widening,
                    decimals_for_num = decimal_choice,
                    remove_lead_for_num = remove_lead_choice) {

  nrows <- nrow(for_widening)

  if(nrows == 1) {
    output <-
      for_widening %>%
      dplyr::mutate(lower_2 = as.numeric(NA),
                    upper_2 = as.numeric(NA),
                    lower_3 = as.numeric(NA),
                    upper_3 = as.numeric(NA),
                    label_2 = as.character(NA),
                    label_3 = as.character(NA),
                    label_total = for_widening[[1, "jimbilben_label"]])

    output$long_hdi <- 1
  }
  else if(nrows == 2) {
    lowest_hdi <- min(for_widening$jimbilben_lower)
    highest_hdi <- max(for_widening$jimbilben_upper)

    output <-
      for_widening %>%
      dplyr::mutate(lower_2 = for_widening[[2, "jimbilben_lower"]],
                    upper_2 = for_widening[[2, "jimbilben_upper"]],
                    lower_3 = as.numeric(NA),
                    upper_3 = as.numeric(NA),
                    label_2 = for_widening[[2, "jimbilben_label"]],
                    label_3 = as.character(NA),
                    label_total = glue::glue('{jimbilben::nice_num(jimbilben_point_estimate, remove_lead = remove_lead_for_num, decimals = decimals_for_num)} [{jimbilben::nice_num(lowest_hdi, remove_lead = remove_lead_for_num, decimals = decimals_for_num)} - {jimbilben::nice_num(highest_hdi, remove_lead = remove_lead_for_num, decimals = decimals_for_num)}]'))

    output$long_hdi <- 1:nrows
  }
  else if(nrows > 2) {
    lowest_hdi <- min(for_widening$jimbilben_lower)
    highest_hdi <- max(for_widening$jimbilben_upper)

    output <-
      for_widening %>%
      dplyr::mutate(lower_2 = for_widening[[2, "jimbilben_lower"]],
                    upper_2 = for_widening[[2, "jimbilben_upper"]],
                    lower_3 = for_widening[[3, "jimbilben_lower"]],
                    upper_3 = for_widening[[3, "jimbilben_upper"]],
                    label_2 = for_widening[[2, "jimbilben_label"]],
                    label_3 = for_widening[[3, "jimbilben_label"]],
                    label_total = glue::glue('{jimbilben::nice_num(jimbilben_point_estimate, remove_lead = remove_lead_for_num, decimals = decimals_for_num)} [{jimbilben::nice_num(lowest_hdi, remove_lead = remove_lead_for_num, decimals = decimals_for_num)} - {jimbilben::nice_num(highest_hdi, remove_lead = remove_lead_for_num, decimals = decimals_for_num)}]'))

    output$long_hdi <- 1:nrows
  }

  return(output)

}

# library(tidyverse)
# library(glue)
# library(jimbilben)
#

