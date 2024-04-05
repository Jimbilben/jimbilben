#' Nice Posterior
#'
#' Generate a character version of a number in a nice format for plotting.
#'
#' @param posterior A set of posterior draws to be nicely summarised
#' @param interval The width of the uncertainty interval to be calculated, defaults to .95
#' @param point_est What point estimate would you like to summarise the posterior with, defaults to "median" - other options: "mean", "mode"
#' @param uncertainty What type of interval would you like to summarise the posterior with, defaults to "hdi" - other options: "eti"
#' @param above A number against which to test the proportion of the posterior equal to or above it
#' @param below A number against which to test the proportion of the posterior equal to or below it
#' @param between A vector of 2 numbers to test the proportion of the posterior falling between
#' @param decimals The number of decimals to be included in summary labels, defaults to 2
#' @param remove_lead Whether the leading 0 should be removed in summary labels, defaults to TRUE
#' @param percentage When the posterior represents a proportion, we can represent it as a percentage instead, defaults to FALSE

#'
#' @export
nice_post <- function(posterior,
                      estimate = vars(proportion),
                      interval = .95,
                      point_est = "median",
                      decimals = 2,
                      remove_lead = TRUE,
                      percentage = FALSE,
                      above = NULL,
                      below = NULL,
                      between = NULL) {

  hdi_checking <-
    posterior %>%
    dplyr::summarise(check = length(tidybayes::hdi({{estimate}}, interval)))

  hdi_checking <-
    max(hdi_checking$check)

  if(percentage == FALSE) {
    post_summ <- posterior %>%
      dplyr::summarise(jimbilben_hdi_check = length(tidybayes::hdi({{estimate}}, interval)),
                       jimbilben_mean = mean({{estimate}}),
                       jimbilben_median = median({{estimate}}),
                       jimbilben_mode = density({{estimate}})$x[which.max(density({{estimate}})$y)],
                       jimbilben_hdi_reference = tidybayes::hdi({{estimate}}, interval),
                       jimbilben_lower = jimbilben_hdi_reference[ , 1],
                       jimbilben_upper = jimbilben_hdi_reference[ , 2],
                       jimbilben_sd = sd({{estimate}}),
                       jimbilben_mad = mad({{estimate}}),
                       jimbilben_lower_eti = quantile({{estimate}}, 0 + ((1 - interval) / 2)),
                       jimbilben_upper_eti = quantile({{estimate}}, 1 - ((1 - interval) / 2)),
                       jimbilben_above = case_when(is.null(above) ~ as.numeric(NA),
                                                   is.null(above) == FALSE ~ (sum({{estimate}} >= above) / n())),
                       jimbilben_below = case_when(is.null(below) ~ as.numeric(NA),
                                                   is.null(below) == FALSE ~ (sum({{estimate}} <= below) / n())),
                       jimbilben_between = case_when(is.null(between) ~ as.numeric(NA),
                                                     is.null(between) == FALSE ~ (sum({{estimate}} > between[1] & {{estimate}} < between[2])) / n()),
                       jimbilben_point_estimate = case_when(point_est == "median" ~ jimbilben_median,
                                                            point_est == "mean" ~ jimbilben_mean,
                                                            point_est == "mode" ~ jimbilben_mode),
                       jimbilben_label = glue::glue('{jimbilben::nice_num(jimbilben_point_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(jimbilben_lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(jimbilben_upper, remove_lead = remove_lead, decimals = decimals)}]'),
                       jimbilben_label_eti = glue::glue('{jimbilben::nice_num(jimbilben_point_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(jimbilben_lower_eti, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(jimbilben_upper_eti, remove_lead = remove_lead, decimals = decimals)}]'),
                       jimbilben_hdi_width = abs(jimbilben_lower - jimbilben_upper),
                       jimbilben_eti_width = abs(jimbilben_lower_eti - jimbilben_upper_eti))

  }
  else if(percentage == TRUE) {
    post_summ <- posterior %>%
      dplyr::summarise(jimbilben_hdi_check = length(tidybayes::hdi({{estimate}}, interval)),
                       jimbilben_mean = mean({{estimate}} * 100),
                       jimbilben_median = median({{estimate}} * 100),
                       jimbilben_mode = density({{estimate}} * 100)$x[which.max(density({{estimate}} * 100)$y)],
                       jimbilben_hdi_reference = tidybayes::hdi({{estimate}}, interval),
                       jimbilben_lower = jimbilben_hdi_reference[ , 1] * 100,
                       jimbilben_upper = jimbilben_hdi_reference[ , 2] * 100,
                       jimbilben_sd = sd({{estimate}} * 100),
                       jimbilben_mad = mad({{estimate}} * 100),
                       jimbilben_lower_eti = quantile({{estimate}} * 100, 0 + ((1 - interval) / 2)),
                       jimbilben_upper_eti = quantile({{estimate}} * 100, 1 - ((1 - interval) / 2)),
                       jimbilben_above = case_when(is.null(above) ~ as.numeric(NA),
                                                   is.null(above) == FALSE ~ (sum({{estimate}} >= above) / n())),
                       jimbilben_below = case_when(is.null(below) ~ as.numeric(NA),
                                                   is.null(below) == FALSE ~ (sum({{estimate}} <= below) / n())),
                       jimbilben_between = case_when(is.null(between) ~ as.numeric(NA),
                                                     is.null(between) == FALSE ~ (sum({{estimate}} > between[1] & {{estimate}} < between[2])) / n()),
                       jimbilben_point_estimate = case_when(point_est == "median" ~ jimbilben_median,
                                                            point_est == "mean" ~ jimbilben_mean,
                                                            point_est == "mode" ~ jimbilben_mode),
                       jimbilben_label = glue::glue('{jimbilben::nice_num(jimbilben_point_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(jimbilben_lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(jimbilben_upper, remove_lead = remove_lead, decimals = decimals)}]'),
                       jimbilben_label_eti = glue::glue('{jimbilben::nice_num(jimbilben_point_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(jimbilben_lower_eti, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(jimbilben_upper_eti, remove_lead = remove_lead, decimals = decimals)}]'),
                       jimbilben_hdi_width = abs(jimbilben_lower - jimbilben_upper),
                       jimbilben_eti_width = abs(jimbilben_lower_eti - jimbilben_upper_eti))

  }

  any_grouping <- group_vars(posterior)

  remove_lead_choice <- remove_lead
  decimal_choice <- decimals

  if(length(any_grouping) > 0) {
    post_summ_split <-
      post_summ %>%
      group_by(across(all_of(any_grouping))) %>%
      group_split()

    post_summ <-
      map_dfr(.x = post_summ_split,
              .f = widener,
              decimals_for_num = decimal_choice,
              remove_lead_for_num = remove_lead_choice)
  }
  else {

    post_summ <-
      post_summ %>%
      jimbilben::widener(decimals_for_num = decimal_choice,
                         remove_lead_for_num = remove_lead_choice)

  }

  if(is.null(above)) {
    post_summ <- post_summ %>%
      dplyr::select(-jimbilben_above)
  }

  if(is.null(below)) {
    post_summ <- post_summ %>%
      dplyr::select(-jimbilben_below)
  }

  if(is.null(between)) {
    post_summ <- post_summ %>%
      dplyr::select(-jimbilben_between)
  }

  post_summ <-
    post_summ %>%
    dplyr::select(-jimbilben_hdi_reference)

  final_names <- stringr::str_remove(names(post_summ), "jimbilben_")
  names(post_summ) <- final_names

  if(hdi_checking > 2) {
    warning('Your HDI is split. This may make simple graphical summaries more tricky. The summary tibble has been extended to include multiple HDIs.')
  }

  return(post_summ)

}
