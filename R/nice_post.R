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

  if(percentage == FALSE) {
    post_summ <- posterior %>%
      dplyr::summarise(jimbilben_mean = mean({{estimate}}),
                       jimbilben_median = median({{estimate}}),
                       jimbilben_mode = density({{estimate}})$x[which.max(density({{estimate}})$y)],
                       jimbilben_lower = tidybayes::hdi({{estimate}}, interval)[1],
                       jimbilben_upper = tidybayes::hdi({{estimate}}, interval)[2],
                       jimbilben_hdi_check = length(tidybayes::hdi({{estimate}})),
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
                       jimbilben_label = glue('{jimbilben::nice_num(jimbilben_point_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(jimbilben_lower, remove_lead = remove_lead, decimals = decimals)} - {jimbilben::nice_num(jimbilben_upper, remove_lead = remove_lead, decimals = decimals)}]'),
                       jimbilben_label_eti = glue('{jimbilben::nice_num(jimbilben_point_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(jimbilben_lower_eti, remove_lead = remove_lead, decimals = decimals)} - {jimbilben::nice_num(jimbilben_upper_eti, remove_lead = remove_lead, decimals = decimals)}]'),
                       jimbilben_hdi_width = abs(jimbilben_lower - jimbilben_upper),
                       jimbilben_eti_width = abs(jimbilben_lower_eti - jimbilben_upper_eti)) %>%
      select(-c(jimbilben_point_estimate))
  }
  else if(percentage == TRUE) {
    post_summ <- posterior %>%
      dplyr::summarise(jimbilben_mean = mean({{estimate}} * 100),
                       jimbilben_median = median({{estimate}} * 100),
                       jimbilben_mode = density({{estimate}} * 100)$x[which.max(density({{estimate}} * 100)$y)],
                       jimbilben_lower = tidybayes::hdi({{estimate}} * 100, interval)[1],
                       jimbilben_upper = tidybayes::hdi({{estimate}} * 100, interval)[2],
                       jimbilben_hdi_check = length(tidybayes::hdi({{estimate}} * 100)),
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
                       jimbilben_label = glue('{jimbilben::nice_num(jimbilben_point_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(jimbilben_lower, remove_lead = remove_lead, decimals = decimals)} - {jimbilben::nice_num(jimbilben_upper, remove_lead = remove_lead, decimals = decimals)}]'),
                       jimbilben_label_eti = glue('{jimbilben::nice_num(jimbilben_point_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(jimbilben_lower_eti, remove_lead = remove_lead, decimals = decimals)} - {jimbilben::nice_num(jimbilben_upper_eti, remove_lead = remove_lead, decimals = decimals)}]'),
                       jimbilben_hdi_width = abs(jimbilben_lower - jimbilben_upper),
                       jimbilben_eti_width = abs(jimbilben_lower_eti - jimbilben_upper_eti)) %>%
      select(-c(jimbilben_point_estimate))
  }



  if(is.null(above)) {
    post_summ <- post_summ %>%
      select(-jimbilben_above)
  }

  if(is.null(below)) {
    post_summ <- post_summ %>%
      select(-jimbilben_below)
  }

  if(is.null(between)) {
    post_summ <- post_summ %>%
      select(-jimbilben_between)
  }

  final_names <- str_remove(names(post_summ), "jimbilben_")
  names(post_summ) <- final_names

  if(any(post_summ$hdi_check > 2)) {
    warning('Your HDI is split. This may make simple graphical summaries more tricky.')
  }

  if(any(post_summ$hdi_check > 2) == FALSE) {
    post_summ <- post_summ %>%
      select(-hdi_check)
  }

  return(post_summ)

}
