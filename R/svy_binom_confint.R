#' Survey Binomial Confidence Interval
#'
#' Generate a tibble summarizing the estimates and confidence intervals of a survey weighted binomial regression fit model.
#'
#' @param object A survey weighted binomial regression fit model object.
#' @param interval The width of the confidence interval to be calculated, defaults to .95.
#' @param decimals The number of decimals to be included in summary labels, defaults to 1.
#' @param percentage Whether to represent the estimates as percentages, defaults to TRUE.
#' @param remove_lead Whether the leading 0 should be removed in summary labels, defaults to TRUE.
#' @param provide_inverse Whether to provide the inverse probability estimates, defaults to TRUE.
#' @param present The value representing presence, defaults to 1.
#' @param absent The value representing absence, defaults to 0.
#'
#' @return A tibble with columns for the estimate, lower and upper bounds of the confidence interval, and a formatted label. If `provide_inverse` is TRUE, includes the inverse probability estimates.
#' @import survey
#' @export
svy_binom_confint <- function(object,
                              interval = .95,
                              decimals = 1,
                              percentage = TRUE,
                              remove_lead = TRUE,
                              provide_inverse = TRUE,
                              present = 1,
                              absent = 0) {

  se_version <-
    marginaleffects::predictions(object,
                                 newdata = tibble::tibble(anchor = 1),
                                 conf_level = interval)

  coef_mean <- exp(object$coefficients) / (1 + exp(object$coefficients))
  coef_lower <- exp(confint(object, level = interval)[1]) / (1 + exp(confint(object, level = interval)[1]))
  coef_upper <- exp(confint(object, level = interval)[2]) / (1 + exp(confint(object, level = interval)[2]))

  if(percentage == TRUE) {
    output <-
      tibble::tibble(estimate = coef_mean * 100,
                     lower = coef_lower * 100,
                     upper = coef_upper * 100,
                     label = glue::glue('{jimbilben::nice_num(estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(upper, remove_lead = remove_lead, decimals = decimals)}]'),
                     se_estimate = se_version$estimate[1] * 100,
                     se_lower = se_version$conf.low[1] * 100,
                     se_upper = se_version$conf.high[1] * 100,
                     se_label = glue::glue('{jimbilben::nice_num(se_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(se_lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(se_upper, remove_lead = remove_lead, decimals = decimals)}]'))

    if(provide_inverse == TRUE) {

      inv_output <-
        tibble::tibble(response = absent,
                       estimate = 100 - (coef_mean * 100),
                       lower = 100 - (coef_upper * 100),
                       upper = 100 - (coef_lower * 100),
                       label = glue::glue('{jimbilben::nice_num(estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(upper, remove_lead = remove_lead, decimals = decimals)}]'),
                       se_estimate = 100 - (se_version$estimate[1] * 100),
                       se_lower = 100 - (se_version$conf.high[1] * 100),
                       se_upper = 100 - (se_version$conf.low[1] * 100),
                       se_label = glue::glue('{jimbilben::nice_num(se_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(se_lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(se_upper, remove_lead = remove_lead, decimals = decimals)}]'))

      output <-
        output %>%
        dplyr::mutate(response = present) %>%
        dplyr::relocate(response) %>%
        dplyr::bind_rows(inv_output)

    }
  }
  else if(percentage == FALSE) {
    output <-
      tibble::tibble(estimate = coef_mean,
                     lower = coef_lower,
                     upper = coef_upper,
                     label = glue::glue('{jimbilben::nice_num(estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(upper, remove_lead = remove_lead, decimals = decimals)}]'),
                     se_estimate = se_version$estimate[1],
                     se_lower = se_version$conf.low[1],
                     se_upper = se_version$conf.high[1],
                     se_label = glue::glue('{jimbilben::nice_num(se_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(se_lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(se_upper, remove_lead = remove_lead, decimals = decimals)}]'))

    if(provide_inverse == TRUE) {

      inv_output <-
        tibble::tibble(response = absent,
                       estimate = 1 - (coef_mean),
                       lower = 1 - (coef_upper),
                       upper = 1 - (coef_lower),
                       label = glue::glue('{jimbilben::nice_num(estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(upper, remove_lead = remove_lead, decimals = decimals)}]'),
                       se_estimate = 1 - (se_version$estimate[1]),
                       se_lower = 1 - (se_version$conf.high[1]),
                       se_upper = 1 - (se_version$conf.low[1]),
                       se_label = glue::glue('{jimbilben::nice_num(se_estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(se_lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(se_upper, remove_lead = remove_lead, decimals = decimals)}]'))

      output <-
        output %>%
        dplyr::mutate(response = present) %>%
        dplyr::relocate(response) %>%
        dplyr::bind_rows(inv_output)

    }
  }

  return(output)

}
