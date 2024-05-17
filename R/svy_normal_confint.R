#' Survey Normal Confidence Interval
#'
#' Generate a tibble summarizing the estimates and confidence intervals of a survey weighted regression fit model.
#'
#' @param object A survey weighted regression fit model object.
#' @param interval The width of the confidence interval to be calculated, defaults to .95.
#' @param decimals The number of decimals to be included in summary labels, defaults to 1.
#' @param remove_lead Whether the leading 0 should be removed in summary labels, defaults to TRUE.
#'
#' @return A tibble with columns for the estimate, lower and upper bounds of the confidence interval, and a formatted label.
#' @import survey
#' @export
svy_normal_confint <- function(object, # survey weighted regression fit model
                               interval = .95,
                               decimals = 1,
                               remove_lead = TRUE) {

  output <-
    tibble::tibble(estimate = object$coefficients,
                   lower = confint(object, level = interval)[1],
                   upper = confint(object, level = interval)[2],
                   label = glue::glue('{jimbilben::nice_num(estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(upper, remove_lead = remove_lead, decimals = decimals)}]'))

  return(output)

}
