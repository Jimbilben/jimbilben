#' Survey Categorical Confidence Interval
#'
#' Generate a tibble summarizing the estimates and confidence intervals of a survey weighted categorical regression fit model.
#'
#' @param object A survey weighted categorical regression fit model object.
#' @param data The data frame used to fit the model.
#' @param category_name The name of the categorical outcome variable, defaults to "outcome".
#' @param interval The width of the confidence interval to be calculated, defaults to .95.
#' @param decimals The number of decimals to be included in summary labels, defaults to 1.
#' @param percentage Whether to represent the estimates as percentages, defaults to TRUE.
#' @param remove_lead Whether the leading 0 should be removed in summary labels, defaults to TRUE.
#'
#' @return A tibble with columns for the estimate, lower and upper bounds of the confidence interval, standard error, and formatted labels.
#' @import svyVGAM
#' @export
svy_cat_confint <- function(object,
                            data,
                            category_name = "outcome",
                            interval = .95,
                            decimals = 1,
                            percentage = TRUE,
                            remove_lead = TRUE) {

  prob_func <- function(coef) {
    exp_coef <- exp(coef)
    total <- 1 + sum(exp_coef)  # Now correctly includes all exp(coef)
    probabilities <- exp_coef / total  # Probabilities for each category
    c(1 / total, probabilities)  # Including the reference category at the start
  }

  coef_summary <-
    tibble::tibble(coefficient = 0) %>% # 0 is for the reference category
    dplyr::bind_rows(
      tibble(coefficient = object$coef) # get the coefficients for the other categories
    )

  # we want to extract the different categorical outcomes, in order
  coef_summary$response <-
    data %>%
    dplyr::pull(!!sym(outcome_name)) %>%
    unique() %>%
    sort()

  # generate the expected proportions based on the coefficients
  coef_summary$estimate <- prob_func(coef_summary$coefficient[-1])

  grad_matrix <- numDeriv::jacobian(prob_func, coef_summary$coefficient[-1])

  vcov_matrix <- object$var

  cov_matrix <- grad_matrix %*% vcov_matrix %*% t(grad_matrix)

  se_probs <- sqrt(diag(cov_matrix))

  target_percentile <-
    1 - ((1 - interval) * .5)

  z_score <- qnorm(target_percentile)

  coef_summary$se <- se_probs

  coef_summary <-
    coef_summary %>%
    dplyr::mutate(lower = estimate - z_score * se,
                  upper = estimate + z_score * se) %>%
    mutate(capped_lower = case_when(lower < 0 ~ 0,
                                    lower > 1 ~ 1,
                                    TRUE ~ lower),
           capped_upper = case_when(upper < 0 ~ 0,
                                    upper > 1 ~ 1,
                                    TRUE ~ upper))

  if(percentage == TRUE) {
    coef_summary <-
      coef_summary %>%
      dplyr::mutate(estimate = estimate * 100,
                    se = se * 100,
                    lower = lower * 100,
                    upper = upper * 100,
                    capped_lower = capped_lower * 100,
                    capped_upper = capped_upper * 100) %>%
      mutate(label = glue::glue("{jimbilben::nice_num(estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(upper, remove_lead = remove_lead, decimals = decimals)}]"),
             capped_label = glue::glue("{jimbilben::nice_num(estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(capped_lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(capped_upper, remove_lead = remove_lead, decimals = decimals)}]"))

  } else {
    coef_summary <-
      coef_summary %>%
      mutate(label = glue::glue("{jimbilben::nice_num(estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(upper, remove_lead = remove_lead, decimals = decimals)}]"),
             capped_label = glue::glue("{jimbilben::nice_num(estimate, remove_lead = remove_lead, decimals = decimals)} [{jimbilben::nice_num(capped_lower, remove_lead = remove_lead, decimals = decimals)}; {jimbilben::nice_num(capped_upper, remove_lead = remove_lead, decimals = decimals)}]"))
  }

  return(coef_summary)

}
