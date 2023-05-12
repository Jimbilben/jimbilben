#' Tidy Correlation Test
#'
#' This function computes the correlation between two numeric variables and returns a tidy data frame with the results.
#' It supports Pearson, Spearman, and Kendall correlation methods.
#'
#' @param x A character string or variable name representing the first variable for the correlation test.
#' @param y A character string or variable name representing the second variable for the correlation test.
#' @param data A data frame containing the variables specified in `x` and `y`.
#' @param method A character string specifying the correlation method. Options are "spearman" (default), "pearson", and "kendall".
#' @param alternative A character string specifying the alternative hypothesis for the correlation test. Options are "two.sided" (default), "less", and "greater".
#' @param threshold A numeric value indicating the significance level for flagging correlations as significant. Default is 0.01.
#' @param continuity A logical value indicating whether to apply a continuity correction in the correlation test. Default is TRUE.
#' @param sep A character string specifying the separator between variable names in the 'pairing' column of the output. Default is " - ".
#' @param ... Additional arguments passed to `cor.test()`.
#'
#' @return A data frame with the following columns:
#'   - estimate: The correlation coefficient.
#'   - p_value: The p-value of the correlation test.
#'   - statistic: The test statistic.
#'   - x_var: The name of the first variable.
#'   - y_var: The name of the second variable.
#'   - method: The correlation method used (Spearman, Pearson, or Kendall).
#'   - sig: A logical value indicating whether the correlation is significant based on the specified threshold.
#'   - pos_neg: The direction of the correlation (Positive, Negative, or Neutral).
#'   - pairing: A combination of the variable names separated by the specified separator.
#'
#' @examples
#' data(mtcars)
#' tidy_cortest(mpg, disp, data = mtcars)
#' tidy_cortest("mpg", "disp", data = mtcars, method = "pearson")
#' tidy_cortest(mpg, disp, data = mtcars, alternative = "greater", threshold = 0.05)
#'
#' @export
tidy_cortest <- function(x, y, data, method = "spearman", alternative = "two.sided", threshold = .01, continuity = TRUE, sep = " - ", ...) {

  x <- as.character(ensym(x))
  y <- as.character(ensym(y))

  correlation <-
    stats::cor.test(x = data %>% dplyr::pull({{x}}),
                    y = data %>% dplyr::pull({{y}}),
                    method = method,
                    alternative = alternative,
                    continuity = continuity) %>%
    broom::tidy() %>%
    dplyr::mutate(x_var = x,
                  y_var = y,
                  pairing = glue::glue("{x}{sep}{y}"),
                  sig = dplyr::case_when(p.value <= threshold ~ TRUE,
                                         TRUE ~ FALSE),
                  alternative = stringr::str_replace_all(alternative,
                                                         "\\.",
                                                         " "),
                  pos_neg = dplyr::case_when(estimate > 0 ~ "Positive",
                                             estimate < 0 ~ "Negative",
                                             estimate == 0 ~ "Neutral"),
                  method = dplyr::case_when(grepl("pearman", method) ~ "Spearman",
                                            grepl("earson", method) ~ "Pearson",
                                            grepl("endall", method) ~ "Kendall")) %>%
    dplyr::rename(p_value = p.value) %>%
    dplyr::select(estimate, p_value, statistic, x_var, y_var, method, sig, pos_neg, pairing)

  return(correlation)

}
