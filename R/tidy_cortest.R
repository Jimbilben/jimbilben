#' Tidy Correlation Test
#'
#' This function computes the correlation between two numeric variables and returns a tidy data frame with the results.
#' It supports Pearson, Spearman, Kendall correlation methods and Probability of Median Superiority.
#'
#' @param data A data frame containing the variables specified in `x` and `y`.
#' @param x A character string or variable name representing the first variable for the correlation test.
#' @param y A character string or variable name representing the second variable for the correlation test.
#' @param method A character string specifying the correlation method. Options are "spearman" (default), "pearson", "kendall" and "pmedsup", "psup", "psupmed" for Probability of Median Superiority.
#' @param alternative A character string specifying the alternative hypothesis for the correlation test. Options are "two.sided" (default), "less", and "greater".
#' @param threshold A numeric value indicating the significance level for flagging correlations as significant (or the size of the HDI interval to use for Probability of Median Superiority, which is 1 - the threshold). Default is 0.01.
#' @param continuity A logical value indicating whether to apply a continuity correction in the correlation test. Default is TRUE for Pearson, Spearman, and Kendall methods.
#' @param sep A character string specifying the separator between variable names in the 'pairing' column of the output. Default is " - ".
#' @param prior Prior specification for the Bayesian model when method is one of "pmedsup", "psup", "psupmed". Default is a normal distribution with mean 0 and standard deviation 0.5.
#' @param .lower Lower bound for the ROPE. Default is 45.
#' @param .upper Upper bound for the ROPE. Default is 55.
#' @param point_est A character string specifying the posterior point estimate to use with Probability of Median Superiority. Default is "mean".
#' @param decimals Number of decimal places to round to when presenting results for Probability of Median Superiority. Default is 1.
#' @param percentage A logical value indicating whether to present results as percentages when assessing Probability of Median Superiority. Default is TRUE.
#' @param ... Additional arguments passed to `cor.test()`.
#'
#' @return A data frame with the following columns:
#'   - estimate: The correlation coefficient.
#'   - p_value: The p-value of the correlation test (only for Pearson, Spearman, Kendall methods).
#'   - statistic: The test statistic (only for Pearson, Spearman, Kendall methods).
#'   - x_var: The name of the first variable.
#'   - y_var: The name of the second variable.
#'   - method: The correlation method used (Spearman, Pearson, Kendall or Probability of Median Superiority).
#'   - sig: A logical value indicating whether the correlation is significant based on the specified threshold.
#'   - pos_neg: The direction of the correlation (Positive, Negative, or Neutral).
#'   - pairing: A combination of the variable names separated by the specified separator.
#'   - rope_decision: The decision based on the ROPE analysis (only for Probability of Median Superiority).
#'   - rope: The Range Of Practical Equivalence (ROPE) (only for Probability of Median Superiority).
#'   - lower: The lower limit of the HDI (only for Probability of Median Superiority).
#'   - upper: The upper limit of the HDI (only for Probability of Median Superiority).
#'   - below: The proportion of the posterior distribution below the lower ROPE threshold (only for Probability of Median Superiority).
#'   - above: The proportion of the posterior distribution above the upper ROPE threshold (only for Probability of Median Superiority).
#'   - between: The proportion of the posterior distribution within the ROPE (only for Probability of Median Superiority).
#' @export
tidy_cortest <- function(data, x, y, method = "spearman", alternative = "two.sided", threshold = .01, continuity = TRUE, sep = " - ", prior = brms::set_prior("normal(0 , .75)", class = "Intercept"), .lower = 45, .upper = 55, point_est = "mean", decimals = 1, percentage = TRUE, ...) {

  x <- as.character(ensym(x))
  y <- as.character(ensym(y))

  if(tolower(method) %in% c("pearson", "kendall", "spearman")) {
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
  }
  else if(tolower(method) %in% c("pmedsup", "psup", "psupmed")) {

    data <-
      tibble(x = data %>% dplyr::pull({{x}}) %>% as.numeric(),
             y = data %>% dplyr::pull({{y}}) %>% as.numeric()) %>%
      mutate(xmed = dplyr::case_when(x > median(x, na.rm = TRUE) ~ 1,
                                     x == median(x, na.rm = TRUE) ~ .5,
                                     x < median(x, na.rm = TRUE) ~ 0),
             ymed = dplyr::case_when(y > median(y, na.rm = TRUE) ~ 1,
                                     y == median(y, na.rm = TRUE) ~ .5,
                                     y < median(y, na.rm = TRUE) ~ 0),
             pmedsup = dplyr::case_when(xmed == .5 | ymed == .5 ~ "equal",
                                        xmed == ymed ~ "pos",
                                        TRUE ~ "neg"))

    unique_length <-
      data %>% pull(pmedsup) %>% unique() %>% na.omit() %>% length()


    interval <- 1 - threshold
    iterations <- if(interval >= .98) {
      4000
    }
    else{
      2500
    }

    if(unique_length > 1) {
      pmedsup_model <-
        brms::brm(formula = pmedsup ~ 1,
                  family = brms::categorical(),
                  data = data,
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  prior = prior,
                  chains = 4,
                  cores = 4,
                  iter = iterations,
                  warmup = 500,
                  init = 0,
                  backend = 'cmdstanr',
                  threads = brms::threading(4),
                  seed = 1010,
                  stan_model_args=list(stanc_options = list('O1')))

      pmedsup_post <-
        tidybayes::add_epred_draws(object = pmedsup_model,
                                   newdata = data[1,]) %>%
        dplyr::as_tibble() %>%
        ungroup() %>%
        dplyr::select(.category,
                      .epred,
                      .draw) %>%
        dplyr::mutate(psup = dplyr::case_when(.category == "neg" ~ 0,
                                              .category == "pos" ~ .epred,
                                              .category == "equal" ~ .5 * .epred)) %>%
        dplyr::group_by(.draw) %>%
        dplyr::summarise(psup = sum(psup)) %>%
        dplyr::select(psup)

      correlation <-
        pmedsup_post %>%
        jimbilben::nice_post(psup,
                             below = .lower / 100,
                             above = .upper / 100,
                             between = c(.lower / 100, .upper / 100),
                             percentage = percentage,
                             point_est = point_est,
                             decimals = decimals,
                             interval = interval) %>%
        dplyr::select({{point_est}}, lower, upper, label, below, above, between) %>%
        dplyr::mutate(x_var = x,
                      y_var = y,
                      pairing = glue::glue("{x}{sep}{y}"),
                      pos_neg = dplyr::case_when({{point_est}} > .5 ~ "Positive",
                                                 {{point_est}} < .5 ~ "Negative",
                                                 {{point_est}} == .5 ~ "Neutral"),
                      method = "Probability of median superiority",
                      sig = dplyr::case_when(lower > .upper | upper < .lower ~ TRUE,
                                             TRUE ~ FALSE),
                      rope_decision = dplyr::case_when(lower > .upper ~ "Positive",
                                                       upper < .lower ~ "Negative",
                                                       lower > .lower & upper < .upper ~ "Equivalent",
                                                       TRUE ~ "Unclear"),
                      rope = glue::glue("{.lower} - {.upper}")) %>%
        dplyr::select({{point_est}}, label, x_var, y_var, method, sig, rope_decision, pos_neg, pairing, rope, lower, upper, below, above, between)
    }
    else {

      unique_value <-
        data %>% pull(pmedsup) %>% unique() %>% na.omit()

      if(unique_value == "equal") {
        correlation <-
          tibble(mean = 50,
                 label = "50.0 [NA - NA]",
                 x_var = x,
                 y_var = y,
                 pairing = glue::glue("{x}{sep}{y}"),
                 pos_neg = "Neutral",
                 method = "Probability of median superiority",
                 sig = NA,
                 rope_decision = NA,
                 rope = glue::glue("{.lower} - {.upper}"),
                 lower = NA, upper = NA, below = NA, above = NA, between = NA) %>%
          dplyr::select(mean, label, x_var, y_var, method, sig, rope_decision, pos_neg, pairing, rope, lower, upper, below, above, between)

        names(correlation)[1] <- point_est

      }

      else if(unique_value == "neg") {
        correlation <-
          tibble(mean = 0,
                 label = "0.0 [NA - NA]",
                 x_var = x,
                 y_var = y,
                 pairing = glue::glue("{x}{sep}{y}"),
                 pos_neg = "Negative",
                 method = "Probability of median superiority",
                 sig = NA,
                 rope_decision = NA,
                 rope = glue::glue("{.lower} - {.upper}"),
                 lower = NA, upper = NA, below = NA, above = NA, between = NA) %>%
          dplyr::select(mean, label, x_var, y_var, method, sig, rope_decision, pos_neg, pairing, rope, lower, upper, below, above, between)

        names(correlation)[1] <- point_est

      }

      else if(unique_value == "pos") {
        correlation <-
          tibble(mean = 100,
                 label = "100.0 [NA - NA]",
                 x_var = x,
                 y_var = y,
                 pairing = glue::glue("{x}{sep}{y}"),
                 pos_neg = "Positive",
                 method = "Probability of median superiority",
                 sig = NA,
                 rope_decision = NA,
                 rope = glue::glue("{.lower} - {.upper}"),
                 lower = NA, upper = NA, below = NA, above = NA, between = NA) %>%
          dplyr::select(mean, label, x_var, y_var, method, sig, rope_decision, pos_neg, pairing, rope, lower, upper, below, above, between)

        names(correlation)[1] <- point_est

      }

    }


  }


  return(correlation)

}
