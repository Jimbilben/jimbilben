#' Get confidence interval
#'
#' Provides frequentist confidence intervals for ordinal, binary, or categorical outcomes.
#'
#' @param variable Character string, indicating the outcome variable name for which confidence interval will be generated
#' @param data Your data
#' @param type A string indicating whether to outcome is ordinal (or likert, cumulative), binary (or bernoulli, binomial), or categorical (or multinomial, nominal). Defaults to ordinal
#' @param level The level of the confidence interval, defaults to .95
#' @param decimals How many decimal places to show in the label. Defaults to 1

#'
#' @export

get_ci <- function(variable = "outcome", data, type = "ordinal", level = .95, decimals = 1) {

  type <- tolower(type)

  if(type == "ordinal" | type == "likert" | type == "cumulative") {
    cumulative_model <-
      ordinal::clm(
        formula = glue::glue("{variable} ~ 1"),
        data = data,
        link = "probit"
      )

    summary <-
      emmeans::emmeans(cumulative_model,
                       {{variable}},
                       mode = "prob",
                       level = level) %>%
      tibble::as_tibble() %>%
      dplyr::rename(std_error = SE,
                    lower_prob = asymp.LCL,
                    upper_prob = asymp.UCL) %>%
      dplyr::mutate(pct = prob * 100,
                    lower = lower_prob * 100,
                    upper = upper_prob * 100,
                    std_error = std_error * 100,
                    label = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper, remove_lead = FALSE, decimals = decimals)}]'),
                    level = level) %>%
      dplyr::relocate(1, pct, lower, upper, label, std_error, level, prob, lower_prob, upper_prob)

    return(summary)
  }

  else if(type == "binomial" | type == "bernoulli" | type == "binary") {

    binom_form <- as.formula(glue::glue("{variable} ~ 1"))

    data <- data.frame(data)

    if(is.logical(data[, variable]) == FALSE & is.numeric(data[, variable]) == FALSE) {

      counted_data <-
        data %>%
        tidystats::count_data(eval(as.symbol(variable)), na.rm = TRUE) %>%
        dplyr::arrange(-n)

      new_data <-
        tibble::tibble(outcome = c(rep(1, counted_data[[1, "n"]]), rep(0, counted_data[[2, "n"]])))

      binom_model <-
        glm(formula = outcome ~ 1,
            family = binomial,
            data = new_data)

      summary <-
        emmeans::emmeans(binom_model,
                specs = ~ 1,
                type = "response",
                level = level) %>%
        tibble::as_tibble() %>%
        dplyr::rename(std_error = SE,
               lower_prob = asymp.LCL,
               upper_prob = asymp.UCL) %>%
        dplyr::mutate(pct = prob * 100,
                      lower = lower_prob * 100,
                      upper = upper_prob * 100,
                      std_error = std_error * 100,
                      level = level) %>%
        dplyr::relocate(1, pct, lower, upper, std_error, level, prob, lower_prob, upper_prob) %>%
        dplyr::mutate(label = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper, remove_lead = FALSE, decimals = decimals)}]')) %>%
        dplyr::relocate(label, .after = upper)

      summary_yes <-
        summary

      summary_no <-
        summary %>%
        dplyr::mutate(pct = 100 - summary_yes[[1, "pct"]],
                      lower = 100 - summary_yes[[1, "upper"]],
                      upper = 100 - summary_yes[[1, "lower"]],
                      prob = 1 - summary_yes[[1, "prob"]],
                      lower_prob = 1 - summary_yes[[1, "upper_prob"]],
                      upper_prob = 1 - summary_yes[[1, "lower_prob"]]) %>%
        dplyr::mutate(label = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper, remove_lead = FALSE, decimals = decimals)}]')) %>%
        dplyr::relocate(label, .after = upper)

      summary <-
        bind_rows(
          summary_yes,
          summary_no
        )

      names(summary)[1] <- variable
      summary[,1] <- c(counted_data[[1, 1]], counted_data[[2, 1]])

    }

    else {

      binom_model <-
        glm(formula = binom_form,
            family = binomial,
            data = data)

      summary <-
        emmeans::emmeans(binom_model,
                specs = ~ 1,
                type = "response",
                level = level) %>%
        tibble::as_tibble() %>%
        dplyr::rename(std_error = SE,
               lower_prob = asymp.LCL,
               upper_prob = asymp.UCL) %>%
        dplyr::mutate(pct = prob * 100,
                      lower = lower_prob * 100,
                      upper = upper_prob * 100,
                      std_error = std_error * 100,
                      level = level) %>%
        dplyr::relocate(1, pct, lower, upper, std_error, level, prob, lower_prob, upper_prob) %>%
        dplyr::mutate(label = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper, remove_lead = FALSE, decimals = decimals)}]')) %>%
        dplyr::relocate(label, .after = upper)

      summary_yes <-
        summary

      summary_no <-
        summary %>%
        dplyr::mutate(pct = 100 - summary_yes[[1, "pct"]],
                      lower = 100 - summary_yes[[1, "upper"]],
                      upper = 100 - summary_yes[[1, "lower"]],
                      prob = 1 - summary_yes[[1, "prob"]],
                      lower_prob = 1 - summary_yes[[1, "upper_prob"]],
                      upper_prob = 1 - summary_yes[[1, "lower_prob"]]) %>%
        dplyr::mutate(label = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper, remove_lead = FALSE, decimals = decimals)}]')) %>%
        dplyr::relocate(label, .after = upper)

      summary <-
        bind_rows(
          summary_yes,
          summary_no
        )

      names(summary)[1] <- variable
      summary[,1] <- c(1, 0)

    }

    return(summary)
  }

  else if(type == "nominal" | type == "categorical" | type == "multinomial") {
    multinom_form <- as.formula(glue::glue("{variable} ~ 1"))

    multinom_model <-
      nnet::multinom(formula = multinom_form,
                     data = data)

    summary <-
      emmeans::emmeans(multinom_model,
                       {{variable}},
                       level = level,
                       type = "response") %>%
      tibble::as_tibble() %>%
      dplyr::rename(std_error = SE,
                    lower_prob = lower.CL,
                    upper_prob = upper.CL) %>%
      dplyr::mutate(pct = prob * 100,
                    lower = lower_prob * 100,
                    upper = upper_prob * 100,
                    std_error = std_error * 100,
                    level = level,
                    label = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper, remove_lead = FALSE, decimals = decimals)}]')) %>%
      dplyr::relocate(1, pct, lower, upper, label, std_error, level, prob, lower_prob, upper_prob)

    return(summary)

  }

}
