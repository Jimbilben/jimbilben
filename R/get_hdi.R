#' Get highest density interval
#'
#' Provides Bayesian highest density intervals for ordinal, binary, or categorical outcomes.
#'
#' @param variable Character string, indicating the outcome variable name for which confidence interval will be generated
#' @param data Your data
#' @param type A string indicating whether to outcome is ordinal (or likert, cumulative), binary (or bernoulli, binomial), or categorical (or multinomial, nominal). Defaults to ordinal
#' @param level The level of the HDI, defaults to .95
#' @param decimals How many decimal places to show in the label. Defaults to 1
#' @param point_est Character string, either median, mean, or mode, to select which type of posterior point estimate to include in the label and as the pct output column
#' @param prior Optionally set a more specific prior for the model using the typical brms set_prior function, for example: brms::set_prior("student_t(3, 0, 1.5)", class = "Intercept")
#' @param outcome_name By default, the variable indicating the name of the outcome (e.g., Yes vs. No for binomial) will be the string provided in 'variable'. This can be overridden by specifying a different string to outcome_name.

#'
#' @export

get_hdi <- function(data, variable = "outcome", type = "ordinal", level = .95, decimals = 1, point_est = "median", prior = NULL, outcome_name = NULL) {

  type <- tolower(type)

  names(data) <- map_chr(.x = make.names(names(data)), .f = str_replace_all, pattern = "__", replacement = "")
  variable_brm <-  str_replace_all(make.names(variable), pattern = "__", replacement = "")

  if(type == "ordinal" | type == "likert" | type == "cumulative") {

    ordinal_levels <- data %>% dplyr::pull({{variable_brm}}) %>% levels()

    if(is.null(prior)) {
      ordinal_priors <- brms::set_prior("student_t(3, 0, 1.5)", class = "Intercept")
    }
    else {
      ordinal_priors <- prior
    }

    my_formula <- glue::glue("`{variable_brm}` ~ 1")

    ordinal_model <-
      brms::brm(formula = my_formula,
                family = brms::cumulative("probit"),
                data = data,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                prior = ordinal_priors,
                chains = 4,
                cores = 4,
                iter = 3500,
                warmup = 1000,
                init = 0,
                seed = 1010)

    summary <- rstantools::posterior_epred(ordinal_model,
                                           ndraws = 5000,
                                           newdata = data[1,]) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "outcome",
                          values_to = "proportion") %>%
      dplyr::mutate(outcome = str_remove(outcome, "1.")) %>%
      dplyr::group_by(outcome) %>%
      dplyr::summarise(hdi_check = length(tidybayes::hdi(proportion, level)),
                       mean = mean(proportion * 100),
                       median = median(proportion * 100),
                       mode = density(proportion * 100)$x[which.max(density(proportion * 100)$y)],
                       pct = dplyr::case_when(point_est == "median" ~ median,
                                              point_est == "mean" ~ mean,
                                              point_est == "mode" ~ mode),
                       hdi_reference = tidybayes::hdi(proportion, level),
                       lower = hdi_reference[ , 1] * 100,
                       upper = hdi_reference[ , 2] * 100,
                       lower_eti = quantile(proportion * 100, 0 + ((1 - level) / 2)),
                       upper_eti = quantile(proportion * 100, 1 - ((1 - level) / 2)),
                       label = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper, remove_lead = FALSE, decimals = decimals)}]'),
                       label_eti = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower_eti, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper_eti, remove_lead = FALSE, decimals = decimals)}]'),
                       level = level) %>%
      dplyr::select(outcome, pct, lower:upper_eti, label, label_eti, mean, median, mode, level, hdi_check)

    if(!is.null(ordinal_levels)) {
      summary <-
        summary %>%
        dplyr::mutate(outcome = factor(outcome,
                                       levels = ordinal_levels,
                                       ordered = TRUE))
    }

    if(is.null(outcome_name)) {
      names(summary)[1] <- variable
    }
    else {
      names(summary)[1] <- outcome_name
    }

    return(summary %>% tibble::as_tibble())

  }


  else if(type == "binomial" | type == "bernoulli" | type == "binary") {

    bernoulli_levels <-
      data %>% dplyr::pull({{variable_brm}}) %>% levels()

    print(bernoulli_levels)

    if(is.null(prior)) {
      bernoulli_priors <- brms::set_prior("student_t(3, 0 , 2.5)", class = "Intercept")
    }
    else {
      bernoulli_priors <- prior
    }

    my_formula <- glue::glue("`{variable_brm}` ~ 1")

    print(my_formula)

    bernoulli_model <-
      brms::brm(formula = my_formula,
                family = brms::bernoulli("logit"),
                data = data,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                prior = bernoulli_priors,
                chains = 4,
                cores = 4,
                iter = 3000,
                warmup = 1000,
                init = 0,
                seed = 1010)

    summary <- rstantools::posterior_epred(bernoulli_model,
                                           ndraws = 5000,
                                           newdata = data[1,]) %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      dplyr::rename(proportion = 1) %>%
      dplyr::summarise(hdi_check = length(tidybayes::hdi(proportion, level)),
                       mean = mean(proportion * 100),
                       median = median(proportion * 100),
                       mode = density(proportion * 100)$x[which.max(density(proportion * 100)$y)],
                       pct = dplyr::case_when(point_est == "median" ~ median,
                                              point_est == "mean" ~ mean,
                                              point_est == "mode" ~ mode),
                       hdi_reference = tidybayes::hdi(proportion, level),
                       lower = hdi_reference[ , 1] * 100,
                       upper = hdi_reference[ , 2] * 100,
                       lower_eti = quantile(proportion * 100, 0 + ((1 - level) / 2)),
                       upper_eti = quantile(proportion * 100, 1 - ((1 - level) / 2)),
                       label = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper, remove_lead = FALSE, decimals = decimals)}]'),
                       label_eti = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower_eti, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper_eti, remove_lead = FALSE, decimals = decimals)}]'),
                       level = level) %>%
      dplyr::select(pct, lower:upper_eti, label, label_eti, mean, median, mode, hdi_check)

    summary_2 <-
      summary %>%
      mutate(pct = 100 - pct,
             lower = 100 - summary[[1, "upper"]],
             upper = 100 - summary[[1, "lower"]],
             lower_eti = 100 - summary[[1, "upper_eti"]],
             upper_eti = 100 - summary[[1, "lower_eti"]],
             mean = 100 - summary[[1, "mean"]],
             median = 100 - summary[[1, "median"]],
             mode = 100 - summary[[1, "mode"]],
             label = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper, remove_lead = FALSE, decimals = decimals)}]'),
             label_eti = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower_eti, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper_eti, remove_lead = FALSE, decimals = decimals)}]'))

    summary <-
      bind_rows(summary,
                summary_2)

    if(!is.null(bernoulli_levels)) {
      summary <-
        summary %>%
        mutate(outcome = bernoulli_levels,
               outcome = factor(bernoulli_levels,
                                levels = bernoulli_levels)) %>%
        relocate(outcome)
    }
    else {
      summary <-
        summary %>%
        mutate(outcome = data %>% dplyr::pull({{variable_brm}}) %>% unique() %>% sort() %>% rev()) %>%
        relocate(outcome)
    }

    if(is.null(outcome_name)) {
      names(summary)[1] <- variable
    }
    else {
      names(summary)[1] <- outcome_name
    }

    return(summary %>% tibble::as_tibble())

  }

  else if(type == "nominal" | type == "categorical" | type == "multinomial") {

    categorical_levels <- data %>% dplyr::pull({{variable_brm}}) %>% levels()

    if(is.null(prior)) {
      categorical_priors <- brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept")
    }
    else {
      categorical_priors <- prior
    }

    my_formula <- glue::glue("`{variable_brm}` ~ 1")

    categorical_model <-
      brms::brm(formula = my_formula,
                family = brms::categorical("logit"),
                data = data,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                prior = categorical_priors,
                chains = 4,
                cores = 4,
                iter = 3000,
                warmup = 1000,
                init = 0,
                seed = 1010)

    summary <- rstantools::posterior_epred(categorical_model,
                                           ndraws = 5000,
                                           newdata = data[1,]) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "outcome",
                          values_to = "proportion") %>%
      dplyr::mutate(outcome = str_remove(outcome, "1.")) %>%
      dplyr::group_by(outcome) %>%
      dplyr::summarise(hdi_check = length(tidybayes::hdi(proportion, level)),
                       mean = mean(proportion * 100),
                       median = median(proportion * 100),
                       mode = density(proportion * 100)$x[which.max(density(proportion * 100)$y)],
                       pct = dplyr::case_when(point_est == "median" ~ median,
                                              point_est == "mean" ~ mean,
                                              point_est == "mode" ~ mode),
                       hdi_reference = tidybayes::hdi(proportion, level),
                       lower = hdi_reference[ , 1] * 100,
                       upper = hdi_reference[ , 2] * 100,
                       lower_eti = quantile(proportion * 100, 0 + ((1 - level) / 2)),
                       upper_eti = quantile(proportion * 100, 1 - ((1 - level) / 2)),
                       label = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper, remove_lead = FALSE, decimals = decimals)}]'),
                       label_eti = glue::glue('{jimbilben::nice_num(pct, remove_lead = FALSE, decimals = decimals)} [{jimbilben::nice_num(lower_eti, remove_lead = FALSE, decimals = decimals)} - {jimbilben::nice_num(upper_eti, remove_lead = FALSE, decimals = decimals)}]'),
                       level = level) %>%
      dplyr::select(outcome, pct, lower:upper_eti, label, label_eti, mean, median, mode, level, hdi_check)

    if(!is.null(categorical_levels)) {

      summary <-
        summary %>%
        dplyr::mutate(outcome = factor(outcome,
                                       levels = categorical_levels))

    }

    if(is.null(outcome_name)) {
      names(summary)[1] <- variable
    }
    else {
      names(summary)[1] <- outcome_name
    }

    return(summary %>% tibble::as_tibble())

  }

}
