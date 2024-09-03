#' Conduct within subjects PSup
#'
#' Performs a Bayesian within subjects probability of superiority analysis
#'
#' @param variable The name of the psup comparison variable
#' @param data The data frame where the variable is
#' @param percent Whether or not to show the PSup as a percentage format or not (defaults to FALSE)
#' @param decimals How many decimals to show in the summary label - defaults to 2
#' @param prior A prior for the analysis
#' @param prefix The prefix string, if any, in the psup comparison column - defaults to "psup_".
#' @param sep The separator string, if any, in the psup comparison column - defaults to "_min_".
#' @return A brms fit object and several posterior objects and summaries
#' @export

conduct_psup_within <- function(variable,
                                data,
                                percent = FALSE,
                                decimals = 2,
                                prior = NULL,
                                prefix = "psup_",
                                sep = "_min_",
                                intercept_prior = "normal(0 , 1.25)",
                                dpar_prior = "normal(0 , 1)",
                                my_iter = 2000,
                                my_warmup = 500,
                                weighted = FALSE,
                                weight_name = "mod_weights") {

  unique_outcomes <- data %>% pull(!!sym(variable)) %>% unique() %>% na.omit()
  print(unique_outcomes)
  n_outcomes <- length(unique_outcomes)
  print(n_outcomes)

  if(is.null(prior)) {

    if(n_outcomes == 3) {
      prior <-
        c(brms::set_prior(glue::glue("{intercept_prior}"), class = "Intercept"),
          brms::set_prior(glue::glue("{dpar_prior}"), class = "Intercept", dpar = "mu0"),
          brms::set_prior(glue::glue("{dpar_prior}"), class = "Intercept", dpar = "mu1"))
    }
    else if(n_outcomes == 2) {
      if(0.5 %in% as.numeric(unique_outcomes) == FALSE) {
        prior <-
          c(brms::set_prior(glue::glue("{intercept_prior}"), class = "Intercept"),
            brms::set_prior(glue::glue("{dpar_prior}"), class = "Intercept", dpar = "mu1"))
      }
      else if(0 %in% as.numeric(unique_outcomes) == FALSE) {
        prior <-
          c(brms::set_prior(glue::glue("{intercept_prior}"), class = "Intercept"),
            brms::set_prior(glue::glue("{dpar_prior}"), class = "Intercept", dpar = "mu1"))
      }
      else if(1 %in% as.numeric(unique_outcomes) == FALSE) {
        prior <-
          c(brms::set_prior(glue::glue("{intercept_prior}"), class = "Intercept"),
            brms::set_prior(glue::glue("{dpar_prior}"), class = "Intercept", dpar = "mu0"))
      }
    }

  }

  data <-
    data %>%
    dplyr::mutate({{variable}} := factor(!!sym(variable),
                                  levels = c("0.5", "0", "1")))

  if(weighted != TRUE) {
    psup_fit <-
      brms::brm(formula = glue::glue("{variable} ~ 1"),
                family = categorical(),
                data = data,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                prior = prior,
                chains = 4,
                cores = 4,
                iter = my_iter,
                warmup = my_warmup,
                init = 0,
                backend = 'cmdstanr',
                threads = threading(4),
                seed = 1010,
                stan_model_args = list(stanc_options = list('O1')))
  }
  else if(weighted == TRUE) {
    psup_fit <-
      brms::brm(formula = glue::glue("{variable} | weights({weight_name}) ~ 1"),
                family = categorical(),
                data = data,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                prior = prior,
                chains = 4,
                cores = 4,
                iter = my_iter,
                warmup = my_warmup,
                init = 0,
                backend = 'cmdstanr',
                threads = threading(4),
                seed = 1010,
                stan_model_args = list(stanc_options = list('O1')))
  }


  psup_category_post <-
    tidybayes::add_epred_draws(newdata = tibble(filler_col = "1"),
                               object = psup_fit,
                               value = "proportion",
                               category = "outcome",
                               ndraws = 4 * (my_iter - my_warmup)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-filler_col) %>%
    dplyr::rename(draw = .draw) %>%
    dplyr::mutate(psup_val = as.numeric(as.character(outcome)) * proportion,
           outcome = dplyr::case_when(outcome == "0.5" ~ "Equal",
                               outcome == "1" ~ "Sup",
                               outcome == "0" ~ "Inf"),
           outcome = factor(outcome,
                            levels = c("Inf", "Equal", "Sup"))) %>%
    dplyr::mutate("variable" = variable) %>%
    tidyr::separate(col = "variable",
             into = c("a", "b"),
             sep = sep,
             remove = FALSE) %>%
    dplyr::mutate(a = stringr::str_remove(a, prefix)) %>%
    dplyr::relocate(a, b)

  psup_post <-
    psup_category_post %>%
    dplyr::group_by(draw) %>%
    dplyr::summarise(psup = sum(psup_val)) %>%
    dplyr::mutate("variable" = variable) %>%
    tidyr::separate(col = "variable",
             into = c("a", "b"),
             sep = sep,
             remove = FALSE) %>%
    dplyr::mutate(a = stringr::str_remove(a, prefix)) %>%
    dplyr::relocate(a, b)

  psup_raw <-
    data %>%
    tidystats::count_data(!!sym(variable),
               na.rm = TRUE) %>%
    dplyr::mutate(psup_val = as.numeric(as.character(!!sym(variable))) * (pct / 100),
           outcome =  dplyr::case_when(!!sym(variable) == "0.5" ~ "raw_equal",
                                !!sym(variable) == "0" ~ "raw_inf",
                                !!sym(variable) == "1" ~ "raw_sup"),
           n = sum(n),
           raw_psup = sum(psup_val)) %>%
    dplyr::select(-psup_val, -{{variable}}) %>%
    tidyr::pivot_wider(names_from = outcome,
                values_from = pct)

  psup_summary <-
    psup_post %>%
    jimbilben::nice_post(psup,
                         decimals = decimals,
                         remove_lead = TRUE) %>%
    dplyr::mutate("variable" = variable) %>%
    tidyr::separate(col = "variable",
             into = c("a", "b"),
             sep = sep,
             remove = FALSE) %>%
    dplyr::mutate(a = stringr::str_remove(a, prefix)) %>%
    dplyr::relocate(a, b) %>%
    dplyr::bind_cols(psup_raw)

  psup_category_summary <-
    psup_category_post %>%
    dplyr::group_by(outcome) %>%
    jimbilben::nice_post(proportion,
                         percentage = TRUE,
                         decimals = 1,
                         remove_lead = FALSE) %>%
    dplyr::mutate("variable" = variable) %>%
    tidyr::separate(col = "variable",
             into = c("a", "b"),
             sep = sep,
             remove = FALSE) %>%
    dplyr::mutate(a = stringr::str_remove(a, prefix)) %>%
    dplyr::relocate(a, b)

  psup_raw <-
    psup_raw  %>%
    dplyr::mutate("variable" = variable) %>%
    tidyr::separate(col = "variable",
             into = c("a", "b"),
             sep = sep,
             remove = FALSE) %>%
    dplyr::mutate(a = stringr::str_remove(a, prefix)) %>%
    dplyr::relocate(a, b)

  output <-
    list(
      "psup_fit" = psup_fit,
      "psup_category_post" = psup_category_post,
      "psup_post" = psup_post,
      "psup_summary" = psup_summary,
      "psup_category_summary" = psup_category_summary,
      "psup_raw" = psup_raw
    )

  print(variable)

  return(output)

}
