#' Weighted Probability of Superiority
#'
#' This performs a weighted regression to generate probability of superiority.
#'
#' @param data A dataframe containing the psup comparison variable and weights used in the model.
#' @param var_name A string specifying the name of the psup variable of interest.
#' @param psup_prior A list of priors for the model's intercepts, tailored for the categorical levels and weights. By default,
#'         it includes normal priors for three intercepts with mean 0 and standard deviation 1.
#'
#' @export
#' @examples
#' weighted_psup(data = my_data, var_name = "my_psup_var")
#'
#' @return A dataframe summarizing the weighted psup values for the target variable.
#'
#' @details
#' The function internally fits a Bayesian categorical model with weights using `brms::brm` and converts the values to PSup. Posterior predictions
#' are performed via `tidybayes::add_epred_draws` to generate expected PSup values. The final output is
#' processed with `jimbilben::nice_post` to produce a clean summary table.
#'
#' @note Ensure that `brms`, `tidybayes`, `dplyr`, `tibble`, and `glue` packages are installed and loaded into your R session
#'       to use this function without issues.

weighted_psup <- function(data,
                          var_name,
                          psup_prior = c(brms::set_prior("normal(0 , 1)", class = "Intercept"),
                                         brms::set_prior("normal(0 , 1)", class = "Intercept", dpar = "mu0"),
                                         brms::set_prior("normal(0 , 1)", class = "Intercept", dpar = "mu1"))) {

  psup_fit <-
    brms::brm(formula = glue::glue("{var_name} | weights(mod_weights) ~ 1"),
              family = brms::categorical(),
              data = data,
              control = list(adapt_delta = 0.96, max_treedepth = 15),
              prior = psup_prior,
              chains = 4,
              cores = 4,
              iter = 2000,
              warmup = 500,
              init = 0,
              backend = 'cmdstanr',
              threads = threading(4),
              seed = 1010,
              silent = 0,
              refresh = 100,
              stan_model_args = list(stanc_options = list('O1')))

  psup_epred <-
    tidybayes::add_epred_draws(
      object = psup_fit,
      newdata = tibble::tibble(guide = 1),
      ndraws = 1000
    )

  psup_post <-
    psup_epred %>%
    dplyr::group_by(.draw) %>%
    dplyr::mutate(.category = as.numeric(as.character(.category)),
                  psup = .category * .epred) %>%
    dplyr::summarise(psup = sum(psup))

  psup_summary <-
    jimbilben::nice_post(psup_post,
                         estimate = psup) %>%
    dplyr::mutate(comparison = var_name)

  print(var_name)

  return(psup_summary)

}
