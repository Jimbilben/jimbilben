#' Weighted Categorical Regression
#'
#' This function performs a weighted regression analysis for categorical variables using Bayesian methods.
#' It estimates the proportions of each category adjusted by weights provided in the data.
#'
#' @param data A dataframe containing the categorical variable and weights used in the model.
#' @param var_name A string specifying the name of the categorical variable of interest.
#' @param categ_prior A list of priors for the model's intercepts, typically including normal priors for the intercepts
#'        with mean 0 and standard deviation 1.
#'
#' @export
#' @examples
#' weighted_cat(data = my_data, var_name = "my_category_var")
#'
#' @return A dataframe summarizing the estimated proportions for each category of the variable,
#'         neatly formatted with the variable name relocated to the first column and all numeric results rounded
#'         to one decimal place. Additional formatting modifies the variable name in specific contexts.
#'
#' @details
#' The function fits a Bayesian categorical model using `brms::brm` with weighted data to accurately represent the
#' distribution of categories as influenced by the specified weights. Posterior predictions are extracted using
#' `tidybayes::add_epred_draws` and summarized using `jimbilben::nice_post` to calculate mean proportions and express
#' them as percentages.
#'
#' @note To successfully run this function, ensure the following packages are installed and loaded: `brms`, `tidybayes`,
#'       `dplyr`, `tibble`, `glue`, and `stringr` for string manipulations in the final output adjustments.
weighted_cat <- function(data,
                         var_name,
                         categ_prior = c(brms::set_prior("normal(0 , 1)", class = "Intercept"))) {


  categ_fit <-
    brms::brm(formula = glue::glue("{var_name} | weights(mod_weights) ~ 1"),
              family = brms::categorical(),
              data = data,
              control = list(adapt_delta = 0.96, max_treedepth = 15),
              prior = categ_prior,
              chains = 4,
              cores = 4,
              iter = 2000,
              warmup = 500,
              init = 0,
              backend = 'cmdstanr',
              threads = brms::threading(4),
              seed = 1010,
              silent = 0,
              refresh = 100,
              stan_model_args = list(stanc_options = list('O1')))

  categ_epred <-
    tidybayes::add_epred_draws(
      object = categ_fit,
      newdata = tibble::tibble(guide = 1),
      ndraws = 1000
    ) %>%
    dplyr::rename(proportion = .epred,
                  response = .category)

  categ_summary <-
    jimbilben::nice_post(categ_epred %>% dplyr::group_by(response),
                         estimate = proportion,
                         percentage = TRUE,
                         point_est = "mean",
                         decimals = 1) %>%
    dplyr::mutate(outcome = var_name)  %>%
    dplyr::relocate(outcome) %>%
    dplyr::mutate(outcome = str_replace(outcome, "timeline", "Statement "))

  print(var_name)

  return(categ_summary)

}
