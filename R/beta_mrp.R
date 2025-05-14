#' Fit Beta MRP Model
#'
#' Perform multilevel regression for a continuous outcome variable using a Beta distribution.
#'
#' @param variable_name String. The name of the continuous variable to be modeled.
#' @param variable_label Optional. A human-readable label for the variable. Defaults to \code{variable_name}.
#' @param convert_beta Logical. Whether to transform the outcome variable to be suitable for Beta regression. Defaults to \code{TRUE}.
#' @param my_data A data frame containing the data for the regression model. Defaults to \code{set_my_data}.
#' @param save_model Logical. If \code{TRUE}, saves the fitted model object. Defaults to \code{save_my_model}.
#' @param save_epred Logical. If \code{TRUE}, saves the posterior predictive draws. Defaults to \code{save_my_epred}.
#' @param my_draw_ids Numeric vector. Indices of posterior draws to use for predictions. Defaults to a sequence of 1000 evenly spaced draws from 4000 iterations.
#' @param my_prior A list of prior distributions for the model. Defaults to:
#'   \itemize{
#'     \item \code{"normal(0 , 1)"} for the intercept.
#'     \item \code{"normal(0 , 0.67)"} for the coefficients.
#'     \item \code{"exponential(2)"} for random effects.
#'     \item \code{"normal(5 , 2.5)"} for the precision parameter \code{phi}, with a lower bound of \code{0.01}.
#'   }
#' @param my_init Initial values for the model. Defaults to \code{0}.
#' @param my_refresh Numeric. Frequency of progress updates during model fitting. Defaults to \code{250}.
#' @param my_iter Numeric. Total number of iterations per chain. Defaults to \code{set_my_iter}.
#' @param my_warmup Numeric. Number of warmup iterations for the sampler. Defaults to \code{set_my_warmup}.
#' @param my_poststrat A data frame for poststratification. Defaults to \code{set_my_poststrat}.
#' @param my_adapt_delta Numeric. The Stan sampler control parameter to improve convergence. Defaults to \code{set_my_adapt_delta}.
#' @param mrp_form Optional string specifying a custom formula for the regression model. If \code{NULL}, a default formula is used.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{"model"}: The fitted \code{brms} model object.
#'     \item \code{"epred"}: Posterior epred from the fitted model.
#'     \item \code{"phi"}: Posterior draws of the precision parameter \code{phi}.
#'   }
#'
#' @details
#' This function fits a Beta regression model with multilevel structure using the \code{brms} package and \code{cmdstanr} backend.
#' If \code{convert_beta} is set to \code{TRUE}, the outcome variable is transformed to fit within the (0, 1) range using \code{jimbilben::transform_beta}.
#' The function constructs a model formula based on a default or user-specified structure, applies user-defined priors, and fits the model.
#' Posterior predictions and the precision parameter \code{phi} are returned, and optionally saved for future use.

#' @export
beta_mrp <- function(variable_name,
                     variable_label = NULL,
                     convert_beta = TRUE,
                     my_data = set_my_data,
                     save_model = save_my_model,
                     save_epred = save_my_epred,
                     my_draw_ids = seq(from = 1, to = 4000, length.out = 1000) %>% round(),
                     my_prior = c(set_prior("normal(0 , 1)", class = "Intercept"),
                                  set_prior("normal(0 , .67)", class = "b"),
                                  set_prior("exponential(2)", class = "sd"),
                                  set_prior("normal(5 , 2.5)", class = "phi", lb = .01)),
                     my_init = 0,
                     my_refresh = 250,
                     my_iter = set_my_iter,
                     my_warmup = set_my_warmup,
                     my_poststrat = set_my_poststrat,
                     my_adapt_delta = set_my_adapt_delta,
                     mrp_form = NULL,
                     name_addition  = "") {

  if(convert_beta == TRUE) {
    beta_n <-
      my_data %>%
      dplyr::pull(!!sym(variable_name))

    beta_n <-
      sum(!is.na(beta_n))

    print(beta_n)
    my_data <-
      my_data %>%
      dplyr::mutate(!!sym(variable_name) := jimbilben::transform_beta(!!sym(variable_name), sample_size = beta_n))

  }

  if(is.null(variable_label)) {
    variable_label <- variable_name
  }

  variable_name <-
    stringr::str_replace_all(variable_name,
                             " - | |-",
                             "_")

  names(my_data) <-
    stringr::str_replace_all(names(my_data),
                             " - | |-",
                             "_")

  if(is.null(mrp_form)) {

    formula_string <-
      " ~ (1 | state) + (1 | race) + (1 | age_fine) +
    (1 | education_collapse) + (1 | income_ces) + male + (1 | male:race) + (1 | male:age_fine) +
    (1 | education_collapse:age_fine) + (1 | partyid) + (1 | region) + repvote_cent"
  }
  else {
    formula_string <- mrp_form
  }

  beta_form <-
    as.formula(glue::glue("`{variable_name}`{formula_string}"))

  print(glue::glue("Running regression for {variable_label}"))

  beta_fit <-
    brms::brm(formula = beta_form,
              family = brms::Beta(),
              data = my_data,
              control = list(adapt_delta = my_adapt_delta, max_treedepth = 15),
              prior = my_prior,
              chains = 4,
              cores = 4,
              iter = my_iter,
              warmup = my_warmup,
              init = my_init,
              backend = 'cmdstanr',
              threads = brms::threading(4),
              seed = 1010,
              silent = 0,
              refresh = my_refresh,
              stan_model_args = list(stanc_options = list('O1')))

  print(glue::glue("Completed regression for {variable_label}"))

  print(glue::glue("Computing posterior predictions for {variable_label}"))

  beta_epred <-
    rstantools::posterior_epred(object = beta_fit,
                                newdata = my_poststrat,
                                ndraws = 1000,
                                draw_ids = my_draw_ids,
                                allow_new_levels = TRUE)

  beta_phi <-
    beta_fit %>%
    posterior::as_draws_df() %>%
    tibble::as_tibble() %>%
    dplyr::select(phi)

  beta_phi <-
    beta_phi[my_draw_ids,]

  print(glue::glue("Completed posterior predictions for {variable_label}"))


  if(save_epred == TRUE) {
    print(glue::glue("Saving posterior predictions for {variable_label}"))

    saveRDS(beta_epred,
            file = glue::glue("mrp_epreds/{variable_name}{name_addition}_epred.rds"))

    saveRDS(beta_phi,
            file = glue::glue("mrp_epreds/{variable_name}{name_addition}_epred_phi.rds"))

  }

  if(save_model == TRUE) {
    print(glue::glue("Saving regression model for {variable_label}"))

    save(beta_fit,
         file = glue::glue("mrp_models/{variable_name}{name_addition}_fit.RData"))
  }

  return(list("model" = beta_fit,
              "epred" = beta_epred,
              "phi" = beta_phi))

}
