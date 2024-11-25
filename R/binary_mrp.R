#' Run Binary MRP
#'
#' Perform the regression and epred steps of a multilevel regression and poststratification (MRP) for a binary outcome variable.
#'
#' @param variable_name String. The name of the binary variable to be modeled.
#' @param variable_label Optional. A human-readable label for the variable. Defaults to \code{variable_name}.
#' @param my_data A data frame containing the data for the regression model. Defaults to \code{set_my_data}.
#' @param save_model Logical. If \code{TRUE}, saves the fitted model object to an mrp_models folder in your directory. Defaults to \code{save_my_model}.
#' @param save_epred Logical. If \code{TRUE}, saves the posterior epred draws to an mrp_epreds folder in your directory. Defaults to \code{save_my_epred}.
#' @param my_prior A list of prior distributions for the model. Defaults to:
#'   \itemize{
#'     \item \code{Intercept: normal(0, 2)}
#'     \item \code{Coefficients: normal(0, 1)}
#'     \item \code{Random effects: exponential(2)}
#'   }
#' @param my_init Initial values for the model. Default is \code{0}.
#' @param my_warmup Number of warmup iterations for the sampler. Default is \code{set_my_warmup}.
#' @param my_refresh Frequency of progress updates during model fitting. Default is \code{250}.
#' @param my_iter Total number of iterations per chain. Default is \code{set_my_iter}.
#' @param my_poststrat A data frame for poststratification. Default is \code{set_my_poststrat}.
#' @param my_adapt_delta Numeric value for the Stan sampler control parameter to improve convergence. Default is \code{set_my_adapt_delta}.
#' @param weighted Logical. If \code{TRUE}, performs weighted regression using the specified weights. Defaults to \code{FALSE}.
#' @param weight_name The name of the weight variable in \code{my_data}. Defaults to \code{"mod_weights_mrp"}.
#' @param mrp_form Optional string specifying a custom formula for the regression model. If \code{NULL}, a default formula is used.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{"model"}: The fitted \code{brms} model object.
#'     \item \code{"epred"}: Posterior predictions from the fitted model.
#'   }
#'
#' @details
#' This function performs multilevel Bayesian regression using the \code{brms} package.
#' It constructs a model formula based on the specified or default structure, fits the model
#' using the \code{cmdstanr} backend, and generates posterior epred draws for
#' a target population. The model and predictions can be optionally saved for future use.
#'
#'
#' @note Requires the \code{brms} and \code{cmdstanr} packages.
#' Ensure that the \code{my_data} and \code{my_poststrat} arguments are properly formatted.
#'
#' @export

binary_mrp <- function(variable_name,
                       variable_label = NULL,
                       my_data = set_my_data,
                       save_model = save_my_model,
                       save_epred = save_my_epred,
                       my_prior = c(set_prior("normal(0 , 2)", class = "Intercept"),
                                    set_prior("normal(0 , 1)", class = "b"),
                                    set_prior("exponential(2)", class = "sd")),
                       my_init = 0,
                       my_warmup = set_my_warmup,
                       my_refresh = 250,
                       my_iter = set_my_iter,
                       my_poststrat = set_my_poststrat,
                       my_adapt_delta = set_my_adapt_delta,
                       weighted = FALSE,
                       weight_name = "mod_weights_mrp",
                       mrp_form = NULL) {

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
    if(weighted == FALSE) {
      formula_string <-
        " ~ (1 | state) + (1 | race) + (1 | age_fine) +
    (1 | education_collapse) + (1 | income_ces) + male + (1 | male:race) + (1 | male:age_fine) +
    (1 | education_collapse:age_fine) + (1 | partyid) + (1 | region) + repvote_cent"
    }
    else if(weighted == TRUE) {
      formula_string <- " | weights(mod_weights_mrp) ~ (1 | state) + (1 | race) + (1 | age_fine) +
    (1 | education_collapse) + (1 | income_ces) + male + (1 | male:race) + (1 | male:age_fine) +
    (1 | education_collapse:age_fine) + (1 | partyid) + (1 | region) + repvote_cent"
    }
  }
  else {
    formula_string <- mrp_form
  }

  binary_form <-
    as.formula(glue::glue("`{variable_name}`{formula_string}"))

  print(glue::glue("Running regression for {variable_label}"))

  binary_fit <-
    brms::brm(formula = binary_form,
        family = brms::bernoulli(),
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

  binary_epred <-
    rstantools::posterior_epred(object = binary_fit,
                    newdata = my_poststrat,
                    ndraws = 1000,
                    allow_new_levels = TRUE)

  print(glue::glue("Completed posterior predictions for {variable_label}"))


  if(save_epred == TRUE) {
    print(glue::glue("Saving posterior predictions for {variable_label}"))

    if(weighted == TRUE) {
      saveRDS(binary_epred,
              file = glue::glue("mrp_epreds/{variable_name}_weighted_epred.rds"))
    }
    else {
      saveRDS(binary_epred,
              file = glue::glue("mrp_epreds/{variable_name}_epred.rds"))
    }

  }

  if(save_model == TRUE) {
    print(glue::glue("Saving regression model for {variable_label}"))

    if(weighted == TRUE) {
      save(binary_fit,
           file = glue::glue("mrp_models/{variable_name}_weighted_fit.RData"))
    }
    else {
      saveRDS(binary_fit,
              file = glue::glue("mrp_models/{variable_name}_fit.RData"))
    }

  }

  return(list("model" = binary_fit,
              "epred" = binary_epred))

}
