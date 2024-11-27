#' Fit Categorical MRP Model
#'
#' Perform multilevel regression for a categorical outcome variable using the \code{brms} package.
#'
#' @param variable_name String. The name of the categorical variable to be modeled.
#' @param my_data A data frame containing the data for the regression model. Defaults to \code{my_data}.
#' @param variable_label Optional. A human-readable label for the variable. Defaults to \code{variable_name}.
#' @param reference_level String. The reference level for the categorical variable. Defaults to \code{"Neither agree nor disagree"}.
#' @param my_intercept_prior String. Prior for the intercept. Defaults to \code{"normal(0, 2)"}.
#' @param my_b_prior String. Prior for the coefficients. Defaults to \code{"normal(0, 1)"}.
#' @param my_sd_prior String. Prior for the standard deviation of random effects. Defaults to \code{"exponential(3)"}.
#' @param save_model Logical. If \code{TRUE}, saves the fitted model object. Defaults to \code{save_my_model}.
#' @param my_init Initial values for the model. Defaults to \code{0}.
#' @param my_refresh Numeric. Frequency of progress updates during model fitting. Defaults to \code{100}.
#' @param my_iter Numeric. Total number of iterations per chain. Defaults to \code{set_my_iter}.
#' @param my_warmup Numeric. Number of warmup iterations for the sampler. Defaults to \code{set_my_warmup}.
#' @param my_adapt_delta Numeric. The Stan sampler control parameter to improve convergence. Defaults to \code{0.99}.
#' @param mrp_form Optional string specifying a custom formula for the regression model. If \code{NULL}, a default formula is used.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{"model"}: The fitted \code{brms} model object.
#'   }
#'
#' @details
#' This function fits a categorical multilevel regression model using \code{brms} with \code{cmdstanr} as the backend.
#' It applies specified priors to the intercept, coefficients, and random effects using \code{mrp_cat_prior}.
#' The function constructs a default or user-specified model formula, adjusts the categorical variable's reference level, and fits the model.
#' Optionally, the fitted model can be saved for future use.
#'
categorical_mrp_fit <- function(variable_name,
                                my_data = my_data,
                                variable_label = NULL,
                                reference_level = "Neither agree nor disagree",
                                my_intercept_prior = "normal(0, 2)",
                                my_b_prior = "normal(0, 1)",
                                my_sd_prior = "exponential(3)",
                                save_model = save_my_model,
                                my_init = 0,
                                my_refresh = 100,
                                my_iter = set_my_iter,
                                my_warmup = set_my_warmup,
                                my_adapt_delta = .99,
                                mrp_form = NULL) {

  # set up categorical priors
  my_data <-
    dplyr::mutate(my_data,
           !!sym(variable_name) := forcats::fct_relevel(!!sym(variable_name),
                                               reference_level))


  cat_prior <-
    jimbilben::mrp_cat_prior(categorical_var = my_data %>% pull(!!sym(variable_name)),
                             intercept_prior = my_intercept_prior,
                             b_prior = my_b_prior,
                             sd_prior = my_sd_prior)

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

  cat_form <-
    as.formula(glue::glue("`{variable_name}`{formula_string}"))

  print(glue::glue("Running regression for {variable_label}"))

  cat_fit <-
    brms::brm(formula = cat_form,
        family = brms::categorical(),
        data = my_data,
        control = list(adapt_delta = my_adapt_delta, max_treedepth = 15),
        prior = cat_prior,
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

  if(save_model == TRUE) {
    print(glue::glue("Saving regression model for {variable_label}"))

    save(cat_fit,
         file = glue::glue("mrp_models/{variable_label}_fit.RData"))
  }

  return(list("model" = cat_fit))

}
