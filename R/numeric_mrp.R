#' Run Numeric MRP
#'
#' Perform the regression and epred steps of a multilevel regression and poststratification (MRP) for a numeric outcome variable.
#'
#' @param variable_name String. The name of the numeric variable to be modeled.
#' @param variable_label Optional. A human-readable label for the variable. Defaults to \code{variable_name}.
#' @param center_value Numeric. A value to center the variable (e.g., 4 to make 4/neutral become 0). Defaults to \code{set_center_value}.
#' @param my_data A data frame containing the data for the regression model. Defaults to \code{set_my_data}.
#' @param save_model Logical. If \code{TRUE}, saves the fitted model object to an mrp_models folder in your directory. Defaults to \code{save_my_model}.
#' @param save_epred Logical. If \code{TRUE}, saves the posterior epred draws to an mrp_epreds folder in your directory. Defaults to \code{save_my_epred}.
#' @param my_prior A list of prior distributions for the model. Defaults to:
#'   \itemize{
#'     \item \code{Intercept: normal(0, 2)}
#'     \item \code{Coefficients: normal(0, 1)}
#'     \item \code{Random effects: exponential(2)}
#'     \item \code{Residual standard deviation: normal(1,1), lower bound 0.01}
#'   }
#' @param my_init Initial values for the model. Default is \code{0}.
#' @param my_warmup Number of warmup iterations for the sampler. Default is \code{set_my_warmup}.
#' @param my_refresh Frequency of progress updates during model fitting. Default is \code{250}.
#' @param my_iter Total number of iterations per chain. Default is \code{set_my_iter}.
#' @param my_poststrat A data frame for poststratification. Default is \code{set_my_poststrat}.
#' @param my_adapt_delta Numeric value for the Stan sampler control parameter to improve convergence. Default is \code{set_my_adapt_delta}.
#' @param mrp_form Defaults to NULL, in which case our standard MRP formula is used, but can be changed to reflect a different MRP formula
#' @param name_addition String, defaults to "" (i.e., nothing). Can provide a string to more uniquely identify what the file will be named as.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{"model"}: The fitted \code{brms} model object.
#'     \item \code{"epred"}: Posterior predictions from the fitted model.
#'   }
#'
#' @details
#' This function performs multilevel Bayesian regression using the \code{brms} package.
#' It constructs a model formula based on a predefined structure, fits the model
#' using the \code{cmdstanr} backend, and generates posterior epred draws for
#' a target population. The model and predictions can be optionally saved for future use.
#'
#' If the variable is ordinal or categorical, it will be converted to numeric.
#' Ensure that the ordering of factors is correctly set using \code{ordered = TRUE}.
#'
#' @note Requires the \code{brms} and \code{cmdstanr} packages.
#' Ensure that the \code{my_data} and \code{my_poststrat} arguments are properly formatted.
#'
#' @export
numeric_mrp <- function(variable_name,
                        variable_label = NULL,
                        center_value = set_center_value, # e.g., 4 to make 4/neutral become 0
                        my_data = set_my_data,
                        save_model = save_my_model,
                        save_epred = save_my_epred,
                        my_prior = c(set_prior("normal(0 , 2)", class = "Intercept"),
                                     set_prior("normal(0 , 1)", class = "b"),
                                     set_prior("exponential(2)", class = "sd"),
                                     set_prior("normal(1, 1)", class = "sigma", lb = .01)),
                        my_init = 0,
                        my_refresh = 250,
                        my_iter = set_my_iter,
                        my_warmup = set_my_warmup,
                        my_poststrat = set_my_poststrat,
                        my_adapt_delta = set_my_adapt_delta,
                        mrp_form = NULL,
                        name_addition = "") {

  print("If variable is ordinal/categorical, it will be converted to numeric. Make sure that you set the ordering of factors correctly, using ordered = TRUE")
  my_data <-
    my_data %>%
    mutate(!!sym(variable_name) := as.numeric(!!sym(variable_name)))

  if(!is.null(center_value)) {
      my_data %>%
      mutate(!!sym(variable_name) := !!sym(variable_name) - center_value)
  }

  if(is.null(variable_label)) {
    variable_label <- variable_name
  }

  variable_name <-
    str_replace_all(variable_name,
                    " - | |-",
                    "_")

  names(my_data) <-
    str_replace_all(names(my_data),
                    " - | |-",
                    "_")

  if(is.null(mrp_form)) {
    formula_string <-
      " ~ (1 | state) + (1 | race) + (1 | age_fine) +
  (1 | education_collapse) + (1 | income_ces) + male + (1 | male:race) +
  (1 | education_collapse:age_fine) + (1 | partyid) + (1 | region) + repvote_sd_cent"
  }
  else {
    formula_string <-
      mrp_form
  }

  numeric_form <-
    as.formula(glue::glue("`{variable_name}`{formula_string}"))

  print(glue::glue("Running regression for {variable_label}"))

  numeric_fit <-
    brm(formula = numeric_form,
        family = gaussian(),
        data = my_data,
        control = list(adapt_delta = my_adapt_delta, max_treedepth = 15),
        prior = my_prior,
        chains = 4,
        cores = 4,
        iter = my_iter,
        warmup = my_warmup,
        init = my_init,
        backend = 'cmdstanr',
        threads = threading(4),
        seed = 1010,
        silent = 0,
        refresh = my_refresh,
        stan_model_args = list(stanc_options = list('O1')))

  print(glue::glue("Completed regression for {variable_label}"))

  print(glue::glue("Computing posterior predictions for {variable_label}"))

  numeric_epred <-
    posterior_epred(object = numeric_fit,
                    newdata = my_poststrat,
                    ndraws = 1000,
                    allow_new_levels = TRUE)

  print(glue::glue("Completed posterior predictions for {variable_label}"))

  if(save_epred == TRUE) {

    print(glue::glue("Saving posterior predictions for {variable_label}"))

    saveRDS(numeric_epred,
            file = glue::glue("mrp_epreds/{variable_name}{name_addition}_epred.rds"))
  }

  if(save_model == TRUE) {
    print(glue::glue("Saving regression model for {variable_label}"))

    save(numeric_fit,
         file = glue::glue("mrp_models/{variable_name}{name_addition}_fit.RData"))
  }

  return(list("model" = numeric_fit,
              "epred" = numeric_epred))

}
