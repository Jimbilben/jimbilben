#' Rapid MRP with brms
#'
#' A convenience function to run Multilevel Regression and Poststratification (MRP) using the brms package with reasonable default settings.
#'
#' @param data The data frame containing the variables used in the MRP model
#' @param variable The name of the outcome variable (Default: "outcome")
#' @param pred_formula The predictor formula for the MRP model (Default: "~ (1 | state) + (1 | race) + (1 | age_fine) +
#' (1 | education_collapse) + (1 | income_ces) + male + (1 | male:race) +
#' (1 | education_collapse:age_fine) + (1 | partyid) + factor(region) + z_repvote")
#' @param .prior A character vector of prior specifications compatible with the brms package
#' @param .family The family of the outcome variable (Default: brms::categorical())
#' @param .iter The number of iterations for the MCMC sampler (Default: 4000)
#' @param .warmup The number of warmup iterations (Default: 1000)
#' @param zero_init A logical value indicating whether to initialize parameters at zero (Default: TRUE)
#' @param .chains The number of chains for the MCMC sampler (Default: 4)
#' @param .cores The number of cores used for parallel computation (Default: 4)
#' @param .control A list of control arguments for the MCMC sampler (Default: list(adapt_delta = .995, max_treedepth = 15))
#' @param .stan_model_args A list of arguments to be passed to the Stan model (Default: list(stanc_options = list('O1')))
#' @param .threading The number of threads for within-chain parallelization (Default: 8)
#' @param save_to_file A logical value indicating whether to save the model to a file (Default: TRUE)
#' @param .seed The random seed for reproducibility (Default: 1010)
#'
#' @return A brmsfit object containing the results of the MRP model
#' @export
#'
#' @examples
#' # Example with a data frame containing MRP variables
#' # data <- read.csv("path/to/your/data.csv")
#' # priors <- mrp_cat_prior(data$region)
#' # brmrp(data, variable = "outcome", .prior = priors)

brmrp <- function(data, variable = "outcome", pred_formula = "~ (1 | state) + (1 | race) + (1 | age_fine) +
                                 (1 | education_collapse) + (1 | income_ces) + male + (1 | male:race) +
                                 (1 | education_collapse:age_fine) + (1 | partyid) + factor(region) + z_repvote", .prior, .family = brms::categorical(), .iter = 4000, .warmup = 1000, zero_init = TRUE, .chains = 4, .cores = 4, .control = list(adapt_delta = .995, max_treedepth = 15), .stan_model_args = list(stanc_options = list('O1')), .threading = 8, save_to_file = TRUE, .seed = 1010) {

  my_formula <- as.formula(paste(variable, pred_formula))

  t1 <- Sys.time()

  if(zero_init == TRUE) {
    brms_model <- brms::brm(formula = my_formula,
                            family = .family,
                            data = data,
                            control = .control,
                            prior = .prior,
                            chains = .chains,
                            cores = .cores,
                            iter = .iter,
                            warmup = .warmup,
                            init = 0,
                            backend = 'cmdstanr',
                            threads = brms::threading(.threading),
                            seed = .seed,
                            stan_model_args = .stan_model_args)
  }
  else {
    brms_model <- brms::brm(formula = my_formula,
                            family = .family,
                            data = data,
                            control = list(adapt_delta = 0.995, max_treedepth = 15),
                            prior = .prior,
                            chains = .chains,
                            cores = .cores,
                            iter = .iter,
                            warmup = .warmup,
                            backend = 'cmdstanr',
                            threads = brms::threading(.threading),
                            seed = .seed,
                            stan_model_args = .stan_model_args)
  }





  t2 <- Sys.time()

  print(t2 - t1)

  return(brms_model)

}
