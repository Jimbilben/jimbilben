#' Brm Code
#'
#' Print out code for a typical brms model with sensible defaults.
#'
#' @export
brm_code <- function() {

  glue::glue("brm(formula = my_formula,
          family = categorical(),
          data = my_data,
          control = list(adapt_delta = 0.99, max_treedepth = 15),
          prior = my_priors,
          chains = 4,
          cores = 4,
          iter = 2000,
          warmup = 500,
          #init = 0,
          backend = 'cmdstanr',
          threads = threading(4),
          seed = 1010,
          silent = 0,
          refresh = 25,
          stan_model_args=list(stanc_options = list('O1')))")

}
