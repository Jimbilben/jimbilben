#' MRP functions set values
#'
#' Print out code for setting various parameters that the MRP functions call upon
#'
#' @export
code_mrp_params <- function() {

  print(glue::glue('    # reset whether to save models and epreds here:'))
  print(glue::glue('    save_my_model <- TRUE'))
  print(glue::glue('    save_my_epred <- TRUE'))
  print(glue::glue('    set_save_output <- TRUE'))
  print(glue::glue('    # and change iterations and warmup here'))
  print(glue::glue('    set_my_iter <- 1500'))
  print(glue::glue('    set_my_warmup <- 500'))
  print(glue::glue('    # center numeric outcomes'))
  print(glue::glue('    set_center_value <- 4'))
  print(glue::glue('    # whether to return subrouping by state, default being false'))
  print(glue::glue('    set_state <- FALSE'))
  print(glue::glue('    set_my_adapt_delta <- .99'))
  print(glue::glue('    # whether to use weighted model'))
  print(glue::glue('    set_weighted <- FALSE'))
  print(glue::glue('    # choose the poststratification type:'))
  print(glue::glue('    set_my_poststrat <- acs5_2022_poststrat_with_partyid_short'))
  print(glue::glue('    set_my_epred <- acs5_2022_model_expected_n_short'))


}
