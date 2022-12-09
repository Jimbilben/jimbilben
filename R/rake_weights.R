#' Rake Weights
#'
#' Generates weights using the Anesrake procedure, trying to get convergence starting at a low cap and the increasing if convergence is not reached. The function also calculates the design effect.
#' @param targets A list of weighting targets (see the gen_us() function to generate these easily)
#' @param data Data for which you want to generate weights - unlike like anesrake this can be a tibble, and the function will also factor the target variables for you.
#' @param id_col A string indicating the name of the column in the data that uniquely identifies each respondent. Defaults to "id"
#' @param weight_name An optional name for the weights column that will be appended to your data if you select append_to_data = TRUE. Defaults to "weights"
#' @param append_to_data TRUE or FALSE - should the function return the data with the weights attached? Defaults to false.
#' @param include_modified_weight Whether or not to return a vector (and append to the data if append_to_data = TRUE) a vector of the weights divided by the design effect, which can be used in pseudo-weighted analyses.
#' @param show_reminder The function will, by default (TRUE), show you the names of the objects it has created and tell you what they are.
#' @param max_attempts Numeric, the number of attempts that the function should make when increasing the cap to try and get the weighting to converge. Defaults to 50.
#' @param .pctlim Same as the pctlim parameter of anesrake: the amount of deviation from the target population proportions that will be allowed. This should be set somewhere between .02 and .05

rake_weights <- function(targets,
                         data,
                         id_col = "id",
                         weight_name = "weights",
                         append_to_data = FALSE,
                         include_modified_weight = TRUE,
                         show_reminder = TRUE,
                         max_attempts = 50,
                         .weightvec = NULL,
                         .cap = NULL,
                         .verbose = FALSE,
                         .maxit = 3000,
                         .type = "pctlim",
                         .pctlim = .02,
                         .iterate = TRUE,
                         .convcrit = .01,
                         .force1 = TRUE,
                         .center.baseweights = TRUE) {

  target_variables <- names(targets)

  weight_data <-
    data %>%
    dplyr::mutate(across(.cols = tidyselect::all_of(target_variables),
                         factor)) %>%
    as.data.frame()

  if(is.null(.cap) == FALSE) {

    anesrake_object <-
      anesrake::anesrake(inputter = targets,
                         dataframe = weight_data,
                         caseid = weight_data[1:nrow(data), id_col],
                         weightvec = .weightvec,
                         cap = .cap,
                         verbose = .verbose,
                         maxit = .maxit,
                         type = .type,
                         pctlim = .pctlim,
                         iterate = .iterate,
                         convcrit = .convcrit,
                         force1 = .force1,
                         center.baseweights = .center.baseweights)

  }
  else {

    convergence <- "Not yet converged"
    attempts <- 0
    what_cap <- 3

    while (convergence == "Not yet converged" & attempts <= max_attempts) {

      attempts <- attempts + 1

      what_cap <- if(attempts == 1) {
        what_cap
      }
      else {
        what_cap + 1
      }

      suppressWarnings(anesrake_object <-
                         anesrake::anesrake(inputter = targets,
                                            dataframe = weight_data,
                                            caseid = weight_data[1:nrow(data), id_col],
                                            weightvec = .weightvec,
                                            cap = what_cap,
                                            verbose = .verbose,
                                            maxit = .maxit,
                                            type = .type,
                                            pctlim = .pctlim,
                                            iterate = .iterate,
                                            convcrit = .convcrit,
                                            force1 = .force1,
                                            center.baseweights = .center.baseweights))

      print(glue::glue("Attempts = {attempts}, Current cap = {what_cap}"))

      convergence <-
        if(anesrake_object$converge != "Complete convergence was achieved") {
          "Not yet converged"
        }
      else {
        "Complete convergence was achieved"
      }

    }

    if(convergence != "Complete convergence was achieved") {
      warning(glue::glue("Anesrake did not converge in {max_attempts} attempts. You could increase max_attempts, consider alternative weighting targets, and/or increase .pctlim from {.pctlim}"))
    }
    else if(convergence == "Complete convergence was achieved") {
      message(glue::glue("Anesrake converged in {attempts} attempts, using a cap of {what_cap}"))
    }

  }

  output_cap <-
    if(is.null(.cap)) {
      what_cap
    }
  else {
    .cap
  }

  output_design_effect <- anesrake::generaldesigneffect(anesrake_object$weightvec)

  message(glue::glue("The general design effect is {round(output_design_effect, 2)}"))

  if(include_modified_weight == TRUE) {
    output_modified_weights <- anesrake_object$weightvec / output_design_effect
  }

  if(append_to_data == TRUE) {
    data[, weight_name] <- anesrake_object$weightvec

    if(include_modified_weight == TRUE) {
      mod_weight_name <- paste0(c("mod_", weight_name), collapse = "")

      data[, mod_weight_name] <- output_modified_weights
    }

  }

  if(append_to_data == TRUE & include_modified_weight == TRUE) {
    if(show_reminder == TRUE) {
      print(glue::glue("Your output objects:"))
      print(glue::glue("anes = The full object output from anesrake"))
      print(glue::glue("cap = The cap used for weighting"))
      print(glue::glue("weights = A vector of weights that can be added to your data"))
      print(glue::glue("design_effect = A value indicating the anesrake::generaldesigneffect() for your weights"))
      print(glue::glue("data = Your input data with the weights appended"))
      print(glue::glue("modified_weights = A vector of weights divided by the design effect, used for pseudo-weighted analyses"))
      print(glue::glue("To suppress this message, set show_reminder to FALSE"))
    }

    return(list("anes" = anesrake_object,
                "cap" = output_cap,
                "weights" = anesrake_object$weightvec,
                "design_effect" = output_design_effect,
                "data" = data,
                "modified_weights" = output_modified_weights))
  }
  else if(append_to_data == FALSE & include_modified_weight == TRUE) {
    if(show_reminder == TRUE) {
      print(glue::glue("Your output objects:"))
      print(glue::glue("anes = The full object output from anesrake"))
      print(glue::glue("cap = The cap used for weighting"))
      print(glue::glue("weights = A vector of weights that can be added to your data"))
      print(glue::glue("design_effect = A value indicating the anesrake::generaldesigneffect() for your weights"))
      print(glue::glue("modified_weights = A vector of weights divided by the design effect, used for pseudo-weighted analyses"))
      print(glue::glue("To suppress this message, set show_reminder to FALSE"))
    }

    return(list("anes" = anesrake_object,
                "cap" = output_cap,
                "weights" = anesrake_object$weightvec,
                "design_effect" = output_design_effect,
                "modified_weights" = output_modified_weights))
  }
  else if(append_to_data == TRUE & include_modified_weight == FALSE) {
    print(glue::glue("Your output objects:"))
    print(glue::glue("anes = The full object output from anesrake"))
    print(glue::glue("cap = The cap used for weighting"))
    print(glue::glue("weights = A vector of weights that can be added to your data"))
    print(glue::glue("design_effect = A value indicating the anesrake::generaldesigneffect() for your weights"))
    print(glue::glue("data = Your input data with the weights appended"))
    print(glue::glue("To suppress this message, set show_reminder to FALSE"))

    return(list("anes" = anesrake_object,
                "cap" = output_cap,
                "weights" = anesrake_object$weightvec,
                "design_effect" = output_design_effect,
                "data" = data))
  }
  else if(append_to_data == FALSE & include_modified_weight == FALSE) {
    print(glue::glue("Your output objects:"))
    print(glue::glue("anes = The full object output from anesrake"))
    print(glue::glue("cap = The cap used for weighting"))
    print(glue::glue("weights = A vector of weights that can be added to your data"))
    print(glue::glue("design_effect = A value indicating the anesrake::generaldesigneffect() for your weights"))
    print(glue::glue("To suppress this message, set show_reminder to FALSE"))

    return(list("anes" = anesrake_object,
                "cap" = output_cap,
                "weights" = anesrake_object$weightvec,
                "design_effect" = output_design_effect))
  }

}
