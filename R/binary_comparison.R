#' Binary comparison
#'
#' Add a formal comparison between two MRP posterior outputs.
#' This function compares two lists of MRP posterior outputs corresponding to
#' different time points. It appends to the `current` list a formal comparison
#' with the `previous` time point, including difference, ratio, and log-ratio
#' estimates.
#'
#' @param current A list containing posterior outputs from the current time point,
#'   structured as in MRP results (must include `current$population$posterior`).
#' @param previous A list containing posterior outputs from a previous time point
#'   (must include `previous$population$posterior`).
#' @param name_previous A string label identifying the previous time point. This is
#'   used to name the new elements added to `current`. Default is `"2024"`.
#' @param .decimals Number of decimal places to use when summarizing the differences.
#'   Default is 2.
#'
#' @return The `current` list, with a new element added at
#'   `current$difference[[paste0("difference_", name_previous)]]`. This element contains:
#'   - `posterior`: a tibble with raw difference (`diff_*`), ratio (`prop_*`), and
#'     log-ratio (`logprop_*`) values.
#'   - `summary`: a summarized version of the same metrics, formatted using `nice_post()`.
#'
#' @details The function assumes that both `current` and `previous` lists include a
#'   `population$posterior` data frame or tibble, with a `proportion` column. The function
#'   calculates the difference in proportions (multiplied by 100), the ratio of current to
#'   previous proportions, and the log of that ratio. These values are added to the
#'   `current` list under the `difference` sublist, using the provided `name_previous`
#'   string to label the comparison.
binary_comparison <- function(current,
                              previous,
                              name_previous = "2024",
                              .decimals = 2) {

  my_diff_string <-
    glue::glue("difference_{name_previous}")

  difference_tib <-
    current$population$posterior %>%
    dplyr::mutate(temp_proportion = previous$population$posterior$proportion,
                  temp_diff = proportion - temp_proportion,
                  temp_prop = proportion / temp_proportion,
                  temp_logprop = log(temp_prop))

  difference_tib_summary <-
    difference_tib %>%
    dplyr::mutate(temp_diff = temp_diff * 100) %>%
    tidyr::pivot_longer(
      cols = c(temp_diff,
               temp_prop,
               temp_logprop),
      names_to = "type",
      values_to = "value"
    ) %>%
    dplyr::group_by(
      type
    ) %>%
    nice_post(
      estimate = value,
      decimals = .decimals,
      remove_lead = FALSE
    )

  names(difference_tib)[names(difference_tib) == "temp_proportion"] <- glue::glue("proportion_{name_previous}")
  names(difference_tib)[names(difference_tib) == "temp_diff"] <- glue::glue("diff_{name_previous}")
  names(difference_tib)[names(difference_tib) == "temp_prop"] <- glue::glue("prop_{name_previous}")
  names(difference_tib)[names(difference_tib) == "temp_logprop"] <- glue::glue("logprop_{name_previous}")

  difference_tib_summary <-
    difference_tib_summary %>%
    dplyr::mutate(
      type = dplyr::case_when(type == "temp_diff" ~ glue::glue("diff_{name_previous}"),
                              type == "temp_prop" ~ glue::glue("prop_{name_previous}"),
                              type == "temp_logprop" ~ glue::glue("logprop_{name_previous}"))
    )


  current[[my_diff_string]]$posterior <-
    difference_tib

  current[[my_diff_string]]$summary <-
    difference_tib_summary

  return(current)

}
