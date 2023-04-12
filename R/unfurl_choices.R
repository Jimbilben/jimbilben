#' Unfurl choices
#' Generate new columns indicating the presence or absence of each possible item in a multiple choice question where people can select more than one thing.
#'
#' @param data the target dataframe
#' @param variable the variable/column name in which the multiple choice items are stored
#' @param prefix = A string. Each variable name should, but does not have to be, preceded by a prefix so that the new variables can be easily identified/selected and e.g., pivot_longer from. Defaults to choice_
#' @param sep Defaults to ','. What separator separates the different choices when more than one is selected?
#' @param choice_options If NULL - the default - the unique items will be generated from those that are present in the column. You can also supply your own e.g., to confirm if some known choice was never selected. You can use the unique_choices() function to generate this vector from the data.
#' @param choice_names Can only be used in combination with choice_options. A vector of the same length as choice_options indicating the desired variable name that corresponds to each new choice column that will be created.
#' @param present Defaults to 1 - what the new variable should be if the item is present. Must be of same type as 'absent'
#' @param absent Defaults to 0 - what the new variable should be if the item is absent. Must be of same type as 'present'

#' @export

unfurl_choices <- function(data,
                           variable,
                           prefix = "choice_",
                           sep = ",",
                           choice_options = NULL,
                           choice_names = NULL,
                           present = 1,
                           absent = 0) {

  variable_quoted <- rlang::enquo(variable)

  target_column <- dplyr::select(data, !!variable_quoted) %>% dplyr::pull()

  if (is.null(choice_options)) {
    unique_choices <- unique(unlist(stringr::str_split(na.omit(target_column), sep)))
  } else {
    unique_choices <- choice_options
  }

  if(is.null(choice_names)) {
    for (choice in unique_choices) {
      data[[glue::glue("{prefix}{choice}")]] <- ifelse(is.na(target_column), NA, ifelse(stringr::str_detect(target_column, choice), present, absent))
    }
  } else {

    n_choices <- length(choice_options)
    n_choices <- 1:n_choices

    for (n in n_choices) {
      data[[glue::glue("{prefix}{choice_names[n]}")]] <- ifelse(is.na(target_column), NA, ifelse(stringr::str_detect(target_column, choice_options[n]), present, absent))
    }
  }

  return(data)
}
