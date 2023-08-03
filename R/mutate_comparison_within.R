#' Mutate Within Subjects Comparison
#'
#' This function takes a data frame and two sets of column names (a and b) and generates new columns that are the differences between the corresponding columns in a and b. The new columns are named by appending 'a' and 'b' with a separator. If psup_convert is set to TRUE, it then converts these difference values into a psup (probability superiority) format.
#'
#' @param df The data frame for which the mutate will be made.
#' @param a A character vector containing the names of the first set of columns.
#' @param b A character vector containing the names of the second set of columns. 'a' and 'b' must have the same length.
#' @param sep A character string specifying the separator to be used between the names of 'a' and 'b' in the new column names - defaults to "_min_".
#' @param append A character string to be appended before the new column names - defaults to "psup_".
#' @param psup_convert A logical indicating whether to convert the difference values into a psup format. If TRUE, negative difference values are converted to 0, positive difference values are converted to 1, and zero difference values remain as 0.5. Defaults to TRUE.
#' @return A dataframe with the original columns and the newly generated columns.
#' @export

mutate_comparison_within <- function(df, a, b, sep = "_min_", append = "psup_", psup_convert = TRUE) {
  if (length(a) != length(b)) {
    stop("a and b must have the same length.")
  }

  for (i in seq_along(a)) {
    df <- df %>%
      dplyr::mutate(!!glue::glue("{append}{a[i]}{sep}{b[i]}") :=
                      df[[a[i]]] - df[[b[i]]])
  }

  if(psup_convert == TRUE) {
    df <-
      df %>%
      dplyr::mutate(
        dplyr::across(.cols = dplyr::starts_with(append),
                      ~ dplyr::case_when(. < 0 ~ 0,
                                         . > 0 ~ 1,
                                         . == 0 ~ .5)))
  }

  return(df)
}
