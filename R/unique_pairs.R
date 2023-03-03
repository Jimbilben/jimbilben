#' Unique Pairs
#'
#' Get a list of unique pairs of items, for use in making pairwise comparisons or correlations.
#' @param a A vector for the first set of items
#' @param b A vector for the second set of items - if NULL (the default) then it will just be the same as a
#' @param sep A string indicating how to separate the two items when pasting them together. Defaults to" " - "

#'
unique_pairs <- function(a = 1:5, b = NULL, sep = " - ") {

  if(is.null(b)) {
    b <- a
  }

  outcome_pairs <-
    tidyr::crossing(a = a, b = b) %>%
    dplyr::mutate(first = case_when(a < b ~ a,
                             TRUE ~ b),
           second = case_when(a < b ~ b,
                              TRUE ~ a),
           combo = paste(first, second, sep = sep)) %>%
    dplyr::select(first, second, combo) %>%
    dplyr::filter(first != second) %>%
    dplyr::distinct()

  return(outcome_pairs)

}

?crossing
