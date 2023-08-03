#' Unique ordered pairs
#'
#' Returns unique pairwise items from a list
#'
#' @param x Character vector - the unique items, in the order that they should appear from highest to lowest values
#' @return A tibble with pairs of items
#' @export


unique_ordered_pairs <- function(x) {

  ignore_these_items <<- x[1]

  make_pairing <- function(xsingle) {

    single_output <-
      dplyr::tibble(a = xsingle,
                    b = x[-which(x %in% ignore_these_items)])

    ignore_these_items <<- c(ignore_these_items, xsingle)

    return(single_output)

  }

  main_output <-
    purrr::map_df(.x = x,
                  .f = make_pairing) %>%
    dplyr::filter(a != b)

  return(main_output)

}
