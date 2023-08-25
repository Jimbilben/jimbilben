#' PSup steps code
#'
#' Print out code for the steps to conduct Bayesian within subjects PSup
#'
#' @export
psup_steps_code <- function(item_prefix = "item") {

  glue::glue('ordered_items_list <- paste0("{item_prefix}_", rev(ordered_items))

item_pairings <-
  jimbilben::unique_ordered_pairs(ordered_items_list)

my_data <-
  jimbilben::mutate_comparison_within(my_data,
                                      item_pairings$a,
                                      item_pairings$b)

psup_outcomes <-
  map(.x = my_data %>% select(starts_with("psup_") & contains("{item_prefix}")) %>% names(),
      .f = conduct_psup_within,
      data = my_data)')

}
