#' Weighted posterior code
#'
#' Print out code for a weighted posterior summary
#'
#' @export
weighted_post_code <- function() {

  glue::glue(
    'weighted_summary <-
    add_epred_draws(
      object = weighted_reg,
      newdata = weighted_data[1,],
      ndraws = 4000
    ) %>%
    ungroup() %>%
    select(.row:.epred) %>%
    rename(choice = .category,
           proportion = .epred) %>%
    group_by(choice) %>%
    nice_post(proportion,
              percentage = TRUE,
              point_est = "mean",
              decimals = 1,
              remove_lead = FALSE) %>%
    arrange(-mean)'
    )

}
