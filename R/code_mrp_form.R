#' MRP Formula Code
#'
#' Print out code for a standard MRP formula.
#'
#' @export
code_mrp_form <- function() {

  glue::glue("~ (1 | state) + (1 | race) + (1 | age_fine) +
              (1 | education_collapse) + (1 | income_ces) + male + (1 | male:race) +
              (1 | education_collapse:age_fine) + (1 | partyid) + (1 | region) + z_repvote")

  glue::glue("~ (1 | state) + (1 | race) + (1 | age_fine) +
              (1 | education_collapse) + (1 | income_ces) + male + (1 | male:race) +
              (1 | education_collapse:age_fine) + (1 | partyid) + (1 | region) + repvote_cent")

}
