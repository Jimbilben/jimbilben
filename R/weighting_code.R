#' Weighting Code
#'
#' Print out code as exaple of how to use the weighting functions
#'
#' @export
weighting_code <- function(assess = FALSE) {

  if(assess == FALSE) {

    glue::glue('rake_weights(targets = list(age = gen_us("age"),\nrace_collapse = gen_us("race_collapse"),\nmale = gen_us("male"),\npartyid = gen_us("partyid")),\ndata = my_data,\nid_col = "id",\nappend_to_data = TRUE)')

  }
  else if(assess == TRUE) {
    glue::glue('assess_weighting(targets = list(age = gen_us("age"),\nrace_collapse = gen_us("race_collapse"),\nmale = gen_us("male"),\npartyid = gen_us("partyid")),\ndata = my_data %>% mutate(male = as.factor(male)))')
  }

}
