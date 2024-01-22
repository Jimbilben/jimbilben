#' Print summarise code
#'
#' Print out code for a commonly desired summary and corresponding label
#' @param outcome The name of the variable that is being summarised
#' @export
code_summarise <- function(outcome = "rating") {

  code <- glue::glue(
    'summarise(mean = mean({outcome}, na.rm = TRUE),
               lower = t.test({outcome}, na.rm = TRUE)$conf.int[1],
               upper = t.test({outcome}, na.rm = TRUE)$conf.int[2],
               sd = sd({outcome}, na.rm = TRUE),
               median = median({outcome}, na.rm = TRUE),
               n = sum(!is.na({outcome}))) %>%
      mutate(label = glue("{{nice_num(mean, 2)}} [{{nice_num(lower, 2)}} - {{nice_num(upper, 2)}}], SD = {{nice_num(sd, 2, remove_lead = FALSE)}}"))')

  cat(code, "\n")

}
