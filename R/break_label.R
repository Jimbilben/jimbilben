#' Break label
#'
#' Add a line break to a label of the form 'X.XX [Y.YY - Z.ZZ]' so that the confidence interval comes on a new line
#'
#' @param data The data for which the mutate will be made
#' @param from The variable name of the original label - defaults to label
#' @param to The variable name for the converted label - defaluts to label_break

#'
#' @export
break_label <- function(data, from = label, to = label_break) {

  data <-
    data %>%
    dplyr::mutate({{to}} := stringr::str_replace({{from}}, "\\s\\[", "\n["))

  return(data)

}
