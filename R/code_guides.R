#' Guides code
#'
#' Print out code for manipulating multiple aspects of the guide_legend
#'
#' @export


code_guides <- function() {

  glue::glue('guides(fill = guide_legend(ncol = 3, reverse = FALSE, byrow = FALSE, order = 1, title.position = "top", override.aes = list(alpha = .8)))')

}
