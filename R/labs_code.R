#' Labs Code
#'
#' Print out code for setting several commonly needed labels in a ggplot
#'
#' @export
labs_code <- function() {

  title <- glue::glue('title = "this",')
  subtitle <- glue::glue('subtitle = "this",')
  caption <- glue::glue('caption = "this",')
  x <- glue::glue('x = "this",')
  y <- glue::glue('y = "this",')
  fill <- glue::glue('fill = "this"')

  glue::glue('labs({title}\n{subtitle}\n{caption}\n{x}\n{y}\n{fill})')

}
