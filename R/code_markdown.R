#' Markdown code
#'
#' Print out code for changing colors within a geom_richtext() or element_markdown() string.
#'
#' @export
code_markdown <- function() {

  glue::glue("this is an example of <span style='color: #53b3e2;'>color</span>
  this is an example of <span style='font-family: Arial;'>font</span>
  this is an example of <span style='font-size: 20px;'>size</span>
  this is an example of <span style='color: rgba(255,0,0,0.5);'>alpha</span>
  this is an example of <strong>bold</strong>
  this is an example of <em>italics</em>
  this is an example of <span style='color: #53b3e2; font-family: Arial; font-size: 20px;'><strong><em>combined styling</em></strong></span>
  this is another <span style='color: #53b3e2; font-family: Times New Roman; font-size: 18px;'><strong>example</strong> of <em>combined styling</em></span>")

}
