#' Rectangle code
#'
#' Print out code for ggplot rectangle and label text
#'
#' @param axis String, the axis being referenced - defaults to "x" (vs. "y")
#' @export

code_rect <- function(min = 100,
                      max = 125,
                      fill = "grey99",
                      color = "grey95",
                      linewidth = .33) {

  print(glue::glue('geom_rect(aes(xmin = {min}, xmax = {max}, ymin = -Inf, ymax = Inf), fill = "{fill}", color = "{color}", linewidth = {linewidth}) +'))
  print(glue::glue('geom_text(aes(x = mean(c({min}, {max})), label = label), family = "Jost", size = 3)'))

}
