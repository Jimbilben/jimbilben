#' Rectangle code
#'
#' Print out code for ggplot rectangle and label text
#'
#' @param axis String, the axis being referenced - defaults to "x" (vs. "y")
#' @export

code_rect <- function(min = 100,
                      max = 125,
                      break_width = 20,
                      fill = "grey99",
                      color = "grey95",
                      linewidth = .33) {

  print(glue::glue('coord_cartesian(xlim = c(1, 9)) +'))
  print(glue::glue('scale_x_continuous(limits = c(0, 9), breaks = seq(1, 7, 1), expand = expansion(0), labels = c("1<br>Strongly<br>disagree",
                                                                                                      "2",
                                                                                                      "3",
                                                                                                      "4<br>Neither agree<br>nor disagree",
                                                                                                      "5",
                                                                                                      "6",
                                                                                                      "7<br>Strongly<br>agree")) +'))

  print(glue::glue('scale_x_continuous(limits = c(0, {max}), breaks = seq(0, {min}, {break_width}), expand = expansion(0)) +'))
  print(glue::glue('geom_rect(aes(xmin = {min}, xmax = {max}, ymin = -Inf, ymax = Inf), fill = "{fill}", color = "{color}", linewidth = {linewidth}) +'))
  print(glue::glue('geom_text(aes(x = mean(c({min}, {max})), label = label), family = "Jost", size = 3)'))

}
