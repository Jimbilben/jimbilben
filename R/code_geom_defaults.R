#' Default Geoms Code
#'
#' Print out code for setting some geom defaults in ggplot
#'
#' @export
code_geom_defaults <- function() {

  print('update_geom_defaults("text", list(family = "Jost",
                                  size = 2.75))')
  print('update_geom_defaults("errorbar", list(linewidth = .25))')
  print('update_geom_defaults("errorbarh", list(linewidth = .25))')
  print('update_geom_defaults("point", list(size = 1,
                                   shape = 21,
                                   fill = "white",
                                   color = "black"))')
  print('update_geom_defaults("col", list(linewidth = .25,
                                 alpha = .9,
                                 fill = light_blue))')

}
