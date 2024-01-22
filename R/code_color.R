#' Color Code
#'
#' Print out code and some useful links for getting color palettes/color information.
#'
#' @export
code_color <- function() {

  met_colors <- glue::glue('MetBrewer::met.brewer("Hiroshige", 10)[1:10]')
  viridis_colors <- glue::glue('viridis::viridis(option = "magma", 5)')
  viridis_option <- glue::glue('"magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E")')
  html_site <- glue::glue('https://htmlcolorcodes.com/')
  met_github <- glue::glue('https://github.com/BlakeRMills/MetBrewer#palettes')
  colourpicket <- "Use the colourpicker add-in to select a color"

  glue::glue('{met_colors}\n{viridis_colors}\n{viridis_option}\n{html_site}\n{met_github}')

}
