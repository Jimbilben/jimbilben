#' Geom Frame
#'

#' A geom that places a 'frame' around a ggplot panel

#'
#' @export
#'

GeomFrame <- ggplot2::ggproto("GeomFrame", ggplot2::GeomRect,
                              default_aes = ggplot2::aes(colour = "black",
                                                         fill = NA,
                                                         size = .5,
                                                         linetype = 1,
                                                         alpha = .025,
                                                         xmin = -Inf,
                                                         xmax = Inf,
                                                         ymin = -Inf,
                                                         ymax = Inf),

                              required_aes = NULL
)

geom_frame <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity",
                       ..., linejoin = "mitre", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE) {

  mapping <- structure(c(mapping,
                         aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)),
                       class = "uneval")
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomFrame,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(linejoin = linejoin, na.rm = na.rm, ...)
  )

}
