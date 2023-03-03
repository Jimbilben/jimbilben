#' geom_xblock2
#'
#' Add a geom_rect() that functions as a number or annotation block, allowing easy display of numbers on the side. This should be used in conjunction with nice_x, with number_block set to TRUE.
#' @param xmin Variable to which the xmin should be mapped - e.g., max_{var} from mutate_limits()
#' @param fill Color for the fill of the block, defaults to grey95
#' @param alpha Numeric value from 0 to 1 to indicate alpha/transparency for the block. Defaults to .5
#' @param color Color for the line around the variable, defaults to NULL
#' @param linetype Linetype for the geom, defaults to "blank"
#' @param size Size of the line around the block. Defaults to 0 - i.e., no line around the block
#'
#' @export
geom_xblock2 <- function(xmin,
                         fill = "grey95",
                         alpha = .5,
                         color = "grey95",
                         linetype = "blank",
                         size = 0) {

  geom_rect(aes(xmin = {{xmin}},
                xmax = Inf,
                ymin = -Inf,
                ymax = Inf),
            alpha = alpha,
            fill = fill,
            color = color,
            size = size,
            linetype = "blank")

}
