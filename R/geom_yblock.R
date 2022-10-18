#' geom_yblock
#'
#' Add a geom_rect() that functions as a number or annotation block, allowing easy display of numbers on the side. This should be used in conjunction with nice_y, with number_block set to TRUE.
#' @param ymin y position where the block will start - call get_ypos()$start with the variables from nice_y()
#' @param fill Color for the fill of the block, defaults to grey95
#' @param alpha Numeric value from 0 to 1 to indicate alpha/transparency for the block. Defaults to .5
#' @param color Color for the line around the variable, defaults to NULL
#' @param linetype Linetype for the geom, defaults to "blank"
#' @param size Size of the line around the block. Defaults to 0 - i.e., no line around the block
#'
#' @export
geom_yblock <- function(ymin,
                        fill = "grey95",
                        alpha = .5,
                        color = "grey95",
                        linetype = "blank",
                        size = 0) {

  geom_rect(aes(xmin = -Inf,
                xmax = Inf,
                ymin = ymin,
                ymax = Inf),
            alpha = alpha,
            fill = fill,
            color = color,
            size = size,
            linetype = "blank")

}
