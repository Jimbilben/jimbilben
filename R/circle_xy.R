#' Circle XY
#'
#' Get the x and y coordinates for points around a circle, with a given center, radius, and degree.
#' @param degrees The angle (or angles) around the circle that you wish to compute the x y coordinates from.
#' @param radius The radius of the circle to compute.
#' @param center_x The center of the circle in the x plane.
#' @param center_x The center of the circle in the y plane.

#'
#' @export
#'
circle_xy <- function(degrees = 0, radius = 1, center_x = 0, center_y = 0) {

  circle <- tibble(degree = as.numeric(degrees),
                   start_x = center_x,
                   start_y = center_y,
                   r = radius, # radius,
                   R = 90, # right angle opposite radius line
                   X = case_when(degree <= 90 ~ degree,
                                 degree <= 180 & degree > 90 ~ degree - 90,
                                 degree <= 270 & degree > 180 ~ degree - 180,
                                 degree <= 360 & degree > 270 ~ degree - 270), # input angle, opposite the x line
                   Y = 180 - 90 - X, # the remaining angle opposite our y line length
                   R_rad = R * (pi / 180),
                   X_rad = X * (pi / 180),
                   Y_rad = Y * (pi / 180),
                   r_over_sine_R = r / (sin(R_rad))) %>%
    mutate(x_basic = r_over_sine_R * sin(X_rad),
           y_basic = r_over_sine_R * sin(Y_rad),
           x = case_when(degree <= 90 ~ x_basic + start_x,
                         degree <= 180 & degree > 90 ~ y_basic + start_x,
                         degree <= 270 & degree > 180 ~ -x_basic + start_x,
                         degree <= 360 & degree > 270 ~ -y_basic + start_x),
           y = case_when(degree <= 90 ~ y_basic + start_y,
                         degree <= 180 & degree > 90 ~ -x_basic + start_y,
                         degree <= 270 & degree > 180 ~ -y_basic + start_y,
                         degree <= 360 & degree > 270 ~ x_basic + start_y))

  x_output <- circle$x
  y_output <- circle$y

  return(list("x" = x_output, "y" = y_output))

}
