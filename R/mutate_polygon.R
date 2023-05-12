#' Mutate polygon
#'
#' This function generates a polygon based on the input data and modifies the data
#' to include polygon coordinates. It is useful for creating visualisations where
#' data points are represented as polygons, such as radar or spider charts.
#'
#' @param data A data frame containing a categorical variable `dimension` and a numeric variable `rating`.
#' @param dimension A character string or variable name specifying the column name for the dimensions of the rating in the data e.g., a column 'attributes' with 'speed', 'strength', 'agility'. Default is "dimension".
#' @param rating A character string or variable name specifying the column name for the numeric rating variable in `data`. Default is "rating".
#' @param radius_proportion A numeric value specifying the proportion of the radius used to shift the rating. Default is NULL, which converts to 0.
#' @param max_rating A numeric value specifying the maximum in principle rating value to plot everything relative to. Default is NULL.
#' @param .radius A numeric value specifying the radius of the polygon. Default is 1.
#' @param .center_x A numeric value specifying the x-coordinate of the center of the polygon. Default is 0.
#' @param .center_y A numeric value specifying the y-coordinate of the center of the polygon. Default is 0.
#' @param start A numeric value specifying the start angle in degrees for the polygon. Default is 0.
#' @param end A numeric value specifying the end angle in degrees for the polygon. Default is 360.
#' @param arrange A logical value indicating whether to sort the resulting data frame by the `dimension` variable. Default is FALSE.
#' @param var_levels A vector specifying the levels of the `dimension` variable. Default is NULL, which will take existing factor levels of a factor variable, or order character and numeric variables.
#' @param base_coords_only A logical value indicating whether to return only the base polygon coordinates without modifying the input data. Default is FALSE.
#'
#' @return A data frame with the input data and additional columns for the polygon coordinates (poly_x, poly_y, poly_xpos, and poly_ypos).
#'
#' @examples
#' my_data <- data.frame(dimension = c("A", "B", "C"),
#'                       rating = c(30, 50, 90))
#'
#' result <- mutate_polygon(data = my_data)
#'
#' print(result)
#'
#' @export
mutate_polygon <- function(data,
                           dimension = "dimension",
                           rating = "rating",
                           radius_proportion = NULL,
                           max_rating = NULL,
                           .radius = 1,
                           .center_x = 0,
                           .center_y = 0,
                           start = 0,
                           end = 360,
                           arrange = FALSE,
                           var_levels = NULL,
                           base_coords_only = FALSE) {

  dimension <- as.character(rlang::ensym(dimension))
  rating <- as.character(rlang::ensym(rating))

  if(is.null(var_levels)) {

    var_levels <-
      dplyr::pull(data, {{dimension}}) %>%
      unique() %>%
      na.omit() %>%
      sort()

  }

  var_n <- length(var_levels)

  # get the degrees that we need
  if(end == 360 & start == 0 | end == 0 & start == 0) {
    degrees_seq <- seq(from = start,
                       to = end,
                       length.out = var_n + 1)
    degrees_seq <- degrees_seq[-length(degrees_seq)]
  }
  else {
    degrees_seq <- seq(from = start,
                       to = end,
                       length.out = var_n)
  }

  polygon_tibble_maker <- function(my_degrees, my_radius = .radius, my_center_x = .center_x, my_center_y = .center_y) {

    jimbilben::circle_xy(degrees = my_degrees, radius = my_radius, center_x = my_center_x, center_y = my_center_y) %>%
      dplyr::as_tibble()

  }

  polygon_tibble <-
    purrr::map_df(.x = degrees_seq,
                  .f = polygon_tibble_maker) %>%
    dplyr::mutate("{dimension}" := var_levels) %>%
    dplyr::rename(poly_x = x,
                  poly_y = y)

  if(base_coords_only == TRUE) {
    return(polygon_tibble)
  }

  else {

    if(is.null(max_rating)) {
      max_rating <- data %>% dplyr::pull({{rating}}) %>% max(na.rm = TRUE)
    }
    if(is.null(radius_proportion)) {
      radius_proportion <- 0
    }

    data <-
      data %>%
      dplyr::mutate("shifted_rating" := (!!rlang::sym(rating) * ((max_rating - (radius_proportion * max_rating)) / max_rating)) + (radius_proportion * max_rating))

    data <-
      dplyr::left_join(data,
                       polygon_tibble,
                       by = {{dimension}}) %>%
      dplyr::mutate(poly_xpos = (shifted_rating / max_rating) * poly_x,
                    poly_ypos = (shifted_rating / max_rating) * poly_y) %>%
      dplyr::rename(!!paste0("shifted_", rating) := shifted_rating)

    if(arrange == TRUE) {
      data <-
        dplyr::arrange(data, {{dimension}})
    }

    return(data)

  }

}

