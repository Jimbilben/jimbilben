% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutate_polygon.R
\name{mutate_polygon}
\alias{mutate_polygon}
\title{Mutate polygon}
\usage{
mutate_polygon(
  data,
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
  base_coords_only = FALSE
)
}
\arguments{
\item{data}{A data frame containing a categorical variable `dimension` and a numeric variable `rating`.}

\item{dimension}{A character string or variable name specifying the column name for the dimensions of the rating in the data e.g., a column 'attributes' with 'speed', 'strength', 'agility'. Default is "dimension".}

\item{rating}{A character string or variable name specifying the column name for the numeric rating variable in `data`. Default is "rating".}

\item{radius_proportion}{A numeric value specifying the proportion of the radius used to shift the rating. Default is NULL, which converts to 0.}

\item{max_rating}{A numeric value specifying the maximum in principle rating value to plot everything relative to. Default is NULL.}

\item{.radius}{A numeric value specifying the radius of the polygon. Default is 1.}

\item{.center_x}{A numeric value specifying the x-coordinate of the center of the polygon. Default is 0.}

\item{.center_y}{A numeric value specifying the y-coordinate of the center of the polygon. Default is 0.}

\item{start}{A numeric value specifying the start angle in degrees for the polygon. Default is 0.}

\item{end}{A numeric value specifying the end angle in degrees for the polygon. Default is 360.}

\item{arrange}{A logical value indicating whether to sort the resulting data frame by the `dimension` variable. Default is FALSE.}

\item{var_levels}{A vector specifying the levels of the `dimension` variable. Default is NULL, which will take existing factor levels of a factor variable, or order character and numeric variables.}

\item{base_coords_only}{A logical value indicating whether to return only the base polygon coordinates without modifying the input data. Default is FALSE.}
}
\value{
A data frame with the input data and additional columns for the polygon coordinates (poly_x, poly_y, poly_xpos, and poly_ypos).
}
\description{
This function generates a polygon based on the input data and modifies the data
to include polygon coordinates. It is useful for creating visualisations where
data points are represented as polygons, such as radar or spider charts.
}
\examples{
my_data <- data.frame(dimension = c("A", "B", "C"),
                      rating = c(30, 50, 90))

result <- mutate_polygon(data = my_data)

print(result)

}
