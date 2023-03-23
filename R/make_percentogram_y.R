#' Make percentogram X
#'
#' Takes a vector of values and returns coordinates to make a percentogram (variable bin width histogram with set percentile bars) using a plotting device such as geom_rect. Provides values for making the bins show the values on the Y axis - use make_percentogram_x for making it on the X axis.
#'
#' @param data Ideally a single vector of your values to plot as a percentogram. Will also take a dataframe provided you give the name of the variable to 'variable_name'
#' @param percent A numeric value indicating the percentage you want each bar to represent. Defaults to 5. Other suggested values as 2.5, 4, and 10. Ideally, but does not necessarily, divide 100 without remainders.
#' @param percentile_range A character string or vector of character strings to request whether to also return designations of particular percentiles: 'quartile', 'quintile', 'octile', and 'decile' are supported. These can then be used to color bars according to the percentiles they cover.
#' @param variable_name A character string indicating the name of the variable if a dataframe is provided rather than a vector.
#' @param .xmin Defaults to 0 - the value on the x axis at which the bottom of the bars will be plotted.
#' @param .xmax Defaults to .75 - the highest value on the x axis at which the widest bar will be plotted.

#'
#' @export


make_percentogram_y <- function(data, percent = 5, percentile_range = FALSE, variable_name = "y", .xmin = 0, .xmax = .75) {

  # check whether a vector or dataframe was given to the function
  # and convert to a vector
  if(is.data.frame(data) == TRUE) {

    # extract the column of interest
    data <-
      data[, variable_name] %>%
      dplyr::pull()
  }

  # get length of all the data points, including possible NAs
  full_length <- length(data)

  # remove NAs
  data <- na.omit(data)

  # get new length of data points without NAs
  nona_length <- length(data)

  # get difference in lengths:
  difference <- full_length - nona_length

  # if there were NAs, provide message so user knows
  if(difference > 0) {
    print(glue::glue("Note: {difference} NA values were dropped from the data"))
  }

  # get quantiles that will be used based on the percentage provided
  pg_quantiles <-
    quantile(data, seq(0, 1, percent/100))

  if(100 %% percent != 0) {
    print(glue::glue("Chosen percent of {percent} does not perfectly split 100. Your final percentile/quantile bar ends at {round(names(pg_quantiles) %>% readr::parse_number() %>% last(), 3)}"))
  }

  pg_data <-
    tibble::tibble(ymin = pg_quantiles[1:length(pg_quantiles)-1],
                   ymax = pg_quantiles[2:length(pg_quantiles)],
                   height = abs(ymin - ymax),
                   lq = names(pg_quantiles) %>% readr::parse_number() %>% .[1:length(pg_quantiles)-1],
                   uq = names(pg_quantiles) %>% readr::parse_number() %>% .[2:length(pg_quantiles)],
                   lq_50 = abs(lq - 50),
                   uq_50 = abs(uq - 50),
                   xmin = .xmin,
                   xmax = 1 / height)

  max_width <- max(pg_data$xmax)

  pg_data <-
    pg_data %>%
    dplyr::mutate(xmax = ((xmax / max_width) * .xmax) + .xmin) %>%
    dplyr::relocate(ymin, ymax, xmin, xmax)

  if("quartile" %in% percentile_range) {
    pg_data <-
      pg_data %>%
      dplyr::mutate(quartile = case_when(lq >= 0 & uq <= 25 ~ "1",
                                         lq >= 25 & uq <= 50 ~ "2",
                                         lq >= 50 & uq <= 75 ~ "3",
                                         lq >= 75 & uq <= 100 ~ "4",
                                         TRUE ~ "Crosses quartiles"),
                    quartile = factor(quartile,
                                      levels = c("1", "2", "3", "4", "Crosses quartiles")))

  }
  if("quintile" %in% percentile_range) {
    pg_data <-
      pg_data %>%
      dplyr::mutate(quintile = case_when(lq >= 0 & uq <= 20 ~ "1",
                                         lq >= 20 & uq <= 40 ~ "2",
                                         lq >= 40 & uq <= 60 ~ "3",
                                         lq >= 60 & uq <= 80 ~ "4",
                                         lq >= 80 & uq <= 100 ~ "5",
                                         TRUE ~ "Crosses quintiles"),
                    quintile = factor(quintile,
                                      levels = c("1", "2", "3", "4", "5", "Crosses quintile")))

  }
  if("octile" %in% percentile_range) {
    pg_data <-
      pg_data %>%
      dplyr::mutate(octile = case_when(lq >= 0 & uq <= 12.5 ~ "1",
                                       lq >= 12.5 & uq <= 25 ~ "2",
                                       lq >= 25 & uq <= 37.5 ~ "3",
                                       lq >= 37.5 & uq <= 50 ~ "4",
                                       lq >= 50 & uq <= 62.5 ~ "5",
                                       lq >= 62.5 & uq <= 75 ~ "6",
                                       lq >= 75 & uq <= 87.5 ~ "7",
                                       lq >= 87.5 & uq <= 100 ~ "8",
                                       TRUE ~ "Crosses octiles"),
                    octile = factor(octile,
                                    levels = c("1", "2", "3", "4", "5", "6", "7", "8", "Crosses octiles")))

  }
  if("decile" %in% percentile_range) {
    pg_data <-
      pg_data %>%
      dplyr::mutate(decile = case_when(lq >= 0 & uq <= 10 ~ "1",
                                       lq >= 10 & uq <= 20 ~ "2",
                                       lq >= 20 & uq <= 30 ~ "3",
                                       lq >= 30 & uq <= 40 ~ "4",
                                       lq >= 40 & uq <= 50 ~ "5",
                                       lq >= 50 & uq <= 60 ~ "6",
                                       lq >= 60 & uq <= 70 ~ "7",
                                       lq >= 70 & uq <= 80 ~ "8",
                                       lq >= 80 & uq <= 90 ~ "9",
                                       lq >= 90 & uq <= 100 ~ "10",
                                       TRUE ~ "Crosses decile"),
                    decile = factor(decile,
                                    levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Crosses deciles")))

  }

  return(pg_data)

}
