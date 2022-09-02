#' Make Colours
#'
#' Generate a vector of colours selected from a gradient between chosen colours.
#'
#' @param colours A vector of colours from which the gradient will be made
#' @param type "diverging" or "linear" - if you don't specify your own colours, we can give a default diverging or linear gradient.
#' @param n The number of different colours to select from the gradient e.g., the number of Likert items you are colouring. Defaults to 5.
#' @param alpha A value from 0 to 1, to indicate the level of transparency to add to the colours. Defaults to 1 (no transparency)
#' @param append_missing Whether an additional colour outside the gradient should be added e.g., for missing values or DK responses. Can take values of "start", "end", or "none". Defaults to "none".
#' @param missing_colours A single colour or vector of colours that will be placed in for values outside the gradient. Defaults to "D9D9D9" - aka 'grey85'
#'
#' @export
make_colours <- function(colours = NULL, type = "diverging", n = 5, alpha = 1, append_missing = "none", missing_colours = "#D9D9D9") {

  if(is.null(colours)) {
    if(type == "diverging") {
      colours <- c("#56a3cf", "#cecece", "#f2763c")
    }
    else if(type == "linear") {
      colours <- c("#cbe8e7", "#037876")
    }
  }

  gradient_function <- colorRampPalette(colours)

  color_output <- gradient_function(n)

  if(append_missing == "start") {
    hex_missing_colours <- colorRampPalette(missing_colours)
    missing_colours <- hex_missing_colours(length(missing_colours))

    color_output <- c(missing_colours, color_output)
  }
  else if(append_missing == "end") {
    hex_missing_colours <- colorRampPalette(missing_colours)
    missing_colours <- hex_missing_colours(length(missing_colours))

    color_output <- c(color_output, missing_colours)
  }

  if(alpha != 1) {
    alpha_codes <- tibble(alpha_string = c("FF",
                                           "FC",
                                           "FA",
                                           "F7",
                                           "F5",
                                           "F2",
                                           "F0",
                                           "ED",
                                           "EB",
                                           "E8",
                                           "E6",
                                           "E3",
                                           "E0",
                                           "DE",
                                           "DB",
                                           "D9",
                                           "D6",
                                           "D4",
                                           "D1",
                                           "CF",
                                           "CC",
                                           "C9",
                                           "C7",
                                           "C4",
                                           "C2",
                                           "BF",
                                           "BD",
                                           "BA",
                                           "B8",
                                           "B5",
                                           "B3",
                                           "B0",
                                           "AD",
                                           "AB",
                                           "A8",
                                           "A6",
                                           "A3",
                                           "A1",
                                           "9E",
                                           "9C",
                                           "99",
                                           "96",
                                           "94",
                                           "91",
                                           "8F",
                                           "8C",
                                           "8A",
                                           "87",
                                           "85",
                                           "82",
                                           "80",
                                           "7D",
                                           "7A",
                                           "78",
                                           "75",
                                           "73",
                                           "70",
                                           "6E",
                                           "6B",
                                           "69",
                                           "66",
                                           "63",
                                           "61",
                                           "5E",
                                           "5C",
                                           "59",
                                           "57",
                                           "54",
                                           "52",
                                           "4F",
                                           "4D",
                                           "4A",
                                           "47",
                                           "45",
                                           "42",
                                           "40",
                                           "3D",
                                           "3B",
                                           "38",
                                           "36",
                                           "33",
                                           "30",
                                           "2E",
                                           "2B",
                                           "29",
                                           "26",
                                           "24",
                                           "21",
                                           "1F",
                                           "1C",
                                           "1A",
                                           "17",
                                           "14",
                                           "12",
                                           "0F",
                                           "0D",
                                           "0A",
                                           "08",
                                           "05",
                                           "03",
                                           "00"),
                          alpha_value = seq(1, 0, -.01))

    selected_alpha <- filter(alpha_codes,
                             round(alpha_value, 2) == round(alpha, 2))

    append_alpha <- function(color, .alpha) {
      new_color <- glue::glue("{color}{.alpha}")
      return(new_color)
    }

    color_output <- unlist(map(.x = color_output,
                        .f = append_alpha,
                        .alpha = selected_alpha[1, "alpha_string"]))

  }

  return(color_output)

}
