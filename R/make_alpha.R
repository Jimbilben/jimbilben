#' Make Alpha
#'
#' Generate a vector of transparent colors based on color inputs.
#'
#' @param colors A color or vector of colors from which the gradient will be made
#' @param alpha A value or vector of values from 0 to 1, to indicate the level of transparency to add to the colors. Defaults to .67 (slight transparency)
#'
#' @export
make_alpha <- function(colors = c("#56a3cf", "#cecece", "#f2763c"), alpha = .67) {

  gradient_function <- colorRampPalette(colors)

  color_output <- gradient_function(length(colors))

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

  append_alpha <- function(color, .alpha) {

    selected_alpha <- filter(alpha_codes,
                             round(alpha_value, 2) == round(.alpha, 2))

    selected_alpha <- selected_alpha[1, "alpha_string"]

    new_color <- glue::glue("{color}{selected_alpha}")

    return(new_color)
  }

  color_output <- unlist(map2(.x = color_output,
                              .y = alpha,
                              .f = append_alpha))

  return(color_output)

}
