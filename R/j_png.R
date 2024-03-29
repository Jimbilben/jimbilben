#' Jimbilben PNG
#'
#' This function creates a PNG file with specified dimensions and resolution, and saves it to the designated path.
#'
#' @param filename A string specifying the name of the output PNG file. Defaults to ".png".
#' @param width A numeric value specifying the width of the PNG file in the given units. Defaults to 6.
#' @param height A numeric value specifying the height of the PNG file in the given units. Defaults to 6.
#' @param units A string specifying the units for the width and height parameters. Defaults to "in" (inches).
#' @param type A string specifying the type of PNG device to use. Defaults to "cairo".
#' @param res A numeric value specifying the resolution of the PNG file in dots per inch (dpi). Defaults to 1200.
#' @param path A string specifying the directory path for saving the PNG file. Defaults to a "pngs" folder within the current working directory.
#' @param directory A string specifying the current working directory. Defaults to the result of getwd().
#' @param ... Additional arguments passed to the png() function.
#'
#' @return None. The function saves a PNG file to the specified path with the provided parameters.
#'
#' @examples
#' # Save a PNG file with default settings
#' j_png("test_plot.png")
#'
#' # Save a PNG file with custom dimensions and resolution
#' j_png("test_plot_large.png", width = 12, height = 12, res = 300)

j_png <- function(filename = ".png",
                  width = 6,
                  height = 6,
                  units = "in",
                  type = "cairo",
                  res = 1200,
                  full_path = glue::glue("{getwd()}/pngs/{filename}"),
                  ...) {

  if (!grepl("\\.png$", full_path)) {
    full_path <- paste0(full_path, ".png")
  }

  # Check if the directory exists, create it if not
  dir.create(dirname(full_path), showWarnings = FALSE)

  png(filename = full_path,
      width = width,
      height = height,
      units = units,
      type = type,
      res = res,
      ...)
}
