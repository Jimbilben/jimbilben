#' get_ypos
#'
#' Get the start and middle points for a number/annotation block.
#' @param data Copy from call to nice_y() - The data from which the y variable is to be taken
#' @param variable Copy from call to nice_y() - A character string naming the y variable
#' @param nearest Copy from call to nice_y() - The nearest number to round up to - defaults to 10
#' @param break_width Copy from call to nice_y() - Size of the major break, defaults to 'nearest'
#' @param number_block_multiple Copy from call to nice_y() - Proportional expansion of the axis to accommodate the number block - defaults to 1.2

#'
#' @export
get_ypos <- function(data,
                     variable,
                     nearest = 10,
                     break_width = NULL,
                     number_block_multiple = 1.2) {

  break_width <- if(is.null(break_width)) {
    nearest
  }
  else {
    break_width
  }

  max_num <- max(data[ , variable])
  max_rounded <- jimbilben::round_up(max_num, nearest)

  upper_limit <- if(max_num == max_rounded) {
    max_rounded + (break_width / 4)
  }
  else {
    max_rounded
  }

  upper_extension <- upper_limit * number_block_multiple

  start <- upper_limit * 1.0002
  middle <- (upper_limit + (upper_limit * number_block_multiple)) / 2

  return(list("start" = start,
              "middle" = middle))

}
