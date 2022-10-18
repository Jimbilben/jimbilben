#' nice_y
#'
#' Generate a nicely functioning x axis based on the numbers of the x variable. Includes possible expansion for a number/annotation block.
#'
#' @param data The data from which the y variable is to be taken
#' @param variable A character string naming the y variable
#' @param nearest The nearest number to round up to - defaults to 10
#' @param break_width Size of the major break, defaults to 'nearest'
#' @param lower_limit The lower limit of the x axis, defaults to 0
#' @param expand_low Additive expansion for the low end of x, defaults to 0
#' @param expand_high Additive expansion for the high end of x, defaults to 0
#' @param number_block Whether to expand the upper limit to include space for an annotation rectangle, defaults to FALSE
#' @param number_block_multiple Proportional expansion of the axis to accommodate the number block - defaults to 1.2

#'
#' @export
nice_y <- function(data,
                   variable,
                   nearest = 10,
                   break_width = NULL,
                   number_block_multiple = 1.2,
                   number_block = FALSE,
                   lower_limit = 0,
                   expand_low = 0,
                   expand_high = 0
                   ) {

  break_width <- if(is.null(break_width)) {
    nearest
  }
  else {
    break_width
  }

  if(number_block == FALSE) {

    max_num <- max(data[ , variable])
    max_rounded <- jimbilben::round_up(max_num, nearest)

    upper_limit <- if(max_num == max_rounded) {
      max_rounded + (break_width / 4)
    }
    else {
      max_rounded
    }

    scale_y_continuous(limits = c(lower_limit, upper_limit),
                       breaks = seq(lower_limit, upper_limit, break_width),
                       expand = expansion(add = c(expand_low, expand_high)))
  }
  else if(number_block == TRUE) {

    max_num <- max(data[ , variable])
    max_rounded <- jimbilben::round_up(max_num, nearest)

    upper_limit <- if(max_num == max_rounded) {
      max_rounded + (break_width / 4)
    }
    else {
      max_rounded
    }

    scale_y_continuous(limits = c(lower_limit, upper_limit * number_block_multiple),
                       breaks = seq(lower_limit, upper_limit, break_width),
                       expand = expansion(add = c(expand_low, expand_high)))

  }

}
