#' linebreaker

#' Place line breaks after a designated number of characters in a string.
#'
#' @param text The text to put line breaks in
#' @param max_chars The number of characters after which to place a line break
#' @param linebreak Defaults to <br> - the type of line break to inset. Could also be '\n', for example
#'
#' @export

linebreaker <- function(text, max_chars = 100, linebreak = "<br>") {

  words <- strsplit(text, " ")[[1]]

  current_line <- ""

  lines <- c()

  for (word in words) {
    temp_line <- paste(current_line, word)

    if (nchar(temp_line) <= max_chars) {
      current_line <- temp_line
    } else {
      lines <- append(lines, trimws(current_line))
      current_line <- word
    }
  }

  # Add the last line
  lines <- append(lines, trimws(current_line))

  # Join the lines with <br>
  result <- paste(lines, collapse = linebreak)
  return(result)
}
