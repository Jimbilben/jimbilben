#' Extract text from specific XML/HTML tags
#'
#' This function extracts text enclosed within specified XML or HTML tags from a given text string.
#' The function uses a regular expression to match and extract the content. It requires the text
#' from which to extract the content and the specific tag name to look for.
#'
#' @param text The full text string from which to extract the tagged content.
#' @param tag The name of the tag from which to extract the content, without angle brackets. Defaults to "response".
#' @return A character string containing the text extracted from between the specified tags.
#' If the tag does not exist in the text, the function will return the original text.
#'
#' @examples
#' text_example <- "<customtag>Here is the content</customtag>"
#' tag_extract(text_example, "customtag")
#'
#' @export
tag_extract <- function(text, tag = "response") {
  pattern <- sprintf(".*<%s>(.*?)</%s>.*", tag, tag)
  extracted_text <- sub(pattern, "\\1", text, perl = TRUE)
  return(extracted_text)
}
