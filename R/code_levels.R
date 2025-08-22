#' Factor levels code
#'
#' Print common factor levels
#'
#' @export
code_levels <- function() {
  glue::glue('agree_levels <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")
support_levels <- c("Strongly oppose", "Oppose", "Somewhat oppose", "Neither support nor oppose", "Somewhat support", "Support", "Strongly support")
sentiment_levels <- c("Very negative", "Negative",
                        "Slightly negative", "Neutral", "Slightly positive", "Positive", "Very positive")')
}
