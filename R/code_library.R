#' Library Code
#'
#' Print out code for commonly-used libraries
#'
#' @export
code_library <- function() {

  tidyverse <- glue::glue('library(tidyverse)')
  jimbilben <- glue::glue('library(jimbilben)')
  tidystats <- glue::glue('library(tidystats)')
  glue <- glue::glue('library(glue)')
  brms <- glue::glue('library(brms)')
  tidybayes <- glue::glue('library(tidybayes)')
  scales <- glue::glue('library(scales)')
  magrittr <- glue::glue('library(magrittr)')
  ggtext <- glue::glue('library(ggtext)')
  readxl <- glue::glue('library(readxl)')

  glue::glue('{tidyverse}\n{jimbilben}\n{tidystats}\n{glue}\n{brms}\n{tidybayes}\n{scales}\n{magrittr}\n{ggtext}\n{readxl}')

}
