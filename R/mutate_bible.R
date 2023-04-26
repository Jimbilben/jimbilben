#' Mutate Bible
#'
#' Mutate a Bible views variable with shortened response names used in weighting. The mutated variable will be called 'bible'.
#'
#' @param data The data for which the mutate will be made
#' @param var_name The target of the mutate where the original data is.
#'
#' @export
mutate_bible <- function(data, var_name = bible_og) {

  data <-
    data %>%
    dplyr::mutate(bible = case_when({{var_name}} == "The Bible is the actual word of God and is to be taken literally, word for word" ~ "Word of God",
                                    {{var_name}} == "The Bible is the inspired word of God but not everything in it should be taken literally, word for word" ~ "Inspired word",
                                    {{var_name}} == "The Bible is an ancient book of fables, legends, history, and moral precepts recorded by men" ~ "Book of fables",
                                    TRUE ~ {{var_name}}))

  return(data)

}
