#' Mutate Education Collapse
#'
#' Mutate a fine-grained education variable into the larger categories we use for MPR. High school and less than high school are combined. The mutated variable will be called 'education_collapse'.
#'
#' @param data The data for which the mutate will be made
#' @param var_name The variable in which the education categories are contained.

#'
#' @export
mutate_education_collapse <- function(data, var_name = education_og) {

  data <-
    data %>%
    dplyr::mutate(education_collapse = case_when({{var_name}} == "Some high school" ~ "High school or less",
                                                 grepl("ome high sch", {{var_name}}) ~ "High school or less",
                                                 {{var_name}} == "Graduated from high school (Diploma/GED or equivalent)" ~ "High school or less",
                                                 grepl("raduated from high", {{var_name}}) ~ "High school or less",
                                                 {{var_name}} == "Some college, no degree" ~ "Some college, no degree",
                                                 grepl("ome college, no degree", {{var_name}}) ~ "Some college, no degree",
                                                 {{var_name}} == "Completed associate's degree" ~ "Some college, no degree",
                                                 grepl("ompleted associate", {{var_name}}) ~ "Some college, no degree",
                                                 {{var_name}} == "Completed bachelor's degree" ~ "Graduated from college",
                                                 grepl("ompleted bachelor", {{var_name}}) ~ "Graduated from college",
                                                 {{var_name}} == "Completed master’s degree" ~ "Completed graduate school",
                                                 grepl("ompleted master", {{var_name}}) ~ "Completed graduate school",
                                                 {{var_name}} == "Completed professional degree beyond a bachelor’s degree (e.g., M.D., J.D.)" ~ "Completed graduate school",
                                                 grepl("ompleted professional degree beyond a bachelor", {{var_name}}) ~ "Completed graduate school",
                                                 {{var_name}} == "Completed doctorate degree" ~ "Completed graduate school",
                                                 grepl("ompleted bachelor", {{var_name}}) ~ "Graduated from college"))

  return(data)

}
