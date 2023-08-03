#' Permute Within Subjects Comparison
#'
#' This function conducts permutation testing for paired (i.e., within subjects) ratings. It will provide estimates for 'Probability of Superiority' and 'Observation oriented modelling', in which tied values count against any directional difference.
#'
#' @param data The data frame for which the permutation test will be conducted.
#' @param a The name of the first column in the pair for permutation testing.
#' @param b The name of the second column in the pair for permutation testing.
#' @param direction Specifies the expected direction of the difference between the pair of columns. It can be "superior" (a > b), "inferior" (a < b), or "two-sided" (non-directional). Defaults to "two-sided".
#' @param sims The number of random permutations to perform. Defaults to 2500.
#' @param current_prefix The current prefix of the column names in the data frame. Defaults to "item_".
#' @param new_prefix The new prefix to be appended before the new column names. Defaults to "comp_".
#' @param sep A character string specifying the separator to be used between the names of 'a' and 'b' in the new column name. Defaults to "_vs_".
#' @param verbose A logical indicating whether to print detailed output. If TRUE, it returns both the detailed comparisons for each simulation and the summary. If FALSE, it returns only the summary. Defaults to FALSE.
#' @return A dataframe containing the summary of the permutation tests. If verbose = TRUE, a list containing both the detailed comparisons for each simulation and the summary is returned.
#' @export

permute_within <- function(data, a, b, direction = "two-sided", sims = 2500, current_prefix = "item_", new_prefix = "comp_", sep = "_vs_", verbose = FALSE) {
  
  mod_direction <- tolower(direction)
  
  superior <- c("greater", "higher", "larger", "above", "sup", "superior", "over", ">", "more")
  inferior <- c("lesser", "lower", "smaller", "below", "inf", "inferior", "under", "<", "less")
  either <- c("either", "difference", "different", "none", "no", "not same", "two sided", "two-sided", "2-sided", "2 sided", "a difference", "some difference", "non-directional", "non directional")
  
  new_data <-
    dplyr::tibble(a = data %>% pull(!!sym(a)) %>% as.numeric(),
                  b = data %>% pull(!!sym(b)) %>% as.numeric()) %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(psup_sup_est = dplyr::case_when(a > b ~ 1,
                                                  a == b ~ .5,
                                                  a < b ~ 0),
                  psup_inf_est = dplyr::case_when(a > b ~ 0,
                                                  a == b ~ .5,
                                                  a < b ~ 1),
                  exact_sup_est = dplyr::case_when(a > b ~ 1,
                                                   a < b ~ 0,
                                                   a == b ~ 0),
                  exact_inf_est = dplyr::case_when(a == b ~ 0,
                                                   a > b ~ 0,
                                                   a < b ~ 1))
  
  raw_summary <-
    new_data %>% 
    dplyr::summarise(raw_psup_sup_est = mean(psup_sup_est),
                     raw_psup_inf_est = mean(psup_inf_est),
                     raw_exact_sup_est = mean(exact_sup_est),
                     raw_exact_inf_est = mean(exact_inf_est))
  
  twosided_direction <-
    if(raw_summary$raw_psup_sup_est >= raw_summary$raw_psup_inf_est) {
      "superior"
    }
  else {
    "inferior"
  }
  
  n_pairs <- nrow(new_data)
  
  randomise_me <- function(sim = 1) {
    
    randomised <- sample(c("switch", "stick"), replace = TRUE, size = n_pairs)
    
    # randomly flip some pairs around
    # and code the comparisons according to psup values
    # and exact differences. Code it to apply to either direction
    chance_tibble <-
      new_data %>%
      dplyr::mutate(randomiser = randomised,
                    new_a = dplyr::case_when(randomiser == "switch" ~ b,
                                             randomiser == "stick" ~ a),
                    new_b = dplyr::case_when(randomiser == "switch" ~ a,
                                             randomiser == "stick" ~ b),
                    psup_sup_est = dplyr::case_when(new_a > new_b ~ 1,
                                                    new_a == new_b ~ .5,
                                                    new_a < new_b ~ 0),
                    psup_inf_est = dplyr::case_when(new_a > new_b ~ 0,
                                                    new_a == new_b ~ .5,
                                                    new_a < new_b ~ 1),
                    exact_sup_est = dplyr::case_when(new_a > new_b ~ 1,
                                                     new_a < new_b ~ 0,
                                                     new_a == new_b ~ 0),
                    exact_inf_est = dplyr::case_when(new_a == new_b ~ 0,
                                                     new_a > new_b ~ 0,
                                                     new_a < new_b ~ 1)) %>% 
      dplyr::summarise(psup_sup_est = mean(psup_sup_est),
                       psup_inf_est = mean(psup_inf_est),
                       exact_sup_est = mean(exact_sup_est),
                       exact_inf_est = mean(exact_inf_est),
                       sim = sim)
    
    return(chance_tibble)
    
  }
  
  plan(future::multisession)
  
  options <- furrr::furrr_options(seed = 1010)
  
  chance_comparisons <-
    furrr::future_map_dfr(.x = 1:sims,
                          .f = randomise_me,
                          .options = options) %>% 
    dplyr::mutate(pairing = glue::glue("{new_prefix}{str_remove(a, current_prefix)}{sep}{str_remove(b, current_prefix)}")) %>% 
    dplyr::relocate(pairing) %>% 
    dplyr::mutate(raw_psup_sup_est = raw_summary$raw_psup_sup_est,
                  raw_psup_inf_est = raw_summary$raw_psup_inf_est,
                  raw_exact_sup_est = raw_summary$raw_exact_sup_est,
                  raw_exact_inf_est = raw_summary$raw_exact_inf_est,
                  
                  # assess the number of times the chance generated value equals or exceeds that actually observed value
                  chance_psup = dplyr::case_when(direction %in% superior & psup_sup_est >= raw_psup_sup_est ~ 1,
                                                 direction %in% superior & psup_sup_est < raw_psup_sup_est ~ 0,
                                                 direction %in% inferior & psup_inf_est >= raw_psup_inf_est ~ 1,
                                                 direction %in% inferior & psup_inf_est < raw_psup_inf_est ~ 0,
                                                 
                                                 direction %in% either & twosided_direction == "superior" & psup_sup_est >= raw_psup_sup_est ~ 1,
                                                 direction %in% either & twosided_direction == "superior" & psup_sup_est < raw_psup_sup_est ~ 0,
                                                 
                                                 direction %in% either & twosided_direction == "inferior" & psup_inf_est >= raw_psup_inf_est ~ 1,
                                                 direction %in% either & twosided_direction == "inferior" & psup_inf_est < raw_psup_inf_est ~ 0),
                  
                  chance_exact = dplyr::case_when(direction %in% superior & exact_sup_est >= raw_exact_sup_est ~ 1,
                                                  direction %in% superior & exact_sup_est < raw_exact_sup_est ~ 0,
                                                  direction %in% inferior & exact_inf_est >= raw_exact_inf_est ~ 1,
                                                  direction %in% inferior & exact_inf_est < raw_exact_inf_est ~ 0,
                                                  
                                                  direction %in% either & twosided_direction == "superior" & exact_sup_est >= raw_exact_sup_est ~ 1,
                                                  direction %in% either & twosided_direction == "superior" & exact_sup_est < raw_exact_sup_est ~ 0,
                                                  
                                                  direction %in% either & twosided_direction == "inferior" & exact_inf_est >= raw_exact_inf_est ~ 1,
                                                  direction %in% either & twosided_direction == "inferior" & exact_inf_est < raw_exact_inf_est ~ 0))
  
  
  chance_summary <-
    chance_comparisons %>%
    dplyr::group_by(pairing) %>% 
    dplyr::summarise(chance_psup = mean(chance_psup),
                     chance_exact = mean(chance_exact)) %>% 
    dplyr::mutate(corrected_chance_psup = dplyr::case_when(direction %in% either ~ chance_psup * 2,
                                                           TRUE ~ chance_psup),
                  corrected_chance_exact = dplyr::case_when(direction %in% either ~ chance_exact * 2,
                                                            TRUE ~ chance_exact),
                  direction = direction) %>% 
    dplyr::select(pairing,
                  direction,
                  chance_psup,
                  chance_exact,
                  corrected_chance_psup,
                  corrected_chance_exact) %>% 
    dplyr::mutate(raw_psup_sup_est = raw_summary$raw_psup_sup_est,
                  raw_psup_inf_est = raw_summary$raw_psup_inf_est,
                  raw_exact_sup_est = raw_summary$raw_exact_sup_est,
                  raw_exact_inf_est = raw_summary$raw_exact_inf_est)
  
  if(direction %in% either) {
    warning("Because you chose a non-directional comparison, you should use the corrected chance value, or divide your chance threshold by 2")
  }
  
  if(verbose == TRUE) {
    return(list("comparisons" = chance_comparisons,
                "summary" = chance_summary))
  }
  else {
    return(chance_summary)
  }
  
}