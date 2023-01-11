#' Assess Weighting
#'
#' Takes your target weighting and your data and returns data and plots compared your observed %ages with their desired targets. This can be used to help select what variables to include and determine the degree of deviation from the targets in the sample.
#' @param targets A list of weighting targets (see the gen_us() function to generate these easily)
#' @param data Data for which you want to generate weights.

assess_weighting <- function(targets,
                             data) {

  target_names <- names(targets)

  compare_obs_target <- function(var_name,
                                 .data) {

    target_tibble <-
      dplyr::tibble(level = names(targets[[var_name]]),
                    pct = targets[[var_name]] * 100,
                    variable = var_name,
                    type = "Target")

    data_tibble <-
      .data %>%
      tidystats::count_data((!!sym(var_name))) %>%
      dplyr::mutate(level = (!!sym(var_name)),
                    variable = var_name,
                    type = "Observed") %>%
      dplyr::select(level, pct, variable, type)

    combo_tibble <-
      bind_rows(target_tibble,
                data_tibble)

    comparison_tibble <-
      combo_tibble %>%
      tidyr::pivot_wider(id_cols = c(level, variable), names_from = type, values_from = pct) %>%
      dplyr::mutate(Difference = Observed - Target,
                    Ratio = Observed / Target)

    ############

    if(var_name == "education") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Less than high school', 'Graduated from high school', 'Some college, no degree', 'Graduated from college', 'Completed graduate school'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Less than high school', 'Graduated from high school', 'Some college, no degree', 'Graduated from college', 'Completed graduate school'))

    }
    else if(var_name == "education_collapse") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('High school or less', 'Some college, no degree', 'Graduated from college', 'Completed graduate school'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('High school or less', 'Some college, no degree', 'Graduated from college', 'Completed graduate school'))

    }
    else if(var_name == "sex") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Male', 'Female'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Male', 'Female'))

    }
    else if(var_name == "race") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Asian or Asian American', 'Black or African American', 'Hispanic or Latino', 'White or Caucasian', 'Other'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Asian or Asian American', 'Black or African American', 'Hispanic or Latino', 'White or Caucasian', 'Other'))

    }

    else if(var_name == "race_collapse") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Black or African American', 'Hispanic or Latino', 'White or Caucasian', 'Other'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Black or African American', 'Hispanic or Latino', 'White or Caucasian', 'Other'))


    }
    else if(var_name == "age") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c("18-24", "25-44", "45-64", "65+"))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c("18-24", "25-44", "45-64", "65+"))

    }
    else if(var_name == "age_alt" | var_name == "age_alternative") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c("18-24", "25-39", "40-54", "55+"))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c("18-24", "25-39", "40-54", "55+"))

    }
    else if(var_name == "income" | var_name == "income_ces") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Under $20,000', 'Between $20,000 and $49,999', 'Between $50,000 and $79,999', 'Between $80,000 and $99,999', 'Between $100,000 and $150,000', 'Over $150,000'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Under $20,000', 'Between $20,000 and $49,999', 'Between $50,000 and $79,999', 'Between $80,000 and $99,999', 'Between $100,000 and $150,000', 'Over $150,000'))
    }

    else if(var_name == "region") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Midwest', 'Northeast', 'South', 'West'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Midwest', 'Northeast', 'South', 'West'))

    }
    else if(var_name == "division") {


      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('East North Central', 'East South Central', 'Mid Atlantic', 'Mountain', 'New England', 'Pacific', 'South Atlantic', 'West North Central', 'West South Central'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('East North Central', 'East South Central', 'Mid Atlantic', 'Mountain', 'New England', 'Pacific', 'South Atlantic', 'West North Central', 'West South Central'))

    }
    else if(var_name == "trust") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c("Can trust", "Can't be too careful"))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c("Can trust", "Can't be too careful"))

    }
    else if(var_name == "spanking") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Strongly disagree','Disagree', 'Agree', 'Strongly disagree'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Strongly disagree','Disagree', 'Agree', 'Strongly disagree'))

    }
    else if(var_name == "bible") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('word of god', 'inspired word', 'book of fables'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('word of god', 'inspired word', 'book of fables'))

    }
    else if(var_name == "polviews" | var_name == "lib_con" | var_name == "left_right" | var_name == "libcon") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Conservative', 'Moderate', 'Liberal'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Conservative', 'Moderate', 'Liberal'))

    }
    else if(grepl("urban", var_name) | grepl("rural", var_name)) {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Urban', 'Suburban', 'Rural'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Urban', 'Suburban', 'Rural'))

    }
    else if(var_name == "conmedic" | grepl("medicine", var_name)) {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Hardly any', 'Only some', 'A great deal'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Hardly any', 'Only some', 'A great deal'))
    }
    else if(var_name == "consci" | grepl("science", var_name)) {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c("Hardly any", "Only some", "A great deal"))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c("Hardly any", "Only some", "A great deal"))
    }
    else if(var_name == "partyid") {

      combo_tibble$level <- factor(combo_tibble$level,
                                   levels = c('Republican', 'Independent', 'Democrat'))
      comparison_tibble$level <- factor(comparison_tibble$level,
                                        levels = c('Republican', 'Independent', 'Democrat'))

    }

    ############
    nblock_start <- get_xpos(data = combo_tibble, variable = "pct", number_block_multiple = 1.3)$start
    nblock_middle <- get_xpos(data = combo_tibble, variable = "pct", number_block_multiple = 1.3)$middle

    perc_pos <- (nblock_start + nblock_middle) / 2
    diff_pos <- nblock_middle + ((nblock_middle - nblock_start)/2)

    output_plot <-
      ggplot2::ggplot(combo_tibble) +
      jimbilben::nice_x(data = combo_tibble, variable = "pct", number_block = TRUE, number_block_multiple = 1.3) +
      jimbilben::geom_xblock(xmin = nblock_start, fill = "grey98", alpha = 1) +
      ggplot2::geom_col(aes(y = level, x = pct, fill = type), position = position_dodge(.8), width = .8, alpha = .9, color = "grey20") +
      ggplot2::geom_text(aes(label = glue::glue("{round(pct, 1)}%"), y = level, x = perc_pos, color = type), position = position_dodge(.8), size = 3, family = "Jost", show.legend = FALSE) +
      ggplot2::geom_text(data = comparison_tibble, aes(label = glue::glue("{round(Difference, 1)}%\n{round(Ratio, 1)}"), y = level, x = diff_pos), size = 3, family = "Jost", color = "black") +
      ggplot2::scale_color_manual(values = c("black", "black")) +
      ggplot2::scale_fill_manual(values = c("#216d99", "#f3cf1f")) +
      ggplot2::labs(title = glue::glue({var_name}),
                    y = "Level of variable",
                    x = "Percentage") +
      jimbilben::theme_jimbilben_dark() +
      ggplot2::theme(legend.position = "bottom",
                     legend.title = element_blank(),
                     axis.line.y = element_line(color = "black"),
                     axis.title.y = element_blank())

    return(list("plot" = output_plot,
                "comparison" = comparison_tibble))

  }

  output <-
    purrr::map(.x = target_names,
               .f = compare_obs_target,
               .data = data)

  extract_comparisons <- function(number) {
    comparison <- output[[number]]$comparison
    return(comparison)
  }

  extract_plots <- function(number) {
    output_plot <- output[[number]]$plot
    return(output_plot)
  }


  output_data <- purrr::map(1:length(targets),
                            .f = extract_comparisons)
  names(output_data) <- names(targets)

  output_plots <- purrr::map(1:length(targets),
                             .f = extract_plots)
  names(output_plots) <- names(targets)


  return(list("comparisons" = output_data,
              "plots" = output_plots))

}
