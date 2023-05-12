#' Quick Scatter Plot for Exploratory Analysis
#'
#' This function creates a scatter plot to explore the relationship between two numeric variables,
#' with options to display a correlation, linear regression, spline, or loess fitline,
#' and display summary statistics on the plot.
#'
#' @param x A string representing the name of the x-axis variable
#' @param y A string representing the name of the y-axis variable
#' @param plot_data The data frame containing the x and y variables (default: ai_data)
#' @param corr Logical, whether to display the Pearson and Spearman correlation coefficients (default: TRUE)
#' @param fitline A character vector specifying the type(s) of fitline to display: "lm", "spline", or "loess" (default: c("lm", "loess"))
#' @param descriptives Logical, whether to display summary statistics on the plot (default: TRUE)
#' @param df Degrees of freedom for the spline fitline (default: 3)
#' @param jitter_height Numeric, the height of the jitter in geom_jitter (default: .2)
#' @param jitter_width Numeric, the width of the jitter in geom_jitter (default: .2)
#' @param alpha Numeric, the transparency of the jitter points (default: .2)
#' @param dot_color A string representing the color of the geom_jitter points (default: "grey65")
#' @param ... Additional arguments passed to ggplot2::aes()
#'
#' @return A ggplot2 scatter plot object
#'
#' @examples
#' quick_scatter(x = "income", y = "happiness", plot_data = my_data, fitline = c("lm", "loess"), dot_color = "#336699")
#'
#' @export
quick_scatter <- function(x, y, plot_data = ai_data, corr = TRUE, fitline = c("lm", "loess"), descriptives = TRUE, df = 3, jitter_height = .2, jitter_width = .2, alpha = .2, dot_color = "grey65", ...) {

  my_plot <-
    ggplot2::ggplot(data = plot_data, aes(x = !!rlang::sym(x), y = !!rlang::sym(y), ...)) +
    ggplot2::geom_jitter(alpha = alpha, height = jitter_height, width = jitter_width, color = dot_color) +
    ggplot2::labs(
      title = glue::glue("{x} *by* {y}")
    )

  if(corr == TRUE) {

    correlations <- dplyr::bind_rows(stats::cor.test(x = dplyr::pull(plot_data, {{x}}), y = dplyr::pull(plot_data, {{y}})) %>% broom::tidy(),
                                     stats::cor.test(x = dplyr::pull(plot_data, {{x}}), y = dplyr::pull(plot_data, {{y}}), method = "spearman") %>% broom::tidy())

    summary_text <- glue::glue("Pearson: {jimbilben::nice_num(correlations[1, 'estimate'], 2, TRUE)}, p = {jimbilben::nice_num(correlations[1, 'p.value'], 2, TRUE)}<br>Spearman: {jimbilben::nice_num(correlations[2, 'estimate'], 2, TRUE)}, p = {jimbilben::nice_num(correlations[2, 'p.value'], 2, TRUE)}")

    my_plot <-
      my_plot +
      ggplot2::labs(subtitle = summary_text)
  }

  if(grepl("lm", paste(fitline, collapse = " "))) {

    my_plot <-
      my_plot +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#efa800", fill = "#efa800", alpha = .2)

  }
  if(grepl("spline", paste(fitline, collapse = " "))) {

    my_plot <-
      my_plot +
      ggplot2::geom_smooth(method = "lm", formula = y ~ splines::ns(x, df = df), se = TRUE, color = "#39789f", fill = "#39789f", alpha = .2)
  }
  if(grepl("loess", paste(fitline, collapse = " "))) {
    my_plot <-
      my_plot +
      ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#ee6864", fill = "#ee6864", alpha = .2)
  }


  if(descriptives == TRUE) {

    my_summary <-
      plot_data %>%
      dplyr::summarise(mean_x = mean(!!sym(x), na.rm = TRUE),
                       mean_y = mean(!!sym(y), na.rm = TRUE),
                       median_x = median(!!sym(x), na.rm = TRUE),
                       median_y = median(!!sym(y), na.rm = TRUE),
                       sd_x = sd(!!sym(x), na.rm = TRUE),
                       sd_y = sd(!!sym(y), na.rm = TRUE))

    my_summary_long <-
      my_summary %>%
      dplyr::select(mean_x:median_y) %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "type",
                          values_to = "value") %>%
      dplyr::mutate(`Stat:` = dplyr::case_when(grepl("mean", type) ~ "Mean",
                                               grepl("median", type) ~ "Median"))

    my_plot <-
      my_plot +
      ggplot2::geom_hline(data = my_summary_long %>% dplyr::filter(grepl("y", type)), aes(yintercept = value, linetype = `Stat:`), alpha = .4) +
      ggplot2::geom_vline(data = my_summary_long %>% dplyr::filter(grepl("x", type)), aes(xintercept = value, linetype = `Stat:`), alpha = .4) +
      ggplot2::labs(caption = glue::glue("SD x = {jimbilben::nice_num(purrr::pluck(my_summary[1, 'sd_x']))}, SD y = {jimbilben::nice_num(purrr::pluck(my_summary[1, 'sd_y']))}"))

  }

  return(my_plot)

}
