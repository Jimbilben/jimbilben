#' Mutate Mean Label
#'
#' Converts numeric point estimate and confidence interval columns into a formatted mean label. If linebreak is set to TRUE, it adds a line break before the confidence interval. If linebreak is set to FALSE, the entire label is on one line.
#'
#' @param data The data for which the mutate will be made.
#' @param point_est The column name for point estimates - defaults to "mean".
#' @param lower The column name for lower bounds of confidence intervals - defaults to "lower".
#' @param upper The column name for upper bounds of confidence intervals - defaults to "upper".
#' @param size The font size for the confidence interval part of the label - defaults to 5.5.
#' @param linebreak A logical indicating whether to put the confidence interval on a new line. If TRUE, a line break is added before the confidence interval. Defaults to TRUE.
#' @return A dataframe with the original columns replaced by a new "mean_label" column.
#' @export

mutate_mean_label <- function(data,
                              point_est = "mean",
                              lower = "lower",
                              upper = "upper",
                              size = 5.5,
                              linebreak = TRUE) {


  if(linebreak == TRUE) {
    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(tmp_point_est = !!sym(point_est),
             tmp_lower = !!sym(lower),
             tmp_upper = !!sym(upper),
             mean_label = glue("**{jimbilben::nice_num(tmp_point_est, 1)}**<br><span style='color:#757575; font-size:{size}pt'>{jimbilben::nice_num(tmp_lower, 1, remove_lead = FALSE)} - {jimbilben::nice_num(tmp_upper, 1, remove_lead = FALSE)}</span>")) %>%
      dplyr::select(-tmp_point_est, -tmp_lower, -tmp_upper)
  }
  else {
    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(tmp_point_est = !!sym(point_est),
             tmp_lower = !!sym(lower),
             tmp_upper = !!sym(upper),
             mean_label = glue("**{jimbilben::nice_num(tmp_point_est, 1)}** <span style='color:#757575; font-size:{size}pt'>[{jimbilben::nice_num(tmp_lower, 1, remove_lead = FALSE)} - {jimbilben::nice_num(tmp_upper, 1, remove_lead = FALSE)}]</span>")) %>%
      dplyr::select(-tmp_point_est, -tmp_lower, -tmp_upper)
  }


  return(data)

}
