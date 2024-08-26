#' Do Principal Component Analysis (PCA)
#'
#' This function performs Principal Component Analysis (PCA) on a given dataset,
#' excluding the specified ID column. It also generates various plots and diagnostics
#' related to the PCA, including scree plots, biplots, and loading plots.
#'
#' @param data A data frame or tibble containing the data for PCA. The dataset should
#'   include a column specified by the `id` argument that will be excluded from the analysis.
#' @param id A character string specifying the name of the column to be excluded from the PCA.
#'   This column typically contains row identifiers. Default is "rowid".
#' @param .center A logical value indicating whether the variables should be centered before
#'   PCA is performed. Default is TRUE.
#' @param .scale A logical value indicating whether the variables should be scaled before
#'   PCA is performed. Default is TRUE.
#' @param limit_comps An optional integer specifying the number of principal components to
#'   limit in the loadings plot. If NULL, all components are included. Default is NULL.
#'
#' @return A list containing the following elements:
#'   \item{pca}{The PCA object resulting from `prcomp`.}
#'   \item{individual_scores}{A tibble of individual scores (principal component scores) with
#'   the specified ID column included.}
#'   \item{scree}{A ggplot object representing the scree plot of eigenvalues.}
#'   \item{loadings_plot}{A ggplot object representing the loadings plot of the PCA components.}
#'   \item{plot_1}{A ggplot object representing the PCA individuals plot, colored by cos2 values.}
#'   \item{plot_2}{A ggplot object representing the PCA variables plot, colored by contributions.}
#'   \item{plot_3}{A ggplot object representing the PCA biplot.}
#'   \item{loadings}{A tibble of PCA loadings with the variable names and their respective loadings.}
#'   \item{var_diagnostics}{A list of diagnostics related to the PCA variables from `factoextra::get_pca_var`.}
#'   \item{ind_diagnostics}{A list of diagnostics related to the PCA individuals from `factoextra::get_pca_ind`.}
#'
#' @examples
#' # Example usage of the do_pca function
#' data <- mtcars
#' result <- do_pca(data, id = "carb")
#' result$scree
#' result$plot_1
#'
#' @import dplyr
#' @import factoextra
#' @import ggplot2
#' @import tibble
#' @import tidyr
#' @export
do_pca <- function(data,
                   id = "rowid",
                   .center = TRUE,
                   .scale = TRUE,
                   limit_comps = NULL) {

  pca <-
    prcomp(data %>% dplyr::select(-!!sym(id)),
           center = .center,
           scale. = .scale)

  num_scores <-
    ncol(data %>% dplyr::select(-!!sym(id)))

  scree <-
    factoextra::fviz_eig(pca,
                         ncp = num_scores,
                         barfill = "#f4cdcd",
                         barcolor = "#f4cdcd") +
    jimbilben::theme_jimbilben(10)

  plot_1 <-
    factoextra::fviz_pca_ind(pca,
                             col.ind = "cos2", # Color by the quality of representation
                             gradient.cols = c("#98cbe4", "#f2b831", "#ea5b21"),
                             repel = TRUE     # Avoid text overlapping
    ) +
    ggplot2::theme(
      legend.position = "right",
      text = ggplot2::element_text(family = "Jost", size = 10, color = "black"),
      panel.background = ggplot2::element_rect(fill = NA, color = NA)
    )

  plot_2 <-
    factoextra::fviz_pca_var(pca,
                             col.var = "contrib", # Color by contributions to the PC
                             gradient.cols = c("#98cbe4", "#f2b831", "#ea5b21"),
                             repel = TRUE     # Avoid text overlapping
    ) +
    ggplot2::theme(
      legend.position = "right",
      text = ggplot2::element_text(family = "Jost", size = 10, color = "black"),
      panel.background = ggplot2::element_rect(fill = NA, color = NA)
    )

  plot_3 <-
    factoextra::fviz_pca_biplot(pca,
                                repel = TRUE,
                                col.var = "#b9441e", # Variables color
                                col.ind = "grey80"  # Individuals color
    ) +
    ggplot2::theme(
      legend.position = "right",
      text = ggplot2::element_text(family = "Jost", size = 10, color = "black"),
      panel.background = ggplot2::element_rect(fill = NA, color = NA)
    )

  variable_diagnostics <-
    factoextra::get_pca_var(pca)

  individual_diagnostics <-
    factoextra::get_pca_ind(pca)

  rotation <-
    pca$rotation %>%
    tibble::as_tibble() %>%
    dplyr::mutate(items = data %>% dplyr::select(-!!sym(id)) %>% names(),
                  item_nr = 1:num_scores) %>%
    dplyr::relocate(items,
                    item_nr)

  if(!is.null(limit_comps)) {
    rotation_plot_data <-
      rotation[,c(1:(limit_comps + 2))]
  }
  else {
    rotation_plot_data <-
      rotation
  }

  rotation_plot <-
    rotation_plot_data %>%
    tidyr::pivot_longer(cols = starts_with("PC"),
                        names_to = "Component",
                        values_to = "Loading") %>%
    dplyr::mutate(Component = factor(Component,
                                     levels = names(rotation)[-c(1:2)])) %>%
    ggplot2::ggplot(aes(x = Component, y = item_nr, fill = Loading)) +
    ggplot2::scale_y_continuous(breaks = 1:num_scores, labels = rotation$items, limits = c(.5, num_scores + .5), expand = expansion(0)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(aes(label = jimbilben::nice_num(Loading, 2)), size = 2.8, family = "Jost") +
    ggplot2::scale_fill_gradient2(low = "#98cbe4",
                                  mid = "white",
                                  high = "#ea5b21",
                                  midpoint = 0,
                                  limits = c(-1, 1)) +
    ggplot2::labs(x = "",
                  y = "",
                  fill = "") +
    ggplot2::theme(
      legend.position = "right",
      text = ggplot2::element_text(family = "Jost", size = 10, color = "black"),
      panel.background = ggplot2::element_rect(fill = NA, color = NA)
    )

  individual_scores <-
    pca$x %>%
    tibble::as_tibble() %>%
    dplyr::mutate({{id}} := data %>% dplyr::pull(!!sym(id))) %>%
    dplyr::relocate(!!sym(id))

  return(list("pca" = pca,
              "individual_scores" = individual_scores,
              "scree" = scree,
              "loadings_plot" = rotation_plot,
              "plot_1" = plot_1,
              "plot_2" = plot_2,
              "plot_3" = plot_3,
              "loadings" = rotation,
              "var_diagnostics" = variable_diagnostics,
              "ind_diagnostics" = individual_diagnostics
  ))

}
