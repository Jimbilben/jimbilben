#' Do KMeans
#'
#' Performs a kmeans clustering analysis, with plots that show cluster characteristics, as well as returning data linking ids to cluster numbers.
#'
#' @param nclusters Numeric, indicating the number of clusters to generate (use NbClust::NbClust() to determine how many clusters are best)
#' @param data_with_id Your data to be clustered. It should include a unique id column and the target columns to be clustered on. Rows containing missing values will be removed.
#' @param id_name A string indicating the name of your unique id column, defaults to "id"
#' @param decimals Numeric, indicating the number of decimal places to display in the output graphs. Defaults to 1

#'
#' @export

do_kmeans <- function(nclusters = 3,
                      data_with_id,
                      id_name = "id",
                      decimals = 1) {

  pre <- nrow(data_with_id)

  # in case NAs are in fact included in the data that is input:
  data_with_id <- tidyr::drop_na(data_with_id)

  post <- nrow(data_with_id)

  if(post < pre) {
    message(glue::glue("{pre - post} row(s) dropped from the data due to containing missing values."))
  }

  ids <- pull(data_with_id[1:nrow(data_with_id), id_name])

  data_for_kmeans <-
    data_with_id[, -which(names(data_with_id) == id_name)]

  km_out <-
    stats::kmeans(data_for_kmeans,
                  centers = nclusters)

  km_cluster_means <-
    km_out$centers %>%
    tidyr::as_tibble()

  km_cluster_means$Cluster <- 1:nclusters

  if(nclusters <= 12) {
    color_values <-
      c("1" = "#90dac8", "2" = "#efa800", "3" = "#39789f", "4" = "#ffc1c1",
        "5" = "#7aa277", "6" = "#cccccc", "7" = "#2cabde", "8" = "#8a6d2a",
        "9" = "#afe1ff", "10" = "#ba4b4b", "11" = "#346c30", "12" = "#5a5a5a")
  }
  else {
    color_values <- MetBrewer::met.brewer("Cross", nclusters)[1:nclusters]
  }

  cluster_plot <-
    factoextra::fviz_cluster(list(data = data_for_kmeans,
                                  cluster = km_out$cluster),
                             geom = "point", alpha = .4) +
    ggplot2::scale_color_manual(values = color_values, limits = force) +
    ggplot2::scale_fill_manual(values = color_values, limits = force) +
    jimbilben::theme_jimbilben_dark(base_size = 10)

  km_cluster_means_long <-
    km_cluster_means %>%
    tidyr::pivot_longer(cols = -Cluster,
                        names_to = "Item",
                        values_to = "Mean")

  ncols <-
    if(nclusters < 6) {
      1
    }
  else if(nclusters < 12) {
    2
  }
  else {
    3
  }

  # install tidytext and use reorder within
  average_per_cluster <-
    km_cluster_means_long %>%
    dplyr::mutate(Item = tidytext::reorder_within(Item, Mean, Cluster)) %>%
    ggplot2::ggplot() +
    tidytext::scale_y_reordered() +
    ggplot2::scale_x_continuous(expand = expansion(mult = c(0, .1))) +
    ggplot2::geom_col(aes(y = Item, x = Mean, fill = as.factor(Cluster)), alpha = .67, width = .75) +
    ggplot2::geom_text(aes(y = Item, x = Mean, label = jimbilben::nice_num(Mean, decimals, FALSE)), hjust = 1.5, size = 2.5, family = "Jost") +
    labs(fill = "Cluster") +
    ggplot2::facet_wrap(~Cluster, ncol = ncols, scales = "free_y") +
    ggplot2::scale_fill_manual(values = color_values, limits = force) +
    jimbilben::theme_jimbilben_dark(base_size = 10) +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_blank()
    )

  km_cluster_means_avg <-
    km_cluster_means_long %>%
    dplyr::group_by(Item) %>%
    dplyr::summarise(mean = mean(Mean))

  number_of_items <- nrow(km_cluster_means_avg)

  subtract_mean <- function(item) {

    filter_by_item <-
      km_cluster_means_long %>%
      dplyr::filter(Item == item)

    filter_by_item$vs_mean <- filter_by_item$Mean - km_cluster_means_avg[[which(km_cluster_means_avg$Item == item), "mean"]]

    return(filter_by_item)
  }

  km_cluster_means_long <-
    purrr::map_df(.x = km_cluster_means_avg$Item,
                  .f = subtract_mean)

  cluster_differences <-
    ggplot2::ggplot(km_cluster_means_long) +
    ggplot2::scale_x_continuous(breaks = 1:nclusters) +
    ggplot2::geom_col(aes(x = Cluster, y = vs_mean, fill = as.factor(Cluster)), alpha = .67, width = .75) +
    ggplot2::geom_text(aes(x = Cluster, y = 0, label = jimbilben::nice_num(vs_mean, decimals, FALSE)), vjust = -.2, size = 2.5, family = "Jost") +
    ggplot2::geom_hline(aes(yintercept = 0), color = "grey70") +
    labs(title = "Difference of cluster mean vs. average of cluster means",
         fill = "Cluster") +
    ggplot2::facet_wrap(~Item, ncol = 2) +
    ggplot2::scale_fill_manual(values = color_values, limits = force) +
    jimbilben::theme_jimbilben_dark(base_size = 10) +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_blank())

  id_cluster_tibble <-
    dplyr::tibble(cluster = km_out$cluster,
                  id = ids)

  names(id_cluster_tibble)[2] <- id_name


  output <- list("kmeans" = km_out,
                 "id_cluster_tibble" = id_cluster_tibble,
                 "cluster_means_data" = km_cluster_means_long,
                 "cluster_plot" = cluster_plot,
                 "average_per_cluster" = average_per_cluster,
                 "cluster_differences" = cluster_differences)

  return(output)

}
