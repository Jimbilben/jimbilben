#' Bayesian correlation posterior
#'
#' Generate the posterior for residual/pairwise correlations, the inverted correlation matrix, and partial correlation matrix.
#'
#' @param post A draw from a posterior of the brms rescor posterior object
#' @export
#'
bayes_corr_post <- function(post) {

  post_long <-
    post %>%
    tidyr::pivot_longer(starts_with("rescor"),
                        names_to = "pair",
                        values_to = "rescor") %>%
    tidyr::separate(pair,
                    sep = "__",
                    into = c("chuck",
                             "x",
                             "y")) %>%
    dplyr::select(-chuck)

  post_long_2 <-
    post_long

  post_long_2$x <- post_long$y
  post_long_2$y <- post_long$x

  pure_pair <-
    tibble::tibble(.draw = post_long$.draw[1],
                   x = unique(c(post_long$y, post_long$x) %>% sort()),
                   y = unique(c(post_long$y, post_long$x) %>% sort()),
                   rescor = 1)

  post_long <-
    dplyr::bind_rows(
      post_long,
      post_long_2,
      pure_pair
    )

  wide_df <-
    post_long %>%
    dplyr::arrange(x, y) %>%
    tidyr::pivot_wider(names_from = y,
                       values_from = rescor)

  wide_matrix <-
    wide_df %>%
    dplyr::select(-x, -.draw) %>%
    as.matrix()

  my_names <- colnames(wide_matrix)

  rownames(wide_matrix) <- my_names

  if (det(wide_matrix) != 0) {
    # Calculate the inverse of the correlation matrix
    inverse_matrix <- MASS::ginv(wide_matrix)
    #print(inverse_matrix)
  } else {
    print("The correlation matrix is singular and cannot be inverted.")
  }

  rownames(inverse_matrix) <- my_names
  colnames(inverse_matrix) <- my_names

  n <- nrow(inverse_matrix) # Number of variables
  partial_correlations <- matrix(0, n, n)

  rownames(partial_correlations) <- my_names
  colnames(partial_correlations) <- my_names

  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        partial_correlations[i, j] <- -inverse_matrix[i, j] / sqrt(inverse_matrix[i, i] * inverse_matrix[j, j])
      }
    }
  }

  inverse_df <-
    inverse_matrix %>%
    tibble::as_tibble() %>%
    dplyr::mutate(x = my_names) %>%
    tidyr::pivot_longer(cols = -x,
                        names_to = "y",
                        values_to = "invcor")

  partial_df <-
    partial_correlations %>%
    tibble::as_tibble() %>%
    dplyr::mutate(x = my_names) %>%
    tidyr::pivot_longer(cols = -x,
                        names_to = "y",
                        values_to = "pcor")

  output_df <-
    post_long %>%
    dplyr::left_join(inverse_df,
                     by = c("x", "y")) %>%
    dplyr::left_join(partial_df,
                     by = c("x", "y")) %>%
    dplyr::rename(draw = .draw)

  my_draw <-
    unique(output_df$draw)

  if(my_draw %% 50 == 0) {
    print(my_draw)
  }

  return(output_df)

}
