#' Correlate data
#'
#' `hd_run_correlation()` calculates the correlation matrix of the input dataset.
#'
#' @param x A numeric vector, matrix or tibble.
#' @param y A numeric vector, matrix or tibble with compatible dimensions with `x`. Default is NULL.
#' @param use  A character string. The method to use for computing correlations. Default is "pairwise.complete.obs". Other options are "everything", "all.obs", "complete.obs", or "na.or.complete".
#' @param method A character string. The correlation method to use. Default is "pearson". Other options are "kendall" or "spearman".
#'
#' @return A matrix of protein-protein correlations.
#' @export
#'
#' @examples
#' # Correlate features in a dataset (column wise)
#' dat <- example_data |>
#'  dplyr::select(DAid, Assay, NPX) |>
#'  tidyr::pivot_wider(names_from = "Assay", values_from = "NPX") |>
#'  dplyr::select(-DAid)
#'
#' hd_run_correlation(dat)[1:5, 1:5]  # Subset of the correlation matrix
#'
#' # Correlate 2 vectors
#' vec1 <- c(1, 2, 3, 4, 5)
#' vec2 <- c(5, 4, 3, 2, 1)
#' hd_run_correlation(vec1, vec2)
hd_run_correlation <- function(x, y = NULL, use = "pairwise.complete.obs", method = "pearson") {

  cor_matrix <- round(
    stats::cor(x, y, use = use, method = method),
    2
  )

  return(cor_matrix)
}


#' Plot correlation heatmap
#'
#' `hd_plot_cor_heatmap()` calculates the correlation matrix of the input dataset.
#' It creates a heatmap of the correlation matrix. It also filters the feature
#' pairs with correlation values above the threshold and returns them in a tibble.
#'
#' @param x A numeric vector, matrix or data frame.
#' @param y A numeric vector, matrix or data frame with compatible dimensions with `x`. Default is NULL.
#' @param use A character string. The method to use for computing correlations.
#' Default is "pairwise.complete.obs". Other options are "everything", "all.obs", "
#' complete.obs", or "na.or.complete".
#' @param method A character string. The correlation method to use.
#' Default is "pearson". Other options are "kendall" or "spearman".
#' @param threshold The reporting protein-protein correlation threshold. Default is 0.8.
#' @param cluster_rows Whether to cluster the rows. Default is TRUE.
#' @param cluster_cols Whether to cluster the columns. Default is TRUE.
#'
#' @return A list with the correlation matrix, the filtered pairs and their correlation values, and the heatmap
#' @details
#' You can read more about the method for computing covariances in the presence of missing values
#' and the coefficient that is calculated in the documentation of the `cor()` function in the `stats` package.
#'
#' @export
#'
#' @examples
#' # Prepare data
#' dat <- example_data |>
#'   dplyr::select(DAid, Assay, NPX) |>
#'   tidyr::pivot_wider(names_from = "Assay", values_from = "NPX") |>
#'   dplyr::select(-DAid)
#'
#' # Correlate proteins
#' results <- hd_plot_cor_heatmap(dat, threshold = 0.7)
#'
#' # Print results
#' results$cor_matrix[1:5, 1:5]  # Subset of the correlation matrix
#'
#' results$cor_results  # Filtered protein pairs exceeding correlation threshold
#'
#' results$cor_heatmap  # Heatmap of protein-protein correlations
hd_plot_cor_heatmap <- function(x,
                                y = NULL,
                                use = "pairwise.complete.obs",
                                method = "pearson",
                                threshold = 0.8,
                                cluster_rows = TRUE,
                                cluster_cols = TRUE) {

  cor_matrix <- hd_run_correlation(x = x, y = y, use = use, method = method)

  cor_long <- as.data.frame(as.table(cor_matrix),
                            .name_repair = "minimal",
                            stringsAsFactors = FALSE)

  cor_results <- cor_long |>
    dplyr::filter(!!rlang::sym("Var1") != !!rlang::sym("Var2")) |>
    dplyr::filter(!!rlang::sym("Freq") > threshold |
                  !!rlang::sym("Freq") < -threshold) |>
    dplyr::arrange(dplyr::desc(!!rlang::sym("Freq"))) |>
    dplyr::rename(Protein1 = !!rlang::sym("Var1"),
                  Protein2 = !!rlang::sym("Var2"),
                  Correlation = !!rlang::sym("Freq"))

  cor_plot <- ggplotify::as.ggplot(
    tidyheatmaps::tidyheatmap(cor_long,
                              rows = !!rlang::sym("Var1"),
                              columns = !!rlang::sym("Var2"),
                              values = !!rlang::sym("Freq"),
                              cluster_rows = cluster_rows,
                              cluster_cols = cluster_cols,
                              show_selected_row_labels = c(""),
                              show_selected_col_labels = c(""),
                              color_legend_min = -1,
                              color_legend_max = 1,
                              treeheight_row = 20,
                              treeheight_col = 20,
                              silent = TRUE))

  corr_object <- list("cor_matrix" = cor_matrix,
                      "cor_results" = cor_results,
                      "cor_heatmap" = cor_plot)
  class(corr_object) <- "hd_corr"
  return(corr_object)
}
