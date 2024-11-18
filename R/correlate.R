#' utils::globalVariables(c("Var1", "Var2", "Freq"))
#' #' Correlate data
#' #'
#' #' `correlate()` calculates the correlation matrix of the input dataset.
#' #'
#' #' @param x A numeric vector, matrix or tibble.
#' #' @param y A numeric vector, matrix or tibble with compatible dimensions with `x`. Default is NULL.
#' #' @param use  A character string. The method to use for computing correlations. Default is "pairwise.complete.obs".
#' #' @param method A character string. The correlation method to use. Default is "pearson".
#' #'
#' #' @return A matrix of protein-protein correlations.
#' #' @keywords internal
#' correlate <- function(x, y = NULL, use = "pairwise.complete.obs", method = "pearson") {
#'
#'   cor_matrix <- round(
#'     stats::cor(x, y, use = "pairwise.complete.obs", method = "pearson"),
#'     2
#'   )
#'
#'   return(cor_matrix)
#' }
#'
#'
#' #' Plot correlation heatmap
#' #'
#' #' `create_corr_heatmap()` calculates the correlation matrix of the input dataset.
#' #' It creates a heatmap of the correlation matrix. It also filters the protein
#' #' pairs with correlation values above the threshold and returns them in a tibble.
#' #'
#' #' @param x A numeric vector, matrix or data frame.
#' #' @param y A numeric vector, matrix or data frame with compatible dimensions with `x`. Default is NULL.
#' #' @param use A character string. The method to use for computing correlations. Default is "pairwise.complete.obs".
#' #' @param method A character string. The correlation method to use. Default is "pearson".
#' #' @param threshold The reporting protein-protein correlation threshold. Default is 0.8.
#' #' @param cluster_rows Whether to cluster the rows. Default is TRUE.
#' #' @param cluster_cols Whether to cluster the columns. Default is TRUE.
#' #' @param show_heatmap Whether to show the heatmap. Default is TRUE.
#' #'
#' #' @return A list containing the following elements:
#' #'   - cor_matrix: A matrix of protein-protein correlations.
#' #'   - cor_results: A tibble with the filtered protein pairs and their correlation values.
#' #'   - cor_plot: A heatmap of protein-protein correlations.
#' #' @export
#' #'
#' #' @examples
#' #' # Prepare data
#' #' df <- example_data |>
#' #'   dplyr::select(DAid, Assay, NPX) |>
#' #'   tidyr::pivot_wider(names_from = "Assay", values_from = "NPX") |>
#' #'   dplyr::select(-DAid)
#' #'
#' #' # Correlate proteins
#' #' results <- create_corr_heatmap(df, threshold = 0.7)
#' #'
#' #' # Print results
#' #' results$cor_plot  # Heatmap of protein-protein correlations
#' #'
#' #' results$cor_matrix[1:5, 1:5]  # Subset of the correlation matrix
#' #'
#' #' results$cor_results  # Filtered protein pairs exceeding correlation threshold
#' create_corr_heatmap <- function(x,
#'                                 y = NULL,
#'                                 use = "pairwise.complete.obs",
#'                                 method = "pearson",
#'                                 threshold = 0.8,
#'                                 cluster_rows = TRUE,
#'                                 cluster_cols = TRUE,
#'                                 show_heatmap = TRUE) {
#'
#'   cor_matrix <- correlate(x,
#'                           y = NULL,
#'                           use = "pairwise.complete.obs",
#'                           method = "pearson")
#'
#'   cor_long <- as.data.frame(as.table(cor_matrix), .name_repair = "minimal", stringsAsFactors = FALSE)
#'
#'   cor_results <- cor_long |>
#'     dplyr::filter(Var1 != Var2) |>
#'     dplyr::filter(Freq > threshold | Freq < -threshold) |>
#'     dplyr::arrange(dplyr::desc(Freq)) |>
#'     dplyr::rename(Protein1 = Var1, Protein2 = Var2, Correlation = Freq)
#'
#'   cor_plot <- tidyheatmaps::tidyheatmap(cor_long,
#'                                         rows = Var1,
#'                                         columns = Var2,
#'                                         values = Freq,
#'                                         cluster_rows = TRUE,
#'                                         cluster_cols = TRUE,
#'                                         show_selected_row_labels = c(""),
#'                                         show_selected_col_labels = c(""),
#'                                         color_legend_min = -1,
#'                                         color_legend_max = 1,
#'                                         treeheight_row = 20,
#'                                         treeheight_col = 20,
#'                                         silent = isFALSE(show_heatmap))
#'
#'   return(list("cor_matrix" = cor_matrix, "cor_results" = cor_results, "cor_plot" = cor_plot))
#' }
