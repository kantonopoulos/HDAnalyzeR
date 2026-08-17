#' Correlate data
#'
#' `hd_correlate()` calculates the correlation matrix of the input dataset.
#'
#' @param x A numeric vector, matrix or tibble.
#' @param y A numeric vector, matrix or tibble with compatible dimensions with `x`. Default is NULL.
#' @param use  A character string. The method to use for computing correlations. Default is "pairwise.complete.obs". Other options are "everything", "all.obs", "complete.obs", or "na.or.complete".
#' @param method A character string. The correlation method to use. Default is "pearson". Other options are "kendall" or "spearman".
#'
#' @return A correlation matrix.
#' @details
#' You can read more about the method for computing covariances in the presence of missing values
#' and the coefficient that is calculated in the documentation of the `cor()` function in the `stats` package.
#'
#' When the input contains no missing values, `"pairwise.complete.obs"` is
#' silently replaced by `"everything"`. The two are equivalent in that case, but
#' the pairwise code path compares every pair of columns separately and is
#' several times slower on datasets with thousands of features.
#'
#' @export
#'
#' @examples
#' # Correlate features in a dataset (column wise)
#' dat <- example_data |>
#'  dplyr::select(DAid, Assay, NPX) |>
#'  tidyr::pivot_wider(names_from = "Assay", values_from = "NPX") |>
#'  dplyr::select(-DAid)
#'
#' hd_correlate(dat)[seq_len(5), seq_len(5)]  # Subset of the correlation matrix
#'
#' # Correlate 2 vectors
#' vec1 <- c(1, 2, 3, 4, 5)
#' vec2 <- c(5, 4, 3, 2, 1)
#' hd_correlate(vec1, vec2)
hd_correlate <- function(x, y = NULL, use = "pairwise.complete.obs", method = "pearson") {

  # Without missing values the pairwise machinery is redundant, just slower
  if (identical(use, "pairwise.complete.obs") &&
      !anyNA(x) &&
      (is.null(y) || !anyNA(y))) {
    use <- "everything"
  }

  cor_matrix <- round(
    stats::cor(x, y, use = use, method = method),
    2
  )

  return(cor_matrix)
}


#' Extract the feature pairs above a correlation threshold
#'
#' `cor_pairs_above()` pulls the entries of a correlation matrix whose absolute
#' value exceeds a threshold, without expanding the matrix into a long table.
#'
#' @param cor_matrix A correlation matrix.
#' @param threshold The reporting correlation threshold.
#' @param chunk_size The number of columns to scan at a time. Default is 1000.
#'
#' @return A tibble with `Protein1`, `Protein2` and `Correlation`, sorted by
#' correlation in decreasing order. Each pair appears in both directions, as it
#' does in the correlation matrix itself.
#' @details
#' Reshaping the matrix to long format allocates one row per entry. At 10,000
#' features that is 100 million rows across three columns, which exhausts memory
#' before any filtering happens. Scanning blocks of columns and keeping only the
#' entries above the threshold makes the cost proportional to the number of
#' reported pairs instead.
#' @keywords internal
cor_pairs_above <- function(cor_matrix, threshold, chunk_size = 1000) {
  cor_matrix <- as.matrix(cor_matrix)

  row_names <- rownames(cor_matrix)
  if (is.null(row_names)) {
    row_names <- as.character(seq_len(nrow(cor_matrix)))
  }
  col_names <- colnames(cor_matrix)
  if (is.null(col_names)) {
    col_names <- as.character(seq_len(ncol(cor_matrix)))
  }

  pieces <- list()
  for (start in seq(1, ncol(cor_matrix), by = chunk_size)) {
    cols <- seq(start, min(start + chunk_size - 1, ncol(cor_matrix)))
    block <- cor_matrix[, cols, drop = FALSE]

    # `which()` drops NAs, matching the behaviour of a filter on a long table
    hits <- which(block > threshold | block < -threshold, arr.ind = TRUE)
    if (nrow(hits) == 0) {
      next
    }

    protein1 <- row_names[hits[, 1]]
    protein2 <- col_names[cols[hits[, 2]]]
    keep <- protein1 != protein2

    pieces[[length(pieces) + 1]] <- tibble::tibble(
      Protein1 = protein1[keep],
      Protein2 = protein2[keep],
      Correlation = block[hits][keep]
    )
  }

  cor_results <- dplyr::bind_rows(pieces)
  if (nrow(cor_results) == 0) {
    return(tibble::tibble(
      Protein1 = character(0),
      Protein2 = character(0),
      Correlation = numeric(0)
    ))
  }

  cor_results[
    order(cor_results[["Correlation"]], decreasing = TRUE),
    ,
    drop = FALSE
  ]
}


#' Plot correlation heatmap
#'
#' `hd_plot_cor_heatmap()` calculates the correlation matrix of the input dataset.
#' It creates a heatmap of the correlation matrix. This matrix is created via `hd_correlate()`.
#' It also filters the feature pairs with correlation values above the threshold and
#' returns them in a tibble.
#'
#' @param x A numeric vector, matrix or data frame.
#' @param y A numeric vector, matrix or data frame with compatible dimensions with `x`. Default is NULL.
#' @param use A character string. The method to use for computing correlations.
#' Default is "pairwise.complete.obs". Other options are "everything", "all.obs", "
#' complete.obs", or "na.or.complete".
#' @param method A character string. The correlation method to use.
#' Default is "pearson". Other options are "kendall" or "spearman".
#' @param threshold The reporting correlation threshold. Default is 0.8.
#' @param cluster_rows Whether to cluster the rows. Default is TRUE.
#' @param cluster_cols Whether to cluster the columns. Default is TRUE.
#' @param max_heatmap_features The largest number of features to draw a heatmap for. Default is 1000. Above this the correlation matrix and the reported pairs are still returned, but the heatmap is skipped.
#'
#' @return A list with the correlation matrix, the filtered pairs and their correlation values, and the heatmap.
#'
#' @details
#' Drawing the heatmap requires hierarchical clustering of every feature and a
#' cell per feature pair, both of which grow quadratically. Past a few thousand
#' features the plot stops being readable long before it stops being computable,
#' so `max_heatmap_features` caps it: beyond that the correlation matrix and the
#' reported pairs are returned as usual and `cor_heatmap` is `NULL`. Raise the
#' limit to force the plot, or subset the data to the features of interest.
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
#' results$cor_matrix[seq_len(5), seq_len(5)]  # Subset of the correlation matrix
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
                                cluster_cols = TRUE,
                                max_heatmap_features = 1000) {

  cor_matrix <- hd_correlate(x = x, y = y, use = use, method = method)

  cor_results <- cor_pairs_above(cor_matrix, threshold)

  # NROW/NCOL rather than nrow/ncol: cor() drops to a scalar for two vectors
  n_features <- max(NROW(cor_matrix), NCOL(cor_matrix))

  if (n_features > max_heatmap_features) {
    warning(
      "Skipping the correlation heatmap: ", n_features, " features exceed the ",
      max_heatmap_features, " feature limit. Clustering and drawing a heatmap ",
      "this size is prohibitively slow and unreadable. The correlation matrix ",
      "and the reported pairs are still returned. Raise ",
      "`max_heatmap_features` or subset the data to plot a heatmap.",
      call. = FALSE
    )
    cor_plot <- NULL
  } else {
    cor_long <- as.data.frame(as.table(cor_matrix),
                              .name_repair = "minimal",
                              stringsAsFactors = FALSE)

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
  }

  corr_object <- list("cor_matrix" = cor_matrix,
                      "cor_results" = cor_results,
                      "cor_heatmap" = cor_plot)
  class(corr_object) <- "hd_corr"
  return(corr_object)
}
