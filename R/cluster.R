#' utils::globalVariables(c("v1", "v2", "val", "x", "y"))
#' #' Cluster data
#' #'
#' #' `cluster_data()` takes a dataset and returns the same dataset ordered
#' #' according to the hierarchical clustering of the rows and columns. This data
#' #' can be used to plot a heatmap with ggplot2 that is not having clustering functionality.
#' #'
#' #' @param df The dataset to cluster.
#' #' @param distance_method The distance method to use. Default is "euclidean".
#' #' @param clustering_method The clustering method to use. Default is "ward.D2".
#' #' @param cluster_rows Whether to cluster rows. Default is TRUE.
#' #' @param cluster_cols Whether to cluster columns. Default is TRUE.
#' #' @param wide Whether the data is wide or long. Default is TRUE.
#' #'
#' #' @return (list). A list with the following elements:
#' #'  - clustered_data: A dataset ordered according to the hierarchical clustering of the rows and columns.
#' #'  - hc_rows: The hierarchical clustering object for rows.
#' #'  - hc_cols: The hierarchical clustering object for columns.
#' #' @export
#' #'
#' #' @examples
#' #' # Original data
#' #' clean_df <- example_data |> dplyr::select(DAid, Assay, NPX)
#' #' clean_df
#' #'
#' #' # Clustered data
#' #' cluster_data(clean_df, wide = FALSE)
#' cluster_data <- function(df,
#'                          distance_method = "euclidean",
#'                          clustering_method = "ward.D2",
#'                          cluster_rows = TRUE,
#'                          cluster_cols = TRUE,
#'                          wide = TRUE) {
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(df) |>
#'         tibble::column_to_rownames(var = names(df)[1])
#'   } else {
#'     wide_data <- df |>
#'       tibble::column_to_rownames(var = names(df)[1])
#'   }
#'
#'     order_row <- rownames(wide_data)
#'     order_col <- colnames(wide_data)
#'
#'     if(isTRUE(cluster_rows)) {
#'       hc_rows <- wide_data |>
#'         stats::dist(method = distance_method) |>
#'         stats::hclust(method = clustering_method)
#'       order1 <- hc_rows$labels[hc_rows$order]
#'     } else {
#'       hc_rows <- NULL
#'       order1 <- order_row
#'     }
#'
#'     if(isTRUE(cluster_cols)) {
#'       hc_cols <- wide_data |>
#'         t() |>
#'         stats::dist(method = distance_method) |>
#'         stats::hclust(method = clustering_method)
#'       order2 <- hc_cols$labels[hc_cols$order]
#'     } else {
#'       hc_cols <- NULL
#'       order2 <- order_col
#'     }
#'
#'     clustering_results <- wide_data |>
#'       tibble::rownames_to_column() |>
#'       dplyr::rename(v1 = 1) |>
#'       tidyr::gather(v2, val, -1) |>
#'       dplyr::rename(x = v1, y = v2, value = val) |>
#'       dplyr::mutate(x = factor(x, levels = order1),
#'                     y = factor(y, levels = order2)) |>
#'       dplyr::arrange(x, y)
#'
#'     clustered_data <- tibble::as_tibble(clustering_results)
#'     return(list("clustered_data" = clustered_data, "hc_rows" = hc_rows, "hc_cols" = hc_cols))
#' }
