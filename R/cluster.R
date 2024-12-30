#' Cluster data
#'
#' `hd_cluster()` takes a dataset and returns the same dataset ordered
#' according to the clustering method of the rows and columns. This dataset
#' can then be used to plot a heatmap with ggplot2 that is not having clustering functionality.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.
#' @param distance_method The distance method to use. Default is "euclidean".
#' Other options are "maximum", "manhattan", "canberra", "binary" or "minkowski".
#' @param clustering_method The clustering method to use. Default is "ward.D2".
#' Other options are "ward.D", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
#' "median" (= WPGMC) or "centroid" (= UPGMC)
#' @param cluster_rows Whether to cluster rows. Default is TRUE.
#' @param cluster_cols Whether to cluster columns. Default is TRUE.
#' @param normalize A logical value indicating whether to normalize the data. Z-score normalization is applied using the `hd_normalize()` function. Default is TRUE.
#'
#' @return A list with the dataset ordered according to the clustering of the rows and columns and the hierarchical clustering object for rows and columns.
#' @details
#' You can read more about the distance and clustering methods in the documentation
#' of the `dist()` and `hclust()` functions in the `stats` package.
#'
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Clustered data
#' hd_cluster(hd_object)
hd_cluster <- function(dat,
                       distance_method = "euclidean",
                       clustering_method = "ward.D2",
                       cluster_rows = TRUE,
                       cluster_cols = TRUE,
                       normalize = TRUE) {

  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The HDAnalyzeR object does not contain any data.")
    }
    sample_id <- dat[["sample_id"]]
    wide_data <- dat[["data"]] |> tibble::column_to_rownames(var = dat[["sample_id"]])
  } else {
    sample_id <- colnames(dat)[1]
    wide_data <- dat |> tibble::column_to_rownames(var = names(dat)[1])
  }

  check_numeric <- check_numeric_columns(wide_data)

  if (normalize) {
    wide_data <- hd_normalize(wide_data, center = TRUE, scale = TRUE)
  }

  order_row <- rownames(wide_data)
  order_col <- colnames(wide_data)

  if(isTRUE(cluster_rows)) {
    hc_rows <- wide_data |>
      stats::dist(method = distance_method) |>
      stats::hclust(method = clustering_method)
    order1 <- hc_rows$labels[hc_rows$order]
  } else {
    hc_rows <- NULL
    order1 <- order_row
  }

  if(isTRUE(cluster_cols)) {
    hc_cols <- wide_data |>
      t() |>
      stats::dist(method = distance_method) |>
      stats::hclust(method = clustering_method)
    order2 <- hc_cols$labels[hc_cols$order]
  } else {
    hc_cols <- NULL
    order2 <- order_col
  }

  clustering_results <- wide_data |>
    tibble::rownames_to_column() |>
    dplyr::rename(v1 = 1) |>
    tidyr::gather(!!rlang::sym("v2"), !!rlang::sym("val"), -1) |>
    dplyr::rename(x = !!rlang::sym("v1"),
                  y = !!rlang::sym("v2"),
                  value = !!rlang::sym("val"))

  if (!is.null(order1)){
    clustering_results <- clustering_results |>
      dplyr::mutate(x = factor(!!rlang::sym("x"), levels = order1))
  }

  if (!is.null(order2)){
    clustering_results <- clustering_results |>
      dplyr::mutate(y = factor(!!rlang::sym("y"), levels = order2))
  }

  clustering_results <- clustering_results |>
    dplyr::arrange(!!rlang::sym("x"), !!rlang::sym("y")) |>
    tidyr::pivot_wider(names_from = !!rlang::sym("y"),
                       values_from = !!rlang::sym("value")) |>
    dplyr::rename(!!rlang::sym(sample_id) := !!rlang::sym("x"))

  clustered_data <- tibble::as_tibble(clustering_results)

  cluster_object <- list("cluster_res" = clustered_data,
                         "cluster_rows" = hc_rows,
                         "cluster_cols" = hc_cols)
  class(cluster_object) <- "hd_cluster"

  return(cluster_object)
}
