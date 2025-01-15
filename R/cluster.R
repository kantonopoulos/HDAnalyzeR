#' Cluster data helper function
#'
#' `cluster_help()` is a helper function that acts as a wrapper for the `hclust()` function.
#' It takes a dataset and returns the clustering of the rows or columns.
#' @param x A numeric matrix or data frame.
#' @param k The number of clusters to return. If NULL, the function simply returns the hierarchical clustering object.
#' @param distance The distance method to use.
#' @param method The clustering method to use.
#'
#' @returns A list with the cluster assignment.
#' @keywords internal
cluster_help <- function(x, k = NULL, distance, method) {
  clust <- x |>
    stats::dist(method = distance) |>
    stats::hclust(method = method)

  if (!is.null(k)) {
    clust <- stats::cutree(clust, k)
  }

  return(list("cluster" = clust))
}


#' Get optimal number of clusters
#'
#' `get_optimal_k()` takes a dataset and returns the optimal number of clusters
#' based on the gap statistic.
#'
#' @param x A numeric matrix or data frame.
#' @param k_max The maximum number of clusters to test. Default is 15.
#' @param b The number of times to repeat the clustering in the gap analysis. Default is 50.
#' @param distance The distance method to use.
#' @param method The clustering method to use.
#' @param verbose A logical value indicating whether to print the progress of the gap analysis. Default is FALSE.
#'
#' @returns A list with the gap statistic results and the optimal number of clusters.
#' @keywords internal
get_optimal_k <- function(x, k_max = 15, b = 50, distance, method, verbose = FALSE) {

  gap_stat <- cluster::clusGap(x,
                               FUNcluster = cluster_help,
                               K.max = k_max,
                               B = b,
                               distance = distance,
                               method = method,
                               verbose = verbose)

  # Get optimal k as smallest k where gap_k >= gap_k+1 - SE_k+1
  gap_tab <- gap_stat[["Tab"]] |>
    as.data.frame() |>
    # Gap minus SE for next k
    dplyr::mutate(gms = dplyr::lead(!!rlang::sym("gap")) - dplyr::lead(!!rlang::sym("SE.sim")),
                  gap_larger = !!rlang::sym("gap") >= !!rlang::sym("gms"))

  optimal_k <- which(gap_tab[["gap"]] == (gap_tab |>
                                            dplyr::filter(!!rlang::sym("gap_larger")) |>
                                            dplyr::pull(!!rlang::sym("gap")) |>
                                            min()))

  return(list("results" = gap_stat, "optimal_k" = optimal_k))
}


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
      cluster_help(k = NULL, distance = distance_method, method = clustering_method)
    hc_rows <- hc_rows[["cluster"]]
    order1 <- hc_rows$labels[hc_rows$order]
  } else {
    hc_rows <- NULL
    order1 <- order_row
  }

  if(isTRUE(cluster_cols)) {
    hc_cols <- wide_data |>
      t() |>
      cluster_help(k = NULL, distance = distance_method, method = clustering_method)
    hc_cols <- hc_cols[["cluster"]]
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


#' Cluster samples by identifying optimal number of clusters
#'
#' `hd_cluster_samples()` takes a dataset and returns the clustering of the samples.
#' The optimal number of clusters is determined using the gap statistic. If data contain
#' missing values, the function imputes them using the k-nearest neighbors algorithm (k = 5).
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.
#' @param distance_method The distance method to use. Default is "euclidean".
#' @param clustering_method The clustering method to use. Default is "ward.D2".
#' @param normalize A logical value indicating whether to normalize the data. Z-score normalization is applied using the `hd_normalize()` function. Default is TRUE.
#' @param k_max The maximum number of clusters to test. Default is 15.
#' @param gap_b The number of times to repeat the clustering in the gap analysis. Default is 100.
#' @param seed The seed to use for reproducibility. Default is 123.
#' @param verbose A logical value indicating whether to print the progress of the gap analysis. Default is FALSE.
#'
#' @returns A list with the cluster assignment.
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Clustered data
#' clustering <- hd_cluster_samples(hd_object, gap_b = 10)
#'
#' # Access the results
#' head(clustering[["cluster_res"]])
hd_cluster_samples <- function(dat,
                               distance_method = "euclidean",
                               clustering_method = "ward.D2",
                               normalize = TRUE,
                               k_max = 15,
                               gap_b = 100,
                               seed = 123,
                               verbose = FALSE) {

  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("The 'cluster' package is required but not installed. Please install it using install.packages('cluster').")
  }

  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The HDAnalyzeR object does not contain any data.")
    }
    sample_id <- dat[["sample_id"]]
    wide_data <- dat[["data"]]
  } else {
    sample_id <- colnames(dat)[1]
    wide_data <- dat
  }

  check_numeric <- check_numeric_columns(wide_data)

  if (normalize) {
    wide_data <- hd_normalize(wide_data, center = TRUE, scale = TRUE)
  }

  # Imputation of missing values
  clust_in <- hd_impute_knn(wide_data, k = 5, verbose = FALSE) |>
    tibble::column_to_rownames(sample_id)

  set.seed(seed)

  k <- get_optimal_k(clust_in,
                     k_max = k_max,
                     b = gap_b,
                     verbose = verbose,
                     distance = distance_method,
                     method = clustering_method)[["optimal_k"]]

  message(paste("Optimal number of clusters: ", k))

  clust <- cluster_help(clust_in, k = k, distance = distance_method, method = clustering_method)[["cluster"]]

  clust_df <- tibble::tibble(sample_id = rownames(clust_in),
                             "cluster" = clust[!!rlang::sym("sample_id")])

  cluster_object <- list("cluster_res" = clust_df)

  class(cluster_object) <- "hd_cluster"

  return(cluster_object)
}
