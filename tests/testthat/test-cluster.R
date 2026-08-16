clusterable <- function() {
  # Two well-separated blocks of samples and two blocks of features.
  withr::local_seed(5)
  block <- function(mu) matrix(stats::rnorm(6 * 4, mean = mu, sd = 0.1), nrow = 6)
  m <- rbind(
    cbind(block(0), block(5)),
    cbind(block(5), block(0))
  )
  colnames(m) <- paste0("f", seq_len(ncol(m)))
  tibble::as_tibble(m) |>
    dplyr::mutate(DAid = paste0("S", seq_len(nrow(m))), .before = 1)
}

# hd_cluster ------------------------------------------------------------------

test_that("hd_cluster() keeps every sample and feature", {
  dat <- clusterable()
  res <- hd_cluster(dat)

  expect_s3_class(res, "hd_cluster")
  expect_equal(nrow(res$cluster_res), nrow(dat))
  expect_equal(ncol(res$cluster_res), ncol(dat))
  expect_setequal(colnames(res$cluster_res), colnames(dat))
})

test_that("hd_cluster() preserves the sample IDs", {
  dat <- clusterable()
  res <- hd_cluster(dat)

  expect_setequal(as.character(res$cluster_res$DAid), dat$DAid)
})

test_that("hd_cluster() reorders rows and columns by the clustering", {
  dat <- clusterable()
  res <- hd_cluster(dat)

  ordered_ids <- as.character(res$cluster_res$DAid)
  # The first three and last three samples form the two blocks, so after
  # clustering they must still be contiguous.
  first_block <- paste0("S", 1:6)
  positions <- match(first_block, ordered_ids)
  expect_equal(diff(sort(positions)), rep(1, 5))
})

test_that("hd_cluster() normalises every feature, including the first", {
  dat <- clusterable()
  res <- hd_cluster(dat, cluster_rows = FALSE, cluster_cols = FALSE, normalize = TRUE)

  values <- res$cluster_res |> dplyr::select(-"DAid")
  # z-scoring must leave each feature with mean 0 and sd 1
  expect_equal(unname(round(colMeans(values), 8)), rep(0, ncol(values)))
  expect_equal(
    unname(round(apply(values, 2, stats::sd), 8)),
    rep(1, ncol(values))
  )
})

test_that("hd_cluster() can skip normalisation", {
  dat <- clusterable()
  res <- hd_cluster(dat, cluster_rows = FALSE, cluster_cols = FALSE, normalize = FALSE)

  got <- res$cluster_res[match(dat$DAid, res$cluster_res$DAid), ]
  expect_equal(
    got |> dplyr::select(-"DAid") |> as.matrix(),
    dat |> dplyr::select(-"DAid") |> as.matrix(),
    ignore_attr = TRUE
  )
})

test_that("hd_cluster() returns hclust objects only for the requested margins", {
  dat <- clusterable()

  both <- hd_cluster(dat)
  expect_s3_class(both$cluster_rows, "hclust")
  expect_s3_class(both$cluster_cols, "hclust")

  rows_only <- hd_cluster(dat, cluster_cols = FALSE)
  expect_s3_class(rows_only$cluster_rows, "hclust")
  expect_null(rows_only$cluster_cols)
})

test_that("hd_cluster() works through an HDAnalyzeR object", {
  hd_obj <- hd_initialize(clusterable(), tiny_meta(), is_wide = TRUE)
  res <- hd_cluster(hd_obj)

  expect_s3_class(res, "hd_cluster")
  expect_equal(nrow(res$cluster_res), 12)
})

test_that("hd_cluster() rejects an empty HDAnalyzeR object", {
  hd_obj <- hd_initialize(clusterable(), is_wide = TRUE)
  hd_obj$data <- NULL
  expect_error(hd_cluster(hd_obj), "does not contain any data")
})


# hd_cluster_samples ----------------------------------------------------------

test_that("hd_cluster_samples() assigns every sample to one of k clusters", {
  skip_if_not_installed("cluster")
  dat <- clusterable()
  res <- quietly(hd_cluster_samples(dat, k = 2))

  expect_s3_class(res, "hd_cluster")
  expect_equal(res$k, 2)
  expect_equal(nrow(res$cluster_res), nrow(dat))
  expect_setequal(res$cluster_res$DAid, dat$DAid)
  expect_setequal(sort(unique(res$cluster_res$Cluster)), c(1, 2))
})

test_that("hd_cluster_samples() recovers the two known groups", {
  skip_if_not_installed("cluster")
  res <- quietly(hd_cluster_samples(clusterable(), k = 2))

  assignment <- stats::setNames(res$cluster_res$Cluster, res$cluster_res$DAid)
  group_a <- assignment[paste0("S", 1:6)]
  group_b <- assignment[paste0("S", 7:12)]

  expect_length(unique(group_a), 1)
  expect_length(unique(group_b), 1)
  expect_false(unique(group_a) == unique(group_b))
})

test_that("hd_cluster_samples() can choose k from the gap statistic", {
  skip_if_not_installed("cluster")
  res <- quietly(hd_cluster_samples(clusterable(), gap_b = 5, k_max = 4))

  expect_true(is.numeric(res$k))
  expect_gte(res$k, 1)
})

test_that("hd_cluster_samples() is reproducible for a given seed", {
  skip_if_not_installed("cluster")
  a <- quietly(hd_cluster_samples(clusterable(), gap_b = 5, k_max = 4, seed = 7))
  b <- quietly(hd_cluster_samples(clusterable(), gap_b = 5, k_max = 4, seed = 7))
  expect_equal(a$cluster_res, b$cluster_res)
})


# hd_assess_clusters ----------------------------------------------------------

test_that("hd_assess_clusters() adds a stability table", {
  skip_if_not_installed("cluster")
  skip_if_not_installed("fpc")

  clustering <- quietly(hd_cluster_samples(clusterable(), k = 2))
  res <- quietly(hd_assess_clusters(clustering, nrep = 10, nsample_lim = 1))

  expect_s3_class(res, "hd_cluster")
  expect_true("cluster_assessment" %in% names(res))
  expect_true(all(c("Cluster", "cluster_og", "n", "Mean_ji") %in%
    colnames(res$cluster_assessment)))
  expect_equal(sum(res$cluster_assessment$n), nrow(clustering$cluster_res))
})

test_that("hd_assess_clusters() keeps well-separated clusters", {
  skip_if_not_installed("cluster")
  skip_if_not_installed("fpc")

  clustering <- quietly(hd_cluster_samples(clusterable(), k = 2))
  res <- quietly(hd_assess_clusters(clustering, nrep = 10, nsample_lim = 1))

  # The two blocks are far apart, so both should be stable (Jaccard well above 0.5)
  expect_true(all(res$cluster_assessment$Mean_ji > 0.5))
  expect_false(any(res$cluster_res$Cluster == 0))
})

test_that("hd_assess_clusters() rejects anything that is not a cluster object", {
  expect_error(hd_assess_clusters(list()), "not a valid HDAnalyzeR cluster object")
})
