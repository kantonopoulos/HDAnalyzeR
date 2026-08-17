test_that("hd_correlate() reproduces stats::cor, rounded to 2 digits", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6, 8, 10)
  expect_equal(hd_correlate(x, y), 1)

  z <- c(5, 4, 3, 2, 1)
  expect_equal(hd_correlate(x, z), -1)
})

test_that("hd_correlate() returns a square matrix for a data frame", {
  dat <- tibble::tibble(a = c(1, 2, 3, 4), b = c(4, 3, 2, 1), c = c(1, 3, 2, 4))
  res <- hd_correlate(dat)

  expect_equal(dim(res), c(3L, 3L))
  expect_equal(diag(res), c(a = 1, b = 1, c = 1))
  expect_equal(res["a", "b"], -1)
  expect_equal(res, t(res))
})

test_that("hd_correlate() honours the method argument", {
  # A monotone but non-linear relationship: Spearman is 1, Pearson is not.
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 4, 8, 16)

  expect_equal(hd_correlate(x, y, method = "spearman"), 1)
  expect_lt(hd_correlate(x, y, method = "pearson"), 1)
})

test_that("hd_correlate() tolerates NAs with pairwise deletion", {
  dat <- tibble::tibble(a = c(1, 2, 3, NA), b = c(2, 4, 6, 8))
  expect_equal(hd_correlate(dat)["a", "b"], 1)
})


# hd_plot_cor_heatmap ---------------------------------------------------------

test_that("hd_plot_cor_heatmap() returns the matrix, the pairs and the plot", {
  dat <- tibble::tibble(
    a = c(1, 2, 3, 4, 5),
    b = c(2, 4, 6, 8, 10),
    c = c(5, 1, 4, 2, 3)
  )
  res <- hd_plot_cor_heatmap(dat, threshold = 0.9)

  expect_s3_class(res, "hd_corr")
  expect_named(res, c("cor_matrix", "cor_results", "cor_heatmap"))
  expect_equal(dim(res$cor_matrix), c(3L, 3L))
  expect_s3_class(res$cor_heatmap, "ggplot")
})

test_that("hd_plot_cor_heatmap() reports only pairs beyond the threshold", {
  dat <- tibble::tibble(
    a = c(1, 2, 3, 4, 5),
    b = c(2, 4, 6, 8, 10),
    c = c(5, 1, 4, 2, 3)
  )
  res <- hd_plot_cor_heatmap(dat, threshold = 0.9)

  # a and b are perfectly correlated; both orderings of the pair are reported
  expect_equal(nrow(res$cor_results), 2)
  expect_setequal(
    paste(res$cor_results$Protein1, res$cor_results$Protein2),
    c("a b", "b a")
  )
  # self-correlations must never be reported
  expect_false(any(res$cor_results$Protein1 == res$cor_results$Protein2))
})

test_that("hd_plot_cor_heatmap() picks up strong negative correlations", {
  dat <- tibble::tibble(a = c(1, 2, 3, 4, 5), b = c(5, 4, 3, 2, 1))
  res <- hd_plot_cor_heatmap(dat, threshold = 0.9)

  expect_equal(nrow(res$cor_results), 2)
  expect_true(all(res$cor_results$Correlation == -1))
})

test_that("hd_plot_cor_heatmap() returns no pairs when nothing is correlated", {
  withr::local_seed(3)
  dat <- tibble::as_tibble(matrix(stats::rnorm(200), ncol = 4, dimnames = list(NULL, letters[1:4])))
  res <- hd_plot_cor_heatmap(dat, threshold = 0.95)

  expect_equal(nrow(res$cor_results), 0)
})

test_that("hd_plot_cor_heatmap() skips the heatmap past the feature limit", {
  withr::local_seed(4)
  dat <- tibble::as_tibble(
    matrix(stats::rnorm(200), ncol = 4, dimnames = list(NULL, letters[1:4]))
  )

  expect_warning(
    res <- hd_plot_cor_heatmap(dat, threshold = 0.5, max_heatmap_features = 3),
    "Skipping the correlation heatmap"
  )

  # the expensive plot is dropped, everything else is still computed
  expect_null(res$cor_heatmap)
  expect_equal(dim(res$cor_matrix), c(4L, 4L))
  expect_s3_class(res, "hd_corr")
})


# cor_pairs_above -------------------------------------------------------------

test_that("cor_pairs_above() matches the long-format filter it replaced", {
  withr::local_seed(5)
  dat <- matrix(stats::rnorm(600), ncol = 12)
  colnames(dat) <- paste0("f", seq_len(12))
  cor_matrix <- hd_correlate(dat)

  long_way <- as.data.frame(as.table(cor_matrix), stringsAsFactors = FALSE) |>
    dplyr::filter(!!rlang::sym("Var1") != !!rlang::sym("Var2")) |>
    dplyr::filter(abs(!!rlang::sym("Freq")) > 0.2) |>
    dplyr::arrange(dplyr::desc(!!rlang::sym("Freq")))

  res <- cor_pairs_above(cor_matrix, 0.2)

  expect_equal(res$Protein1, long_way$Var1)
  expect_equal(res$Protein2, long_way$Var2)
  expect_equal(res$Correlation, long_way$Freq)
})

test_that("cor_pairs_above() is unaffected by the chunk size", {
  withr::local_seed(6)
  dat <- matrix(stats::rnorm(600), ncol = 12)
  colnames(dat) <- paste0("f", seq_len(12))
  cor_matrix <- hd_correlate(dat)

  expect_equal(
    cor_pairs_above(cor_matrix, 0.2, chunk_size = 5),
    cor_pairs_above(cor_matrix, 0.2, chunk_size = 1000)
  )
})

test_that("cor_pairs_above() returns an empty tibble when nothing qualifies", {
  cor_matrix <- hd_correlate(
    tibble::tibble(a = c(1, 2, 3, 4, 5), b = c(5, 1, 4, 2, 3))
  )
  res <- cor_pairs_above(cor_matrix, 0.99)

  expect_equal(nrow(res), 0)
  expect_named(res, c("Protein1", "Protein2", "Correlation"))
})

test_that("cor_pairs_above() ignores NA correlations", {
  cor_matrix <- matrix(
    c(1, NA, 0.9, NA, 1, NA, 0.9, NA, 1),
    nrow = 3,
    dimnames = list(letters[1:3], letters[1:3])
  )
  res <- cor_pairs_above(cor_matrix, 0.5)

  expect_equal(nrow(res), 2)
  expect_setequal(paste(res$Protein1, res$Protein2), c("a c", "c a"))
})
