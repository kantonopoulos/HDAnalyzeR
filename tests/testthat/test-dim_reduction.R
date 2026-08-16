pca_input <- function() {
  # Three features that are exact linear combinations of two latent factors, so
  # the first two PCs must capture essentially all of the variance.
  withr::local_seed(21)
  n <- 30
  f1 <- stats::rnorm(n)
  f2 <- stats::rnorm(n)
  tibble::tibble(
    DAid = sprintf("S%02d", seq_len(n)),
    a = f1,
    b = f1 * 2,
    c = f2,
    d = f2 * -1,
    e = f1 + f2
  )
}

pca_meta <- function() {
  tibble::tibble(
    DAid = sprintf("S%02d", seq_len(30)),
    Disease = rep(c("A", "B"), each = 15),
    Sex = rep(c("F", "M"), length.out = 30)
  )
}

# hd_pca ----------------------------------------------------------------------

test_that("hd_pca() returns results, loadings and variance", {
  res <- hd_pca(pca_input(), components = 3)

  expect_s3_class(res, "hd_pca")
  expect_named(res, c("pca_res", "pca_loadings", "pca_variance", "by_sample"))
  expect_true(res$by_sample)
})

test_that("hd_pca() names the components and keeps one row per sample", {
  res <- hd_pca(pca_input(), components = 3)

  expect_equal(colnames(res$pca_res), c("DAid", "PC1", "PC2", "PC3"))
  expect_equal(nrow(res$pca_res), 30)
  expect_setequal(res$pca_res$DAid, pca_input()$DAid)
})

test_that("hd_pca() pads component names consistently past ten", {
  res <- hd_pca(pca_input(), components = 5)
  expect_equal(colnames(res$pca_res), c("DAid", paste0("PC", 1:5)))
})

test_that("hd_pca() puts almost all variance in the first two components", {
  res <- hd_pca(pca_input(), components = 4)
  cumulative <- res$pca_variance$cumulative_percent_variance

  expect_gt(cumulative[2], 99)
  expect_true(!is.unsorted(cumulative))
})

test_that("hd_pca() components are uncorrelated", {
  res <- hd_pca(pca_input(), components = 3)
  correlation <- stats::cor(res$pca_res$PC1, res$pca_res$PC2)
  expect_lt(abs(correlation), 1e-8)
})

test_that("hd_pca() loadings cover every feature for every component", {
  res <- hd_pca(pca_input(), components = 3)

  expect_setequal(unique(res$pca_loadings$terms), c("a", "b", "c", "d", "e"))
  expect_setequal(unique(res$pca_loadings$component), c("PC1", "PC2", "PC3"))
})

test_that("hd_pca() can work on the feature axis instead", {
  res <- hd_pca(pca_input(), components = 2, by_sample = FALSE)

  expect_false(res$by_sample)
  expect_equal(nrow(res$pca_res), 5)
  expect_equal(colnames(res$pca_res)[1], "Features")
})

test_that("hd_pca() caps the number of components at the number of features", {
  expect_warning(
    res <- hd_pca(pca_input(), components = 50),
    "higher than the number of features"
  )
  expect_equal(ncol(res$pca_res) - 1, 5)
})

test_that("hd_pca() imputes missing values when asked, and refuses otherwise", {
  gappy <- pca_input()
  gappy$a[c(1, 5)] <- NA

  expect_s3_class(hd_pca(gappy, components = 2, impute = TRUE), "hd_pca")
  expect_error(
    hd_pca(gappy, components = 2, impute = FALSE),
    "missing values in the data"
  )
})

test_that("hd_pca() is reproducible for a given seed", {
  a <- hd_pca(pca_input(), components = 3, seed = 99)
  b <- hd_pca(pca_input(), components = 3, seed = 99)
  expect_equal(a$pca_res, b$pca_res)
})

test_that("hd_pca() rejects an empty HDAnalyzeR object", {
  hd_obj <- hd_initialize(pca_input(), is_wide = TRUE)
  hd_obj$data <- NULL
  expect_error(hd_pca(hd_obj), "'data' slot .* is empty")
})


# PCA plots -------------------------------------------------------------------

test_that("hd_plot_pca_loadings() and hd_plot_pca_variance() attach plots", {
  res <- hd_pca(pca_input(), components = 3) |>
    hd_plot_pca_loadings(displayed_pcs = 2, displayed_features = 3) |>
    hd_plot_pca_variance()

  expect_renderable_ggplot(res$pca_loadings_plot)
  expect_renderable_ggplot(res$pca_variance_plot)
})

test_that("hd_plot_dim() plots the requested components", {
  hd_obj <- hd_initialize(pca_input(), pca_meta(), is_wide = TRUE)
  res <- hd_pca(hd_obj, components = 3) |>
    hd_plot_dim(hd_obj, x = "PC1", y = "PC2", color = "Disease")

  expect_renderable_ggplot(res$pca_plot)
  expect_match(res$pca_plot$labels$x, "^PC1")
  expect_match(res$pca_plot$labels$y, "^PC2")
})

test_that("hd_plot_dim() adds the explained variance to the axis labels", {
  hd_obj <- hd_initialize(pca_input(), pca_meta(), is_wide = TRUE)
  res <- hd_pca(hd_obj, components = 3) |>
    hd_plot_dim(hd_obj, x = "PC1", y = "PC2", axis_variance = TRUE)

  expect_match(res$pca_plot$labels$x, "PC1 \\([0-9.]+%\\)")
})

test_that("hd_plot_dim() can omit the variance annotation", {
  hd_obj <- hd_initialize(pca_input(), pca_meta(), is_wide = TRUE)
  res <- hd_pca(hd_obj, components = 3) |>
    hd_plot_dim(hd_obj, x = "PC1", y = "PC2", axis_variance = FALSE)

  x_label <- ggplot2::ggplot_build(res$pca_plot)$plot$labels$x
  expect_false(grepl("%", x_label %||% "PC1", fixed = TRUE))
})

test_that("hd_plot_dim() rejects an unknown colour column", {
  hd_obj <- hd_initialize(pca_input(), pca_meta(), is_wide = TRUE)
  pca <- hd_pca(hd_obj, components = 2)

  expect_error(
    hd_plot_dim(pca, hd_obj, x = "PC1", y = "PC2", color = "nope"),
    "does not exist"
  )
})

test_that("hd_plot_dim() draws loadings on distinct axes", {
  hd_obj <- hd_initialize(pca_input(), pca_meta(), is_wide = TRUE)
  res <- hd_pca(hd_obj, components = 3) |>
    hd_plot_dim(hd_obj, x = "PC1", y = "PC2", plot_loadings = "PC1", nloadings = 3)

  expect_renderable_ggplot(res$pca_plot)

  # The arrow tips must use the PC1 loading for x and the PC2 loading for y;
  # using the same value for both would put every arrow on the diagonal.
  segment_index <- which(vapply(
    res$pca_plot$layers,
    function(layer) inherits(layer$geom, "GeomSegment"),
    logical(1)
  ))[1]
  drawn <- ggplot2::ggplot_build(res$pca_plot)$data[[segment_index]]

  expect_equal(nrow(drawn), 3)
  expect_false(isTRUE(all.equal(drawn$xend, drawn$yend)))
})


# hd_auto_pca -----------------------------------------------------------------

test_that("hd_auto_pca() runs the whole PCA pipeline", {
  hd_obj <- hd_initialize(pca_input(), pca_meta(), is_wide = TRUE)
  res <- hd_auto_pca(hd_obj, components = 3, plot_color = "Disease")

  expect_s3_class(res, "hd_pca")
  expect_renderable_ggplot(res$pca_plot)
  expect_renderable_ggplot(res$pca_loadings_plot)
  expect_renderable_ggplot(res$pca_variance_plot)
})


# hd_umap ---------------------------------------------------------------------

test_that("hd_umap() returns one row per sample with named components", {
  skip_if_not_installed("embed")
  res <- quietly(hd_umap(pca_input(), components = 2))

  expect_s3_class(res, "hd_umap")
  expect_equal(colnames(res$umap_res), c("DAid", "UMAP1", "UMAP2"))
  expect_equal(nrow(res$umap_res), 30)
  expect_false(anyNA(res$umap_res$UMAP1))
})

test_that("hd_umap() is reproducible for a given seed", {
  skip_if_not_installed("embed")
  a <- quietly(hd_umap(pca_input(), components = 2, seed = 5))
  b <- quietly(hd_umap(pca_input(), components = 2, seed = 5))
  expect_equal(a$umap_res, b$umap_res)
})

test_that("hd_umap() refuses to silently drop missing values", {
  skip_if_not_installed("embed")
  gappy <- pca_input()
  gappy$a[1] <- NA

  expect_error(
    quietly(hd_umap(gappy, components = 2, impute = FALSE)),
    "missing values in the data"
  )
})

test_that("hd_auto_umap() runs the whole UMAP pipeline", {
  skip_if_not_installed("embed")
  hd_obj <- hd_initialize(pca_input(), pca_meta(), is_wide = TRUE)
  res <- quietly(hd_auto_umap(hd_obj, plot_color = "Disease"))

  expect_s3_class(res, "hd_umap")
  expect_renderable_ggplot(res$umap_plot)
})
