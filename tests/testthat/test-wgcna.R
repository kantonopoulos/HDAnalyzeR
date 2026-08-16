wgcna_input <- function() {
  # Two co-expressed modules of eight features each, so WGCNA has real structure
  # to find.
  withr::local_seed(13)
  n <- 40
  module_a <- stats::rnorm(n)
  module_b <- stats::rnorm(n)

  columns <- c(
    lapply(seq_len(8), function(i) module_a + stats::rnorm(n, sd = 0.3)),
    lapply(seq_len(8), function(i) module_b + stats::rnorm(n, sd = 0.3))
  )
  names(columns) <- c(paste0("a", 1:8), paste0("b", 1:8))

  tibble::as_tibble(columns) |>
    dplyr::mutate(DAid = sprintf("S%02d", seq_len(n)), .before = 1)
}

wgcna_meta <- function() {
  tibble::tibble(
    DAid = sprintf("S%02d", seq_len(40)),
    Disease = rep(c("A", "B"), each = 20),
    Age = seq(20, 80, length.out = 40)
  )
}

test_that("hd_wgcna() returns modules and the power it used", {
  skip_on_cran()
  skip_if_not_installed("WGCNA")

  res <- quietly(hd_wgcna(wgcna_input(), power = 6))

  expect_s3_class(res, "hd_wgcna")
  expect_equal(res$power, 6)
  expect_true("colors" %in% names(res$wgcna))
  expect_equal(length(res$wgcna$colors), 16)
  expect_setequal(names(res$wgcna$colors), colnames(wgcna_input())[-1])
})

test_that("hd_wgcna() can pick the power itself and reports the plots", {
  skip_on_cran()
  skip_if_not_installed("WGCNA")

  res <- quietly(hd_wgcna(wgcna_input()))

  expect_s3_class(res, "hd_wgcna")
  expect_true("power_plots" %in% names(res))
  expect_renderable_ggplot(res$power_plots)
})

test_that("hd_wgcna() validates the power argument", {
  skip_if_not_installed("WGCNA")

  expect_error(hd_wgcna(wgcna_input(), power = "six"), "must be a numeric value")
  expect_error(hd_wgcna(wgcna_input(), power = 0), "between 1 and 30")
  expect_error(hd_wgcna(wgcna_input(), power = 31), "between 1 and 30")
})

test_that("hd_wgcna() rejects an empty HDAnalyzeR object", {
  skip_if_not_installed("WGCNA")

  hd_obj <- hd_initialize(wgcna_input(), is_wide = TRUE)
  hd_obj$data <- NULL
  expect_error(hd_wgcna(hd_obj), "'data' slot .* is empty")
})

test_that("hd_plot_wgcna() attaches the diagnostic plots", {
  skip_on_cran()
  skip_if_not_installed("WGCNA")
  skip_if_not_installed("ppsr")

  hd_obj <- hd_initialize(wgcna_input(), wgcna_meta(), is_wide = TRUE)
  wgcna <- quietly(hd_wgcna(hd_obj, power = 6))
  res <- quietly(hd_plot_wgcna(hd_obj, wgcna = wgcna, clinical_vars = c("Disease", "Age")))

  expect_true(all(c(
    "tom_heatmap", "me_adjacency", "pps",
    "me_pps_heatmap", "var_pps_heatmap", "me_cor_heatmap"
  ) %in% names(res)))
  expect_renderable_ggplot(res$me_pps_heatmap)
  expect_renderable_ggplot(res$me_cor_heatmap)
})

test_that("hd_plot_wgcna() correlates module eigengenes against the right samples", {
  skip_on_cran()
  skip_if_not_installed("WGCNA")
  skip_if_not_installed("ppsr")

  # Shuffle the metadata rows: the correlations must not depend on row order.
  hd_obj <- hd_initialize(wgcna_input(), wgcna_meta(), is_wide = TRUE)
  shuffled <- hd_obj
  withr::local_seed(2)
  shuffled$metadata <- wgcna_meta()[sample(nrow(wgcna_meta())), ]

  wgcna <- quietly(hd_wgcna(hd_obj, power = 6))
  ordered_res <- quietly(hd_plot_wgcna(hd_obj, wgcna = wgcna, clinical_vars = "Age"))
  shuffled_res <- quietly(hd_plot_wgcna(shuffled, wgcna = wgcna, clinical_vars = "Age"))

  expect_equal(
    ordered_res$me_cor_heatmap$data |> dplyr::arrange(.data$ME, .data$Variable),
    shuffled_res$me_cor_heatmap$data |> dplyr::arrange(.data$ME, .data$Variable)
  )
})
