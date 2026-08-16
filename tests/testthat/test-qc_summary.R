test_that("hd_qc_summary() returns a data and a metadata summary", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  res <- quietly(hd_qc_summary(hd_obj, variable = "Disease"))

  expect_s3_class(res, "hd_qc")
  expect_named(res, c("data_summary", "metadata_summary"))
})

test_that("hd_qc_summary() reports the missing values it found", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  res <- quietly(hd_qc_summary(hd_obj, variable = "Disease"))

  na_col <- res$data_summary$na_percentage_col
  expect_equal(na_col$column, "f2")
  # one missing value out of six samples
  expect_equal(na_col$na_percentage, round(100 / 6, 1))

  na_row <- res$data_summary$na_percentage_row
  expect_equal(na_row$DAid, "S3")
})

test_that("hd_qc_summary() leaves out columns without missing values", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  res <- quietly(hd_qc_summary(hd_obj, variable = "Disease"))

  expect_false("f1" %in% res$data_summary$na_percentage_col$column)
})

test_that("hd_qc_summary() attaches renderable plots", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  res <- quietly(hd_qc_summary(hd_obj, variable = "Disease"))

  expect_s3_class(res$data_summary$na_col_hist, "ggplot")
  expect_s3_class(res$data_summary$na_row_hist, "ggplot")
  expect_s3_class(res$data_summary$cor_heatmap, "ggplot")
})

test_that("hd_qc_summary() builds one plot per metadata variable", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  res <- quietly(hd_qc_summary(hd_obj, variable = "Disease"))

  expect_true(all(c("Sex", "Age") %in% names(res$metadata_summary)))
  expect_renderable_ggplot(res$metadata_summary$Age)
  expect_renderable_ggplot(res$metadata_summary$Sex)
})

test_that("hd_qc_summary() needs metadata", {
  expect_error(
    hd_qc_summary(tiny_wide(), variable = "Disease"),
    "'metadata' argument or slot .* is empty"
  )
})

test_that("hd_qc_summary() rejects an empty HDAnalyzeR object", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  hd_obj$data <- NULL
  expect_error(hd_qc_summary(hd_obj, variable = "Disease"), "'data' slot .* is empty")
})

test_that("hd_qc_summary() is silent when verbose = FALSE", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  expect_no_message(hd_qc_summary(hd_obj, variable = "Disease", verbose = FALSE))
})


# The printed summary ---------------------------------------------------------

test_that("the printed summary renders tables rather than deparsed code", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)

  output <- paste(
    capture.output(
      suppressWarnings(hd_qc_summary(hd_obj, variable = "Disease", verbose = TRUE)),
      type = "message"
    ),
    collapse = "\n"
  )

  expect_match(output, "Number of samples: 6")
  # A tibble passed straight to message() comes out as `c("f2")` style code
  expect_no_match(output, 'c\\("', all = FALSE)
  # the missing-value table should be readable, naming the affected column
  expect_match(output, "f2")
})

test_that("the printed summary labels the column type counts", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)

  output <- paste(
    capture.output(
      suppressWarnings(hd_qc_summary(hd_obj, variable = "Disease", verbose = TRUE)),
      type = "message"
    ),
    collapse = "\n"
  )

  expect_match(output, "continuous: [0-9]+")
})
