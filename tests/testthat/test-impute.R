na_frame <- function() {
  tibble::tibble(
    DAid = paste0("S", 1:5),
    a = c(1, 2, NA, 4, 5),
    b = c(NA, 2, 3, 4, 5),
    c = c(1, 2, 3, 4, 5)
  )
}

# hd_omit_na ------------------------------------------------------------------

test_that("hd_omit_na() drops every row with any NA by default", {
  res <- hd_omit_na(na_frame())
  expect_equal(res$DAid, c("S2", "S4", "S5"))
})

test_that("hd_omit_na() only looks at the requested columns", {
  res <- hd_omit_na(na_frame(), columns = "a")
  expect_equal(res$DAid, c("S1", "S2", "S4", "S5"))

  res <- hd_omit_na(na_frame(), columns = c("a", "b"))
  expect_equal(res$DAid, c("S2", "S4", "S5"))
})

test_that("hd_omit_na() rejects unknown columns", {
  expect_error(
    hd_omit_na(na_frame(), columns = c("a", "nope")),
    "The following columns are not in the dataset: nope"
  )
})

test_that("hd_omit_na() round-trips through an HDAnalyzeR object", {
  hd_obj <- hd_initialize(na_frame(), tiny_meta(), is_wide = TRUE)
  res <- hd_omit_na(hd_obj)

  expect_s3_class(res, "HDAnalyzeR")
  expect_equal(nrow(res$data), 3)
})


# hd_impute_median ------------------------------------------------------------

test_that("hd_impute_median() fills NAs with the column median", {
  res <- hd_impute_median(na_frame(), verbose = FALSE)

  expect_false(anyNA(res))
  expect_equal(res$a[3], stats::median(c(1, 2, 4, 5)))
  expect_equal(res$b[1], stats::median(c(2, 3, 4, 5)))
  # untouched values must survive unchanged
  expect_equal(res$c, na_frame()$c)
  expect_equal(res$DAid, na_frame()$DAid)
})

test_that("hd_impute_median() reports missingness readably when verbose", {
  expect_message(hd_impute_median(na_frame()), "a")
})

test_that("hd_impute_median() is silent when verbose = FALSE", {
  expect_no_message(hd_impute_median(na_frame(), verbose = FALSE))
})


# hd_impute_knn ---------------------------------------------------------------

test_that("hd_impute_knn() removes every NA and keeps the sample column", {
  res <- hd_impute_knn(na_frame(), k = 2, verbose = FALSE)

  expect_false(anyNA(res))
  expect_equal(res$DAid, na_frame()$DAid)
  expect_equal(colnames(res), colnames(na_frame()))
})

test_that("hd_impute_knn() imputes near the neighbouring values", {
  # `a` is perfectly collinear with `c`, so the imputed value should be close to 3
  res <- hd_impute_knn(na_frame(), k = 2, verbose = FALSE)
  expect_lt(abs(res$a[3] - 3), 1.5)
})

test_that("hd_impute_knn() is reproducible for a given seed", {
  a <- hd_impute_knn(na_frame(), k = 2, seed = 42, verbose = FALSE)
  b <- hd_impute_knn(na_frame(), k = 2, seed = 42, verbose = FALSE)
  expect_equal(a, b)
})

test_that("hd_impute_knn() round-trips through an HDAnalyzeR object", {
  hd_obj <- hd_initialize(na_frame(), tiny_meta(), is_wide = TRUE)
  res <- hd_impute_knn(hd_obj, k = 2, verbose = FALSE)

  expect_s3_class(res, "HDAnalyzeR")
  expect_false(anyNA(res$data))
})


# hd_impute_missForest --------------------------------------------------------

test_that("hd_impute_missForest() removes every NA", {
  skip_if_not_installed("missForest")
  res <- quietly(hd_impute_missForest(na_frame(), maxiter = 1, ntree = 20, verbose = FALSE))

  expect_false(anyNA(res))
  expect_equal(res$DAid, na_frame()$DAid)
})


# hd_na_search ----------------------------------------------------------------

test_that("hd_na_search() summarises missingness per category", {
  hd_obj <- hd_initialize(na_frame(), tiny_meta(), is_wide = TRUE)
  res <- quietly(hd_na_search(hd_obj, annotation_vars = "Sex"))

  expect_named(res, c("na_data", "na_heatmap"))
  expect_true(all(c("Categories", "NA_percentage") %in% colnames(res$na_data)))
  expect_gt(max(res$na_data$NA_percentage), 0)
})

test_that("hd_na_search() errors when there is nothing missing", {
  complete <- tiny_wide() |> dplyr::mutate(f2 = c(10, 20, 30, 40, 50, 60))
  hd_obj <- hd_initialize(complete, tiny_meta(), is_wide = TRUE)

  expect_error(
    quietly(hd_na_search(hd_obj, annotation_vars = "Sex")),
    "no missing values"
  )
})

test_that("hd_na_search() needs metadata", {
  expect_error(
    hd_na_search(na_frame(), annotation_vars = "Sex"),
    "'metadata' argument or slot .* is empty"
  )
})
