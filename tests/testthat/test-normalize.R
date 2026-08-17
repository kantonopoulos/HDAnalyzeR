test_that("hd_normalize() centres and scales every feature column", {
  dat <- tibble::tibble(
    DAid = paste0("S", 1:4),
    a = c(1, 2, 3, 4),
    b = c(10, 20, 30, 40)
  )
  res <- hd_normalize(dat, center = TRUE, scale = TRUE)

  expect_equal(res$DAid, dat$DAid)
  expect_equal(mean(res$a), 0)
  expect_equal(stats::sd(res$a), 1)
  expect_equal(mean(res$b), 0)
  expect_equal(stats::sd(res$b), 1)
  # a and b are perfectly correlated, so their z-scores must be identical
  expect_equal(res$a, res$b)
})

test_that("hd_normalize() can centre without scaling", {
  dat <- tibble::tibble(DAid = c("S1", "S2", "S3"), a = c(1, 2, 3))
  res <- hd_normalize(dat, center = TRUE, scale = FALSE)

  expect_equal(res$a, c(-1, 0, 1))
})

test_that("hd_normalize() leaves data alone when both flags are FALSE", {
  dat <- tibble::tibble(DAid = c("S1", "S2"), a = c(5, 9))
  expect_equal(hd_normalize(dat, center = FALSE, scale = FALSE)$a, c(5, 9))
})

test_that("hd_normalize() preserves column names and order", {
  dat <- tiny_wide()
  res <- hd_normalize(dat)
  expect_equal(colnames(res), colnames(dat))
})

test_that("hd_normalize() ignores NAs rather than propagating them everywhere", {
  res <- hd_normalize(tiny_wide())
  expect_equal(sum(is.na(res$f2)), 1)
  expect_equal(sum(is.na(res$f1)), 0)
})

test_that("hd_normalize() round-trips through an HDAnalyzeR object", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  res <- hd_normalize(hd_obj)

  expect_s3_class(res, "HDAnalyzeR")
  expect_equal(res$metadata, tiny_meta())
  expect_equal(round(mean(res$data$f1), 10), 0)
})

test_that("hd_normalize() removes a batch effect", {
  withr::local_seed(11)
  n <- 40
  batch <- rep(c("b1", "b2"), each = n / 2)
  # a large additive offset between the two batches
  offset <- ifelse(batch == "b1", 0, 10)
  dat <- tibble::tibble(
    DAid = sprintf("S%02d", seq_len(n)),
    a = stats::rnorm(n) + offset,
    b = stats::rnorm(n) + offset
  )
  meta <- tibble::tibble(DAid = dat$DAid, Cohort = batch)

  before <- abs(diff(tapply(dat$a, batch, mean)))
  corrected <- quietly(
    hd_normalize(dat, metadata = meta, center = FALSE, scale = FALSE, batch = "Cohort")
  )
  after <- abs(diff(tapply(corrected$a, batch, mean)))

  expect_gt(before, 5)
  expect_lt(after, 1e-8)
})

test_that("hd_normalize() needs metadata to remove batch effects", {
  expect_error(
    hd_normalize(tiny_wide(), batch = "Cohort"),
    "'metadata' argument or slot .* is empty"
  )
})

test_that("hd_normalize() rejects an empty HDAnalyzeR object", {
  hd_obj <- hd_initialize(tiny_wide(), is_wide = TRUE)
  hd_obj$data <- NULL
  expect_error(hd_normalize(hd_obj), "does not contain any data")
})
