# Test remove_batch_effects ----------------------------------------------------
test_that("remove_batch_effects removes batch effects", {
  # Example based on limma example (https://rdrr.io/bioc/limma/man/removeBatchEffect.html)
  test_data <- as.data.frame(t(matrix(rnorm(10*9), 10, 9)))
  daid <- c(1:9)
  batch <- c("A","A","A","B","B","B","C","C","C")
  test_data <- test_data |>
    dplyr::mutate(dplyr::across(1:3, ~ . + 5)) |>
    dplyr::mutate(DAid = daid)
  test_metadata <- tibble::tibble(DAid = daid, Batch = batch)
  result <- remove_batch_effects(test_data, test_metadata, sample_id = "DAid", batch = "Batch")
  test_data <- test_data |> dplyr::select(-DAid)
  expected <- tibble::as_tibble(t(limma::removeBatchEffect(t(test_data), batch)))
  expect_equal(result, expected)
  # It gives warning for the `name` as my function expects a tibble with names and not a matrix like the example
})


# Test normalize_data ----------------------------------------------------------
test_that("normalize_data works correctly", {
  # Example data
  test_data <- tibble::tibble(
    SampleID = c("S1", "S2", "S3", "S4"),
    Protein1 = c(10, 20, 30, 40),
    Protein2 = c(15, 25, 35, 45)
  )

  # Example metadata
  test_metadata <- tibble::tibble(
    SampleID = c("S1", "S2", "S3", "S4"),
    Batch = c("A", "A", "B", "B"),
    Cohort = c("X", "Z", "Y", "V")
  )

  # Initialize HDAnalyzeR object
  hd_object <- hd_initialize(test_data, test_metadata, sample_id = "SampleID", is_wide = TRUE)

  # Test 1: Scaling and centering without batch correction
  result <- normalize_data(hd_object, center = TRUE, scale = TRUE)
  expect_equal(dim(result), dim(test_data))
  expect_equal(result$SampleID, test_data$SampleID)
  expect_equal(colnames(result)[-1], colnames(test_data)[-1])
  expect_true(all(abs(colMeans(result[-1])) < 1e-6)) # Mean should be approximately zero
  expect_true(all(apply(result[-1], 2, sd) == 1))   # Standard deviation should be 1

  # Test 2: Batch correction with one batch column
  result <- normalize_data(hd_object, center = TRUE, scale = TRUE, batch = "Batch")
  expect_equal(dim(result), dim(test_data))
  expect_equal(result$SampleID, test_data$SampleID)

  # Test 3: Batch correction with two batch columns
  result <- normalize_data(hd_object, center = TRUE, scale = TRUE, batch = "Batch", batch2 = "Cohort")
  expect_equal(dim(result), dim(test_data))
  expect_equal(result$SampleID, test_data$SampleID)

  # Test 4: Centering without scaling
  result <- normalize_data(hd_object, center = TRUE, scale = FALSE)
  expect_equal(dim(result), dim(test_data))
  expect_equal(result$SampleID, test_data$SampleID)
  expect_true(all(abs(colMeans(result[-1])) < 1e-6)) # Mean should be approximately zero
  expect_false(all(apply(result[-1], 2, sd) == 1))   # Standard deviation should not be 1

  # Test 5: No centering or scaling
  result <- normalize_data(hd_object, center = FALSE, scale = FALSE)
  expect_equal(dim(result), dim(test_data))
  expect_equal(result$SampleID, test_data$SampleID)
  expect_equal(result[-1], test_data[-1]) # Data should be identical

  # Edge Case: Non-HDAnalyzeR object input
  result <- normalize_data(test_data, metadata = test_metadata, center = TRUE, scale = TRUE)
  expect_equal(dim(result), dim(test_data))
  expect_equal(result$SampleID, test_data$SampleID)
})
