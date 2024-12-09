test_that("hd_literature_search works correctly with mock data", {

  # Mock data for testing
  feature_class_list <- list(
    "acute myeloid leukemia" = c("FLT3", "EPO"),
    "chronic lymphocytic leukemia" = c("PARP1")
  )

  # Run the literature search with the mocked functions
  results <- hd_literature_search(feature_class_list, max_articles = 1)

  # Check if the results are returned as expected
  expect_true("acute myeloid leukemia" %in% names(results))
  expect_true("FLT3" %in% names(results[["acute myeloid leukemia"]]))
  expect_true("EPO" %in% names(results[["acute myeloid leukemia"]]))

  # Check the structure of the output (it should contain a data frame with columns pmid, year, journal, First_author, title)
  expect_true("pmid" %in% colnames(results[["acute myeloid leukemia"]][["FLT3"]]))
  expect_true("First_author" %in% colnames(results[["acute myeloid leukemia"]][["FLT3"]]))
  expect_true("title" %in% colnames(results[["acute myeloid leukemia"]][["FLT3"]]))

  feature_class_list <- list(
    "acute myeloid leukemia" = c("FLT3", "EPO"),
    "chronic lymphocytic leukemia" = c("WRONGGGGG")
  )

  results <- hd_literature_search(feature_class_list, max_articles = 1)

  # Check if the results are returned as expected
  expect_true("acute myeloid leukemia" %in% names(results))
  expect_true("FLT3" %in% names(results[["acute myeloid leukemia"]]))
  expect_true("EPO" %in% names(results[["acute myeloid leukemia"]]))

  # Check the structure of the output (it should contain a data frame with columns pmid, year, journal, First_author, title)
  expect_true("pmid" %in% colnames(results[["acute myeloid leukemia"]][["FLT3"]]))
  expect_true("First_author" %in% colnames(results[["acute myeloid leukemia"]][["FLT3"]]))
  expect_true("title" %in% colnames(results[["acute myeloid leukemia"]][["FLT3"]]))

})
