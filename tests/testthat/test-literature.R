# `hd_literature_search()` queries PubMed over the network. NCBI throttles
# unauthenticated clients, and a throttled request can stall for minutes, so
# these tests are opt-in: set HDANALYZER_TEST_PUBMED=true to run them.

skip_unless_pubmed <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_offline()
  testthat::skip_if_not_installed("easyPubMed")
  testthat::skip_if_not(
    identical(tolower(Sys.getenv("HDANALYZER_TEST_PUBMED")), "true"),
    "set HDANALYZER_TEST_PUBMED=true to run the PubMed tests"
  )
}

test_that("hd_literature_search() returns the documented columns", {
  skip_unless_pubmed()

  res <- quietly(hd_literature_search(
    list("Acute Myeloid Leukemia" = "TCL1A"),
    max_results = 2,
    min_year = 2015
  ))

  expect_s3_class(res, "data.frame")
  skip_if(nrow(res) == 0, "PubMed returned no articles for the test query")

  expect_named(res, c("Disease", "Protein", "PMID", "Title", "Abstract"))
  expect_true(all(res$Disease == "Acute Myeloid Leukemia"))
  expect_true(all(res$Protein == "TCL1A"))
  expect_lte(nrow(res), 2)
})

test_that("hd_literature_search() covers every disease-protein pair", {
  skip_unless_pubmed()

  res <- quietly(hd_literature_search(
    list(
      "Acute Myeloid Leukemia" = c("TCL1A", "TNFRSF9"),
      "Chronic Lymphocytic Leukemia" = "CD22"
    ),
    max_results = 1
  ))

  skip_if(nrow(res) == 0, "PubMed returned no articles for the test query")
  expect_lte(nrow(res), 3)
  expect_true(all(res$Disease %in%
    c("Acute Myeloid Leukemia", "Chronic Lymphocytic Leukemia")))
})

test_that("hd_literature_search() returns an empty frame when nothing matches", {
  skip_unless_pubmed()

  res <- quietly(hd_literature_search(
    list("Zzzznotarealdisease" = "Zzzznotarealprotein"),
    max_results = 1
  ))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("hd_literature_search() explains itself without easyPubMed", {
  skip_if(requireNamespace("easyPubMed", quietly = TRUE))
  expect_error(hd_literature_search(list(a = "b")), "easyPubMed")
})
