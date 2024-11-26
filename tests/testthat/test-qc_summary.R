# Test calc_na_percentages_col -------------------------------------------------
test_that("calc_na_percentage_col calculates NA percentages", {
  result <- calc_na_percentage_col(example_metadata)
  expected <- tibble::tibble(
    column = c("Grade"),
    na_percentage = c(91.5)
  )
  expect_equal(result, expected)
})


test_that("calc_na_percentage_col handles dataframe with no NAs", {
  result <- calc_na_percentage_col(example_data)
  expected <- tibble::tibble(
    column = character(),
    na_percentage = numeric()
  )
  expect_equal(result, expected)
})


# Test calc_na_percentages_row -------------------------------------------------
test_that("calc_na_percentage_row calculates NA percentages", {
  test_data <- tibble::tibble(
    DAid = c("1", "2", "3", "4", "5"),
    A = c(80, 44, NA, 50, 29),
    B = c(30, NA, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  result <- calc_na_percentage_row(test_data, "DAid")
  expected <- tibble::tibble(
    DAid = c("2", "3"),
    na_percentage = c(20.0, 20.0)
  )
  expect_equal(result, expected)
})


test_that("calc_na_percentage_row handles dataframe with no NAs", {
  test_data <- tibble::tibble(
    DAid = c("1", "2", "3", "4", "5"),
    A = c(80, 44, 6, 50, 29),
    B = c(30, 5, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  result <- calc_na_percentage_row(test_data, "DAid")
  expected <- tibble::tibble(
    DAid = character(),
    na_percentage = numeric()
  )
  expect_equal(result, expected)
})


# Test qc_summary_data ---------------------------------------------------------
test_that("qc_summary_data calculates NA percentages in cols", {
  test_data <- tibble::tibble(
    DAid = c("1", "2", "3", "4", "5"),
    A = c(80, 44, NA, 50, 29),
    B = c(30, NA, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  result <- qc_summary_data(test_data, sample_id = "DAid", verbose = FALSE)
  result <- result$na_percentage_col
  expected <- tibble::tibble(
    column = c("A", "B"),
    na_percentage = c(20.0, 20.0)
  )
  expect_equal(result, expected)
})

test_that("qc_summary_data calculates NA percentages in rows", {
  test_data <- tibble::tibble(
    DAid = c("1", "2", "3", "4", "5"),
    A = c(80, 44, NA, 50, 29),
    B = c(30, NA, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  result <- qc_summary_data(test_data, sample_id = "DAid", verbose = FALSE)
  result <- result$na_percentage_row
  expected <- tibble::tibble(
    DAid = c("2", "3"),
    na_percentage = c(20.0, 20.0)
  )
  expect_equal(result, expected)
})

test_that("qc_summary_data returns the correct output matrix", {
  test_data <- data.frame(
    DAid = 1:10,
    Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
    Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
  )
  result <- qc_summary_data(test_data, sample_id = "DAid", cor_threshold = 0.5, verbose = FALSE)
  result <- result$cor_matrix
  expected <- rbind(c(1, -0.17, -1), c(-0.17, 1, 0.17), c(-1, 0.17, 1))
  rownames(expected) <- colnames(expected) <- c("Column1", "Column2", "Column3")
  expect_equal(result, expected)
})

test_that("qc_summary_data returns the correct filtered output", {
  test_data <- data.frame(
    DAid = 1:10,
    Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
    Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
  )
  result <- qc_summary_data(test_data, sample_id = "DAid", cor_threshold = 0.5, verbose = FALSE)
  result <- result$cor_results
  expected <- data.frame(
    Protein1 = c("Column3", "Column1"),
    Protein2 = c("Column1", "Column3"),
    Correlation = c(-1, -1)
  )
  expect_equal(result, expected)
})


# Test qc_summary_metadata -----------------------------------------------------
test_that("qc_summary_metadata calculates NA percentages in cols", {
  test_metadata <- tibble::tibble(
    DAid = c("1", "2", "3", "4", "5"),
    Disease = c("A", "B", "A", "B", "A"),
    A = c(80, 44, NA, 50, 29),
    B = c(30, NA, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  result <- qc_summary_metadata(test_metadata, sample_id = "DAid", class_var = "Disease", unique_threshold = 5, verbose = FALSE)
  result <- result$na_percentage_col
  expected <- tibble::tibble(
    column = c("A", "B"),
    na_percentage = c(20.0, 20.0)
  )
  expect_equal(result, expected)
})


test_that("qc_summary_metadata calculates NA percentages in rows", {
  test_metadata <- tibble::tibble(
    DAid = c("1", "2", "3", "4", "5"),
    Disease = c("A", "B", "A", "B", "A"),
    A = c(80, 44, NA, 50, 29),
    B = c(30, NA, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  result <- qc_summary_metadata(test_metadata, sample_id = "DAid", class_var = "Disease", unique_threshold = 5, verbose = FALSE)
  result <- result$na_percentage_row
  expected <- tibble::tibble(
    DAid = c("2", "3"),
    na_percentage = c(16.7, 16.7)
  )
  expect_equal(result, expected)
})
