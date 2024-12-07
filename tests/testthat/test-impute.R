# Test hd_omit_na --------------------------------------------------------------
test_that("hd_omit_na omits values in the proper way", {
  test_data <- data.frame(
    A = c(1, 2, NA, 4),
    B = c(NA, 2, 3, 4),
    C = c("X", "Y", "Z", NA)
  )

  # hd_omit_na removes rows with NAs in any column when no columns are specified
  result <- hd_omit_na(test_data)
  expected <- data.frame(
    A = c(2),
    B = c(2),
    C = c("Y")
  )
  rownames(result) <- NULL
  expect_equal(result, expected)


  # hd_omit_na removes rows with NAs in specified columns
  result <- hd_omit_na(test_data, columns = c("B"))
  expected <- data.frame(
    A = c(2, NA, 4),
    B = c(2, 3, 4),
    C = c("Y", "Z", NA)
  )
  rownames(result) <- NULL
  expect_equal(result, expected)


  # hd_omit_na handles non-existent columns gracefully
  expect_error(
    hd_omit_na(test_data, columns = c("D")),
    "The following columns are not in the dataset: D"
  )


  # hd_omit_na retains all rows if no NAs are present in specified columns
  result <- hd_omit_na(test_data, columns = c("C"))
  expected <- data.frame(
    A = c(1, 2, NA),
    B = c(NA, 2, 3),
    C = c("X", "Y", "Z")
  )
  rownames(result) <- NULL
  expect_equal(result, expected)
})


# Test hd_impute_median --------------------------------------------------------
test_that("hd_impute_median imputes values in the proper way", {
  test_data <- tibble::tibble(
    ID = c(1, 2, 3, 4, 5),
    A = c(80, 44, NA, 50, 29),
    B = c(30, NA, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  hd_object <- hd_initialize(test_data, sample_id = "ID", is_wide = TRUE)
  result <- hd_impute_median(hd_object)
  expected <- tibble::tibble(
    ID = c(1, 2, 3, 4, 5),
    A = c(80, 44, 47, 50, 29),
    B = c(30, 62, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  expect_equal(result$data, expected)
})


test_that("hd_impute_median handles columns excluded from imputation", {
  test_data <- tibble::tibble(
    ID = c(1, 2, 3, 4, 5),
    A = c(80, 44, NA, 50, 29),
    B = c(30, NA, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  result <- hd_impute_median(test_data)
  expected <- tibble::tibble(
    ID = c(1, 2, 3, 4, 5),
    A = c(80, 44, 47, 50, 29),
    B = c(30, 62, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  expect_equal(result, expected)
})


# Test hd_impute_knn -----------------------------------------------------------
test_that("hd_impute_knn imputes values in the proper way", {
  test_data <- tibble::tibble(
    ID = c(1, 2, 3, 4, 5),
    A = c(80, 44, NA, 50, 29),
    B = c(30, NA, 85, 70, 54),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  hd_object <- hd_initialize(test_data, sample_id = "ID", is_wide = TRUE)
  result <- hd_impute_knn(hd_object, k = 2)
  expected <- tibble::tibble(
    ID = c(1, 2, 3, 4, 5),
    A = c(80, 44, 47, 50, 29),
    B = c(30.0, 57.5, 85.0, 70.0, 54.0),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  expect_equal(result$data, expected)
})


test_that("hd_impute_knn handles columns excluded from imputation", {
  test_data <- tibble::tibble(
    ID = c(1, 2, 3, 4, 5),
    A = c(80, 44, NA, 50, 29),  # The 2 neighbors of the 1st row are 44 and 50
    B = c(30, NA, 85, 70, 54),  # The 2 neighbors of the 2nd row are 30 and 85
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  result <- hd_impute_knn(test_data, k = 2)
  expected <- tibble::tibble(
    ID = c(1, 2, 3, 4, 5),
    A = c(80, 44, 47, 50, 29),
    B = c(30.0, 57.5, 85.0, 70.0, 54.0),
    C = c(7, 10, 25, 74, 49),
    D = c(14, 0, 5, 9, 20)
  )
  expect_equal(result, expected)
})
