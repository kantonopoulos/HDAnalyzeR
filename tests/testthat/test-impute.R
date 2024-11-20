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
