# # Test impute_median -----------------------------------------------------------
# test_that("impute_median imputes values in the proper way", {
#   test_data <- tibble::tibble(
#     A = c(80, 44, NA, 50, 29),  # The median of the 1st col are 47
#     B = c(30, NA, 85, 70, 54),  # The median of the 2nd col are 62
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   result <- impute_median(test_data)
#   expected <- tibble::tibble(
#     A = c(80, 44, 47, 50, 29),
#     B = c(30, 62, 85, 70, 54),
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   expect_equal(result, expected)
# })
#
#
# test_that("impute_median handles columns excluded from imputation", {
#   test_data <- tibble::tibble(
#     ID = c(1, 2, 3, 4, 5),
#     A = c(80, 44, NA, 50, 29),  # The 2 neighbors of the 1st col are 44 and 50
#     B = c(30, NA, 85, 70, 54),  # The 2 neighbors of the 2nd col are 30 and 85
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   result <- impute_median(test_data, exclude_cols = "ID")
#   expected <- tibble::tibble(
#     ID = c(1, 2, 3, 4, 5),
#     A = c(80, 44, 47, 50, 29),
#     B = c(30, 62, 85, 70, 54),
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   expect_equal(result, expected)
# })
#
#
# # Test impute_knn --------------------------------------------------------------
# test_that("impute_knn imputes values in the proper way", {
#   test_data <- tibble::tibble(
#     A = c(80, 44, NA, 50, 29),  # The 2 neighbors of the 1st row are 44 and 50
#     B = c(30, NA, 85, 70, 54),  # The 2 neighbors of the 2nd row are 30 and 85
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   result <- impute_knn(test_data, k = 2)
#   expected <- tibble::tibble(
#     A = c(80, 44, 47, 50, 29),
#     B = c(30.0, 57.5, 85.0, 70.0, 54.0),
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   expect_equal(result, expected)
# })
#
#
# test_that("impute_knn handles columns excluded from imputation", {
#   test_data <- tibble::tibble(
#     ID = c(1, 2, 3, 4, 5),
#     A = c(80, 44, NA, 50, 29),  # The 2 neighbors of the 1st row are 44 and 50
#     B = c(30, NA, 85, 70, 54),  # The 2 neighbors of the 2nd row are 30 and 85
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   result <- impute_knn(test_data, k = 2, exclude_cols = "ID")
#   expected <- tibble::tibble(
#     ID = c(1, 2, 3, 4, 5),
#     A = c(80, 44, 47, 50, 29),
#     B = c(30.0, 57.5, 85.0, 70.0, 54.0),
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   expect_equal(result, expected)
# })
#
#
# # Test impute_missForest -------------------------------------------------------
# test_that("impute_missForest imputes values in the proper way", {
#   test_data <- example_data |>
#     dplyr::select(DAid, Assay, NPX) |>
#     tidyr::pivot_wider(names_from = "Assay", values_from = "NPX") |>
#     dplyr::slice_head(n = 100) |>
#     dplyr::select(-DAid)
#   result <- impute_missForest(test_data, maxiter = 1, ntree = 50, parallelize = "no")
#   set.seed(123)
#   test_data <- as.data.frame(test_data)  # Convert to data frame for missForest
#   expected <- missForest::missForest(test_data, maxiter = 1, ntree = 50, parallelize = "no")$ximp
#   expected <- tibble::as_tibble(expected)
#   expect_equal(result, expected)
# })
