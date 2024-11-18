# # Test calc_na_percentages_col -------------------------------------------------
# test_that("calc_na_percentage_col calculates NA percentages", {
#   result <- calc_na_percentage_col(example_metadata)
#   expected <- tibble::tibble(
#     column = c("Grade"),
#     na_percentage = c(91.5)
#   )
#   expect_equal(result, expected)
# })
#
#
# test_that("calc_na_percentage_col handles dataframe with no NAs", {
#   result <- calc_na_percentage_col(example_data)
#   expected <- tibble::tibble(
#     column = character(),
#     na_percentage = numeric()
#   )
#   expect_equal(result, expected)
# })
#
#
# # Test calc_na_percentages_row -------------------------------------------------
# test_that("calc_na_percentage_row calculates NA percentages", {
#   test_data <- tibble::tibble(
#     DAid = c("1", "2", "3", "4", "5"),
#     A = c(80, 44, NA, 50, 29),
#     B = c(30, NA, 85, 70, 54),
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   result <- calc_na_percentage_row(test_data)
#   expected <- tibble::tibble(
#     DAid = c("2", "3"),
#     na_percentage = c(20.0, 20.0)
#   )
#   expect_equal(result, expected)
# })
#
#
# test_that("calc_na_percentage_row handles dataframe with no NAs", {
#   test_data <- tibble::tibble(
#     DAid = c("1", "2", "3", "4", "5"),
#     A = c(80, 44, 6, 50, 29),
#     B = c(30, 5, 85, 70, 54),
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   result <- calc_na_percentage_row(test_data)
#   expected <- tibble::tibble(
#     DAid = character(),
#     na_percentage = numeric()
#   )
#   expect_equal(result, expected)
# })
#
#
# # Test check_normality ---------------------------------------------------------
# test_that("check_normality estimates normality properly", {
#   set.seed(123)
#   test_data <- tibble::tibble(
#     Protein1 = stats::rnorm(100, mean = 50, sd = 10),  # Normally distributed
#     Protein2 = stats::rnorm(100, mean = 30, sd = 5),   # Normally distributed
#     Protein3 = stats::rexp(100, rate = 0.1)            # Exponentially distributed (not normal)
#   )
#   result <- check_normality(test_data) |>
#     dplyr::select(Protein, is_normal)
#   attr(result$is_normal, "names") <- NULL
#   expected <- tibble::tibble(
#     Protein = c("Protein3", "Protein2", "Protein1"),
#     is_normal = c(FALSE, TRUE, TRUE)
#   )
#   expect_equal(result, expected)
# })
#
#
# # Test create_corr_heatmap -----------------------------------------------------
# test_that("create_corr_heatmap returns the correct output matrix", {
#   test_data <- data.frame(
#     Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#     Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
#     Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
#   )
#   result <- create_corr_heatmap(test_data, threshold = 0.5)
#   result <- result$cor_matrix
#   expected <- rbind(c(1, -0.17, -1), c(-0.17, 1, 0.17), c(-1, 0.17, 1))
#   rownames(expected) <- colnames(expected) <- c("Column1", "Column2", "Column3")
#   expect_equal(result, expected)
# })
#
#
# test_that("create_corr_heatmap returns the correct filtered output", {
#   test_data <- data.frame(
#     Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#     Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
#     Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
#   )
#   result <- create_corr_heatmap(test_data, threshold = 0.5)
#   result <- result$cor_results
#   expected <- data.frame(
#     Protein1 = c("Column3", "Column1"),
#     Protein2 = c("Column1", "Column3"),
#     Correlation = c(-1, -1)
#   )
#   expect_equal(result, expected)
# })
#
#
# # Test qc_summary_data ---------------------------------------------------------
# test_that("qc_summary_data calculates NA percentages in cols", {
#   test_data <- tibble::tibble(
#     DAid = c("1", "2", "3", "4", "5"),
#     A = c(80, 44, NA, 50, 29),
#     B = c(30, NA, 85, 70, 54),
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   result <- qc_summary_data(test_data, report = FALSE)
#   result <- result$na_percentage_col
#   expected <- tibble::tibble(
#     column = c("A", "B"),
#     na_percentage = c(20.0, 20.0)
#   )
#   expect_equal(result, expected)
# })
#
#
# test_that("qc_summary_data calculates NA percentages in rows", {
#   test_data <- tibble::tibble(
#     DAid = c("1", "2", "3", "4", "5"),
#     A = c(80, 44, NA, 50, 29),
#     B = c(30, NA, 85, 70, 54),
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   result <- qc_summary_data(test_data, report = FALSE)
#   result <- result$na_percentage_row
#   expected <- tibble::tibble(
#     DAid = c("2", "3"),
#     na_percentage = c(20.0, 20.0)
#   )
#   expect_equal(result, expected)
# })
#
#
# test_that("qc_summary_data estimates normality properly", {
#   set.seed(123)
#   test_data <- tibble::tibble(
#     DAid = 1:100,
#     Protein1 = stats::rnorm(100, mean = 50, sd = 10),  # Normally distributed
#     Protein2 = stats::rnorm(100, mean = 30, sd = 5),   # Normally distributed
#     Protein3 = stats::rexp(100, rate = 0.1)            # Exponentially distributed (not normal)
#   )
#   result <- qc_summary_data(test_data, report = FALSE)
#   result <- result$normality_results |>
#     dplyr::select(Protein, is_normal)
#   attr(result$is_normal, "names") <- NULL
#   expected <- tibble::tibble(
#     Protein = c("Protein3", "Protein2", "Protein1"),
#     is_normal = c(FALSE, TRUE, TRUE)
#   )
#   expect_equal(result, expected)
# })
#
#
# test_that("qc_summary_data returns the correct output matrix", {
#   test_data <- data.frame(
#     DAid = 1:10,
#     Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#     Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
#     Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
#   )
#   result <- qc_summary_data(test_data, threshold = 0.5, report = FALSE)
#   result <- result$cor_matrix
#   expected <- rbind(c(1, -0.17, -1), c(-0.17, 1, 0.17), c(-1, 0.17, 1))
#   rownames(expected) <- colnames(expected) <- c("Column1", "Column2", "Column3")
#   expect_equal(result, expected)
# })
#
#
# test_that("qc_summary_data returns the correct filtered output", {
#   test_data <- data.frame(
#     DAid = 1:10,
#     Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#     Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
#     Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
#   )
#   result <- qc_summary_data(test_data, threshold = 0.5, report = FALSE)
#   result <- result$cor_results
#   expected <- data.frame(
#     Protein1 = c("Column3", "Column1"),
#     Protein2 = c("Column1", "Column3"),
#     Correlation = c(-1, -1)
#   )
#   expect_equal(result, expected)
# })
#
#
# # Test qc_summary_metadata -----------------------------------------------------
# test_that("qc_summary_metadata calculates NA percentages in cols", {
#   test_metadata <- tibble::tibble(
#     DAid = c("1", "2", "3", "4", "5"),
#     A = c(80, 44, NA, 50, 29),
#     B = c(30, NA, 85, 70, 54),
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   result <- qc_summary_metadata(test_metadata, categorical = NULL, numeric = NULL, report = FALSE)
#   result <- result$na_percentage_col
#   expected <- tibble::tibble(
#     column = c("A", "B"),
#     na_percentage = c(20.0, 20.0)
#   )
#   expect_equal(result, expected)
# })
#
#
# test_that("qc_summary_metadata calculates NA percentages in rows", {
#   test_metadata <- tibble::tibble(
#     DAid = c("1", "2", "3", "4", "5"),
#     A = c(80, 44, NA, 50, 29),
#     B = c(30, NA, 85, 70, 54),
#     C = c(7, 10, 25, 74, 49),
#     D = c(14, 0, 5, 9, 20)
#   )
#   result <- qc_summary_metadata(test_metadata, categorical = NULL, numeric = NULL, report = FALSE)
#   result <- result$na_percentage_row
#   expected <- tibble::tibble(
#     DAid = c("2", "3"),
#     na_percentage = c(20.0, 20.0)
#   )
#   expect_equal(result, expected)
# })
