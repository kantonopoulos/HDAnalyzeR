# test_that("correlate returns the correct output matrix", {
#   test_data <- data.frame(
#     Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#     Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
#     Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
#   )
#   result <- correlate(test_data)
#   expected <- rbind(c(1, -0.17, -1), c(-0.17, 1, 0.17), c(-1, 0.17, 1))
#   rownames(expected) <- colnames(expected) <- c("Column1", "Column2", "Column3")
#   expect_equal(result, expected)
# })
#
#
# test_that("create_corr_heatmap returns the correct output matrix", {
#   test_data <- data.frame(
#     Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#     Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
#     Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
#   )
#   result <- create_corr_heatmap(test_data)
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
