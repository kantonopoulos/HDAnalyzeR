# # Test cluster_data ------------------------------------------------------------
# test_that("cluster_data clusters wide data properly", {
#   test_data <- tibble::tibble(
#     ID = c("A", "B", "C"),
#     protein1 = c(1, 1, 1.6),
#     protein2 = c(0.8, 0.6, 0.5)
#   )
#   result <- cluster_data(test_data)
#   expected <- tibble::tibble(
#     x = c("C", "C", "A", "A", "B", "B"),
#     y = c("protein1", "protein2", "protein1", "protein2", "protein1", "protein2"),
#     value = c(1.6, 0.5, 1.0, 0.8, 1.0, 0.6)
#   )
#   expected$x <- factor(expected$x, levels = c("C", "A", "B"))
#   expected$y <- factor(expected$y, levels = c("protein1", "protein2"))
#   expect_equal(result$clustered_data, expected)
# })
#
#
# test_that("cluster_data clusters long data properly", {
#   test_data <- tibble::tibble(
#     DAid = c("A", "B", "C", "A", "B", "C"),
#     Assay = c("protein1", "protein1", "protein1", "protein2", "protein2", "protein2"),
#     NPX = c(1, 1, 1.6, 0.8, 0.6, 0.5)
#   )
#   result <- cluster_data(test_data, wide = FALSE)
#   expected <- tibble::tibble(
#     x = c("C", "C", "A", "A", "B", "B"),
#     y = c("protein1", "protein2", "protein1", "protein2", "protein1", "protein2"),
#     value = c(1.6, 0.5, 1.0, 0.8, 1.0, 0.6)
#   )
#   expected$x <- factor(expected$x, levels = c("C", "A", "B"))
#   expected$y <- factor(expected$y, levels = c("protein1", "protein2"))
#   expect_equal(result$clustered_data, expected)
# })
#
#
# test_that("cluster_data do not cluster rows in wide data", {
#   test_data <- tibble::tibble(
#     ID = c("A", "B", "C"),
#     protein1 = c(1, 1, 1.6),
#     protein2 = c(0.8, 0.6, 0.5)
#   )
#   result <- cluster_data(test_data, cluster_rows = FALSE)
#   expected <- tibble::tibble(
#     x = c("A", "A", "B", "B", "C", "C"),
#     y = c("protein1", "protein2", "protein1", "protein2", "protein1", "protein2"),
#     value = c(1.0, 0.8, 1.0, 0.6, 1.6, 0.5)
#   )
#   expected$x <- factor(expected$x, levels = c("A", "B", "C"))
#   expected$y <- factor(expected$y, levels = c("protein1", "protein2"))
#   expect_equal(result$clustered_data, expected)
# })
