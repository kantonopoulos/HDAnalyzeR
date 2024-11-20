# Test hd_correlate ------------------------------------------------------------
test_that("hd_correlate returns the correct output matrix", {
  test_data <- data.frame(
    Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
    Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
  )
  result <- hd_correlate(test_data)
  expected <- rbind(c(1, -0.17, -1), c(-0.17, 1, 0.17), c(-1, 0.17, 1))
  rownames(expected) <- colnames(expected) <- c("Column1", "Column2", "Column3")
  expect_equal(result, expected)
})

test_that("hd_correlate works with different inputs", {
  # 1. Single DataFrame Input
  df <- data.frame(
    A = c(1, 2, 3, 4, 5),
    B = c(2, 4, 6, 8, 10),
    C = c(5, 4, 3, 2, 1)
  )
  result_df <- hd_correlate(df)
  expect_true(is.matrix(result_df))
  expect_equal(rownames(result_df), colnames(df))
  expect_equal(colnames(result_df), colnames(df))
  expect_equal(result_df["A", "B"], 1) # Perfect positive correlation
  expect_equal(result_df["A", "C"], -1) # Perfect negative correlation

  # 2. Two DataFrames Input
  df1 <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
  df2 <- data.frame(C = c(1, 2, 3), D = c(6, 5, 4))
  result_df2 <- hd_correlate(df1, df2)
  expect_true(is.matrix(result_df2))
  expect_equal(dim(result_df2), c(2, 2)) # 2 columns in df1 and df2
  expect_equal(result_df2["A", "C"], 1) # Perfect positive correlation

  # 3. Two Vectors Input
  vec1 <- c(1, 2, 3, 4, 5)
  vec2 <- c(5, 4, 3, 2, 1)
  result_vec <- hd_correlate(vec1, vec2)
  expect_true(result_vec == -1)

  # 4. Single Matrix Input
  mat <- matrix(c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10, 5, 4, 3, 2, 1), nrow = 5, byrow = FALSE)
  colnames(mat) <- c("A", "B", "C")
  result_mat <- hd_correlate(mat)
  expect_true(is.matrix(result_mat))
  expect_equal(dim(result_mat), c(3, 3)) # Correlation for 3 variables
  expect_equal(result_mat["A", "B"], 1)
  expect_equal(result_mat["A", "C"], -1)

  # 5. Two Matrices Input
  mat1 <- matrix(c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10), nrow = 5)
  mat2 <- matrix(c(5, 4, 3, 2, 1, 1, 3, 5, 7, 9), nrow = 5)
  result_mat2 <- hd_correlate(mat1, mat2)
  expect_true(is.matrix(result_mat2))
  expect_equal(dim(result_mat2), c(2, 2)) # Matrices with 2 columns each
  expect_equal(result_mat2[1, 1], -1)

  # 6. Edge Case: Incompatible dimensions
  mat3 <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_error(hd_correlate(mat1, mat3), "incompatible dimensions")
})

# Test hd_plot_cor_heatmap -----------------------------------------------------
test_that("hd_plot_cor_heatmap returns the correct output matrix", {
  test_data <- data.frame(
    Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
    Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
  )
  result <- hd_plot_cor_heatmap(test_data)
  result <- result$cor_matrix
  expected <- rbind(c(1, -0.17, -1), c(-0.17, 1, 0.17), c(-1, 0.17, 1))
  rownames(expected) <- colnames(expected) <- c("Column1", "Column2", "Column3")
  expect_equal(result, expected)
})


test_that("hd_plot_cor_heatmap returns the correct filtered output", {
  test_data <- data.frame(
    Column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Column2 = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
    Column3 = c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)
  )
  result <- hd_plot_cor_heatmap(test_data, threshold = 0.5)
  result <- result$cor_results
  expected <- data.frame(
    Protein1 = c("Column3", "Column1"),
    Protein2 = c("Column1", "Column3"),
    Correlation = c(-1, -1)
  )
  expect_equal(result, expected)
})
