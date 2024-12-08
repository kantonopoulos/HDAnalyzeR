# Test hd_cluster -------------------------------------------------------
test_that("hd_cluster works with various inputs and options", {
  # Example Data
  df <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5"),
    Protein1 = c(1, 2, 3, 4, 5),
    Protein2 = c(5, 4, 3, 2, 1),
    Protein3 = c(2, 2, 3, 4, 4)
  )

  # Convert to HDAnalyzeR Object
  hd_object <- hd_initialize(dat = df, is_wide = TRUE, sample_id = "SampleID")

  # 1. HDAnalyzeR Object Input
  res1 <- hd_cluster(hd_object)
  expect_true(is.list(res1))
  expect_true(is.data.frame(res1$cluster_res))
  expect_true(!is.null(res1$cluster_rows))
  expect_true(!is.null(res1$cluster_cols))

  # Check if clustering has reordered rows and columns
  expect_false(identical(hd_object$data$SampleID, res1$cluster_res$SampleID))

  # 2. DataFrame Input
  res2 <- hd_cluster(df)
  expect_true(is.list(res2))
  expect_true(is.data.frame(res2$cluster_res))
  expect_true(!is.null(res2$cluster_rows))
  expect_true(!is.null(res2$cluster_cols))

  # 3. Cluster Rows Only
  res3 <- hd_cluster(df, cluster_rows = TRUE, cluster_cols = FALSE)
  expect_true(is.null(res3$cluster_cols))
  expect_false(is.null(res3$cluster_rows))
  expect_equal(colnames(res3$cluster_res), colnames(df)) # Columns should not change order

  # 4. Cluster Columns Only
  res4 <- hd_cluster(df, cluster_rows = FALSE, cluster_cols = TRUE)
  expect_true(is.null(res4$cluster_rows))
  expect_false(is.null(res4$cluster_cols))
  expect_equal(rownames(res4$cluster_res), rownames(df)) # Rows should not change order

  # 5. No Clustering
  res5 <- hd_cluster(df, cluster_rows = FALSE, cluster_cols = FALSE)
  expect_true(is.null(res5$cluster_rows))
  expect_true(is.null(res5$cluster_cols))
  expect_equal(rownames(res5$cluster_res), rownames(df)) # Rows unchanged
  expect_equal(colnames(res5$cluster_res), colnames(df)) # Columns unchanged

  # 6. Custom Distance and Clustering Methods
  res6 <- hd_cluster(df, distance_method = "manhattan", clustering_method = "single")
  expect_true(!is.null(res6$cluster_rows))
  expect_true(!is.null(res6$cluster_cols))

  # 7. Edge Case: Missing Values
  df_missing <- df
  df_missing[1, 2] <- NA
  res7 <- hd_cluster(df_missing, distance_method = "euclidean")
  expect_true(!is.null(res7$cluster_rows))
  expect_true(!is.null(res7$cluster_cols))
})
