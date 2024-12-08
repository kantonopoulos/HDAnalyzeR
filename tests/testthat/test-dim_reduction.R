# Test fix_components_names ----------------------------------------------------
test_that("fix_components_names assigns correct column names for PCA by sample", {
  pca_res <- data.frame(matrix(runif(20), nrow = 4)) # Example PCA result
  components <- 4
  sample_id <- "Sample_ID"
  var_name <- "Variable"
  result <- fix_components_names(pca_res, components, by_sample = TRUE,
                                 sample_id = sample_id, var_name = var_name, type = "pca")

  # Check column names
  expected_colnames <- c(sample_id, paste0("PC", 1:components))
  expect_equal(colnames(result), expected_colnames)
})

test_that("fix_components_names assigns correct column names for PCA by variable", {
  pca_res <- data.frame(matrix(runif(20), nrow = 4)) # Example PCA result
  components <- 4
  sample_id <- "Sample_ID"
  var_name <- "Variable"
  result <- fix_components_names(pca_res, components, by_sample = FALSE,
                                 sample_id = sample_id, var_name = var_name, type = "pca")

  # Check column names
  expected_colnames <- c(var_name, paste0("PC", 1:components))
  expect_equal(colnames(result), expected_colnames)
})

test_that("fix_components_names assigns correct column names for UMAP by sample", {
  umap_res <- data.frame(matrix(runif(12), nrow = 4)) # Example UMAP result
  components <- 2
  sample_id <- "Sample_ID"
  var_name <- "Variable"
  result <- fix_components_names(umap_res, components, by_sample = TRUE,
                                 sample_id = sample_id, var_name = var_name, type = "umap")

  # Check column names
  expected_colnames <- c(sample_id, paste0("UMAP", 1:components))
  expect_equal(colnames(result), expected_colnames)
})

test_that("fix_components_names assigns correct column names for UMAP by variable", {
  umap_res <- data.frame(matrix(runif(12), nrow = 4)) # Example UMAP result
  components <- 2
  sample_id <- "Sample_ID"
  var_name <- "Variable"
  result <- fix_components_names(umap_res, components, by_sample = FALSE,
                                 sample_id = sample_id, var_name = var_name, type = "umap")

  # Check column names
  expected_colnames <- c(var_name, paste0("UMAP", 1:components))
  expect_equal(colnames(result), expected_colnames)
})


# Test hd_pca --------------------------------------------------------------
test_that("hd_pca performs PCA correctly with by_sample = TRUE", {
  # Create example data and metadata
  test_data <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Protein1 = rnorm(10),
    Protein2 = rnorm(10),
    Protein3 = rnorm(10)
  )
  test_metadata <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Group = rep(c("A", "B"), 5)
  )

  # Initialize hd_object
  hd_obj <- hd_initialize(test_data, test_metadata, is_wide = TRUE)

  # Run PCA
  pca_res <- hd_pca(hd_obj, components = 2, by_sample = TRUE)

  # Check output class
  expect_s3_class(pca_res, "hd_pca")

  # Check dimensions of pca_res
  expect_equal(ncol(pca_res$pca_res), 3) # DAid + 2 PCs
  expect_equal(nrow(pca_res$pca_res), 10) # 10 samples

  # Check PCA variance
  expect_equal(nrow(pca_res$pca_variance), 2) # 2 components
})

test_that("hd_pca performs PCA correctly with by_sample = FALSE", {
  test_data <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Protein1 = rnorm(10),
    Protein2 = rnorm(10),
    Protein3 = rnorm(10)
  )
  test_metadata <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Group = rep(c("A", "B"), 5)
  )

  # Initialize hd_object
  hd_obj <- hd_initialize(test_data, test_metadata, is_wide = TRUE)

  # Run PCA
  pca_res <- hd_pca(hd_obj, components = 2, by_sample = FALSE)

  # Check output class
  expect_s3_class(pca_res, "hd_pca")

  # Check dimensions of pca_res
  expect_equal(ncol(pca_res$pca_res), 3) # Variable + 2 PCs
  expect_equal(nrow(pca_res$pca_res), 3) # 3 variables

  # Check PCA variance
  expect_equal(nrow(pca_res$pca_variance), 2) # 2 components
})

test_that("hd_pca warns when components exceed available features", {
  test_data <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Protein1 = rnorm(10),
    Protein2 = rnorm(10),
    Protein3 = rnorm(10)
  )
  test_metadata <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Group = rep(c("A", "B"), 5)
  )

  # Initialize hd_object
  hd_obj <- hd_initialize(test_data, test_metadata, is_wide = TRUE)

  # Run PCA with excessive components
  expect_error(
    hd_pca(hd_obj, components = 5),
    "The number of PCs to be calculated is higher than the number of features in the data."
  )
})

test_that("hd_pca stops if hd_object$data is NULL", {
  # Create an invalid hd_object with NULL data
  hd_obj <- list(data = NULL, metadata = NULL)
  class(hd_obj) <- "HDAnalyzeR"
  # Run PCA and expect an error
  expect_error(
    hd_pca(hd_obj),
    "The 'data' slot of the HDAnalyzeR object is empty."
  )
})

test_that("hd_pca produces consistent results for fixed seed", {
  test_data <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Protein1 = rnorm(10),
    Protein2 = rnorm(10),
    Protein3 = rnorm(10)
  )
  test_metadata <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Group = rep(c("A", "B"), 5)
  )

  # Initialize hd_object
  hd_obj <- hd_initialize(test_data, test_metadata, is_wide = TRUE)

  # Run PCA twice with the same seed
  pca_res1 <- hd_pca(hd_obj, components = 2, seed = 123)
  pca_res2 <- hd_pca(hd_obj, components = 2, seed = 123)

  # Compare results
  expect_equal(pca_res1$pca_res, pca_res2$pca_res)
  expect_equal(pca_res1$pca_variance, pca_res2$pca_variance)
})


# Test hd_plot_pca_loadings ----------------------------------------------------
test_that("hd_plot_pca_loadings generates loadings plot", {
  # Create mock pca_loadings data
  pca_loadings <- tibble::tibble(
    component = rep(paste0("PC", 1:6), each = 20),
    terms = paste0("Protein", 1:120),
    value = rnorm(120)
  )

  # Create pca_object
  pca_object <- list("pca_loadings" = pca_loadings)
  class(pca_object) <- "hd_pca"

  # Run the function
  result <- hd_plot_pca_loadings(pca_object, displayed_pcs = 3, displayed_features = 10)

  # Check if pca_loadings_plot exists in the output
  expect_true("pca_loadings_plot" %in% names(result))

  # Check the class of the plot object
  expect_s3_class(result[["pca_loadings_plot"]], "ggplot")

  # Check that the plot uses the expected number of PCs
  plot_data <- ggplot2::ggplot_build(result[["pca_loadings_plot"]])$data[[1]]
  unique_pcs <- unique(plot_data$PANEL)
  expect_equal(length(unique_pcs), 3) # 3 PCs as specified by displayed_pcs

  # Check that the number of terms does not exceed displayed_features per PC
  term_counts <- plot_data |> dplyr::count(PANEL)
  expect_true(all(term_counts$n <= 10)) # Max 10 proteins as specified by displayed_features
})


# Test hd_plot_pca_variance ----------------------------------------------------
test_that("hd_plot_pca_variance generates variance plot", {
  # Create mock pca_variance data
  pca_variance <- tibble::tibble(
    component = 1:5,
    percent_variance = c(40, 30, 20, 7, 3),
    cumulative_percent_variance = c(40, 70, 90, 97, 100)
  )

  # Create pca_object
  pca_object <- list("pca_variance" = pca_variance)
  class(pca_object) <- "hd_pca"

  # Run the function
  result <- hd_plot_pca_variance(pca_object)

  # Check if pca_variance_plot exists in the output
  expect_true("pca_variance_plot" %in% names(result))

  # Check the class of the plot object
  expect_s3_class(result[["pca_variance_plot"]], "ggplot")

  # Validate the data used in the plot
  plot_data <- ggplot2::ggplot_build(result[["pca_variance_plot"]])$data

  # Check the bar data for percent variance
  bar_data <- plot_data[[1]]
  expect_equal(bar_data$y, pca_variance$percent_variance)

  # Check the line data for cumulative variance
  line_data <- plot_data[[2]]
  expect_equal(line_data$y, pca_variance$cumulative_percent_variance)

  # Check the labels for cumulative variance
  text_data <- plot_data[[4]]
  expect_equal(round(as.numeric(sub("%", "", text_data$label))), pca_variance$cumulative_percent_variance)

  # Confirm the correct number of components in the plot
  expect_equal(length(unique(bar_data$x)), nrow(pca_variance))
})


# Test plot_points -------------------------------------------------------------
test_that("plot_points generates scatter plots correctly", {
  # Mock dim_res data
  dim_res <- tibble::tibble(
    X1 = rnorm(100),
    X2 = rnorm(100),
    Group = sample(c("A", "B", "C"), 100, replace = TRUE)
  )

  # Test case: Without color argument
  plot_no_color <- plot_points(dim_res, x = "X1", y = "X2")

  # Check if the result is a ggplot object
  expect_s3_class(plot_no_color, "ggplot")

  # Check the data used in the plot
  plot_data_no_color <- ggplot2::ggplot_build(plot_no_color)$data[[1]]
  expect_equal(plot_data_no_color$x, dim_res$X1)
  expect_equal(plot_data_no_color$y, dim_res$X2)

  # Test case: With color argument
  plot_with_color <- plot_points(dim_res, x = "X1", y = "X2", color = "Group")

  # Check if the result is a ggplot object
  expect_s3_class(plot_with_color, "ggplot")

  # Check the data used in the plot
  plot_data_with_color <- ggplot2::ggplot_build(plot_with_color)$data[[1]]
  expect_equal(plot_data_with_color$x, dim_res$X1)
  expect_equal(plot_data_with_color$y, dim_res$X2)


  # Ensure the plot has a legend for the color
  expect_true("Group" %in% ggplot2::ggplot_build(plot_with_color)$plot$labels)
})


# plot_loadings ----------------------------------------------------------------
test_that("plot_loadings works correctly", {
  # Mock dim_object with pca_loadings
  dim_object <- list(
    pca_loadings = tibble::tibble(
      component = rep(paste0("PC", 1:3), each = 10),
      terms = paste0("Feature", 1:30),
      value = rnorm(30)
    )
  )

  # Test case 1: Request loadings for PC1, fewer than available
  result <- plot_loadings(dim_object, plot_loadings = "PC1", nloadings = 5)
  expect_s3_class(result, "tbl_df")  # Output should be a tibble
  expect_equal(nrow(result), 5)  # Check the number of rows
  expect_equal(unique(result$component), "PC1")  # Ensure only PC1 is returned

  # Test case 2: Request loadings for PC1, more than available
  result <- plot_loadings(dim_object, plot_loadings = "PC1", nloadings = 15)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 10)  # Only 10 rows should be returned since PC1 has 10 loadings
  expect_equal(unique(result$component), "PC1")

  # Test case 3: Zero loadings requested
  result <- plot_loadings(dim_object, plot_loadings = "PC1", nloadings = 0)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)  # No rows should be returned
})


# add_axis_variance ------------------------------------------------------------
test_that("add_axis_variance works correctly", {
  # Mock data for dim_object
  dim_object <- list(
    pca_variance = tibble::tibble(
      component = paste0("PC", 1:5),
      percent_variance = c(40.5, 30.3, 20.2, 5.0, 4.0)
    )
  )

  # Mock base plot (empty ggplot object for simplicity)
  base_plot <- ggplot2::ggplot()

  # Test case 1: Adding axis labels for valid PCs
  updated_plot <- add_axis_variance(dim_object, base_plot, x = "PC1", y = "PC2")
  expect_s3_class(updated_plot, "ggplot")  # Ensure the output is still a ggplot object
})


# hd_plot_dim ------------------------------------------------------------------
test_that("hd_plot_dim works correctly", {
  # Mock data for hd_object and dim_object
  hd_object <- list(
    data = tibble::tibble(
      sample_id = c("Sample1", "Sample2", "Sample3"),
      Var1 = c(1, 2, 3),
      Var2 = c(4, 5, 6)
    ),
    metadata = tibble::tibble(
      sample_id = c("Sample1", "Sample2", "Sample3"),
      Group = c("A", "B", "A")
    ),
    sample_id = "sample_id",
    var_name = "Group"
  )
  class(hd_object) <- "HDAnalyzeR"
  pca_res <- tibble::tibble(
    sample_id = c("Sample1", "Sample2", "Sample3"),
    PC1 = c(1.5, -0.5, 0.2),
    PC2 = c(0.3, 0.7, -1.0)
  )

  pca_loadings <- tibble::tibble(
    component = c("PC1", "PC1", "PC2", "PC2"),
    terms = c("Var1", "Var2", "Var1", "Var2"),
    value = c(0.8, -0.6, 0.7, -0.4)
  )

  pca_variance <- tibble::tibble(
    component = c("PC1", "PC2"),
    percent_variance = c(50.0, 30.0)
  )

  dim_object <- list(
    pca_res = pca_res,
    pca_loadings = pca_loadings,
    pca_variance = pca_variance,
    by_sample = TRUE
  )
  class(dim_object) <- "hd_pca"

  # Test case 1: Basic PCA plot without loadings or palette
  result <- hd_plot_dim(dim_object, hd_object, x = "PC1", y = "PC2")
  expect_s3_class(result, "hd_pca")
  expect_s3_class(result$pca_plot, "ggplot")

  # Test case 2: PCA plot with loadings
  result <- hd_plot_dim(dim_object, hd_object, x = "PC1", y = "PC2", plot_loadings = "PC1", nloadings = 2)
  expect_s3_class(result$pca_plot, "ggplot")

  # Test case 3: PCA plot with color from metadata
  result <- hd_plot_dim(dim_object, hd_object, x = "PC1", y = "PC2", color = "Group")
  expect_true("colour" %in% names(ggplot2::ggplot_build(result$pca_plot)$data[[1]]))

  # Test case 4: UMAP object instead of PCA
  umap_res <- tibble::tibble(
    sample_id = c("Sample1", "Sample2", "Sample3"),
    UMAP1 = c(0.1, -0.2, 0.3),
    UMAP2 = c(0.5, 0.6, -0.4)
  )
  umap_object <- list(
    umap_res = umap_res,
    by_sample = TRUE
  )
  class(umap_object) <- "hd_umap"

  result <- hd_plot_dim(umap_object, hd_object, x = "UMAP1", y = "UMAP2")
  expect_s3_class(result, "hd_umap")
  expect_s3_class(result$umap_plot, "ggplot")
})


# hd_auto_pca ------------------------------------------------------------------
test_that("hd_auto_pca works correctly", {
  # Mock data for hd_object
  hd_object <- list(
    data = tibble::tibble(
      sample_id = c("Sample1", "Sample2", "Sample3"),
      Var1 = c(1, 2, 3),
      Var2 = c(4, 5, 6),
      Var3 = c(7, 8, 9)
    ),
    metadata = tibble::tibble(
      sample_id = c("Sample1", "Sample2", "Sample3"),
      Group = c("A", "B", "A")
    ),
    sample_id = "sample_id",
    var_name = "Group"
  )
  class(hd_object) <- "HDAnalyzeR"
  # Test case 1: Basic PCA with default settings
  pca_result <- hd_auto_pca(hd_object, components = 2)
  expect_s3_class(pca_result, "hd_pca")
  expect_true("pca_res" %in% names(pca_result))
  expect_true("pca_loadings" %in% names(pca_result))
  expect_true("pca_variance" %in% names(pca_result))
  expect_true("pca_plot" %in% names(pca_result))

  # Test case 2: Verify the PCA result structure
  expect_s3_class(pca_result$pca_res, "tbl_df")
  expect_s3_class(pca_result$pca_loadings, "tbl_df")
  expect_s3_class(pca_result$pca_variance, "tbl_df")
  expect_s3_class(pca_result$pca_plot, "ggplot")

  # Edge Case: Non-HDAnalyzeR object input
  pca_result <- hd_auto_pca(hd_object$data, hd_object$metadata, components = 2)
  expect_s3_class(pca_result, "hd_pca")
  expect_true("pca_res" %in% names(pca_result))
  expect_true("pca_loadings" %in% names(pca_result))
  expect_true("pca_variance" %in% names(pca_result))
  expect_true("pca_plot" %in% names(pca_result))
  expect_s3_class(pca_result$pca_res, "tbl_df")
  expect_s3_class(pca_result$pca_loadings, "tbl_df")
  expect_s3_class(pca_result$pca_variance, "tbl_df")
  expect_s3_class(pca_result$pca_plot, "ggplot")
})


# hd_umap ------------------------------------------------------------------
test_that("hd_umap performs UMAP correctly with by_sample = TRUE", {
  # Create example data and metadata
  test_data <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Protein1 = rnorm(10),
    Protein2 = rnorm(10),
    Protein3 = rnorm(10)
  )
  test_metadata <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Group = rep(c("A", "B"), 5)
  )
  # Initialize hd_object
  hd_obj <- hd_initialize(test_data, test_metadata, is_wide = TRUE)

  # Run UMAP
  umap_res <- hd_umap(hd_obj, components = 2, by_sample = TRUE)

  # Check output class
  expect_s3_class(umap_res, "hd_umap")

  # Check dimensions of pca_res
  expect_equal(ncol(umap_res$umap_res), 3)
  expect_equal(nrow(umap_res$umap_res), 10)
})

test_that("hd_umap performs UMAP correctly with by_sample = FALSE", {
  test_data <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Protein1 = rnorm(10),
    Protein2 = rnorm(10),
    Protein3 = rnorm(10)
  )
  test_metadata <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Group = rep(c("A", "B"), 5)
  )

  # Initialize hd_object
  hd_obj <- hd_initialize(test_data, test_metadata, is_wide = TRUE)

  # Run UMAP
  umap_res <- hd_umap(hd_obj, components = 2, by_sample = FALSE)

  # Check output class
  expect_s3_class(umap_res, "hd_umap")

  # Check dimensions of umap_res
  expect_equal(ncol(umap_res$umap_res), 3)
  expect_equal(nrow(umap_res$umap_res), 3)
})

test_that("hd_pca produces consistent results for fixed seed", {
  test_data <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Protein1 = rnorm(10),
    Protein2 = rnorm(10),
    Protein3 = rnorm(10)
  )
  test_metadata <- tibble::tibble(
    DAid = paste0("S", 1:10),
    Group = rep(c("A", "B"), 5)
  )

  # Initialize hd_object
  hd_obj <- hd_initialize(test_data, test_metadata, is_wide = TRUE)

  # Run UMAP twice with the same seed
  umap_res1 <- hd_umap(hd_obj, components = 2, seed = 123)
  umap_res2 <- hd_umap(hd_obj, components = 2, seed = 123)

  # Compare results
  expect_equal(umap_res1$umap_res, umap_res2$umap_res)
})


# hd_auto_umap -----------------------------------------------------------------
test_that("hd_auto_umap works correctly", {
  # Mock data for hd_object
  hd_object <- list(
    data = tibble::tibble(
      sample_id = c("Sample1", "Sample2", "Sample3"),
      Var1 = c(1, 2, 3),
      Var2 = c(4, 5, 6),
      Var3 = c(7, 8, 9)
    ),
    metadata = tibble::tibble(
      sample_id = c("Sample1", "Sample2", "Sample3"),
      Group = c("A", "B", "A")
    ),
    sample_id = "sample_id",
    var_name = "Group"
  )
  class(hd_object) <- "HDAnalyzeR"

  # Test case 1: Basic UMAP with default settings
  umap_result <- hd_auto_umap(hd_object)
  expect_s3_class(umap_result, "hd_umap")
  expect_true("umap_res" %in% names(umap_result))
  expect_true("umap_plot" %in% names(umap_result))

  # Test case 2: Verify the UMAP result structure
  expect_s3_class(umap_result$umap_res, "tbl_df")
  expect_s3_class(umap_result$umap_plot, "ggplot")

  # Edge Case: Non-HDAnalyzeR object input
  umap_result <- hd_auto_umap(hd_object$data, hd_object$metadata)
  expect_s3_class(umap_result, "hd_umap")
  expect_true("umap_res" %in% names(umap_result))
  expect_true("umap_plot" %in% names(umap_result))
  expect_s3_class(umap_result$umap_res, "tbl_df")
  expect_s3_class(umap_result$umap_plot, "ggplot")
})
