# Test hd_de_limma ---------------------------------------------------------
test_that("hd_de_limma works with categorical variables", {
  # Mock dataset
  dat <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5"),
    Protein1 = c(5, 6, 7, 8, 9),
    Protein2 = c(10, 11, 12, 13, 14)
  )

  metadata <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5"),
    Disease = c("AML", "AML", "CLL", "CLL", "AML"),
    Age = c(30, 40, 50, 60, 70)
  )

  # Run differential expression analysis
  result <- hd_de_limma(
    dat,
    metadata = metadata,
    variable = "Disease",
    case = "AML",
    control = "CLL"
  )

  expect_s3_class(result, "hd_de")
  expect_true("de_res" %in% names(result))
  expect_true(all(c("Feature", "logFC", "adj.P.Val") %in% colnames(result$de_res)))
})

test_that("hd_de_limma works with continuous variables", {
  # Mock dataset
  dat <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10"),
    Protein1 = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
    Protein2 = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
  )

  metadata <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10"),
    Age = c(30, 40, 50, 60, 70, 35, 45, 55, 65, 75)
  )

  # Run differential expression analysis
  result <- hd_de_limma(
    dat,
    metadata = metadata,
    variable = "Age",
    case = NULL,
    control = NULL
  )

  expect_s3_class(result, "hd_de")
  expect_true("de_res" %in% names(result))
  expect_true(all(c("Feature", "logFC", "adj.P.Val") %in% colnames(result$de_res)))
})

test_that("hd_de_limma raises an error for invalid input", {
  dat <- data.frame(
    SampleID = c("S1", "S2"),
    Protein1 = c(5, 6)
  )
  metadata <- data.frame(
    SampleID = c("S1", "S2"),
    Disease = c("AML", "CLL")
  )

  expect_error(hd_de_limma(
    dat,
    metadata = metadata,
    variable = "NonexistentColumn",
    case = "AML"
  ), "The variable is not be present in the metadata.")
})

test_that("hd_de_limma removes rows with NAs in relevant columns", {
  dat <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5"),
    Protein1 = c(5, 6, 7, 8, 9),
    Protein2 = c(10, 11, 12, 13, 14)
  )

  metadata <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5"),
    Disease = c("AML", "AML", "CLL", "CLL", NA),
    Age = c(30, 40, NA, 60, 70)
  )

  result <- expect_warning(hd_de_limma(
    dat,
    metadata = metadata,
    variable = "Disease",
    case = "AML"
  ))
  suppressWarnings(result <- hd_de_limma(dat,
                                         metadata = metadata,
                                         variable = "Disease",
                                         case = "AML"))

  expect_true(nrow(result$de_res) > 0) # Ensure some rows were processed
})

test_that("hd_de_limma works with correction variables", {
    dat <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10"),
    Protein1 = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
    Protein2 = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
  )

  metadata <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10"),
    Disease = c("AML", "AML", "CLL", "CLL", "AML", "AML", "AML", "CLL", "CLL", "AML"),
    Age = c(30, 40, 50, 60, 70, 35, 45, 55, 65, 75)
  )

  result <- hd_de_limma(
    dat,
    metadata = metadata,
    variable = "Disease",
    case = "AML",
    control = "CLL",
    correct = "Age"
  )

  expect_s3_class(result, "hd_de")
  expect_true("de_res" %in% names(result))
  expect_true(all(c("Feature", "logFC", "adj.P.Val") %in% colnames(result$de_res)))
})


# Test hd_de_ttest ---------------------------------------------------------
test_that("hd_de_ttest works with categorical variables", {
  # Mock dataset
  dat <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5"),
    Protein1 = c(5, 6, 7, 8, 9),
    Protein2 = c(10, 11, 12, 13, 14)
  )

  metadata <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5"),
    Disease = c("AML", "AML", "CLL", "CLL", "AML"),
    Age = c(30, 40, 50, 60, 70)
  )

  # Run differential expression analysis
  result <- hd_de_ttest(
    dat,
    metadata = metadata,
    variable = "Disease",
    case = "AML",
    control = "CLL"
  )

  expect_s3_class(result, "hd_de")
  expect_true("de_res" %in% names(result))
  expect_true(all(c("Feature", "logFC", "adj.P.Val") %in% colnames(result$de_res)))
})

test_that("hd_de_ttest raises an error for invalid input", {
  dat <- data.frame(
    SampleID = c("S1", "S2"),
    Protein1 = c(5, 6)
  )
  metadata <- data.frame(
    SampleID = c("S1", "S2"),
    Disease = c("AML", "CLL")
  )

  expect_error(hd_de_ttest(
    dat,
    metadata = metadata,
    variable = "NonexistentColumn",
    case = "AML"
  ), "The variable is not be present in the metadata.")
})

test_that("hd_de_ttest removes rows with NAs in relevant columns", {
  dat <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5"),
    Protein1 = c(5, 6, 7, 8, 9),
    Protein2 = c(10, 11, 12, 13, 14)
  )

  metadata <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4", "S5"),
    Disease = c("AML", "AML", "CLL", "CLL", NA),
    Age = c(30, 40, NA, 60, 70)
  )

  result <- expect_warning(hd_de_ttest(
    dat,
    metadata = metadata,
    variable = "Disease",
    case = "AML"
  ))
  suppressWarnings(result <- hd_de_ttest(dat,
                                         metadata = metadata,
                                         variable = "Disease",
                                         case = "AML"))

  expect_true(nrow(result$de_res) > 0) # Ensure some rows were processed
})


# Test hd_plot_volcano ---------------------------------------------------------
test_that("hd_plot_volcano handles valid inputs correctly", {
  # Mock DE results object
  mock_de_object <- list(
    de_res = tibble::tibble(
      Feature = paste0("Protein", 1:100),
      logFC = c(rnorm(50, 2, 1), rnorm(50, -2, 1)),
      adj.P.Val = c(runif(50, 0, 0.04), runif(50, 0.05, 1))
    )
  )
  class(mock_de_object) <- "hd_de"

  # Run the function
  result <- hd_plot_volcano(
    de_object = mock_de_object,
    pval_lim = 0.05,
    logfc_lim = 1.5,
    top_up_prot = 5,
    top_down_prot = 5,
    title = "Volcano Plot Test",
    report_nproteins = TRUE
  )

  # Check class and components of the output
  expect_true(inherits(result, "hd_de"))
  expect_true("volcano_plot" %in% names(result))

  # Verify significant proteins count in the plot subtitle
  plot_object <- result[["volcano_plot"]]
  title_text <- ggplot2::ggplot_build(plot_object)$plot$labels$title
  expect_match(title_text, "Num significant up = [0-9]+")
  expect_match(title_text, "Num significant down = [0-9]+")
})

test_that("hd_plot_volcano handles missing user_defined_proteins gracefully", {
  # Mock DE results object
  mock_de_object <- list(
    de_res = tibble::tibble(
      Feature = paste0("Protein", 1:50),
      logFC = c(rnorm(25, 1.5, 0.5), rnorm(25, -1.5, 0.5)),
      adj.P.Val = runif(50, 0, 0.05)
    )
  )
  class(mock_de_object) <- "hd_de"

  # Run the function without user_defined_proteins
  result <- hd_plot_volcano(
    de_object = mock_de_object,
    user_defined_proteins = NULL
  )

  # Verify it still creates the volcano plot
  expect_true("volcano_plot" %in% names(result))
})

test_that("hd_plot_volcano handles invalid de_object input", {
  # Create an invalid input
  invalid_de_object <- list(de_res = NULL)

  # Check for error
  expect_error(
    hd_plot_volcano(de_object = invalid_de_object),
    "The input object is not a differential expression object."
  )
})

test_that("hd_plot_volcano handles empty DE results", {
  # Create an empty DE results object
  empty_de_object <- list(de_res = tibble::tibble())
  class(empty_de_object) <- "hd_de"

  # Check for error
  expect_error(
    hd_plot_volcano(de_object = empty_de_object),
    "The input object does not contain the differential expression results."
  )
})


# Test extract_protein_list ----------------------------------------------------
test_that("extract_protein_list works as expected", {
  # Mock data for testing
  mock_upset_data <- tibble::tibble(
    Disease1 = c(1, 0, 1),
    Disease2 = c(0, 1, 1)
  )
  mock_proteins <- list(
    Disease1 = c("ProteinA", "ProteinB"),
    Disease2 = c("ProteinB", "ProteinC")
  )

  # Call the function
  result <- extract_protein_list(mock_upset_data, mock_proteins)

  # Expected output
  expected_proteins_list <- list(
    "Disease1" = c("ProteinA", "ProteinB"),
    "Disease2" = c("ProteinB", "ProteinC"),
    "Disease1&Disease2" = c("ProteinB")
  )

  expected_proteins_df <- tibble::tibble(
    Shared_in = c("Disease1", "Disease2", "Disease1&Disease2"),
    `up/down` = "up", # Expected direction
    Feature = c("ProteinA", "ProteinB", "ProteinC")
  )

  # Assertions
  expect_type(result, "list")
  expect_named(result, c("proteins_list", "proteins_df"))
  expect_equal(result$proteins_list, expected_proteins_list)
  expect_equal(result$proteins_df$Feature, expected_proteins_df$Feature)
})


# Test hd_plot_de_summary ------------------------------------------------------
test_that("hd_plot_de_summary works as expected", {
  # Mock data for testing
  mock_de_result1 <- list(
    de_res = tibble::tibble(
      Feature = c("ProteinA", "ProteinB", "ProteinC"),
      logFC = c(1.5, -1.2, 0.5),
      adj.P.Val = c(0.01, 0.03, 0.2),
      Disease = "AML"
    )
  )
  mock_de_result2 <- list(
    de_res = tibble::tibble(
      Feature = c("ProteinD", "ProteinE", "ProteinF"),
      logFC = c(-1.8, 2.0, 0.3),
      adj.P.Val = c(0.02, 0.01, 0.25),
      Disease = "LUNGC"
    )
  )
  mock_de_results <- list("AML" = mock_de_result1, "LUNGC" = mock_de_result2)

  # Call the function
  result <- hd_plot_de_summary(
    de_results = mock_de_results,
    variable = "Disease",
    pval_lim = 0.05,
    logfc_lim = 0.5
  )

  # Assertions
  expect_type(result, "list")
  expect_named(result, c(
    "de_barplot", "upset_plot_up", "upset_plot_down",
    "proteins_df_up", "proteins_df_down",
    "proteins_list_up", "proteins_list_down"
  ))

  # Check if plots and dataframes are created
  expect_true("ggplot" %in% class(result$de_barplot))
  expect_true("upset" %in% class(result$upset_plot_up))
  expect_true("upset" %in% class(result$upset_plot_down))

  # Validate protein lists
  expect_type(result$proteins_list_up, "list")
  expect_type(result$proteins_list_down, "list")
  expect_true("AML" %in% names(result$proteins_list_up))
  expect_true("LUNGC" %in% names(result$proteins_list_down))

  # Check dataframes
  expect_true(all(c("Feature", "Shared_in") %in% colnames(result$proteins_df_up)))
  expect_true(all(c("Feature", "Shared_in") %in% colnames(result$proteins_df_down)))
})
