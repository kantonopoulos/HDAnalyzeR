# Test apply_palette -----------------------------------------------------------
hd_object <- hd_initialize(example_data, example_metadata)

# Unit test for apply_palette function
test_that("apply_palette correctly applies palettes to plots", {

  # Create a basic ggplot
  p <- hd_object[["data"]] |>
    left_join(hd_object[["metadata"]], by = "DAid") |>
    ggplot2::ggplot(ggplot2::aes(x = ADA, y = Age)) +
    ggplot2::geom_point(ggplot2::aes(color = factor(Disease)))

  # Test 1: Apply a valid predefined palette from hd_palettes()
  result <- apply_palette(p, "cancers12", type = "color")
  expect_true("gg" %in% class(result))

  # Test 2: Apply a valid custom palette
  custom_palette <- c("AML" = "purple", "CLL" = "orange", "MYEL" = "pink")
  result <- apply_palette(p, custom_palette, type = "color")
  expect_true("gg" %in% class(result))

  # Test 3: Invalid palette name should stop the function with an error
  expect_error(apply_palette(p, "invalid_palette", type = "color"))
})


# Test hd_plot_feature_boxplot -------------------------------------------------
test_that("hd_plot_feature_boxplot correctly generates boxplots", {

  hd_object <- hd_initialize(example_data, example_metadata)

  # Boxplots for AARSD1 and ABL1 in AML vs all other classes
  boxplot <- hd_plot_feature_boxplot(hd_object,
                                     variable = "Disease",
                                     features = c("AARSD1", "ABL1"),
                                     case = "AML",
                                     palette = "cancers12")

  expect_true("gg" %in% class(boxplot))

  expect_warning(boxplot <- hd_plot_feature_boxplot(hd_object,
                                                    variable = "Disease",
                                                    features = c("AARSD1", "Male_x"),
                                                    case = "AML",
                                                    palette = "cancers12"))
})


# Test hd_plot_regression ------------------------------------------------------
test_that("hd_plot_regression correctly generates regression plots", {

  hd_object <- hd_initialize(example_data, example_metadata)

  # Scatter plot for AARSD1 and ABL1
  p <- hd_plot_regression(hd_object,
                          x = "AARSD1",
                          y = "ABL1",
                          se = TRUE,
                          line_color = "red3")

  expect_true("gg" %in% class(p))

  p <- hd_plot_regression(hd_object,
                          x = "AARSD1",
                          y = "Age",
                          metadata_cols = "Age",
                          se = FALSE,
                          line_color = "red3")

  expect_true("gg" %in% class(p))

})

# Test hd_plot_feature_heatmap -------------------------------------------------
test_that("hd_plot_feature_heatmap correctly generates heatmaps", {
  de_results <- list(
    "MYEL" = list(
      "de_res" = tibble::tibble(
        Feature = c("feature1", "feature2", "feature3"),
        logFC = c(1.5, -2.3, 0.5),
        adj.P.Val = c(0.01, 0.04, 0.03)
      )
    ),
    "LUNGC" = list(
      "de_res" = tibble::tibble(
        Feature = c("feature1", "feature2", "feature4"),
        logFC = c(2.5, -1.1, 1.2),
        adj.P.Val = c(0.02, 0.03, 0.05)
      )
    )
  )

  # Create mock model results
  model_results <- list(
    "MYEL" = list(
      "features" = tibble::tibble(
        Feature = c("feature1", "feature2", "feature3"),
        Scaled_Importance = c(0.8, 0.9, 0.7),
        Sign = c("POS", "NEG", "POS")
      )
    ),
    "LUNGC" = list(
      "features" = tibble::tibble(
        Feature = c("feature1", "feature2", "feature4"),
        Scaled_Importance = c(0.9, 0.8, 0.6),
        Sign = c("NEG", "POS", "NEG")
      )
    )
  )

  # Test with default parameters
  result <- hd_plot_feature_heatmap(
    de_results = de_results,
    model_results = model_results,
    order_by = "MYEL"
  )

  expect_true("gg" %in% class(result))
})


# Test hd_plot_feature_network -------------------------------------------------
test_that("hd_plot_feature_network correctly generates network plots", {
  de_results_aml <- hd_de_limma(hd_object, case = "AML")
  de_results_lungc <- hd_de_limma(hd_object, case = "LUNGC")
  de_results_cll <- hd_de_limma(hd_object, case = "CLL")

  feature_panel <- de_results_aml[["de_res"]] |>
    dplyr::filter(adj.P.Val < 0.05 & abs(logFC) > 1) |>
    dplyr::mutate(Class = "AML") |>
    dplyr::bind_rows(de_results_cll[["de_res"]] |>
                       dplyr::filter(adj.P.Val < 0.05 & abs(logFC) > 1) |>
                       dplyr::mutate(Class = "CLL"),
                     de_results_lungc[["de_res"]] |>
                       dplyr::filter(adj.P.Val < 0.05 & abs(logFC) > 1) |>
                       dplyr::mutate(Class = "LUNGC"))

  p <- hd_plot_feature_network(feature_panel,
                          plot_color = "logFC",
                          class_palette = "cancers12")

  expect_true("gg" %in% class(p))
})
