# # Test plot_loadings -----------------------------------------------------------
# test_that("plot_loadings works with valid input", {
#   tidied_res <- tibble::tibble(
#     terms = rep(paste0("Gene", 1:10), 4),
#     value = rnorm(40),
#     component = rep(paste0("PC", 1:4), each = 10)
#   )
#
#   p <- plot_loadings(tidied_res)
#
#   expect_s3_class(p, "ggplot")
#
#   # Check that the plot contains the correct facets
#   facets <- ggplot2::ggplot_build(p)$layout$layout$PANEL
#   expect_equal(levels(factor(facets)), as.character(1:4))
#
#   # Check that the fill colors are correctly set
#   fill_colors <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
#   expect_true(all(fill_colors %in% c("red3", "darkblue")))
# })
#
#
# # Test plot_dim_reduction ------------------------------------------------------
# test_that("plot_dim_reduction works with valid input and metadata", {
#   res <- tibble::tibble(
#     DAid = 1:10,
#     PC1 = rnorm(10),
#     PC2 = rnorm(10)
#   )
#
#   metadata <- tibble::tibble(
#     DAid = 1:10,
#     Sex = rep(c("Male", "Female"), each = 5)
#   )
#
#   palette <- c("Male" = "blue", "Female" = "red")
#
#   p <- plot_dim_reduction(
#     res = res,
#     x = "PC1",
#     y = "PC2",
#     metadata = metadata,
#     color = "Sex",
#     palette = palette
#   )
#
#   expect_s3_class(p, "ggplot")
#
#   # Check that the color scale is applied correctly
#   scale_colors <- unique(ggplot2::ggplot_build(p)$data[[1]]$colour)
#   expect_true(all(scale_colors %in% c("blue", "red")))
# })
#
#
# # Test do_pca ------------------------------------------------------------------
# test_that("do_pca runs pca analysis properly - pca_res", {
#   expected_subdata <- tibble::tibble(
#     DAid = factor(c("DA00001", "DA00002", "DA00003", "DA00004", "DA00005",
#              "DA00006", "DA00007", "DA00008", "DA00009", "DA00010")),
#     PC1 = c(-3.67, 4.11, -3.34, -4.78, -4.98, 0.39, -10.46, 2.64, -1.79, 3.57),
#     PC2 = c(-4.28, -2.64, 4.72, 0.44, -3.67, 0.06, -2.91, -2.01, -0.46, 0.82),
#     PC3 = c(-2.34, 2.04, 1.41, 1.41, 0.71, -1.90, -0.38, 2.75, 2.79, -0.01),
#     PC4 = c(-3.10, -0.44, 0.88, 0.11, -5.70, -7.75, -0.84, -0.13, -2.61, 1.50),
#     PC5 = c(-2.65, -4.43, -0.56, -1.10, -0.81, 0.71, -1.61, -0.85, -2.71, -2.66)
#   )
#
#   df <- example_data |> dplyr::select(DAid, Assay, NPX)
#   pca <- do_pca(df, wide = F, plots = F)
#   result_subdata <- pca$pca_res |>
#     utils::head(10) |>
#     dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(., 2)))
#   # Keep only the first 10 levels
#   first_10_levels <- unique(result_subdata$DAid)[1:10]
#   result_subdata$DAid <- factor(result_subdata$DAid, levels = first_10_levels)
#   expect_equal(result_subdata, expected_subdata)
# })
#
#
# test_that("do_pca runs pca analysis properly - loadings", {
#   expected_subdata <- tibble::tibble(
#     Assay = c("AARSD1", "ABL1", "ACAA1", "ACAN", "ACE2",
#               "ACOX1", "ACP5", "ACP6", "ACTA2", "ACTN4"),
#     Value = c(-0.13, -0.2, -0.16, 0.01, -0.06, -0.14, -0.06, -0.09, -0.08, -0.04),
#     PC = rep("PC1", 10)
#   )
#
#   df <- example_data |> dplyr::select(DAid, Assay, NPX)
#   pca <- do_pca(df, wide = F, plots = F)
#   result_subdata <- pca$loadings |>
#     utils::head(10) |>
#     dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(., 2))) |>
#     dplyr::select(Assay, Value, PC)
#   expect_equal(result_subdata, expected_subdata)
# })
#
#
# # Test do_umap -----------------------------------------------------------------
# test_that("do_umap runs pca analysis properly - umap_res", {
#   wide_df <- example_data |>
#     dplyr::select(DAid, Assay, NPX) |>
#     tidyr::pivot_wider(names_from = Assay, values_from = NPX)
#   set.seed(123)
#   if (!requireNamespace("umap", quietly = TRUE)) {
#     stop("The 'umap' package is required but not installed. Please install it using install.packages('umap').")
#   }
#   umap_rec <- recipes::recipe( ~ ., data = wide_df) |>
#     recipes::update_role(DAid, new_role = "id")  |>
#     recipes::step_normalize(recipes::all_predictors()) |>
#     recipes::step_impute_knn(recipes::all_predictors(), neighbors = 5) |>
#     embed::step_umap(recipes::all_predictors())
#
#   umap_prep <- recipes::prep(umap_rec)
#
#   expected <-  recipes::juice(umap_prep) |>
#     utils::head(10) |>
#     dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(., 2)))
#
#   df <- example_data |> dplyr::select(DAid, Assay, NPX)
#   umap <- do_umap(df, wide = F, plots = F)
#   result_subdata <- umap |>
#     utils::head(10) |>
#     dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(., 2)))
#
#   # Keep only the first 10 levels
#   first_10_levels <- unique(result_subdata$DAid)[1:10]
#   result_subdata$DAid <- factor(result_subdata$DAid, levels = first_10_levels)
#   expected$DAid <- factor(expected$DAid, levels = first_10_levels)
#   expect_equal(result_subdata, expected)
# })
