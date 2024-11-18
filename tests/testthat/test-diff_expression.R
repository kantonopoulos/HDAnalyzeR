# # Test do_limma_de -------------------------------------------------------------
# test_that("do_limma_de performs DE properly", {
#   test_data <- example_data |>
#     dplyr::select(DAid, Assay, NPX) |>
#     tidyr::pivot_wider(names_from = Assay, values_from = NPX) |>
#     dplyr::left_join(example_metadata |>
#                        dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex"))),
#                      by = "DAid") |>
#     dplyr::select(DAid, Disease, Sex, 2:11)
#
#   result <- do_limma_de(test_data, case = "AML", control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC"), correct = NULL) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
#
#   expected <- tibble::tibble(
#     Assay = c("ABL1", "ACTA2", "ACAN", "ACE2", "ACP6", "ACAA1", "AARSD1", "ACOX1", "ACTN4", "ACP5"),
#     logFC = c(0.75, 0.5, -0.38, 0.47, -0.31, -0.16, 0.11, -0.01, 0, 0.02),
#     CI.L = c(0.32, 0.22, -0.61, 0.14, -0.59, -0.57, -0.22, -0.3, -0.21, -0.21),
#     CI.R = c(1.17, 0.79, -0.15, 0.79, -0.02, 0.25, 0.43, 0.28, 0.21, 0.25),
#     AveExpr = c(1.81, 1.61, 0.57, 0.93, 1.13, 1.01, 3.13, 0.5, 0.39, 0.92),
#     t = c(3.47, 3.44, -3.28, 2.82, -2.12, -0.78, 0.63, -0.07, -0.03, 0.17),
#     P.Value = c(0, 0, 0, 0, 0.03, 0.44, 0.53, 0.95, 0.98, 0.87),
#     adj.P.Val = c(0, 0, 0, 0.01, 0.07, 0.73, 0.75, 0.98, 0.98, 0.98),
#     B = c(-0.48, -0.61, -1.12, -2.44, -4.1, -5.92, -6.02, -6.26, -6.27, -6.27),
#     Disease = rep("AML", 10),
#     sig = c("significant up", "significant up", "significant down", "significant up", "not significant",
#             "not significant", "not significant", "not significant", "not significant", "not significant")
#   )
#
#   expect_equal(result, expected)
# })
#
#
# test_that("do_limma_de corrects for Sex", {
#   test_data <- example_data |>
#     dplyr::select(DAid, Assay, NPX) |>
#     tidyr::pivot_wider(names_from = Assay, values_from = NPX) |>
#     dplyr::left_join(example_metadata |>
#                        dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex"))),
#                      by = "DAid") |>
#     dplyr::select(DAid, Disease, Sex, 2:11)
#
#   result <- do_limma_de(test_data, case = "AML", control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC"), correct = "Sex", correct_type = "factor") |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
#
#   expected <- tibble::tibble(
#     Assay = c("ACTA2", "ACAN", "ABL1", "ACP6", "ACE2", "ACAA1", "AARSD1", "ACOX1", "ACTN4", "ACP5"),
#     logFC = c(0.5, -0.39, 0.66, -0.35, 0.33, -0.19, 0.06, -0.04, -0.01, -0.01),
#     CI.L = c(0.21, -0.62, 0.25, -0.64, 0.01, -0.59, -0.27, -0.33, -0.22, -0.24),
#     CI.R = c(0.79, -0.16, 1.08, -0.06, 0.66, 0.22, 0.39, 0.26, 0.19, 0.22),
#     AveExpr = c(1.61, 0.57, 1.81, 1.13, 0.93, 1.01, 3.13, 0.5, 0.39, 0.92),
#     t = c(3.41, -3.36, 3.13, -2.39, 2.05, -0.9, 0.37, -0.24, -0.13, -0.08),
#     P.Value = c(0, 0, 0, 0.02, 0.04, 0.37, 0.71, 0.81, 0.9, 0.94),
#     adj.P.Val = c(0, 0, 0.01, 0.04, 0.08, 0.61, 0.94, 0.94, 0.94, 0.94),
#     B = c(-0.68, -0.82, -1.52, -3.47, -4.17, -5.76, -6.08, -6.17, -6.19, -6.21),
#     Disease = rep("AML", 10),
#     sig = c("significant up", "significant down", "significant up", "significant down", "not significant",
#             "not significant", "not significant", "not significant", "not significant", "not significant")
#   )
#
#   expect_equal(result, expected)
# })
#
#
# test_that("do_limma_de changes limits of significance", {
#   test_data <- example_data |>
#     dplyr::select(DAid, Assay, NPX) |>
#     tidyr::pivot_wider(names_from = Assay, values_from = NPX) |>
#     dplyr::left_join(example_metadata |>
#                        dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex"))),
#                      by = "DAid") |>
#     dplyr::select(DAid, Disease, Sex, 2:11)
#
#   result <- do_limma_de(test_data, case = "AML", control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC"), correct = NULL, pval_lim = 0.01, logfc_lim = 0.5) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
#
#   expected <- tibble::tibble(
#     Assay = c("ABL1", "ACTA2", "ACAN", "ACE2", "ACP6", "ACAA1", "AARSD1", "ACOX1", "ACTN4", "ACP5"),
#     logFC = c(0.75, 0.5, -0.38, 0.47, -0.31, -0.16, 0.11, -0.01, 0, 0.02),
#     CI.L = c(0.32, 0.22, -0.61, 0.14, -0.59, -0.57, -0.22, -0.3, -0.21, -0.21),
#     CI.R = c(1.17, 0.79, -0.15, 0.79, -0.02, 0.25, 0.43, 0.28, 0.21, 0.25),
#     AveExpr = c(1.81, 1.61, 0.57, 0.93, 1.13, 1.01, 3.13, 0.5, 0.39, 0.92),
#     t = c(3.47, 3.44, -3.28, 2.82, -2.12, -0.78, 0.63, -0.07, -0.03, 0.17),
#     P.Value = c(0, 0, 0, 0, 0.03, 0.44, 0.53, 0.95, 0.98, 0.87),
#     adj.P.Val = c(0, 0, 0, 0.01, 0.07, 0.73, 0.75, 0.98, 0.98, 0.98),
#     B = c(-0.48, -0.61, -1.12, -2.44, -4.1, -5.92, -6.02, -6.26, -6.27, -6.27),
#     Disease = rep("AML", 10),
#     sig = c("significant up", "significant up", "not significant", "not significant", "not significant",
#             "not significant", "not significant", "not significant", "not significant", "not significant")
#   )
#
#   expect_equal(result, expected)
# })
#
#
# # Test do_ttest_de -------------------------------------------------------------
# test_that("do_ttest_de performs DE properly", {
#   test_data <- example_data |>
#     dplyr::select(DAid, Assay, NPX) |>
#     tidyr::pivot_wider(names_from = Assay, values_from = NPX) |>
#     dplyr::left_join(example_metadata |>
#                        dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex", "Age", "BMI"))),
#                      by = "DAid") |>
#     dplyr::select(DAid, Disease, Sex, Age, BMI, 2:11)
#
#   long_data <- test_data |>
#     dplyr::select(-dplyr::any_of(c("Age", "BMI"))) |>
#     tidyr::pivot_longer(!c("DAid", "Disease", "Sex"), names_to = "Assay", values_to = "NPX")
#
#   assays <- unique(long_data$Assay)
#
#   normality_res <- check_normality(
#     test_data |>
#       dplyr::select(-dplyr::any_of(c("DAid", "Disease", "Sex", "Age", "BMI")))
#   ) |>
#     dplyr::pull(is_normal)
#
#   result <- do_ttest_de(long_data, case = "AML", control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC"), assays = assays, normality_res = normality_res) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
#
#   expected <- tibble::tibble(
#     Assay = c("ABL1", "ACAN", "ACTA2", "ACE2", "ACP6", "AARSD1", "ACAA1", "ACTN4", "ACP5", "ACOX1"),
#     P.Value = c(0, 0, 0, 0.01, 0.02, 0.54, 0.49, 0.75, 0.85, 0.97),
#     logFC = c(0.9, -0.37, 0.48, 0.48, -0.37, 0.23, -0.01, 0.03, -0.02, -0.01),
#     Disease = rep("AML", 10),
#     adj.P.Val = c(0, 0, 0, 0.03, 0.04, 0.77, 0.77, 0.94, 0.94, 0.97),
#     sig = c("significant up", "significant down", "significant up", "significant up", "significant down",
#             "not significant", "not significant", "not significant", "not significant", "not significant")
#   )
#
#   expect_equal(result, expected)
# })
#
#
# test_that("do_ttest_de changes limits of significance", {
#   test_data <- example_data |>
#     dplyr::select(DAid, Assay, NPX) |>
#     tidyr::pivot_wider(names_from = Assay, values_from = NPX) |>
#     dplyr::left_join(example_metadata |>
#                        dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex", "Age", "BMI"))),
#                      by = "DAid") |>
#     dplyr::select(DAid, Disease, Sex, Age, BMI, 2:11)
#
#   long_data <- test_data |>
#     dplyr::select(-dplyr::any_of(c("Age", "BMI"))) |>
#     tidyr::pivot_longer(!c("DAid", "Disease", "Sex"), names_to = "Assay", values_to = "NPX")
#
#   assays <- unique(long_data$Assay)
#
#   normality_res <- check_normality(
#     test_data |>
#       dplyr::select(-dplyr::any_of(c("DAid", "Disease", "Sex", "Age", "BMI")))
#   ) |>
#     dplyr::pull(is_normal)
#
#   result <- do_ttest_de(long_data, case = "AML", control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC"), assays = assays, normality_res = normality_res, pval_lim = 0.01, logfc_lim = 0.5) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
#
#   expected <- tibble::tibble(
#     Assay = c("ABL1", "ACAN", "ACTA2", "ACE2", "ACP6", "AARSD1", "ACAA1", "ACTN4", "ACP5", "ACOX1"),
#     P.Value = c(0, 0, 0, 0.01, 0.02, 0.54, 0.49, 0.75, 0.85, 0.97),
#     logFC = c(0.9, -0.37, 0.48, 0.48, -0.37, 0.23, -0.01, 0.03, -0.02, -0.01),
#     Disease = rep("AML", 10),
#     adj.P.Val = c(0, 0, 0, 0.03, 0.04, 0.77, 0.77, 0.94, 0.94, 0.97),
#     sig = c("significant up", "not significant", "not significant", "not significant", "not significant",
#             "not significant", "not significant", "not significant", "not significant", "not significant")
#   )
#
#   expect_equal(result, expected)
# })
#
#
# # Test plot_volcano ----------------------------------------------------------
# test_that("plot_volcano works with valid input", {
#
#   de_result <- data.frame(
#     Assay = paste0("Protein", 1:100),
#     adj.P.Val = runif(100, 0, 0.1),
#     logFC = rnorm(100),
#     sig = sample(c("significant up", "significant down", "not significant"), 100, replace = TRUE)
#   )
#
#   p <- plot_volcano(
#     de_result = de_result,
#     pval_lim = 0.05,
#     logfc_lim = 0,
#     top_up_prot = 10,
#     top_down_prot = 10,
#     palette = "diff_exp",
#     title = "DiseaseX"
#   )
#
#   expect_s3_class(p, "ggplot")
#
#   # Check that the title is correctly set
#   expect_true(grepl("DiseaseX", p$labels$title))
#
#   # Check that the subtitle is correctly set
#   expect_true(grepl("Num significant up =", p$labels$subtitle))
#
#   # Check that the color scale is applied correctly
#   scale_colors <- ggplot2::ggplot_build(p)$data[[1]]$colour
#   expected_colors <- c("grey", "blue", "red")
#   expect_true(all(scale_colors %in% expected_colors))
# })
#
#
# test_that("plot_volcano handles top significant proteins correctly", {
#   de_result <- data.frame(
#     Assay = paste0("Protein", 1:100),
#     adj.P.Val = runif(100, 0, 0.1),
#     logFC = rnorm(100),
#     sig = sample(c("significant up", "significant down", "not significant"), 100, replace = TRUE)
#   )
#
#   p <- plot_volcano(
#     de_result = de_result,
#     pval_lim = 0.05,
#     logfc_lim = 0,
#     top_up_prot = 10,
#     top_down_prot = 10,
#     palette = "diff_exp",
#     title = "DiseaseX"
#   )
#
#   data_built <- ggplot2::ggplot_build(p)$data[[1]]
#
#   top_sig <- de_result |>
#     dplyr::filter(adj.P.Val < 0.05) |>
#     dplyr::arrange(adj.P.Val) |>
#     dplyr::slice_head(n = 40) |>
#     dplyr::pull(Assay)
#
#   points_labels <- ggplot2::ggplot_build(p)$data[[2]]$label
#   expect_true(all(points_labels %in% top_sig))
# })
#
#
# # Test do_limma ----------------------------------------------------------------
# test_that("do_limma performs DE properly", {
#   first_10_unique_assays <- example_data |>
#     dplyr::distinct(Assay) |>
#     dplyr::slice(1:10) |>
#     dplyr::pull(Assay)
#
#   test_data <- example_data |>
#     dplyr::select(DAid, Assay, NPX) |>
#     dplyr::filter(Assay %in% first_10_unique_assays)
#
#   result <- do_limma(test_data, example_metadata, case = "AML", control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC"), correct = NULL, wide = F, volcano = F)
#   result <- result |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
#
#   expected <- tibble::tibble(
#     Assay = c("ABL1", "ACTA2", "ACAN", "ACE2", "ACP6", "ACAA1", "AARSD1", "ACOX1", "ACTN4", "ACP5"),
#     logFC = c(0.75, 0.5, -0.38, 0.47, -0.31, -0.16, 0.11, -0.01, 0, 0.02),
#     CI.L = c(0.32, 0.22, -0.61, 0.14, -0.59, -0.57, -0.22, -0.3, -0.21, -0.21),
#     CI.R = c(1.17, 0.79, -0.15, 0.79, -0.02, 0.25, 0.43, 0.28, 0.21, 0.25),
#     AveExpr = c(1.81, 1.61, 0.57, 0.93, 1.13, 1.01, 3.13, 0.5, 0.39, 0.92),
#     t = c(3.47, 3.44, -3.28, 2.82, -2.12, -0.78, 0.63, -0.07, -0.03, 0.17),
#     P.Value = c(0, 0, 0, 0, 0.03, 0.44, 0.53, 0.95, 0.98, 0.87),
#     adj.P.Val = c(0, 0, 0, 0.01, 0.07, 0.73, 0.75, 0.98, 0.98, 0.98),
#     B = c(-0.48, -0.61, -1.12, -2.44, -4.1, -5.92, -6.02, -6.26, -6.27, -6.27),
#     Disease = rep("AML", 10),
#     sig = c("significant up", "significant up", "significant down", "significant up", "not significant",
#             "not significant", "not significant", "not significant", "not significant", "not significant")
#   )
#
#   expect_equal(result, expected)
# })
#
#
# # Test do_ttest ----------------------------------------------------------------
# test_that("do_ttest_de performs DE properly", {
#   first_10_unique_assays <- example_data |>
#     dplyr::distinct(Assay) |>
#     dplyr::slice(1:10) |>
#     dplyr::pull(Assay)
#
#   test_data <- example_data |>
#     dplyr::select(DAid, Assay, NPX) |>
#     dplyr::filter(Assay %in% first_10_unique_assays)
#
#   result <- do_ttest(test_data, example_metadata, case = "AML", control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC"), wide = F, volcano = F)
#   result <- result |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
#   expected <- tibble::tibble(
#       Assay = c("ABL1", "ACAN", "ACTA2", "ACE2", "ACP6", "AARSD1", "ACAA1", "ACTN4", "ACP5", "ACOX1"),
#       P.Value = c(0, 0, 0, 0.01, 0.02, 0.54, 0.49, 0.75, 0.85, 0.97),
#       logFC = c(0.9, -0.37, 0.48, 0.48, -0.37, 0.23, -0.01, 0.03, -0.02, -0.01),
#       Disease = rep("AML", 10),
#       adj.P.Val = c(0, 0, 0, 0.03, 0.04, 0.77, 0.77, 0.94, 0.94, 0.97),
#       sig = c("significant up", "significant down", "significant up", "significant up", "significant down",
#               "not significant", "not significant", "not significant", "not significant", "not significant")
#     )
#
#     expect_equal(result, expected)
# })
