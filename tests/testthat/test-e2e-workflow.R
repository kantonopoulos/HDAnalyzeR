# End-to-end analyses --------------------------------------------------------
#
# These exercise the pipelines the package is actually used for, on the shipped
# example data, checking that results flow correctly from one step to the next.

test_that("the biomarker discovery workflow runs end to end", {
  skip_on_cran()

  # 1. Load and inspect ------------------------------------------------------
  hd_object <- example_subset()
  expect_s3_class(hd_object, "HDAnalyzeR")
  expect_equal(hd_object$sample_id, "DAid")

  qc <- quietly(hd_qc_summary(hd_object, variable = "Disease"))
  expect_s3_class(qc, "hd_qc")

  # 2. Impute and normalise --------------------------------------------------
  imputed <- hd_impute_knn(hd_object, k = 5, verbose = FALSE)
  expect_false(anyNA(imputed$data))
  expect_equal(nrow(imputed$data), nrow(hd_object$data))

  normalized <- hd_normalize(imputed)
  feature_means <- colMeans(normalized$data[, -1])
  expect_true(all(abs(feature_means) < 1e-8))

  # 3. Dimensionality reduction ---------------------------------------------
  pca <- hd_auto_pca(normalized, components = 5, plot_color = "Disease")
  expect_s3_class(pca, "hd_pca")
  expect_equal(nrow(pca$pca_res), nrow(hd_object$data))
  expect_renderable_ggplot(pca$pca_plot)

  # 4. Differential expression ----------------------------------------------
  de <- quietly(hd_de_limma(hd_object, variable = "Disease", case = "AML"))
  expect_s3_class(de, "hd_de")
  expect_equal(nrow(de$de_res), ncol(hd_object$data) - 1)
  expect_false(is.unsorted(de$de_res$adj.P.Val))

  de <- hd_plot_volcano(de)
  expect_renderable_ggplot(de$volcano_plot)

  # 5. Classification --------------------------------------------------------
  split <- hd_split_data(hd_object, variable = "Disease")
  model <- quietly(hd_model_rreg(
    split, variable = "Disease", case = "AML",
    grid_size = 3, cv_sets = 3, palette = "cancers12", verbose = FALSE
  ))
  expect_s3_class(model, "hd_model")
  expect_true(model$metrics$auc > 0.5)
  expect_gt(nrow(model$features), 0)

  # 6. The DE and model results line up for the summary heatmap --------------
  heatmap <- hd_plot_feature_heatmap(
    list(AML = de), list(AML = model), order_by = "AML"
  )
  expect_renderable_ggplot(heatmap)
})

test_that("a multi-class comparison flows from DE through to the summaries", {
  skip_on_cran()

  hd_object <- example_subset()
  diseases <- c("AML", "CLL", "MYEL")

  de_results <- lapply(diseases, function(case) {
    quietly(hd_de_limma(
      hd_object, variable = "Disease",
      case = case, control = setdiff(diseases, case)
    ))
  })
  names(de_results) <- diseases

  expect_true(all(vapply(de_results, inherits, logical(1), "hd_de")))

  summary_res <- quietly(hd_plot_de_summary(
    de_results, variable = "Disease", class_palette = "cancers12"
  ))

  expect_renderable_ggplot(summary_res$de_barplot)
  expect_true(all(summary_res$proteins_df_up[["up/down"]] == "up"))
  expect_true(all(summary_res$proteins_df_down[["up/down"]] == "down"))

  # every reported feature must come from the DE results
  all_features <- de_results$AML$de_res$Feature
  expect_true(all(summary_res$proteins_df_up$Feature %in% all_features))
})

test_that("several binary models can be summarised together", {
  skip_on_cran()

  hd_object <- example_subset()
  split <- hd_split_data(hd_object, variable = "Disease")

  model_results <- lapply(c("AML", "CLL"), function(case) {
    quietly(hd_model_rreg(
      split, variable = "Disease", case = case,
      grid_size = 2, cv_sets = 2, verbose = FALSE
    ))
  })
  names(model_results) <- c("AML", "CLL")

  summary_res <- quietly(hd_plot_model_summary(
    model_results, class_palette = "cancers12"
  ))

  expect_renderable_ggplot(summary_res$features_barplot)
  expect_renderable_ggplot(summary_res$metrics_barplot)
  expect_true(nrow(summary_res$features_df) > 0)
  expect_true(all(summary_res$features_df$Feature %in% colnames(hd_object$data)))
})

test_that("a multiclass model reports one AUC per disease", {
  skip_on_cran()

  hd_object <- example_subset(diseases = c("AML", "CLL", "MYEL"), n_assays = 30)
  split <- hd_split_data(hd_object, variable = "Disease")

  model <- quietly(hd_model_rreg(
    split, variable = "Disease", case = NULL,
    grid_size = 2, cv_sets = 2, verbose = FALSE
  ))

  expect_equal(model$model_type, "multi_class")
  diseases <- sort(unique(hd_object$metadata$Disease))
  expect_setequal(model$metrics$auc$Disease, c(diseases, "macro", "micro"))
  expect_true(all(model$metrics$auc$AUC >= 0 & model$metrics$auc$AUC <= 1))
})

test_that("results survive a save and import round trip", {
  skip_on_cran()
  withr::local_dir(withr::local_tempdir())

  hd_object <- example_subset()
  de <- quietly(hd_de_limma(hd_object, variable = "Disease", case = "AML"))

  hd_save_data(de$de_res, "results/de.csv")
  reloaded <- quietly(hd_import_data("results/de.csv"))

  expect_equal(nrow(reloaded), nrow(de$de_res))
  expect_equal(reloaded$Feature, de$de_res$Feature)
  expect_equal(reloaded$logFC, de$de_res$logFC, tolerance = 1e-6)
})

test_that("a clustering workflow keeps the samples it was given", {
  skip_on_cran()
  skip_if_not_installed("cluster")

  hd_object <- example_subset() |>
    hd_impute_knn(k = 5, verbose = FALSE)

  clustering <- quietly(hd_cluster_samples(hd_object, k = 4))

  expect_equal(nrow(clustering$cluster_res), nrow(hd_object$data))
  expect_setequal(clustering$cluster_res$DAid, hd_object$data$DAid)
  expect_setequal(sort(unique(clustering$cluster_res$Cluster)), 1:4)
})
