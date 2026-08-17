de_object <- function(...) {
  sd <- signal_data(...)
  quietly(hd_de_limma(
    sd$data, sd$metadata, variable = "Disease", case = "case"
  ))
}

# hd_de_limma -----------------------------------------------------------------

test_that("hd_de_limma() returns one row per feature with the expected columns", {
  res <- de_object()

  expect_s3_class(res, "hd_de")
  expect_setequal(res$de_res$Feature, c("up", "down", "flat", "flat2", "flat3"))
  expect_true(all(c("logFC", "P.Value", "adj.P.Val", "Disease") %in% colnames(res$de_res)))
  expect_true(all(res$de_res$Disease == "case"))
})

test_that("hd_de_limma() recovers the direction and significance of the signal", {
  res <- de_object()
  fc <- stats::setNames(res$de_res$logFC, res$de_res$Feature)
  padj <- stats::setNames(res$de_res$adj.P.Val, res$de_res$Feature)

  expect_gt(fc[["up"]], 2)
  expect_lt(fc[["down"]], -2)
  expect_lt(abs(fc[["flat"]]), 1)

  expect_lt(padj[["up"]], 0.001)
  expect_lt(padj[["down"]], 0.001)
  expect_gt(padj[["flat"]], 0.05)
})

test_that("hd_de_limma() returns numeric statistics, not characters", {
  res <- de_object()
  for (column in c("logFC", "P.Value", "adj.P.Val", "t")) {
    expect_type(res$de_res[[column]], "double")
  }
})

test_that("hd_de_limma() sorts by adjusted p-value", {
  res <- de_object()
  expect_false(is.unsorted(res$de_res$adj.P.Val))
})

test_that("hd_de_limma() restricts the comparison to the chosen control", {
  sd <- signal_data()
  sd$metadata$Disease[sd$metadata$Disease == "ctrl"][1:10] <- "other"

  res <- quietly(hd_de_limma(
    sd$data, sd$metadata, variable = "Disease", case = "case", control = "other"
  ))
  expect_s3_class(res, "hd_de")
  expect_equal(nrow(res$de_res), 5)
})

test_that("hd_de_limma() can correct for a categorical covariate", {
  sd <- signal_data()
  res <- quietly(hd_de_limma(
    sd$data, sd$metadata, variable = "Disease", case = "case", correct = "Sex"
  ))

  fc <- stats::setNames(res$de_res$logFC, res$de_res$Feature)
  expect_gt(fc[["up"]], 2)
})

test_that("hd_de_limma() can correct for a covariate with more than two levels", {
  sd <- signal_data()
  sd$metadata$Cohort <- rep(c("c1", "c2", "c3"), length.out = nrow(sd$metadata))

  res <- quietly(hd_de_limma(
    sd$data, sd$metadata, variable = "Disease", case = "case", correct = "Cohort"
  ))

  expect_s3_class(res, "hd_de")
  expect_equal(nrow(res$de_res), 5)
  expect_gt(stats::setNames(res$de_res$logFC, res$de_res$Feature)[["up"]], 2)
})

test_that("hd_de_limma() can correct for several covariates at once", {
  sd <- signal_data()
  res <- quietly(hd_de_limma(
    sd$data, sd$metadata,
    variable = "Disease", case = "case", correct = c("Sex", "Age")
  ))

  expect_equal(nrow(res$de_res), 5)
  expect_gt(stats::setNames(res$de_res$logFC, res$de_res$Feature)[["up"]], 2)
})

test_that("hd_de_limma() supports a continuous variable of interest", {
  sd <- signal_data()
  # Make `up` track Age so there is a signal to find
  sd$data$up <- sd$metadata$Age / 10 + stats::rnorm(nrow(sd$data), sd = 0.1)

  res <- quietly(hd_de_limma(
    sd$data, sd$metadata, variable = "Age", case = NULL
  ))

  expect_true("logFC" %in% colnames(res$de_res))
  expect_equal(res$de_res$Feature[1], "up")
  expect_lt(res$de_res$adj.P.Val[1], 0.001)
})

test_that("hd_de_limma() validates its inputs", {
  sd <- signal_data()

  expect_error(
    hd_de_limma(sd$data, variable = "Disease", case = "case"),
    "'metadata' argument or slot .* is empty"
  )
  expect_error(
    hd_de_limma(sd$data, sd$metadata, variable = "nope", case = "case"),
    "variable is not"
  )

  hd_obj <- hd_initialize(sd$data, sd$metadata, is_wide = TRUE)
  hd_obj$data <- NULL
  expect_error(hd_de_limma(hd_obj, case = "case"), "'data' slot .* is empty")
})

test_that("hd_de_limma() works from an HDAnalyzeR object", {
  hd_obj <- signal_object()
  res <- quietly(hd_de_limma(hd_obj, variable = "Disease", case = "case"))
  expect_s3_class(res, "hd_de")
})


# hd_de_ttest -----------------------------------------------------------------

test_that("hd_de_ttest() returns numeric statistics, not characters", {
  sd <- signal_data()
  res <- quietly(hd_de_ttest(sd$data, sd$metadata, variable = "Disease", case = "case"))

  for (column in c("logFC", "CI.L", "CI.R", "t", "P.Value", "adj.P.Val")) {
    expect_type(res$de_res[[column]], "double")
  }
})

test_that("hd_de_ttest() agrees with stats::t.test", {
  sd <- signal_data()
  res <- quietly(hd_de_ttest(sd$data, sd$metadata, variable = "Disease", case = "case"))

  case_values <- sd$data$up[sd$metadata$Disease == "case"]
  ctrl_values <- sd$data$up[sd$metadata$Disease == "ctrl"]
  reference <- stats::t.test(case_values, ctrl_values)

  row <- res$de_res[res$de_res$Feature == "up", ]
  expect_equal(row$P.Value, reference$p.value)
  expect_equal(row$logFC, mean(case_values) - mean(ctrl_values))
  expect_equal(row$t, unname(round(reference$statistic, 2)))
})

test_that("hd_de_ttest() recovers the direction of the signal", {
  sd <- signal_data()
  res <- quietly(hd_de_ttest(sd$data, sd$metadata, variable = "Disease", case = "case"))
  fc <- stats::setNames(res$de_res$logFC, res$de_res$Feature)

  expect_gt(fc[["up"]], 2)
  expect_lt(fc[["down"]], -2)
})

test_that("hd_de_ttest() applies FDR correction", {
  sd <- signal_data()
  res <- quietly(hd_de_ttest(sd$data, sd$metadata, variable = "Disease", case = "case"))

  expect_equal(
    res$de_res$adj.P.Val,
    stats::p.adjust(res$de_res$P.Value, method = "fdr")[order(order(res$de_res$adj.P.Val))],
    tolerance = 1e-12
  )
  expect_true(all(res$de_res$adj.P.Val >= res$de_res$P.Value))
})


# hd_plot_volcano -------------------------------------------------------------

test_that("hd_plot_volcano() attaches a renderable plot to the DE object", {
  res <- hd_plot_volcano(de_object())

  expect_s3_class(res, "hd_de")
  expect_renderable_ggplot(res$volcano_plot)
})

test_that("hd_plot_volcano() rejects objects that are not DE results", {
  expect_error(hd_plot_volcano(list()), "not a differential expression object")

  bad <- structure(list(de_res = tibble::tibble(x = 1)), class = "hd_de")
  expect_error(hd_plot_volcano(bad), "does not contain the differential expression results")
})

test_that("hd_plot_volcano() reports the number of significant features", {
  p <- hd_plot_volcano(de_object())$volcano_plot
  expect_match(p$labels$title %||% "", "Num significant up = 1")
})


# hd_plot_de_summary ----------------------------------------------------------

test_that("hd_plot_de_summary() summarises several analyses", {
  sd <- signal_data()
  res_a <- quietly(hd_de_limma(sd$data, sd$metadata, variable = "Disease", case = "case"))
  res_b <- quietly(hd_de_limma(sd$data, sd$metadata, variable = "Disease", case = "ctrl"))

  summary_res <- quietly(hd_plot_de_summary(
    list(case = res_a, ctrl = res_b),
    variable = "Disease"
  ))

  expect_named(
    summary_res,
    c("de_barplot", "upset_plot_up", "upset_plot_down",
      "proteins_df_up", "proteins_df_down",
      "proteins_list_up", "proteins_list_down")
  )
  expect_renderable_ggplot(summary_res$de_barplot)
})

test_that("hd_plot_de_summary() labels up and down features correctly", {
  sd <- signal_data()
  res_a <- quietly(hd_de_limma(sd$data, sd$metadata, variable = "Disease", case = "case"))
  res_b <- quietly(hd_de_limma(sd$data, sd$metadata, variable = "Disease", case = "ctrl"))

  summary_res <- quietly(hd_plot_de_summary(
    list(case = res_a, ctrl = res_b),
    variable = "Disease"
  ))

  expect_true(all(summary_res$proteins_df_up[["up/down"]] == "up"))
  expect_true(all(summary_res$proteins_df_down[["up/down"]] == "down"))
})

test_that("hd_plot_de_summary() puts each feature on the correct side", {
  sd <- signal_data()
  res_a <- quietly(hd_de_limma(sd$data, sd$metadata, variable = "Disease", case = "case"))

  summary_res <- quietly(hd_plot_de_summary(list(case = res_a), variable = "Disease"))

  expect_true("up" %in% summary_res$proteins_df_up$Feature)
  expect_true("down" %in% summary_res$proteins_df_down$Feature)
  expect_false("flat" %in% summary_res$proteins_df_up$Feature)
})
