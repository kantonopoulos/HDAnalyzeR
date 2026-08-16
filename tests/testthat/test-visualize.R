# hd_plot_feature_boxplot -----------------------------------------------------

test_that("hd_plot_feature_boxplot() draws one panel per requested feature", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  p <- hd_plot_feature_boxplot(
    hd_obj, variable = "Disease", features = c("f1", "f3")
  )

  expect_renderable_ggplot(p)
  panels <- unique(ggplot2::ggplot_build(p)$layout$layout$Features)
  expect_setequal(as.character(panels), c("f1", "f3"))
})

test_that("hd_plot_feature_boxplot() collapses controls when asked", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  p <- hd_plot_feature_boxplot(
    hd_obj, variable = "Disease", features = "f1",
    case = "A", type = "case_vs_control"
  )

  expect_renderable_ggplot(p)
  groups <- levels(ggplot2::ggplot_build(p)$plot$data$Disease)
  expect_setequal(groups, c("A", "Control"))
})

test_that("hd_plot_feature_boxplot() needs a case for case_vs_control", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  expect_error(
    hd_plot_feature_boxplot(
      hd_obj, variable = "Disease", features = "f1", type = "case_vs_control"
    ),
    "Please provide the case class"
  )
})

test_that("hd_plot_feature_boxplot() skips unknown features but keeps the rest", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  expect_warning(
    p <- hd_plot_feature_boxplot(
      hd_obj, variable = "Disease", features = c("f1", "nope")
    ),
    "not present in the data"
  )
  expect_renderable_ggplot(p)
})

test_that("hd_plot_feature_boxplot() errors when no feature exists", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  expect_error(
    suppressWarnings(hd_plot_feature_boxplot(
      hd_obj, variable = "Disease", features = "nope"
    )),
    "None of the features are present"
  )
})

test_that("hd_plot_feature_boxplot() accepts a named palette", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  p <- hd_plot_feature_boxplot(
    hd_obj, variable = "Disease", features = "f1",
    palette = c(A = "red", B = "blue")
  )
  expect_renderable_ggplot(p)
})

test_that("hd_plot_feature_boxplot() needs metadata", {
  expect_error(
    hd_plot_feature_boxplot(tiny_wide(), variable = "Disease", features = "f1"),
    "'metadata' argument or slot .* is empty"
  )
})

test_that("hd_plot_feature_boxplot() can add or drop the points and labels", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)

  with_points <- hd_plot_feature_boxplot(
    hd_obj, variable = "Disease", features = "f1", points = TRUE
  )
  without_points <- hd_plot_feature_boxplot(
    hd_obj, variable = "Disease", features = "f1", points = FALSE
  )
  expect_gt(length(with_points$layers), length(without_points$layers))

  expect_renderable_ggplot(hd_plot_feature_boxplot(
    hd_obj, variable = "Disease", features = "f1", x_labels = FALSE
  ))
})


# hd_plot_regression ----------------------------------------------------------

# f1 and f3 in `tiny_wide()` are perfectly collinear, which makes `lm()` warn
# about a perfect fit, so the regression tests use their own noisy data.
regression_object <- function() {
  withr::local_seed(2)
  n <- 20
  dat <- tibble::tibble(
    DAid = sprintf("S%02d", seq_len(n)),
    f1 = stats::rnorm(n),
    f3 = stats::rnorm(n)
  )
  dat$f3 <- dat$f1 * 0.6 + dat$f3
  meta <- tibble::tibble(DAid = dat$DAid, Age = seq(20, 80, length.out = n))
  hd_initialize(dat, meta, is_wide = TRUE)
}

test_that("hd_plot_regression() draws a scatter plot with a fitted line", {
  p <- hd_plot_regression(regression_object(), x = "f1", y = "f3")

  expect_renderable_ggplot(p)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomSmooth" %in% geoms)
})

test_that("hd_plot_regression() can plot against a metadata variable", {
  p <- hd_plot_regression(regression_object(), metadata_cols = "Age", x = "f1", y = "Age")

  expect_renderable_ggplot(p)
  expect_equal(rlang::as_name(p$mapping$y), "Age")
})

test_that("hd_plot_regression() can drop the R-squared annotation", {
  hd_obj <- regression_object()

  with_r2 <- hd_plot_regression(hd_obj, x = "f1", y = "f3", r_2 = TRUE)
  without_r2 <- hd_plot_regression(hd_obj, x = "f1", y = "f3", r_2 = FALSE)
  expect_gt(length(with_r2$layers), length(without_r2$layers))
})

test_that("hd_plot_regression() needs metadata", {
  expect_error(
    hd_plot_regression(tiny_wide(), x = "f1", y = "f3"),
    "'metadata' argument or slot .* is empty"
  )
})


# hd_plot_feature_heatmap -----------------------------------------------------

test_that("hd_plot_feature_heatmap() combines DE and model results", {
  sd <- signal_data(n_per_group = 40)
  hd_obj <- hd_initialize(sd$data, sd$metadata, is_wide = TRUE)

  de <- quietly(hd_de_limma(hd_obj, variable = "Disease", case = "case"))
  split <- hd_split_data(hd_obj, variable = "Disease")
  model <- quietly(hd_model_rreg(
    split, variable = "Disease", case = "case",
    grid_size = 2, cv_sets = 2, verbose = FALSE
  ))

  p <- hd_plot_feature_heatmap(
    list(ctrl = de), list(ctrl = model), order_by = "ctrl"
  )

  expect_renderable_ggplot(p)
  expect_equal(p$labels$x, "Feature")
  expect_equal(p$labels$y, "Control Group")
})


# hd_plot_feature_network -----------------------------------------------------

test_that("hd_plot_feature_network() draws a network of features and classes", {
  panel <- tibble::tibble(
    Feature = c("f1", "f2", "f3", "f1"),
    Class = c("A", "A", "B", "B"),
    Scaled_Importance = c(1, 0.5, 0.8, 0.3)
  )
  p <- hd_plot_feature_network(panel)

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("hd_plot_feature_network() honours the colour arguments", {
  panel <- tibble::tibble(
    Feature = c("f1", "f2", "f3"),
    Class = c("A", "A", "B"),
    logFC = c(2, -1, 3)
  )
  p <- hd_plot_feature_network(
    panel,
    plot_color = "logFC",
    class_palette = c(A = "red", B = "blue"),
    importance_palette = c(high = "grey20", low = "grey90")
  )

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("hd_plot_feature_network() is reproducible for a given seed", {
  panel <- tibble::tibble(
    Feature = c("f1", "f2", "f3"),
    Class = c("A", "A", "B"),
    Scaled_Importance = c(1, 0.5, 0.8)
  )
  a <- ggplot2::ggplot_build(hd_plot_feature_network(panel, seed = 3))$data[[1]]
  b <- ggplot2::ggplot_build(hd_plot_feature_network(panel, seed = 3))$data[[1]]
  expect_equal(a, b)
})
