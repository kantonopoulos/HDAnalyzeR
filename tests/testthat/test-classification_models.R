# multiclass_auc --------------------------------------------------------------

test_that("multiclass_auc() returns one AUC per class plus macro and micro", {
  truth <- factor(c("a", "a", "b", "b", "c", "c"))
  probs <- tibble::tibble(
    .pred_a = c(0.8, 0.7, 0.1, 0.2, 0.1, 0.1),
    .pred_b = c(0.1, 0.2, 0.8, 0.7, 0.1, 0.2),
    .pred_c = c(0.1, 0.1, 0.1, 0.1, 0.8, 0.7)
  )
  res <- multiclass_auc(truth, probs)

  expect_named(res, c("a", "b", "c", "macro", "micro"))
  expect_true(all(res >= 0 & res <= 1))
})

test_that("multiclass_auc() gives 1 for a perfect separation", {
  truth <- factor(c("a", "a", "b", "b"))
  probs <- tibble::tibble(
    .pred_a = c(0.9, 0.8, 0.2, 0.1),
    .pred_b = c(0.1, 0.2, 0.8, 0.9)
  )
  res <- multiclass_auc(truth, probs)

  expect_equal(unname(res[["a"]]), 1)
  expect_equal(unname(res[["b"]]), 1)
  expect_equal(unname(res[["macro"]]), 1)
  expect_equal(unname(res[["micro"]]), 1)
})

test_that("multiclass_auc() gives 0.5 when the probabilities carry no signal", {
  truth <- factor(rep(c("a", "b"), each = 10))
  probs <- tibble::tibble(.pred_a = rep(0.5, 20), .pred_b = rep(0.5, 20))
  res <- multiclass_auc(truth, probs)

  expect_equal(unname(res[["a"]]), 0.5)
  expect_equal(unname(res[["macro"]]), 0.5)
})

test_that("multiclass_auc() matches a one-vs-rest AUC computed by hand", {
  withr::local_seed(4)
  truth <- factor(sample(c("a", "b", "c"), 60, replace = TRUE))
  probs <- tibble::tibble(
    .pred_a = stats::runif(60),
    .pred_b = stats::runif(60),
    .pred_c = stats::runif(60)
  )
  probs <- probs / rowSums(probs)

  res <- multiclass_auc(truth, probs)
  manual <- yardstick::roc_auc_vec(
    factor(ifelse(truth == "a", "event", "non_event"), levels = c("event", "non_event")),
    probs$.pred_a,
    event_level = "first"
  )
  expect_equal(unname(res[["a"]]), manual)
})

test_that("multiclass_auc() macro is the mean of the per-class AUCs", {
  withr::local_seed(8)
  truth <- factor(sample(c("a", "b", "c"), 60, replace = TRUE))
  probs <- tibble::tibble(
    .pred_a = stats::runif(60),
    .pred_b = stats::runif(60),
    .pred_c = stats::runif(60)
  )
  res <- multiclass_auc(truth, probs)

  expect_equal(unname(res[["macro"]]), mean(res[c("a", "b", "c")]))
})

test_that("multiclass_auc() rejects truth that does not match the columns", {
  expect_error(
    multiclass_auc(factor(c("x", "y")), tibble::tibble(.pred_a = c(1, 0), .pred_b = c(0, 1))),
    "do not match the predicted probability columns"
  )
})


# model_importance ------------------------------------------------------------

fit_engine <- function(engine, mode = "classification") {
  withr::local_seed(3)
  n <- 120
  dat <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n), x3 = stats::rnorm(n))
  linear <- 2 * dat$x1 - dat$x2

  if (mode == "classification") {
    dat$y <- factor(
      ifelse(stats::rbinom(n, 1, 1 / (1 + exp(-linear))) == 1, "yes", "no"),
      levels = c("no", "yes")
    )
    spec <- switch(
      engine,
      glm = parsnip::logistic_reg() |> parsnip::set_engine("glm"),
      ranger = parsnip::rand_forest(trees = 100) |>
        parsnip::set_mode("classification") |>
        parsnip::set_engine("ranger", importance = "permutation", seed = 1)
    )
  } else {
    dat$y <- linear + stats::rnorm(n)
    spec <- switch(
      engine,
      glm = parsnip::linear_reg() |> parsnip::set_engine("lm"),
      ranger = parsnip::rand_forest(trees = 100) |>
        parsnip::set_mode("regression") |>
        parsnip::set_engine("ranger", importance = "permutation", seed = 1)
    )
  }

  workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_recipe(recipes::recipe(y ~ ., data = dat)) |>
    parsnip::fit(dat)
}

test_that("model_importance() reports absolute statistics and signs for glm", {
  res <- model_importance(fit_engine("glm"))

  expect_named(res, c("Variable", "Importance", "Sign"))
  expect_setequal(res$Variable, c("x1", "x2", "x3"))
  expect_true(all(res$Importance >= 0))
  expect_false("(Intercept)" %in% res$Variable)

  signs <- stats::setNames(res$Sign, res$Variable)
  expect_equal(signs[["x1"]], "POS")
  expect_equal(signs[["x2"]], "NEG")
})

test_that("model_importance() ranks the informative predictors above the noise", {
  res <- model_importance(fit_engine("glm"))
  importance <- stats::setNames(res$Importance, res$Variable)

  expect_gt(importance[["x1"]], importance[["x3"]])
  expect_gt(importance[["x2"]], importance[["x3"]])
})

test_that("model_importance() reads ranger permutation importance", {
  res <- model_importance(fit_engine("ranger"))

  expect_named(res, c("Variable", "Importance"))
  expect_setequal(res$Variable, c("x1", "x2", "x3"))

  importance <- stats::setNames(res$Importance, res$Variable)
  expect_gt(importance[["x1"]], importance[["x3"]])
})

test_that("model_importance() works for regression too", {
  expect_named(model_importance(fit_engine("glm", "regression")),
               c("Variable", "Importance", "Sign"))
  expect_named(model_importance(fit_engine("ranger", "regression")),
               c("Variable", "Importance"))
})

test_that("model_importance() explains itself for an unsupported engine", {
  skip_if_not_installed("kknn")

  fit <- workflows::workflow() |>
    workflows::add_model(
      parsnip::nearest_neighbor(neighbors = 3) |>
        parsnip::set_mode("regression") |>
        parsnip::set_engine("kknn")
    ) |>
    workflows::add_recipe(
      recipes::recipe(y ~ ., data = data.frame(x = as.numeric(1:10), y = as.numeric(1:10)))
    ) |>
    parsnip::fit(data.frame(x = as.numeric(1:10), y = as.numeric(1:10)))

  expect_error(model_importance(fit), "Variable importance is not available")
})

test_that("model_importance() refuses a random forest fitted without importance", {
  withr::local_seed(3)
  dat <- data.frame(x1 = stats::rnorm(50), y = stats::rnorm(50))
  fit <- workflows::workflow() |>
    workflows::add_model(
      parsnip::rand_forest(trees = 20) |>
        parsnip::set_mode("regression") |>
        parsnip::set_engine("ranger", seed = 1)
    ) |>
    workflows::add_recipe(recipes::recipe(y ~ ., data = dat)) |>
    parsnip::fit(dat)

  expect_error(model_importance(fit), "without variable importance")
})


# hd_split_data ---------------------------------------------------------------

test_that("hd_split_data() splits into train and test without losing samples", {
  hd_obj <- signal_object()
  split <- hd_split_data(hd_obj, variable = "Disease", ratio = 0.75)

  expect_s3_class(split, "hd_model")
  expect_named(split, c("train_data", "test_data"))
  expect_equal(
    nrow(split$train_data) + nrow(split$test_data),
    nrow(hd_obj$data)
  )
  expect_length(intersect(split$train_data$DAid, split$test_data$DAid), 0)
})

test_that("hd_split_data() puts the outcome right after the sample ID", {
  split <- hd_split_data(signal_object(), variable = "Disease")
  expect_equal(colnames(split$train_data)[1:2], c("DAid", "Disease"))
})

test_that("hd_split_data() honours the requested ratio", {
  hd_obj <- signal_object(n_per_group = 50)
  split <- hd_split_data(hd_obj, variable = "Disease", ratio = 0.6)

  expect_equal(nrow(split$train_data) / nrow(hd_obj$data), 0.6, tolerance = 0.05)
})

test_that("hd_split_data() stratifies on the outcome", {
  hd_obj <- signal_object(n_per_group = 50)
  split <- hd_split_data(hd_obj, variable = "Disease", ratio = 0.75)

  train_share <- mean(split$train_data$Disease == "case")
  test_share <- mean(split$test_data$Disease == "case")
  expect_equal(train_share, test_share, tolerance = 0.1)
})

test_that("hd_split_data() can pull in extra metadata predictors", {
  split <- hd_split_data(
    signal_object(), variable = "Disease", metadata_cols = c("Age", "Sex")
  )
  expect_true(all(c("Age", "Sex") %in% colnames(split$train_data)))
})

test_that("hd_split_data() is reproducible for a given seed", {
  a <- hd_split_data(signal_object(), variable = "Disease", seed = 11)
  b <- hd_split_data(signal_object(), variable = "Disease", seed = 11)
  expect_equal(a$train_data$DAid, b$train_data$DAid)
})

test_that("hd_split_data() validates its inputs", {
  sd <- signal_data()
  expect_error(
    hd_split_data(sd$data, variable = "Disease"),
    "'metadata' argument or slot .* is empty"
  )
  expect_error(
    hd_split_data(sd$data, sd$metadata, variable = "nope"),
    "variable is not"
  )
})


# check_data ------------------------------------------------------------------

test_that("check_data() accepts a plain two-element list", {
  split <- hd_split_data(signal_object(), variable = "Disease")
  res <- check_data(list(split$train_data, split$test_data), variable = "Disease")

  expect_s3_class(res, "hd_model")
  expect_equal(nrow(res$train_data), nrow(split$train_data))
})

test_that("check_data() rejects data without the outcome column", {
  split <- hd_split_data(signal_object(), variable = "Disease")
  bad <- list(
    split$train_data |> dplyr::select(-"Disease"),
    split$test_data
  )
  expect_error(check_data(bad, variable = "Disease"), "not.*present in the train data")
})


# The model pipelines ---------------------------------------------------------

test_that("hd_model_rreg() produces metrics, features and plots for a binary task", {
  split <- hd_split_data(signal_object(n_per_group = 40), variable = "Disease")
  model <- quietly(hd_model_rreg(
    split, variable = "Disease", case = "case",
    grid_size = 3, cv_sets = 3, verbose = FALSE
  ))

  expect_s3_class(model, "hd_model")
  expect_equal(model$model_type, "binary_class")
  expect_true(all(c("accuracy", "sensitivity", "specificity", "auc") %in%
    names(model$metrics)))
  expect_true(model$metrics$auc >= 0 && model$metrics$auc <= 1)
  expect_renderable_ggplot(model$roc_curve)
  expect_renderable_ggplot(model$probability_plot)
})

test_that("hd_model_rreg() finds the informative features on a binary task", {
  split <- hd_split_data(signal_object(n_per_group = 40), variable = "Disease")
  model <- quietly(hd_model_rreg(
    split, variable = "Disease", case = "case",
    grid_size = 3, cv_sets = 3, verbose = FALSE
  ))

  expect_true(all(c("Feature", "Importance", "Sign", "Scaled_Importance") %in%
    colnames(model$features)))
  expect_true(all(model$features$Scaled_Importance <= 1))
  expect_equal(max(model$features$Scaled_Importance), 1)

  # the model should separate the groups it was given a real signal for
  expect_gt(model$metrics$auc, 0.9)
})

test_that("hd_model_rreg() handles a multiclass task", {
  sd <- signal_data(n_per_group = 30)
  sd$metadata$Disease <- rep(c("a", "b", "c"), length.out = nrow(sd$metadata))
  hd_obj <- hd_initialize(sd$data, sd$metadata, is_wide = TRUE)
  split <- hd_split_data(hd_obj, variable = "Disease")

  model <- quietly(hd_model_rreg(
    split, variable = "Disease", case = NULL,
    grid_size = 2, cv_sets = 2, verbose = FALSE
  ))

  expect_equal(model$model_type, "multi_class")
  expect_s3_class(model$metrics$auc, "tbl_df")
  expect_setequal(model$metrics$auc$Disease, c("a", "b", "c", "macro", "micro"))
  expect_true(all(model$metrics$auc$AUC >= 0 & model$metrics$auc$AUC <= 1))
  expect_true("Class" %in% colnames(model$features))
})

test_that("hd_model_rreg() handles a continuous outcome", {
  hd_obj <- signal_object(n_per_group = 40)
  split <- hd_split_data(hd_obj, variable = "Age")
  model <- quietly(hd_model_rreg(
    split, variable = "Age", case = NULL,
    grid_size = 2, cv_sets = 2, verbose = FALSE,
    plot_title = c("rmse", "rsq", "features")
  ))

  expect_equal(model$model_type, "regression")
  expect_named(model$metrics, c("rmse", "rsq"))
  expect_renderable_ggplot(model$comparison_plot)
})

test_that("hd_model_rreg() refuses a dataset with too few predictors", {
  sd <- signal_data()
  narrow <- sd$data |> dplyr::select("DAid", "up")
  hd_obj <- hd_initialize(narrow, sd$metadata, is_wide = TRUE)
  split <- hd_split_data(hd_obj, variable = "Disease")

  expect_error(
    quietly(hd_model_rreg(split, variable = "Disease", case = "case")),
    "number of predictors is less than 2"
  )
})

test_that("hd_model_rf() produces metrics and non-negative importances", {
  split <- hd_split_data(signal_object(n_per_group = 40), variable = "Disease")
  model <- quietly(hd_model_rf(
    split, variable = "Disease", case = "case",
    grid_size = 2, cv_sets = 2, verbose = FALSE
  ))

  expect_s3_class(model, "hd_model")
  expect_true(all(model$features$Importance >= 0))
  expect_true(all(model$features$Sign == "POS"))
  expect_gt(model$metrics$auc, 0.9)
})

test_that("hd_model_lr() fits a logistic regression and signs its features", {
  split <- hd_split_data(signal_object(n_per_group = 40), variable = "Disease")
  model <- quietly(hd_model_lr(
    split, variable = "Disease", case = "case", verbose = FALSE
  ))

  expect_s3_class(model, "hd_model")
  expect_true(all(model$features$Sign %in% c("POS", "NEG")))
  expect_gt(model$metrics$auc, 0.9)
})

test_that("hd_model_lr() refuses a multiclass task", {
  sd <- signal_data(n_per_group = 30)
  sd$metadata$Disease <- rep(c("a", "b", "c"), length.out = nrow(sd$metadata))
  hd_obj <- hd_initialize(sd$data, sd$metadata, is_wide = TRUE)
  split <- hd_split_data(hd_obj, variable = "Disease")

  expect_error(
    quietly(hd_model_lr(split, variable = "Disease", case = NULL)),
    "not supported for multiclass"
  )
})

test_that("the model pipelines are reproducible for a given seed", {
  split <- hd_split_data(signal_object(n_per_group = 40), variable = "Disease")
  run <- function() {
    quietly(hd_model_rreg(
      split, variable = "Disease", case = "case",
      grid_size = 2, cv_sets = 2, verbose = FALSE, seed = 77
    ))$metrics$auc
  }
  expect_equal(run(), run())
})


# hd_model_test ---------------------------------------------------------------

test_that("hd_model_test() evaluates a fitted model on new data", {
  sd <- signal_data(n_per_group = 40)
  # take a stratified holdout so both classes appear in the validation set
  train_rows <- c(seq_len(30), 40 + seq_len(30))

  hd_train <- hd_initialize(sd$data[train_rows, ], sd$metadata, is_wide = TRUE)
  hd_val <- hd_initialize(sd$data[-train_rows, ], sd$metadata, is_wide = TRUE)

  split <- hd_split_data(hd_train, variable = "Disease")
  model <- quietly(hd_model_rreg(
    split, variable = "Disease", case = "case",
    grid_size = 2, cv_sets = 2, verbose = FALSE
  ))

  validated <- quietly(hd_model_test(
    model, hd_train, hd_val, variable = "Disease", case = "case"
  ))

  expect_true(all(c("accuracy", "sensitivity", "specificity", "auc") %in%
    names(validated$test_metrics)))
  expect_renderable_ggplot(validated$test_roc_curve)
  expect_renderable_ggplot(validated$test_probability_plot)
  # the original results must survive untouched
  expect_equal(validated$metrics$auc, model$metrics$auc)
})

test_that("hd_model_test() rejects anything that is not a model object", {
  expect_error(
    hd_model_test(list(), NULL, NULL, case = "case"),
    "should be an `hd_model` object"
  )
})


# hd_plot_model_summary -------------------------------------------------------

test_that("hd_plot_model_summary() summarises several models", {
  split <- hd_split_data(signal_object(n_per_group = 40), variable = "Disease")
  fit_one <- function(case) {
    quietly(hd_model_rreg(
      split, variable = "Disease", case = case,
      grid_size = 2, cv_sets = 2, verbose = FALSE
    ))
  }
  results <- list(case = fit_one("case"), ctrl = fit_one("ctrl"))

  summary_res <- quietly(hd_plot_model_summary(results))

  expect_named(
    summary_res,
    c("features_barplot", "metrics_barplot", "upset_plot_features",
      "features_df", "features_list")
  )
  expect_renderable_ggplot(summary_res$features_barplot)
  expect_renderable_ggplot(summary_res$metrics_barplot)
  expect_true(all(c("Shared_in", "Feature") %in% colnames(summary_res$features_df)))
})

test_that("hd_plot_model_summary() skips the UpSet plot for a single model", {
  split <- hd_split_data(signal_object(n_per_group = 40), variable = "Disease")
  model <- quietly(hd_model_rreg(
    split, variable = "Disease", case = "case",
    grid_size = 2, cv_sets = 2, verbose = FALSE
  ))

  summary_res <- quietly(hd_plot_model_summary(list(case = model)))
  expect_null(summary_res$upset_plot_features)
  expect_renderable_ggplot(summary_res$features_barplot)
})
