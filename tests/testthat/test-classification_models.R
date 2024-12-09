# Test hd_split_data -----------------------------------------------------------
test_that("hd_split_data splits data correctly", {
  data_example <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"),
    Protein1 = c(1.5, 2.3, 3.1, 4.1, 2.5, 2.9, 4.2, 3.7),
    Protein2 = c(0.8, 2.5, 3.1, 4.0, 4.2, 3.9, 4.1, 2.9)
  )

  metadata_example <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"),
    Disease = c(1, 0, 1, 0, 1, 0, 1, 0)
  )

  # Initialize HDAnalyzeR object
  hd_object <- hd_initialize(data_example, metadata_example, sample_id = "sample_id", is_wide = TRUE)

  # Call hd_split_data with ratio = 0.75 and seed = 123
  split_object <- hd_split_data(hd_object, variable = "Disease", ratio = 0.75, seed = 123)

  # Check if the train and test data exist and are correctly split
  expect_true("train_data" %in% names(split_object))
  expect_true("test_data" %in% names(split_object))

  # Check if the ratio of training data to test data is approximately 0.75
  train_size <- nrow(split_object$train_data)
  test_size <- nrow(split_object$test_data)
  total_size <- train_size + test_size
  expect_equal(train_size / total_size, 0.75, tolerance = 0.05)

  # Ensure that the stratification works (check Disease variable proportions)
  train_disease_proportion <- mean(split_object$train_data$Disease == 1)
  test_disease_proportion <- mean(split_object$test_data$Disease == 1)

  expect_equal(train_disease_proportion, test_disease_proportion, tolerance = 0.05)
})


# Test balance_groups ----------------------------------------------------------
test_that("balance_groups balances groups correctly", {
  # Create a dataset with an imbalanced Disease variable
  data_example_imbalanced <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4", "S5", "S6"),
    Protein1 = c(1.5, 2.3, 3.1, 4.1, 2.5, 2.9),
    Disease = c(1, 1, 1, 0, 0, 0)
  )

  # Call balance_groups to downsample the control group
  balanced_data <- balance_groups(data_example_imbalanced, variable = "Disease", case = 1, seed = 123)

  # Check that the number of cases (Disease == 1) and controls (Disease == 0) are equal
  case_count <- sum(balanced_data$Disease == 1)
  control_count <- sum(balanced_data$Disease == 0)

  expect_equal(case_count, control_count)

  # Check if the number of rows in the balanced data matches the original case count times 2
  expect_equal(nrow(balanced_data), case_count * 2)
})


# Test check_data --------------------------------------------------------------
test_that("check_data checks train and test data structure", {
  data_example <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"),
    Protein1 = c(1.5, 2.3, 3.1, 4.1, 2.5, 2.9, 4.2, 3.7),
    Protein2 = c(0.8, 2.5, 3.1, 4.0, 4.2, 3.9, 4.1, 2.9),
    Disease = c(1, 0, 1, 0, 1, 0, 1, 0)
  )

  # Create a model object
  model_example <- list(
    train_data = data_example[1:6, ],
    test_data = data_example[7:8, ]
  )
  class(model_example) <- "hd_model"

  # Test with correct data structure
  result <- check_data(model_example, variable = "Disease")
  expect_true("train_data" %in% names(result))
  expect_true("test_data" %in% names(result))

  # Test with missing train_data
  model_missing_train <- model_example
  model_missing_train$train_data <- NULL
  expect_error(check_data(model_missing_train, variable = "Disease"),
               "The 'train_data' slot of the model object is empty")

  # Test with missing test_data
  model_missing_test <- model_example
  model_missing_test$test_data <- NULL
  expect_error(check_data(model_missing_test, variable = "Disease"),
               "The 'test_data' slot of the model object is empty")

  # Test with variable missing in train_data
  model_invalid_variable <- model_example
  model_invalid_variable$train_data$InvalidVar <- NULL
  expect_error(check_data(model_invalid_variable, variable = "InvalidVar"),
               "The variable is not be present in the train data")
})


# Test prepare_data ------------------------------------------------------------
test_that("prepare_data prepares data correctly", {
  data_example <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"),
    Protein1 = c(1.5, 2.3, 3.1, 4.1, 2.5, 2.9, 4.2, 3.7),
    Protein2 = c(0.8, 2.5, 3.1, 4.0, 4.2, 3.9, 4.1, 2.9),
    Disease = c(1, 0, 1, 0, 1, 0, 1, 0)
  )

  # Create a model object with data for preparation
  model_example <- list(
    train_data = data_example[1:6, ],
    test_data = data_example[7:8, ]
  )
  class(model_example) <- "hd_model"

  # Test prepare_data with default parameters
  result <- prepare_data(model_example, variable = "Disease", case = 1, cv_sets = 2, seed = 123)

  # Check if the output has the necessary components
  expect_true("train_data" %in% names(result))
  expect_true("test_data" %in% names(result))
  expect_true("train_folds" %in% names(result))

  # Check if the variable is converted to factor in both train and test sets
  expect_true(is.factor(result$train_data$Disease))
  expect_true(is.factor(result$test_data$Disease))

  # Ensure NAs are filtered out (in this case, none exist)
  expect_equal(nrow(result$train_data), 6)  # Check if rows are as expected
  expect_equal(nrow(result$test_data), 2)

  # Ensure cross-validation sets are created (check the number of folds)
  expect_equal(length(result$train_folds$splits), 2)  # Number of folds = 2

  # Test balance_groups functionality (check if the group is balanced)
  balanced_model <- prepare_data(model_example, variable = "Disease", case = 1, balance_groups = TRUE, cv_sets = 2, seed = 123)
  train_data <- balanced_model$train_data
  case_count <- sum(train_data$Disease == 1)
  control_count <- sum(train_data$Disease == 0)

  # After balancing, the number of cases and controls should be the same
  expect_equal(case_count, control_count)

  # Check the number of rows after balancing (should be twice the case count)
  expect_equal(nrow(train_data), case_count * 2)
})


# Test tune_rreg_model ---------------------------------------------------------
test_that("tune_rreg_model works correctly with binary classification", {
  # Create mock data
  hd_object <- hd_initialize(example_data, example_metadata)
  hd_object[["data"]] <- hd_object[["data"]] |> head(100)
  split_object <- hd_split_data(hd_object, variable = "Disease", ratio = 0.75, seed = 123)

  mock <- list(
    train_data = split_object$train_data |> dplyr::mutate(Disease = dplyr::if_else(Disease == "AML", "1", "0")),
    test_data = split_object$test_data |> dplyr::mutate(Disease = dplyr::if_else(Disease == "AML", "1", "0")),
    model_type = "binary_class",
    train_folds = rsample::vfold_cv(split_object$train_data |> dplyr::mutate(Disease = dplyr::if_else(Disease == "AML", "1", "0")), v = 2)
  )
  # Call the function
  tuned_model <- tune_rreg_model(mock, variable = "Disease", grid_size = 2, verbose = FALSE)

  # Test if the returned object has the correct structure
  expect_true("tune" %in% names(tuned_model))
  expect_true("wf" %in% names(tuned_model))
  expect_false("train_folds" %in% names(tuned_model))
  expect_equal(length(tuned_model$tune$.metrics), 2)  # Assuming 2 metric combinations for 2 folds and 2 grid points
})


# Test tune_rf_model -----------------------------------------------------------
test_that("tune_rf_model works correctly with binary classification", {
  # Create mock data
  hd_object <- hd_initialize(example_data, example_metadata)
  hd_object[["data"]] <- hd_object[["data"]] |> head(100)
  split_object <- hd_split_data(hd_object, variable = "Disease", ratio = 0.75, seed = 123)

  mock <- list(
    train_data = split_object$train_data |> dplyr::mutate(Disease = dplyr::if_else(Disease == "AML", "1", "0")),
    test_data = split_object$test_data |> dplyr::mutate(Disease = dplyr::if_else(Disease == "AML", "1", "0")),
    model_type = "binary_class",
    train_folds = rsample::vfold_cv(split_object$train_data |> dplyr::mutate(Disease = dplyr::if_else(Disease == "AML", "1", "0")), v = 2)
  )
  # Call the function
  tuned_model <- tune_rf_model(mock, variable = "Disease", grid_size = 2, verbose = FALSE)

  # Test if the returned object has the correct structure
  expect_true("tune" %in% names(tuned_model))
  expect_true("wf" %in% names(tuned_model))
  expect_false("train_folds" %in% names(tuned_model))
  expect_equal(length(tuned_model$tune$.metrics), 2)  # Assuming 2 metric combinations for 2 folds and 2 grid points
})

# Test tune_lr_model -----------------------------------------------------------
test_that("tune_lr_model works correctly with binary classification", {
  # Create mock data
  hd_object <- hd_initialize(example_data, example_metadata)
  hd_object[["data"]] <- hd_object[["data"]] |> head(100)
  split_object <- hd_split_data(hd_object, variable = "Disease", ratio = 0.75, seed = 123)

  mock <- list(
    train_data = split_object$train_data |> dplyr::mutate(Disease = dplyr::if_else(Disease == "AML", "1", "0")),
    test_data = split_object$test_data |> dplyr::mutate(Disease = dplyr::if_else(Disease == "AML", "1", "0")),
    model_type = "binary_class")
  # Call the function
  tuned_model <- tune_lr_model(mock, variable = "Disease", verbose = FALSE)

  # Test if the returned object has the correct structure
  expect_true("wf" %in% names(tuned_model))
})


# Test hd_model_rreg -----------------------------------------------------------
# Test case: handling small datasets (less than 2 predictors)
test_that("hd_model_rreg stops with error when number of predictors is less than 2", {

  # Prepare mock data with less than 2 predictors
  small_data <- list(train_data = data.frame(Disease = factor(c(1, 0, 1, 0, 1))),
                     test_data = data.frame(Disease = factor(c(0, 1, 0, 1))))

  # Test if the function throws an error
  expect_error(hd_model_rreg(small_data, variable = "Disease", case = "1"))
})

# Test case: check model evaluation and tuning works
test_that("hd_model_rreg performs model evaluation and tuning correctly", {

  hd_object <- hd_initialize(example_data, example_metadata)
  hd_object[["data"]] <- hd_object[["data"]] |> head(100)
  dat <- hd_split_data(hd_object, variable = "Disease")

  # Run the regularized regression model pipeline with model tuning and evaluation
  result <- hd_model_rreg(dat, variable = "Disease", case = "AML", grid_size = 2, cv_sets = 2, verbose = FALSE)

  # Check if the model is evaluated and metrics are calculated
  expect_true(is.list(result$metrics))
  expect_true(!is.null(result$metrics$accuracy))
  expect_true(!is.null(result$metrics$sensitivity))
  expect_true(!is.null(result$metrics$specificity))
  expect_true(!is.null(result$metrics$auc))

  # Check if the variable importance plot is created
  expect_true(inherits(result$var_imp_plot, "gg") || inherits(result$var_imp_plot, "ggplot"))
})

# Test case: check that multiclass models run without error
test_that("hd_model_rreg works with multiclass classification", {

  hd_object <- hd_initialize(example_data, example_metadata)
  hd_object[["data"]] <- hd_object[["data"]] |> head(140)
  multiclass_data <- hd_split_data(hd_object, variable = "Disease")

  # Run the regularized regression model pipeline for multiclass
  result <- hd_model_rreg(multiclass_data, variable = "Disease", case = NULL, grid_size = 2, cv_sets = 2, verbose = FALSE)

  # Check that the multiclass model evaluation was successful
  expect_true("metrics" %in% names(result))
  expect_true("roc_curve" %in% names(result))
  expect_true("var_imp_plot" %in% names(result))
  expect_true("features" %in% names(result))
})


# Test hd_model_rf -------------------------------------------------------------
test_that("hd_model_rf performs model evaluation and tuning correctly", {

  hd_object <- hd_initialize(example_data, example_metadata)
  hd_object[["data"]] <- hd_object[["data"]] |> head(100)
  dat <- hd_split_data(hd_object, variable = "Disease")

  # Run the regularized regression model pipeline with model tuning and evaluation
  result <- hd_model_rf(dat, variable = "Disease", case = "AML", grid_size = 2, cv_sets = 2, verbose = FALSE)

  # Check if the model is evaluated and metrics are calculated
  expect_true(is.list(result$metrics))
  expect_true(!is.null(result$metrics$accuracy))
  expect_true(!is.null(result$metrics$sensitivity))
  expect_true(!is.null(result$metrics$specificity))
  expect_true(!is.null(result$metrics$auc))

  # Check if the variable importance plot is created
  expect_true(inherits(result$var_imp_plot, "gg") || inherits(result$var_imp_plot, "ggplot"))
})

# Test case: check that multiclass models run without error
test_that("hd_model_rf works with multiclass classification", {

  hd_object <- hd_initialize(example_data, example_metadata)
  hd_object[["data"]] <- hd_object[["data"]] |> head(140)
  multiclass_data <- hd_split_data(hd_object, variable = "Disease")

  # Run the regularized regression model pipeline for multiclass
  result <- hd_model_rf(multiclass_data, variable = "Disease", case = NULL, grid_size = 2, cv_sets = 2, verbose = FALSE)

  # Check that the multiclass model evaluation was successful
  expect_true("metrics" %in% names(result))
  expect_true("roc_curve" %in% names(result))
  expect_true("var_imp_plot" %in% names(result))
  expect_true("features" %in% names(result))
})


# Test hd_model_lr -------------------------------------------------------------
test_that("hd_model_lr performs model evaluation and tuning correctly", {

  hd_object <- hd_initialize(example_data, example_metadata)
  hd_object[["data"]] <- hd_object[["data"]][1:10] |> head(100)
  dat <- hd_split_data(hd_object, variable = "Disease")

  # Run the regularized regression model pipeline with model tuning and evaluation
  result <- hd_model_lr(dat, variable = "Disease", case = "AML", verbose = FALSE)

  # Check if the model is evaluated and metrics are calculated
  expect_true(is.list(result$metrics))
  expect_true(!is.null(result$metrics$accuracy))
  expect_true(!is.null(result$metrics$sensitivity))
  expect_true(!is.null(result$metrics$specificity))
  expect_true(!is.null(result$metrics$auc))

  # Check if the variable importance plot is created
  expect_true(inherits(result$var_imp_plot, "gg") || inherits(result$var_imp_plot, "ggplot"))
})


# Test hd_plot_model_summary ---------------------------------------------------
test_that("hd_plot_model_summary creates expected plots and handles inputs correctly", {

  # Create mock model results (simulating results from hd_model_rreg or similar models)
  model_results <- list(
    AML = list(
      features = data.frame(
        Feature = c("protein1", "protein2", "protein3", "protein4"),
        Scaled_Importance = c(0.6, 0.7, 0.4, 0.8)
      ),
      metrics = list(
        accuracy = 0.9,
        sensitivity = 0.85,
        specificity = 0.80,
        auc = 0.95
      )
    ),
    CLL = list(
      features = data.frame(
        Feature = c("protein5", "protein6", "protein7", "protein8"),
        Scaled_Importance = c(0.3, 0.5, 0.7, 0.9)
      ),
      metrics = list(
        accuracy = 0.88,
        sensitivity = 0.75,
        specificity = 0.82,
        auc = 0.92
      )
    )
  )

  # Test when upset_top_features is FALSE (all features)
  result <- hd_plot_model_summary(model_results, importance = 0.5, upset_top_features = FALSE)

  # Check the structure of the returned result
  expect_named(result, c("features_barplot", "metrics_barplot", "upset_plot_features", "features_df", "features_list"))

  # Check if the features_barplot is a ggplot object
  expect_s3_class(result$features_barplot, "gg")

  # Check if the metrics_barplot is a ggplot object
  expect_s3_class(result$metrics_barplot, "gg")

  # Check if upset_plot_features is a plot (it should be of class UpSet)
  expect_s3_class(result$upset_plot_features, "upset")

  # Check if the features_df is a data frame
  expect_s3_class(result$features_df, "data.frame")

  # Check if the features_list is a list
  expect_type(result$features_list, "list")

  # Test when upset_top_features is TRUE (top features only)
  result_top_features <- hd_plot_model_summary(model_results, importance = 0.6, upset_top_features = TRUE)

  # Check the features bar plot again to see that the top features are correctly filtered
  expect_named(result_top_features, c("features_barplot", "metrics_barplot", "upset_plot_features", "features_df", "features_list"))

  # Check if the filtered features for the upset plot are correctly handled
  filtered_features <- result_top_features$features_list
  expect_true(all(filtered_features$AML == c("protein1", "protein2", "protein4")))
  expect_true(all(filtered_features$CLL == c("protein7", "protein8")))

  # Check that the function handles different class_palette inputs
  result_with_palette <- hd_plot_model_summary(model_results, importance = 0.5, class_palette = "cancers12", upset_top_features = FALSE)

  # Ensure that the color palette was correctly applied
  expect_true("fill" %in% names(ggplot2::ggplot_build(result_with_palette$features_barplot)$data[[1]]))

  # Check that the palette has the expected colors
  ggplot_data <- ggplot2::ggplot_build(result_with_palette$features_barplot)$data[[1]]
  expect_true(any(ggplot_data$fill == "pink"))
  expect_true(any(ggplot_data$fill == "midnightblue"))

  # Test that the function does not break if class_palette is NULL
  result_no_palette <- hd_plot_model_summary(model_results, importance = 0.5, class_palette = NULL, upset_top_features = FALSE)

  # Ensure no palette was set
  ggplot_data_no_palette <- ggplot2::ggplot_build(result_no_palette$features_barplot)$data[[1]]
  expect_true(all(ggplot_data_no_palette$fill == "pink" | ggplot_data_no_palette$fill == "midnightblue"))
})
