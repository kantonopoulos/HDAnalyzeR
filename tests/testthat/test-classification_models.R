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