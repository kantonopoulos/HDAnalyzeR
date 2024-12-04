utils::globalVariables(c(":="))

#' Split data into training and test sets
#'
#' `split_data()` splits the data into training and test sets based on the ratio
#' provided. It also stratifies the data based on the variable provided.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample_id as its first column.
#' @param metadata A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.
#' @param variable The name of the column containing the case and control groups. Default is "Disease".
#' @param metadata_cols The columns to be selected from the metadata as predictors. Default is NULL.
#' @param ratio The ratio of training data to test data. Default is 0.75.
#' @param seed Seed for reproducibility. Default is 123.
#'
#' @return A split object containing train and test data.
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Split the data into training and test sets
#' hd_split <- hd_run_data_split(hd_object, variable = "Disease")
hd_run_data_split <- function(dat,
                              metadata = NULL,
                              variable = "Disease",
                              metadata_cols = NULL,
                              ratio = 0.75,
                              seed = 123){

  Variable <- rlang::sym(variable)
  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data to run the DE analysis.")
    }
    wide_data <- dat[["data"]]
    metadata <- dat[["metadata"]]
    sample_id <- dat[["sample_id"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
  }

  if (isFALSE(variable %in% colnames(metadata))) {
    stop("The variable is not be present in the metadata.")
  }

  join_data <- wide_data |>
    dplyr::left_join(metadata |>
                       dplyr::select(dplyr::all_of(c(sample_id, variable, metadata_cols))),
                     by = sample_id) |>
    dplyr::relocate(!!Variable, .after = sample_id)

  set.seed(seed)
  if (!is.null(variable)) {
    data_split <- rsample::initial_split(join_data, prop = ratio, strata = dplyr::any_of(variable))
  } else {
    data_split <- rsample::initial_split(join_data, prop = ratio)
  }
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  model_object <- list("train_data" = train_data,
                       "test_data" = test_data)

  class(model_object) <- "hd_model"

  return(model_object)
}


#' Balance groups
#'
#' `balance_groups()` balances the groups based on the case variable provided.
#' It downsamples the control group to match the number of samples in the case group.
#'
#' @param dat A dataset containing the case and control groups.
#' @param variable The name of the column containing the case and control groups. Default is "Disease".
#' @param case The case class. Default is 1.
#' @param seed Seed for reproducibility. Default is 123.
#'
#' @return A balanced dataset.
#' @keywords internal
balance_groups <- function(dat,
                           variable,
                           case = 1,
                           seed = 123) {

  Variable <- rlang::sym(variable)

  set.seed(seed)
  case_data <- dat |> dplyr::filter(!!Variable == case)
  control_data <- dat |> dplyr::filter(!!Variable != case)

  case_sample_num <- nrow(case_data)

  group <- case_data

  control_data <- control_data |>
    dplyr::filter(!!Variable != case) |>
    dplyr::sample_n(size = case_sample_num, replace = TRUE)

  group <- rbind(group, control_data)

  return(group)
}


#' Check data structure of object
#'
#' @param dat An `hd_model` object or a list containing the train and test data.
#' @param variable The name of the column containing the case and control groups. Default is "Disease".
#'
#' @return A model object containing the train and test data.
#' @keywords internal
check_data <- function(dat, variable = "Disease") {

  if (inherits(dat, "hd_model")) {
    if (is.null(dat[["train_data"]])) {
      stop("The 'train_data' slot of the model object is empty. Please provide the train data to train the model.")
    }
    if (is.null(dat[["test_data"]])) {
      stop("The 'test_data' slot of the model object is empty. Please provide the test data to evaluate the model.")
    }
    train_data <- dat[["train_data"]]
    test_data <- dat[["test_data"]]
  } else {
    if (is.null(dat[["train_data"]])) {
      stop("The list does not contain train data. Please provide the train data to train the model.")
    }
    if (is.null(dat[["test_data"]])) {
      stop("The list does not contain test data. Please provide the test data to evaluate the model.")
    }
    train_data <- dat[[1]]
    test_data <- dat[[2]]
  }

  if (isFALSE(variable %in% colnames(train_data))) {
    stop("The variable is not be present in the train data")
  }
  if (isFALSE(variable %in% colnames(test_data))) {
    stop("The variable is not be present in the test data")
  }

  model_object <- list("train_data" = train_data,
                       "test_data" = test_data)

  class(model_object) <- "hd_model"

  return(model_object)
}


#' Prepare data for model fitting
#'
#' `prepare_data()` prepares the data for model fitting. It filters out rows with
#' NAs in the variable column, converts the variable to a factor, and creates
#' cross-validation sets.
#'
#' @param dat An `hd_model` object coming from `check_data()`.
#' @param variable The variable to predict. Default is "Disease".
#' @param case The case class.
#' @param control The control class. Default is NULL.
#' @param balance_groups Whether to balance the groups. Default is TRUE.
#' @param cv_sets Number of cross-validation sets. Default is 5.
#' @param seed Seed for reproducibility. Default is 123.
#'
#' @return A model object containing the train and test data and cross-validation sets.
#' @keywords internal
prepare_data <- function(dat,
                         variable = "Disease",
                         case,
                         control = NULL,
                         balance_groups = TRUE,
                         cv_sets = 5,
                         seed = 123) {

  Variable <- rlang::sym(variable)
  train_data <- dat[["train_data"]]
  test_data <- dat[["test_data"]]

  class_count <- length(unique(train_data[[variable]]))
  if (class_count < 2) {

    stop("The variable has less than 2 classes. Please provide a variable with at least 2 classes.")

  } else if (class_count == 2 & is.null(case)) {

    stop("The variable has 2 classes, but case class is not selected. Please select a case class.")

  } else if (!is.null(case)) {

    dat[["model_type"]] <- "binary_class"
    # If control is NULL, set it to the unique values of the variable that are not the case
    if (is.null(control)){
      control <- setdiff(unique(train_data[[variable]]), case)
    }

    train_set <- train_data |>
      dplyr::filter(!!Variable %in% c(case, control)) |>
      dplyr::mutate(!!Variable := ifelse(!!Variable == case, 1, 0))

    if (balance_groups) {
      train_set <- balance_groups(train_set, variable, 1, seed)
    }

    test_set <- test_data |>
      dplyr::filter(!!Variable %in% c(case, control)) |>
      dplyr::mutate(!!Variable := ifelse(!!Variable == case, 1, 0))

  } else {

    dat[["model_type"]] <- "multi_class"

  }

  nrows_before <- nrow(train_set)

  train_set <- train_set |>
    dplyr::filter(!dplyr::if_any(dplyr::all_of(c(variable)), is.na)) |>  # Remove NAs from columns in formula
    dplyr::mutate(!!Variable := as.factor(!!Variable))  # Remove NAs from correct columns

  nrows_after <- nrow(train_set)
  if (nrows_before != nrows_after){
    warning(paste0(nrows_before - nrows_after,
                   " rows were removed from train set because they contain NAs in ",
                   variable,
                   "!"))
  }

  nrows_before <- nrow(test_set)

  test_set <- test_set |>
    dplyr::filter(!dplyr::if_any(dplyr::all_of(c(variable)), is.na)) |>  # Remove NAs from columns in formula
    dplyr::mutate(!!Variable := as.factor(!!Variable))  # Remove NAs from correct columns

  nrows_after <- nrow(test_set)
  if (nrows_before != nrows_after){
    warning(paste0(nrows_before - nrows_after,
                   " rows were removed from test set because they contain NAs in ",
                   variable,
                   "!"))
  }

  set.seed(seed)
  train_folds <- rsample::vfold_cv(train_set, v = cv_sets, strata = !!Variable)

  dat[["train_data"]] <- train_set
  dat[["test_data"]] <- test_set
  dat[["train_folds"]] <- train_folds

  return(dat)
}


#' Hyperparameter optimization for regularized regression models
#'
#' `tune_rreg_model()` performs hyperparameter optimization for regularized
#' regression models. It tunes the model using the provided grid size and
#' cross-validation sets. It returns the best model and hyperparameters.
#'
#' @param dat An `hd_model` object coming from `prepare_data()`.
#' @param variable The variable to predict. Default is "Disease".
#' @param cor_threshold Threshold of absolute correlation values. This will be used
#' to remove the minimum number of features so that all their resulting absolute
#' correlations are less than this value.
#' @param grid_size Size of the hyperparameter optimization grid. Default is 10.
#' @param mixture The mixture parameter for the elastic net. If NULL it will be tuned. Default is NULL.
#' @param verbose Whether to print progress messages. Default is TRUE.
#' @param seed Seed for reproducibility. Default is 123.
#'
#' @return A model object containing the train and test data, the tuned model, and the workflow.
#' @keywords internal
tune_rreg_model <- function(dat,
                            variable = "Disease",
                            cor_threshold = 0.9,
                            grid_size = 30,
                            mixture = NULL,
                            verbose = TRUE,
                            seed = 123) {

  if (verbose){
    message("Tuning regularized regression model...")
  }
  train_set <- dat[["train_data"]]
  train_folds <- dat[["train_folds"]]
  sample_id <- colnames(train_set)[1]

  formula <- stats::as.formula(paste(variable, "~ ."))
  rec <- recipes::recipe(formula, data = train_set) |>
    recipes::update_role(sample_id, new_role = "id") |>
    recipes::step_nzv(recipes::all_numeric()) |>
    recipes::step_normalize(recipes::all_numeric()) |>
    recipes::step_corr(recipes::all_numeric(), threshold = cor_threshold) |>
    recipes::step_impute_knn(recipes::all_numeric()) |>
    recipes::step_dummy(recipes::all_nominal_predictors())

  if (is.null(mixture)) {
    spec <- parsnip::logistic_reg(penalty = tune::tune(),
                                  mixture = tune::tune()) |>
      parsnip::set_engine("glmnet")
  } else {
    spec <- parsnip::logistic_reg(penalty = tune::tune(),
                                  mixture = mixture) |>
      parsnip::set_engine("glmnet")
  }

  wf <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_recipe(rec)

  grid <- wf |>
    workflows::extract_parameter_set_dials() |>
    dials::grid_space_filling(size = grid_size, type = "latin_hypercube")

  ctrl <- tune::control_grid(save_pred = TRUE, parallel_over = "everything", verbose = verbose)

  set.seed(seed)
  tune <- wf |> tune::tune_grid(train_folds,
                                grid = grid,
                                control = ctrl,
                                metrics = yardstick::metric_set(yardstick::roc_auc))

  dat[["tune"]] <- tune
  dat[["wf"]] <- wf
  dat[["train_folds"]] <- NULL

  return(dat)
}


#' Finalize and evaluate the model
#'
#' `evaluate_model()` finalizes the model using the best hyperparameters and evaluates
#' the model using the test set. It calculates the accuracy, sensitivity, specificity,
#' AUC, and confusion matrix. It also plots the ROC curve.
#'
#' @param dat An `hd_model` object coming from a tuning function.
#' @param variable The variable to predict. Default is "Disease".
#' @param case The case class.
#' @param mixture The mixture parameter for the elastic net. If NULL it will be tuned. Default is NULL.
#' @param palette The color palette for the classes. If it is a character, it should be one of the palettes from `hd_palettes()`. Default is NULL.
#' @param verbose Whether to print progress messages. Default is TRUE.
#' @param seed Seed for reproducibility. Default is 123.
#'
#' @return A model object containing the train and test data, the final model, the metrics, the ROC curve, and the mixture parameter.
#' @keywords internal
evaluate_model <- function(dat,
                           variable = "Disease",
                           case,
                           mixture = NULL,
                           palette = NULL,
                           verbose= TRUE,
                           seed = 123) {

  if (verbose){
    message("Evaluating the model...")
  }
  Variable <- rlang::sym(variable)
  train_set <- dat[["train_data"]]
  test_set <- dat[["test_data"]]
  tune <- dat[["tune"]]
  wf <- dat[["wf"]]

  best <- tune |>
    tune::select_best(metric = "roc_auc") |>
    dplyr::select(-dplyr::all_of(c(".config")))

  if (is.null(mixture)) {
    mixture <- best[["mixture"]]
  }

  final_wf <- tune::finalize_workflow(wf, best)

  set.seed(seed)
  final <- final_wf |>
    parsnip::fit(train_set)

  splits <- rsample::make_splits(train_set, test_set)

  preds <- tune::last_fit(final_wf,
                          splits,
                          metrics = yardstick::metric_set(yardstick::roc_auc))

  res <- stats::predict(final, new_data = test_set)

  res <- dplyr::bind_cols(res, test_set |> dplyr::select(!!Variable))

  accuracy <- res |> yardstick::accuracy(!!Variable, !!rlang::sym(".pred_class"))
  sensitivity <- res |> yardstick::sensitivity(!!Variable, !!rlang::sym(".pred_class"), event_level = "second")
  specificity <- res |> yardstick::specificity(!!Variable, !!rlang::sym(".pred_class"), event_level = "second")
  auc <- preds |> tune::collect_metrics()
  cm <- res |> yardstick::conf_mat(!!Variable, !!rlang::sym(".pred_class"))

  if (is.null(names(palette)) && !is.null(palette)) {
    disease_color <- hd_palettes()[[palette]][[case]]
  } else if (!is.null(palette)) {
    disease_color <- palette
  } else {
    disease_color <- "black"
  }

  roc <- preds |>
    tune::collect_predictions(summarize = FALSE) |>
    yardstick::roc_curve(truth = !!Variable, !!rlang::sym(".pred_0")) |>
    ggplot2::ggplot(ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
    ggplot2::geom_path(colour = disease_color, linewidth = 2) +
    ggplot2::geom_abline(lty = 3) +
    ggplot2::coord_equal() +
    theme_hd()

  dat[["final"]] <- final
  dat[["metrics"]] <- list("accuracy" = accuracy$.estimate,
                           "sensitivity" = sensitivity$.estimate,
                           "specificity" = specificity$.estimate,
                           "auc" = auc$.estimate,
                           "confusion_matrix" = cm)
  dat[["roc_curve"]] <- roc
  dat[["mixture"]] <- mixture
  dat[["tune"]] <- NULL
  dat[["wf"]] <- NULL

  return(dat)
}


#' Create subtitle for variable importance plot
#'
#' `generate_subtitle()` generates a subtitle for the variable importance plot.
#'
#' @param features A tibble with features and their model importance.
#' @param accuracy Accuracy of the model.
#' @param sensitivity Sensitivity of the model.
#' @param specificity Specificity of the model.
#' @param auc AUC of the model.
#' @param mixture Mixture of lasso and ridge regularization. In random forest models it is NULL.
#' @param title Vector of subtitle elements to include in the plot.
#'
#' @return The plot subtitle as character vector.
#' @keywords internal
generate_title <- function(features,
                           accuracy,
                           sensitivity,
                           specificity,
                           auc,
                           mixture = NULL,
                           title = c("accuracy",
                                     "sensitivity",
                                     "specificity",
                                     "auc",
                                     "features",
                                     "top-features",
                                     "mixture")) {

  title_parts <- c()

  if ("accuracy" %in% title) {
    title_parts <- c(title_parts, paste0('Accuracy = ', round(accuracy, 2), '    '))
  }

  if ("sensitivity" %in% title) {
    title_parts <- c(title_parts, paste0('Sensitivity = ', round(sensitivity, 2), '    '))
  }

  if ("specificity" %in% title) {
    title_parts <- c(title_parts, paste0('Specificity = ', round(specificity, 2), '    '))
  }

  if ("auc" %in% title) {
    title_parts <- c(title_parts, paste0('AUC = ', round(auc, 2), '    '))
  }

  if (length(title_parts) > 0) {
    title_parts <- c(title_parts, '\n')
  }

  if ("features" %in% title) {
    title_parts <- c(title_parts, paste0('Features = ', nrow(features), '    '))
  }

  if ("top-features" %in% title) {
    title_parts <- c(title_parts, paste0('Top-features = ',
                                         nrow(features |> dplyr::filter(!!rlang::sym("Scaled_Importance") >= 50)),
                                         '    '))
  }

  if ("mixture" %in% title) {
    title_parts <- c(title_parts, paste0('Lasso/Ridge ratio = ', round(mixture, 2), '    '))
  }

  title <- paste(title_parts, collapse = '')

  return(title)
}


#' Extract model features and plot variable importance
#'
#' `variable_imp()` calculates the variable importance of the model and plots the top features.
#' It also generates a title for the plot based on the model metrics and the mixture parameter.
#'
#' @param dat An `hd_model` object coming from `evaluate_model()`.
#' @param variable The variable to predict. Default is "Disease".
#' @param case The case class.
#' @param mixture The mixture parameter for the elastic net. If NULL it will be tuned. Default is NULL.
#' @param palette The color palette for the classes. If it is a character, it should be one of the palettes from `hd_palettes()`. Default is NULL.
#' @param title Vector of title elements to include in the plot.
#' @param seed Seed for reproducibility. Default is 123.
#'
#' @return A model object containing the features and the variable importance plot.
#' @keywords internal
variable_imp <- function(dat,
                         variable = "Disease",
                         case,
                         mixture = NULL,
                         palette = NULL,
                         title = c("accuracy",
                                   "sensitivity",
                                   "specificity",
                                   "auc",
                                   "features",
                                   "top-features"),
                         verbose = TRUE,
                         seed = 123) {

  if (verbose){
    message("Generating visualizations...")
  }

  Variable <- rlang::sym(variable)
  final <- dat[["final"]]
  metrics <- dat[["metrics"]]
  mixture <- dat[["mixture"]]

  features <- final |>
    workflows::extract_fit_parsnip() |>
    vip::vi() |>
    dplyr::mutate(Importance = abs(!!rlang::sym("Importance")),
                  Variable = forcats::fct_reorder(Variable, !!rlang::sym("Importance"))) |>
    dplyr::arrange(dplyr::desc(!!rlang::sym("Importance"))) |>
    dplyr::mutate(Scaled_Importance = scales::rescale(!!rlang::sym("Importance"), to = c(0, 1))) |>
    dplyr::filter(!!rlang::sym("Scaled_Importance") > 0)

  title_text <- generate_title(features = features,
                               accuracy = metrics$accuracy,
                               sensitivity = metrics$sensitivity,
                               specificity = metrics$specificity,
                               auc = metrics$auc,
                               mixture = mixture,
                               title = title)

  pals <- hd_palettes()
  if (!is.null(palette) && is.null(names(palette))) {
    pal <- pals[palette]
    pal <- unlist(pals[[palette]])
  } else if (!is.null(palette)) {
    pal <- palette
  } else {
    pal <- c("#C03830")
  }

  var_imp_plot <- features |>
    ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym("Scaled_Importance"), y = Variable)) +
    ggplot2::geom_col(ggplot2::aes(fill = ifelse(!!rlang::sym("Scaled_Importance") > 0.5, case, NA))) +
    ggplot2::labs(y = NULL) +
    ggplot2::scale_x_continuous(breaks = c(0, 1), expand = c(0, 0)) +  # Keep x-axis tick labels at 0 and 1
    ggplot2::scale_fill_manual(values = pal, na.value = "grey80") +
    ggplot2::ggtitle(label = title_text) +
    ggplot2::xlab('Importance') +
    ggplot2::ylab('Features') +
    theme_hd() +
    ggplot2::theme(legend.position = "none",
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())

  dat[["features"]] <- features
  dat[["var_imp_plot"]] <- var_imp_plot
  dat[["final"]] <- NULL

  return(dat)
}


#' Run regularized regression model pipeline
#'
#' `hd_run_rreg()` runs the regularized regression model pipeline. It creates
#' class-balanced case-control groups for the train set, tunes the model, evaluates
#' the model, and plots the variable importance.
#'
#' @param dat An `hd_model` object or a list containing the train and test data.
#' @param variable The name of the column containing the case and control groups. Default is "Disease".
#' @param case The case class.
#' @param control The control groups. If NULL, it will be set to all other unique values of the variable that are not the case. Default is NULL.
#' @param balance_groups Whether to balance the groups. Default is TRUE.
#' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' @param grid_size Size of the hyperparameter optimization grid. Default is 30.
#' @param cv_sets Number of cross-validation sets. Default is 5.
#' @param mixture The mixture parameter for the elastic net. If NULL it will be tuned. Default is NULL.
#' @param palette The color palette for the classes. If it is a character, it should be one of the palettes from `hd_palettes()`. Default is NULL.
#' @param verbose Whether to print progress messages. Default is TRUE.
#' @param title Vector of title elements to include in the plot.
#' @param seed Seed for reproducibility. Default is 123.
#'
#' @return A model object containing the train and test data, the metrics, the ROC curve, the selected features, the variable importance, and the mixture parameter.
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Split the data into training and test sets
#' hd_split <- hd_run_data_split(hd_object, variable = "Disease")
#'
#' # Run the regularized regression model pipeline
#' hd_run_rreg(hd_split,
#'             variable = "Disease",
#'             case = "AML",
#'             grid_size = 5,
#'             palette = "cancers12")
hd_run_rreg <- function(dat,
                        variable = "Disease",
                        case,
                        control = NULL,
                        balance_groups = TRUE,
                        cor_threshold = 0.9,
                        grid_size = 30,
                        cv_sets = 5,
                        mixture = NULL,
                        palette = NULL,
                        verbose = TRUE,
                        title = c("accuracy",
                                  "sensitivity",
                                  "specificity",
                                  "auc",
                                  "features",
                                  "top-features",
                                  "mixture"),
                        seed = 123) {

  dat <- check_data(dat = dat, variable = variable)
  dat <- prepare_data(dat = dat,
                      variable = variable,
                      case = case,
                      control = control,
                      balance_groups = balance_groups,
                      cv_sets = cv_sets,
                      seed = seed)
  dat <- tune_rreg_model(dat = dat,
                         variable = variable,
                         cor_threshold = cor_threshold,
                         grid_size = grid_size,
                         mixture = mixture,
                         verbose = verbose,
                         seed = seed)
  dat <- evaluate_model(dat = dat,
                        variable = variable,
                        case = case,
                        mixture = mixture,
                        palette = palette,
                        verbose = verbose,
                        seed = seed)
  dat <- variable_imp(dat = dat,
                      variable = variable,
                      case = case,
                      mixture = mixture,
                      palette = palette,
                      title = title,
                      verbose = verbose,
                      seed = seed)
  return(dat)
}
