#' utils::globalVariables(c("roc_auc", ".config", ".pred_class", ".pred_0", "Scaled_Importance",
#'                          "Importance", "Variable", "std_err", "Type", "metric",
#'                          "specificity", "sensitivity", ".level", ".estimate", "Category",
#'                          ".metric", "mtry", "sample_size", "parameter"))
#' #' Split dataset into training and test sets
#' #'
#' #' `split_data()` splits the dataset into training and test sets based on user defined ratio.
#' #' It also stratifies the data based on the variable of interest.
#' #'
#' #' @param join_data Olink data in wide format joined with metadata.
#' #' @param variable Variable to stratify the data. Default is "Disease".
#' #' @param strata Whether to stratify the data. Default is TRUE.
#' #' @param ratio Ratio of training data to test data. Default is 0.75.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with three elements:
#' #'  - train_set: The training set.
#' #'  - test_set: The test set.
#' #'  - data_split: The data split object.
#' #' @keywords internal
#' split_data <- function(join_data,
#'                        variable = "Disease",
#'                        strata = TRUE,
#'                        ratio = 0.75,
#'                        seed = 123) {
#'   set.seed(seed)
#'   if (strata) {
#'     data_split <- rsample::initial_split(join_data, prop = ratio, strata = dplyr::any_of(variable))
#'   } else {
#'     data_split <- rsample::initial_split(join_data, prop = ratio)
#'   }
#'   train_data <- rsample::training(data_split)
#'   test_data <- rsample::testing(data_split)
#'
#'   return(list("train_set" = train_data,
#'               "test_set" = test_data,
#'               "data_split" = data_split))
#' }
#'
#'
#' #' Create control groups for sex specific cases
#' #'
#' #' `filter_sex_specific_disease()` creates control groups for sex-specific cases
#' #' by filtering samples to include only those from the relevant sex and by
#' #' updating the cases vector to include only the cases that will be sampled.
#' #'
#' #' @param control_data Control data to filter.
#' #' @param case Case to create control group.
#' #' @param cases Cases.
#' #' @param only_female Cases that are female specific.
#' #' @param only_male Cases that are male specific.
#' #'
#' #' @return A list with two elements:
#' #'  - control_data: Control data to filter.
#' #'  - cases_subset: Filtered cases vector.
#' #' @keywords internal
#' filter_sex_specific_disease <- function(control_data,
#'                                         case,
#'                                         cases,
#'                                         only_female = NULL,
#'                                         only_male = NULL) {
#'
#'   if(!is.null(only_female) & case %in% only_female) {
#'     control_data <- control_data |>
#'       dplyr::filter(Sex == "F")
#'     cases_subset <- cases[!cases %in% only_male]
#'   } else if(!is.null(only_male) & case %in% only_male) {
#'     control_data <- control_data |>
#'       dplyr::filter(Sex == "M")
#'     cases_subset <- cases[!cases %in% only_female]
#'   } else {
#'     control_data <- control_data
#'     cases_subset <- cases
#'   }
#'
#'   return(list("control_data" = control_data,
#'               "cases_subset" = cases_subset))
#' }
#'
#'
#' #' Create class-balanced case-control groups for classification models
#' #'
#' #' `make_groups()` creates class-balanced case-control groups for classification models.
#' #' It separates the data into control and case groups. It then calculates the amount of data
#' #' from each case in the control group and randomly selects the number of each control class samples.
#' #' It also filters the control data of sex specific cases.
#' #'
#' #' @param join_data Olink data in wide format joined with metadata.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param case Case to predict.
#' #' @param cases Cases.
#' #' @param only_female Cases that are female specific.
#' #' @param only_male Cases that are male specific.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with combined, class-balanced control-case groups for each case.
#' #' @keywords internal
#' make_groups <- function(join_data,
#'                         variable = "Disease",
#'                         case,
#'                         cases,
#'                         only_female = NULL,
#'                         only_male = NULL,
#'                         seed = 123) {
#'   Variable <- rlang::sym(variable)
#'   set.seed(seed)
#'
#'   # Separate data in control and case groups
#'   case_data <- join_data |> dplyr::filter(!!Variable == case)
#'   control_data <- join_data |> dplyr::filter(!!Variable != case)
#'
#'   # Filter for gender if case is gender specific
#'   filtered_results <- filter_sex_specific_disease(control_data,
#'                                                   case,
#'                                                   cases,
#'                                                   only_female,
#'                                                   only_male)
#'
#'   control_data <- filtered_results$control_data
#'   cases_subset <- filtered_results$cases_subset  # Keep only cases that we will pick samples from
#'
#'   # Calculate amount of data from each case in control group
#'   case_sample_num <- nrow(case_data)
#'   samples_per_case <- ceiling(case_sample_num/(length(unique(cases_subset))-1))
#'
#'   # Loop over control group, randomly select the number of each control class samples
#'   group <- case_data
#'   for (control_class in cases_subset[cases_subset != case]) {
#'
#'     control_class_data <- control_data |>
#'       dplyr::filter(!!Variable == control_class) |>
#'       dplyr::sample_n(size = samples_per_case, replace = TRUE)
#'     group <- rbind(group, control_class_data)
#'   }
#'
#'   return(group)
#' }
#'
#'
#' #' Fit logistic regression model for single predictors
#' #'
#' #' `lreg_fit()` fits a logistic regression model for single predictors.
#' #' It uses the glm engine for logistic regression and fits the model using the
#' #' `logistic_reg()` function from the parsnip package. It also calculates the
#' #' accuracy, sensitivity, specificity, AUC, and confusion matrix of the model.
#' #'
#' #' @param train_data Training data set from `make_groups()`.
#' #' @param test_data Testing data set from `make_groups()`.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param case Case to predict.
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned. Default is NULL.
#' #' @param palette Color palette for the ROC curve. Default is NULL.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with two elements:
#' #' - fit_res: A list with 4 elements:
#' #'  - lreg_wf: Workflow object.
#' #'  - train_set: Training set.
#' #'  - test_set: Testing set.
#' #'  - final: Fitted model.
#' #' - metrics: A list with the model metrics:
#' #'  - accuracy: Accuracy of the model.
#' #'  - sensitivity: Sensitivity of the model.
#' #'  - specificity: Specificity of the model.
#' #'  - auc: AUC of the model.
#' #'  - conf_matrix: Confusion matrix of the model.
#' #'  - roc_curve: ROC curve of the model.
#' #' @keywords internal
#' lreg_fit <- function(train_data,
#'                      test_data,
#'                      variable = "Disease",
#'                      case,
#'                      cor_threshold = 0.9,
#'                      cv_sets = 4,
#'                      ncores = 1,
#'                      exclude_cols = NULL,
#'                      palette = NULL,
#'                      seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   if (ncores > 1) {
#'     doParallel::registerDoParallel(cores = ncores)
#'   }
#'
#'   # Prepare train data and create cross-validation sets with binary classifier
#'   train_set <- train_data |>
#'     dplyr::mutate(!!Variable := ifelse(!!Variable == case, 1, 0)) |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols))
#'   train_folds <- rsample::vfold_cv(train_set, v = cv_sets, strata = !!Variable)
#'
#'   test_set <- test_data |>
#'     dplyr::mutate(!!Variable := ifelse(!!Variable == case, 1, 0)) |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols))
#'
#'   formula <- stats::as.formula(paste(variable, "~ ."))
#'
#'   lreg_rec <- recipes::recipe(formula, data = train_set) |>
#'     recipes::update_role(DAid, new_role = "id") |>
#'     recipes::step_normalize(recipes::all_numeric()) |>
#'     recipes::step_nzv(recipes::all_numeric()) |>
#'     recipes::step_corr(recipes::all_numeric(), threshold = cor_threshold) |>
#'     recipes::step_impute_knn(recipes::all_numeric())
#'
#'   lreg_spec <- parsnip::logistic_reg() |>
#'     parsnip::set_engine("glm")
#'
#'   lreg_wf <- workflows::workflow() |>
#'     workflows::add_model(lreg_spec) |>
#'     workflows::add_recipe(lreg_rec)
#'
#'   final <- lreg_wf |>
#'     parsnip::fit(train_set)
#'
#'   splits <- rsample::make_splits(train_set, test_set)
#'
#'   preds <- tune::last_fit(lreg_wf,
#'                           splits,
#'                           metrics = yardstick::metric_set(yardstick::roc_auc))
#'   res <- stats::predict(final, new_data = test_set)
#'
#'   res <- dplyr::bind_cols(res, test_set |> dplyr::select(!!Variable))
#'
#'   accuracy <- res |>
#'     yardstick::accuracy(!!Variable, .pred_class)
#'
#'   sensitivity <- res |>
#'     yardstick::sensitivity(!!Variable, .pred_class)
#'
#'   specificity <- res |>
#'     yardstick::specificity(!!Variable, .pred_class)
#'
#'   if (is.null(names(palette)) && !is.null(palette)) {
#'     disease_color <- get_hpa_palettes()[[palette]][[case]]
#'   } else if (!is.null(palette)) {
#'     disease_color <- palette
#'   } else {
#'     disease_color <- "black"
#'   }
#'
#'   selected_point <- tibble::tibble(x = 1 - specificity$.estimate,
#'                                    y = sensitivity$.estimate)
#'   roc <- preds |>
#'     tune::collect_predictions(summarize = F) |>
#'     yardstick::roc_curve(truth = !!Variable, .pred_0) |>
#'     ggplot2::ggplot(ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
#'     ggplot2::geom_path(colour = disease_color, linewidth = 2) +
#'     ggplot2::geom_point(data = selected_point, ggplot2::aes(x = x, y = y), size = 2, shape = 4, colour = "black") +
#'     ggplot2::geom_abline(lty = 3) +
#'     ggplot2::coord_equal() +
#'     theme_hpa()
#'
#'   auc <- preds |>
#'     tune::collect_metrics()
#'
#'   cm <- res |>
#'     yardstick::conf_mat(!!Variable, .pred_class)
#'
#'   return(list("fit_res" = list("lreg_wf" = lreg_wf,
#'                                "train_set" = train_set,
#'                                "test_set" = test_set,
#'                                "final" = final),
#'               "metrics" = list("accuracy" = round(accuracy$.estimate, 2),
#'                                "sensitivity" = round(sensitivity$.estimate, 2),
#'                                "specificity" = round(specificity$.estimate, 2),
#'                                "auc" = round(auc$.estimate, 2),
#'                                "conf_matrix" = cm,
#'                                "roc_curve" = roc)))
#' }
#'
#'
#' #' Visualize hyperparameter optimization results
#' #'
#' #' `vis_hypopt()` plots the hyperparameter optimization results.
#' #'
#' #' @param tune_res Hyperparameter optimization results.
#' #' @param x X-axis variable of the plot.
#' #' @param color Color variable of the plot.
#' #' @param case Case to predict.
#' #'
#' #' @return Hyperparameter optimization plot.
#' #' @keywords internal
#' vis_hypopt <- function(tune_res,
#'                        x,
#'                        color,
#'                        case) {
#'
#'   hypopt_res <- tune_res |>
#'     tune::collect_metrics()
#'
#'   x <- rlang::sym(x)
#'   mean <- rlang::sym('mean')
#'   if (!is.null(color)) {
#'     color <- rlang::sym(color)
#'   }
#'   hypopt_plot <- ggplot2::ggplot(hypopt_res, ggplot2::aes(x = !!x, y = !!mean, color = !!color)) +
#'     ggplot2::geom_point(size = 3) +
#'     ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - std_err,
#'                                         ymax = mean + std_err),
#'                            alpha = 0.5) +
#'     ggplot2::ggtitle(label = paste0(case,'')) +
#'     viridis::scale_color_viridis() +
#'     ggplot2::labs(y = "metric_mean") +
#'     theme_hpa()
#'
#'   return(hypopt_plot)
#' }
#'
#'
#' #' Hyperparameter optimization for elastic net model
#' #'
#' #' `elnet_hypopt()` tunes an elastic net model and performs hyperparameter optimization.
#' #' It uses the glmnet engine for logistic regression and tunes either only penalty (Lasso or Ridge) or
#' #' both penalty and mixture (Elastic Regression). For the hyperparameter optimization, it uses the
#' #' `grid_space_filling()` function from the dials package.
#' #'
#' #' @param train_data Training data set from `make_groups()`.
#' #' @param test_data Testing data set from `make_groups()`.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param case Case to predict.
#' #' @param type Type of regularization. Default is "lasso". Other options are "ridge" and "elnet".
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the hyperparameter optimization grid. Default is 10.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned. Default is NULL.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with five elements:
#' #'  - elnet_tune: Hyperparameter optimization results.
#' #'  - wf: Workflow object.
#' #'  - train_set: Training set.
#' #'  - test_set: Testing set.
#' #'  - hyperopt_vis: Hyperparameter optimization plot.
#' #' @keywords internal
#' elnet_hypopt <- function(train_data,
#'                          test_data,
#'                          variable = "Disease",
#'                          case,
#'                          type = "lasso",
#'                          cor_threshold = 0.9,
#'                          cv_sets = 5,
#'                          grid_size = 10,
#'                          ncores = 4,
#'                          hypopt_vis = TRUE,
#'                          exclude_cols = NULL,
#'                          seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'   if (ncores > 1) {
#'     doParallel::registerDoParallel(cores = ncores)
#'   }
#'
#'   # Prepare train data and create cross-validation sets with binary classifier
#'   train_set <- train_data |>
#'     dplyr::mutate(!!Variable := ifelse(!!Variable == case, 1, 0)) |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols))
#'   train_folds <- rsample::vfold_cv(train_set, v = cv_sets, strata = !!Variable)
#'
#'   test_set <- test_data |>
#'     dplyr::mutate(!!Variable := ifelse(!!Variable == case, 1, 0)) |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols))
#'
#'   formula <- stats::as.formula(paste(variable, "~ ."))
#'
#'   elnet_rec <- recipes::recipe(formula, data = train_set) |>
#'     recipes::update_role(DAid, new_role = "id") |>
#'     recipes::step_normalize(recipes::all_numeric()) |>
#'     recipes::step_nzv(recipes::all_numeric()) |>
#'     recipes::step_corr(recipes::all_numeric(), threshold = cor_threshold) |>
#'     recipes::step_impute_knn(recipes::all_numeric())
#'
#'   if (type == "elnet") {
#'     elnet_spec <- parsnip::logistic_reg(
#'       penalty = tune::tune(),  # lambda
#'       mixture = tune::tune()  # alpha
#'     ) |>
#'       parsnip::set_engine("glmnet")
#'   } else if (type == "lasso") {
#'     elnet_spec <- parsnip::logistic_reg(
#'       penalty = tune::tune(),
#'       mixture = 1
#'     ) |>
#'       parsnip::set_engine("glmnet")
#'   } else if (type == "ridge") {
#'     elnet_spec <- parsnip::logistic_reg(
#'       penalty = tune::tune(),
#'       mixture = 0
#'     ) |>
#'       parsnip::set_engine("glmnet")
#'   }
#'
#'   elnet_wf <- workflows::workflow() |>
#'     workflows::add_model(elnet_spec) |>
#'     workflows::add_recipe(elnet_rec)
#'
#'   elnet_grid <- elnet_wf |>
#'     workflows::extract_parameter_set_dials() |>
#'     dials::grid_space_filling(size = grid_size)
#'
#'   roc_res <- yardstick::metric_set(yardstick::roc_auc)
#'
#'   set.seed(seed)
#'   ctrl <- tune::control_grid(save_pred = TRUE, parallel_over = "everything")
#'   elnet_tune <- elnet_wf |>
#'     tune::tune_grid(
#'       train_folds,
#'       grid = elnet_grid,
#'       control = ctrl,
#'       metrics = roc_res
#'     )
#'
#'   if (hypopt_vis) {
#'     if (type == "elnet") {
#'       hypopt_plot <- vis_hypopt(elnet_tune, "penalty", "mixture", case)
#'     } else {
#'       hypopt_plot <- vis_hypopt(elnet_tune, "penalty", NULL, case)
#'     }
#'     return(list("elnet_tune" = elnet_tune,
#'                 "elnet_wf" = elnet_wf,
#'                 "train_set" = train_set,
#'                 "test_set" = test_set,
#'                 "hypopt_vis" = hypopt_plot))
#'   }
#'
#'   return(list("elnet_tune" = elnet_tune,
#'               "elnet_wf" = elnet_wf,
#'               "train_set" = train_set,
#'               "test_set" = test_set))
#' }
#'
#'
#' #' Hyperparameter optimization for random forest model
#' #'
#' #' `rf_hypopt()` performs hyperparameter optimization for random forest models.
#' #' It uses the ranger engine for logistic regression and tunes the number of
#' #' predictors that will be randomly sampled at each split when creating the
#' #' tree models, as well as the minimum number of data points in a node that are
#' #' required for the node to be split further. For the hyperparameter optimization,
#' #' it uses the `grid_space_filling()` function from the dials package.
#' #'
#' #' @param train_data Training data set from `make_groups()`.
#' #' @param test_data Testing data set from `make_groups()`.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param case Case to predict.
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param normalize Whether to normalize numeric data to have a standard deviation of one and a mean of zero. Default is TRUE.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the grid for hyperparameter optimization. Default is 10.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned. Default is NULL.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with five elements:
#' #'  - rf_tune: Hyperparameter optimization results.
#' #'  - rf_wf: Workflow object.
#' #'  - train_set: Training set.
#' #'  - test_set: Testing set.
#' #'  - hyperopt_vis: Hyperparameter optimization plot.
#' #' @keywords internal
#' rf_hypopt <- function(train_data,
#'                       test_data,
#'                       variable = "Disease",
#'                       case,
#'                       cor_threshold = 0.9,
#'                       normalize = TRUE,
#'                       cv_sets = 5,
#'                       grid_size = 10,
#'                       ncores = 4,
#'                       hypopt_vis = TRUE,
#'                       exclude_cols = NULL,
#'                       seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'   if (ncores > 1) {
#'     doParallel::registerDoParallel(cores = ncores)
#'   }
#'
#'   # Prepare train data and create cross-validation sets with binary classifier
#'   train_set <- train_data |>
#'     dplyr::mutate(!!Variable := ifelse(!!Variable == case, 1, 0)) |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols)) |>
#'     dplyr::mutate(dplyr::across(tidyselect::where(is.character) & !dplyr::all_of("DAid"), as.factor))  # Solve bug of Gower distance function
#'
#'   train_folds <- rsample::vfold_cv(train_set, v = cv_sets, strata = !!Variable)
#'
#'   test_set <- test_data |>
#'     dplyr::mutate(!!Variable := ifelse(!!Variable == case, 1, 0)) |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols)) |>
#'     dplyr::mutate(dplyr::across(tidyselect::where(is.character) & !dplyr::all_of("DAid"), as.factor))
#'
#'   formula <- stats::as.formula(paste(variable, "~ ."))
#'
#'   rf_rec <- recipes::recipe(formula, data = train_set) |>
#'     recipes::update_role(DAid, new_role = "id")
#'
#'   if (isTRUE(normalize)) {
#'     rf_rec <- rf_rec |> recipes::step_normalize(recipes::all_numeric())
#'   }
#'
#'   rf_rec <- rf_rec |>
#'     recipes::step_nzv(recipes::all_numeric()) |>
#'     recipes::step_corr(recipes::all_numeric(), threshold = cor_threshold) |>
#'     recipes::step_impute_knn(recipes::all_numeric())
#'
#'   rf_spec <- parsnip::rand_forest(
#'     trees = 1000,
#'     mtry = tune::tune(),
#'     min_n = tune::tune()
#'   ) |>
#'     parsnip::set_mode("classification") |>
#'     parsnip::set_engine("ranger", importance = "permutation")
#'
#'   disease_pred <- train_set |> dplyr::select(-dplyr::any_of(c("Disease", "DAid", "Sex", "Age", "BMI")))
#'
#'   rf_wf <- workflows::workflow() |>
#'     workflows::add_model(rf_spec) |>
#'     workflows::add_recipe(rf_rec)
#'
#'   rf_grid <- rf_wf |>
#'     workflows::extract_parameter_set_dials() |>
#'     dials::finalize(disease_pred) |>
#'     dials::grid_space_filling(size = grid_size)
#'
#'   roc_res <- yardstick::metric_set(yardstick::roc_auc)
#'
#'   set.seed(seed)
#'   ctrl <- tune::control_grid(save_pred = TRUE, parallel_over = "everything")
#'   rf_tune <- rf_wf |>
#'     tune::tune_grid(
#'       train_folds,
#'       grid = rf_grid,
#'       control = ctrl,
#'       metrics = roc_res
#'     )
#'
#'   if (hypopt_vis) {
#'     hypopt_plot <- vis_hypopt(rf_tune, "min_n", "mtry", case)
#'
#'     return(list("rf_tune" = rf_tune,
#'                 "rf_wf" = rf_wf,
#'                 "train_set" = train_set,
#'                 "test_set" = test_set,
#'                 "hypopt_vis" = hypopt_plot))
#'   }
#'
#'   return(list("rf_tune" = rf_tune,
#'               "rf_wf" = rf_wf,
#'               "train_set" = train_set,
#'               "test_set" = test_set))
#' }
#'
#'
#' #' Hyperparameter optimization for XGBoost model
#' #'
#' #' `xgboost_hypopt()` tunes an XGBoost model and performs hyperparameter optimization.
#' #' It uses the xgboost engine for logistic regression and tunes the number of trees,
#' #' tree depth, minimum number of data points in a node, loss reduction, sample size,
#' #' and number of predictors randomly sampled at each split. For the hyperparameter optimization,
#' #' it uses the `grid_space_filling()` function from the dials package.
#' #'
#' #' @param train_data Training data set from `make_groups()`.
#' #' @param test_data Testing data set from `make_groups()`.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param case Case to predict.
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param normalize Whether to normalize numeric data to have a standard deviation of one and a mean of zero. Default is TRUE.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the grid for hyperparameter optimization. Default is 10.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned. Default is NULL.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with five elements:
#' #'  - xgboost_tune: Hyperparameter optimization results.
#' #'  - xgboost_wf: Workflow object.
#' #'  - train_set: Training set.
#' #'  - test_set: Testing set.
#' #'  - hypopt_vis: Hyperparameter optimization plot.
#' #' @keywords internal
#' xgboost_hypopt <- function(train_data,
#'                            test_data,
#'                            variable = "Disease",
#'                            case,
#'                            cor_threshold = 0.9,
#'                            normalize = TRUE,
#'                            cv_sets = 5,
#'                            grid_size = 10,
#'                            ncores = 4,
#'                            hypopt_vis = TRUE,
#'                            exclude_cols = NULL,
#'                            seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'   if (ncores > 1) {
#'     doParallel::registerDoParallel(cores = ncores)
#'   }
#'
#'   # Prepare train data and create cross-validation sets with binary classifier
#'   train_set <- train_data |>
#'     dplyr::mutate(!!Variable := ifelse(!!Variable == case, 1, 0)) |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols)) |>
#'     dplyr::mutate(dplyr::across(tidyselect::where(is.character) & !dplyr::all_of("DAid"), as.factor))  # Solve bug of Gower distance function
#'
#'   train_folds <- rsample::vfold_cv(train_set, v = cv_sets, strata = !!Variable)
#'
#'   test_set <- test_data |>
#'     dplyr::mutate(!!Variable := ifelse(!!Variable == case, 1, 0)) |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols)) |>
#'     dplyr::mutate(dplyr::across(tidyselect::where(is.character) & !dplyr::all_of("DAid"), as.factor))
#'
#'   formula <- stats::as.formula(paste(variable, "~ ."))
#'
#'   xgboost_rec <- recipes::recipe(formula, data = train_set) |>
#'     recipes::update_role(DAid, new_role = "id")
#'
#'   if (isTRUE(normalize)) {
#'     xgboost_rec <- xgboost_rec |> recipes::step_normalize(recipes::all_numeric())
#'   }
#'
#'   xgboost_rec <- xgboost_rec |>
#'     recipes::step_nzv(recipes::all_numeric()) |>
#'     recipes::step_corr(recipes::all_numeric(), threshold = cor_threshold) |>
#'     recipes::step_impute_knn(recipes::all_numeric())
#'
#'   xgboost_spec <- parsnip::boost_tree(trees = 1000,
#'                                       tree_depth = tune::tune(),
#'                                       min_n = tune::tune(),
#'                                       loss_reduction = tune::tune(),
#'                                       sample_size = tune::tune(),
#'                                       mtry = tune::tune(),
#'                                       learn_rate = tune::tune()) |>
#'     parsnip::set_engine("xgboost") |>
#'     parsnip::set_mode("classification")
#'
#'   disease_pred <- train_set |> dplyr::select(-dplyr::any_of(c("Disease", "DAid", "Sex", "Age", "BMI")))
#'
#'   xgboost_wf <- workflows::workflow() |>
#'     workflows::add_model(xgboost_spec) |>
#'     workflows::add_recipe(xgboost_rec)
#'
#'   xgboost_grid <- xgboost_wf |>
#'     workflows::extract_parameter_set_dials() |>
#'     dials::finalize(disease_pred) |>
#'     dials::grid_space_filling(size = grid_size)
#'
#'   roc_res <- yardstick::metric_set(yardstick::roc_auc)
#'
#'   set.seed(seed)
#'   ctrl <- tune::control_grid(save_pred = TRUE, parallel_over = "everything")
#'   xgboost_tune <- xgboost_wf |>
#'     tune::tune_grid(
#'       train_folds,
#'       grid = xgboost_grid,
#'       control = ctrl,
#'       metrics = roc_res
#'     )
#'
#'   if (hypopt_vis) {
#'     hypopt_plot <- xgboost_tune |>
#'       tune::collect_metrics() |>
#'       dplyr::filter(.metric == "roc_auc") |>
#'       dplyr::select(mean, mtry:sample_size) |>
#'       tidyr::pivot_longer(mtry:sample_size,
#'                           values_to = "value",
#'                           names_to = "parameter") |>
#'       ggplot2::ggplot(ggplot2::aes(value, mean, color = parameter)) +
#'       ggplot2::geom_point(alpha = 0.8, show.legend = FALSE) +
#'       ggplot2::facet_wrap(~parameter, scales = "free_x") +
#'       ggplot2::labs(x = NULL, y = "AUC")
#'
#'     return(list("xgboost_tune" = xgboost_tune,
#'                 "xgboost_wf" = xgboost_wf,
#'                 "train_set" = train_set,
#'                 "test_set" = test_set,
#'                 "hypopt_vis" = hypopt_plot))
#'   }
#'
#'   return(list("xgboost_tune" = xgboost_tune,
#'               "xgboost_wf" = xgboost_wf,
#'               "train_set" = train_set,
#'               "test_set" = test_set))
#' }
#'
#'
#' #' Hyperparameter optimization for elastic net multiclassification model
#' #'
#' #' `elnet_hypopt_multi()` tunes an elastic net model and performs hyperparameter optimization.
#' #' It uses the glmnet engine for multinomial regression and tunes either only penalty (Lasso or Ridge) or
#' #' both penalty and mixture (Elastic Regression). For the hyperparameter optimization, it
#' #' uses the `grid_space_filling()` function from the dials package.
#' #'
#' #' @param train_data Training data set from `make_groups()`.
#' #' @param test_data Testing data set from `make_groups()`.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param type Type of regularization. Default is "lasso". Other options are "ridge" and "elnet".
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the hyperparameter optimization grid. Default is 10.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned. Default is NULL.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with five elements:
#' #'  - elnet_tune: Hyperparameter optimization results.
#' #'  - elnet_wf: Workflow object.
#' #'  - train_set: Training set.
#' #'  - test_set: Testing set.
#' #'  - hyperopt_vis: Hyperparameter optimization plot.
#' #'
#' #' @keywords internal
#' elnet_hypopt_multi <- function(train_data,
#'                                test_data,
#'                                variable = "Disease",
#'                                type = "lasso",
#'                                cor_threshold = 0.9,
#'                                cv_sets = 5,
#'                                grid_size = 10,
#'                                ncores = 4,
#'                                hypopt_vis = TRUE,
#'                                exclude_cols = NULL,
#'                                seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   if (ncores > 1) {
#'     doParallel::registerDoParallel(cores = ncores)
#'   }
#'
#'   # Prepare train data and create cross-validation sets with binary classifier
#'   train_set <- train_data |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols))
#'   train_folds <- rsample::vfold_cv(train_set, v = cv_sets, strata = !!Variable)
#'
#'   test_set <- test_data |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols))
#'
#'   formula <- stats::as.formula(paste(variable, "~ ."))
#'
#'   elnet_rec <- recipes::recipe(formula, data = train_set) |>
#'     recipes::update_role(DAid, new_role = "id") |>
#'     recipes::step_normalize(recipes::all_numeric()) |>
#'     recipes::step_nzv(recipes::all_numeric()) |>
#'     recipes::step_corr(recipes::all_numeric(), threshold = cor_threshold) |>
#'     recipes::step_impute_knn(recipes::all_numeric())
#'
#'   if (type == "elnet") {
#'     elnet_spec <- parsnip::multinom_reg(
#'       penalty = tune::tune(),  # lambda
#'       mixture = tune::tune()  # alpha
#'     ) |>
#'       parsnip::set_engine("glmnet")
#'   } else if (type == "lasso") {
#'     elnet_spec <- parsnip::multinom_reg(
#'       penalty = tune::tune(),
#'       mixture = 1
#'     ) |>
#'       parsnip::set_engine("glmnet")
#'   } else if (type == "ridge") {
#'     elnet_spec <- parsnip::multinom_reg(
#'       penalty = tune::tune(),
#'       mixture = 0
#'     ) |>
#'       parsnip::set_engine("glmnet")
#'   }
#'
#'   elnet_wf <- workflows::workflow() |>
#'     workflows::add_model(elnet_spec) |>
#'     workflows::add_recipe(elnet_rec)
#'
#'   elnet_grid <- elnet_wf |>
#'     workflows::extract_parameter_set_dials() |>
#'     dials::grid_space_filling(size = grid_size)
#'
#'   roc_res <- yardstick::metric_set(yardstick::roc_auc)
#'
#'   set.seed(seed)
#'   ctrl <- tune::control_grid(save_pred = TRUE, parallel_over = "everything")
#'   elnet_tune <- elnet_wf |>
#'     tune::tune_grid(
#'       train_folds,
#'       grid = elnet_grid,
#'       control = ctrl,
#'       metrics = roc_res
#'     )
#'
#'   if (hypopt_vis) {
#'     if (type == "elnet") {
#'       hypopt_plot <- vis_hypopt(elnet_tune, "penalty", "mixture", "Multiclass")
#'     } else {
#'       hypopt_plot <- vis_hypopt(elnet_tune, "penalty", NULL, "Multiclass")
#'     }
#'     return(list("elnet_tune" = elnet_tune,
#'                 "elnet_wf" = elnet_wf,
#'                 "train_set" = train_set,
#'                 "test_set" = test_set,
#'                 "hypopt_vis" = hypopt_plot))
#'   }
#'
#'   return(list("elnet_tune" = elnet_tune,
#'               "elnet_wf" = elnet_wf,
#'               "train_set" = train_set,
#'               "test_set" = test_set))
#' }
#'
#'
#' #' Hyperparameter optimization for random forest multiclassification model
#' #'
#' #' `rf_hypopt_multi()` performs hyperparameter optimization for random forest models.
#' #' It uses the ranger engine for multinomial regression and tunes the number of
#' #' predictors that will be randomly sampled at each split when creating the
#' #' tree models, as well as the minimum number of data points in a node that are
#' #' required for the node to be split further. For the hyperparameter optimization,
#' #' it uses the `grid_space_filling()` function from the dials package.
#' #'
#' #' @param train_data Training data set from `make_groups()`.
#' #' @param test_data Testing data set from `make_groups()`.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param normalize Whether to normalize numeric data to have a standard deviation of one and a mean of zero. Default is TRUE.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the grid for hyperparameter optimization. Default is 10.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned. Default is NULL.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with five elements:
#' #'  - rf_tune: Hyperparameter optimization results.
#' #'  - rf_wf: Workflow object.
#' #'  - train_set: Training set.
#' #'  - test_set: Testing set.
#' #'  - hyperopt_vis: Hyperparameter optimization plot.
#' #' @keywords internal
#' rf_hypopt_multi <- function(train_data,
#'                             test_data,
#'                             variable = "Disease",
#'                             cor_threshold = 0.9,
#'                             normalize = TRUE,
#'                             cv_sets = 5,
#'                             grid_size = 10,
#'                             ncores = 4,
#'                             hypopt_vis = TRUE,
#'                             exclude_cols = NULL,
#'                             seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   if (ncores > 1) {
#'     doParallel::registerDoParallel(cores = ncores)
#'   }
#'
#'   # Prepare train data and create cross-validation sets with binary classifier
#'   train_set <- train_data |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols)) |>
#'     dplyr::mutate(dplyr::across(tidyselect::where(is.character) & !dplyr::all_of("DAid"), as.factor))  # Solve bug of Gower distance function
#'
#'   train_folds <- rsample::vfold_cv(train_set, v = cv_sets, strata = !!Variable)
#'
#'   test_set <- test_data |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols)) |>
#'     dplyr::mutate(dplyr::across(tidyselect::where(is.character) & !dplyr::all_of("DAid"), as.factor))
#'
#'   formula <- stats::as.formula(paste(variable, "~ ."))
#'
#'   rf_rec <- recipes::recipe(formula, data = train_set) |>
#'     recipes::update_role(DAid, new_role = "id")
#'
#'   if (isTRUE(normalize)) {
#'     rf_rec <- rf_rec |> recipes::step_normalize(recipes::all_numeric())
#'   }
#'
#'   rf_rec <- rf_rec |>
#'     recipes::step_nzv(recipes::all_numeric()) |>
#'     recipes::step_corr(recipes::all_numeric(), threshold = cor_threshold) |>
#'     recipes::step_impute_knn(recipes::all_numeric())
#'
#'   rf_spec <- parsnip::rand_forest(
#'     trees = 1000,
#'     mtry = tune::tune(),
#'     min_n = tune::tune()
#'   ) |>
#'     parsnip::set_mode("classification") |>
#'     parsnip::set_engine("ranger", importance = "permutation")
#'
#'   disease_pred <- train_set |> dplyr::select(-dplyr::any_of(c("Disease", "DAid", "Sex", "Age", "BMI", variable)))
#'
#'   rf_wf <- workflows::workflow() |>
#'     workflows::add_model(rf_spec) |>
#'     workflows::add_recipe(rf_rec)
#'
#'   rf_grid <- rf_wf |>
#'     workflows::extract_parameter_set_dials() |>
#'     dials::finalize(disease_pred) |>
#'     dials::grid_space_filling(size = grid_size)
#'
#'   roc_res <- yardstick::metric_set(yardstick::roc_auc)
#'
#'   set.seed(seed)
#'   ctrl <- tune::control_grid(save_pred = TRUE, parallel_over = "everything")
#'   rf_tune <- rf_wf |>
#'     tune::tune_grid(
#'       train_folds,
#'       grid = rf_grid,
#'       control = ctrl,
#'       metrics = roc_res
#'     )
#'
#'   if (hypopt_vis) {
#'     hypopt_plot <- vis_hypopt(rf_tune, "min_n", "mtry", "Multiclass")
#'
#'     return(list("rf_tune" = rf_tune,
#'                 "rf_wf" = rf_wf,
#'                 "train_set" = train_set,
#'                 "test_set" = test_set,
#'                 "hypopt_vis" = hypopt_plot))
#'   }
#'
#'   return(list("rf_tune" = rf_tune,
#'               "rf_wf" = rf_wf,
#'               "train_set" = train_set,
#'               "test_set" = test_set))
#' }
#'
#'
#' #' Hyperparameter optimization for XGBoost multiclassification model
#' #'
#' #' `xgboost_hypopt_multi()` tunes an XGBoost model and performs hyperparameter optimization.
#' #' It uses the xgboost engine for multinomial regression and tunes the number of trees,
#' #' tree depth, minimum number of data points in a node, loss reduction, sample size,
#' #' and number of predictors randomly sampled at each split. For the hyperparameter optimization,
#' #' it uses the `grid_space_filling()` function from the dials package.
#' #'
#' #' @param train_data Training data set from `make_groups()`.
#' #' @param test_data Testing data set from `make_groups()`.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param normalize Whether to normalize numeric data to have a standard deviation of one and a mean of zero. Default is TRUE.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the grid for hyperparameter optimization. Default is 10.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned. Default is NULL.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with five elements:
#' #'  - xgboost_tune: Hyperparameter optimization results.
#' #'  - xgboost_wf: Workflow object.
#' #'  - train_set: Training set.
#' #'  - test_set: Testing set.
#' #'  - hypopt_vis: Hyperparameter optimization plot.
#' #' @keywords internal
#' xgboost_hypopt_multi <- function(train_data,
#'                                  test_data,
#'                                  variable = "Disease",
#'                                  cor_threshold = 0.9,
#'                                  normalize = TRUE,
#'                                  cv_sets = 5,
#'                                  grid_size = 10,
#'                                  ncores = 4,
#'                                  hypopt_vis = TRUE,
#'                                  exclude_cols = NULL,
#'                                  seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   if (ncores > 1) {
#'     doParallel::registerDoParallel(cores = ncores)
#'   }
#'
#'   # Prepare train data and create cross-validation sets with binary classifier
#'   train_set <- train_data |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols)) |>
#'     dplyr::mutate(dplyr::across(tidyselect::where(is.character) & !dplyr::all_of("DAid"), as.factor))  # Solve bug of Gower distance function
#'
#'   train_folds <- rsample::vfold_cv(train_set, v = cv_sets, strata = !!Variable)
#'
#'   test_set <- test_data |>
#'     dplyr::mutate(!!Variable := as.factor(!!Variable)) |>
#'     dplyr::select(-dplyr::any_of(exclude_cols)) |>
#'     dplyr::mutate(dplyr::across(tidyselect::where(is.character) & !dplyr::all_of("DAid"), as.factor))
#'
#'   formula <- stats::as.formula(paste(variable, "~ ."))
#'
#'   xgboost_rec <- recipes::recipe(formula, data = train_set) |>
#'     recipes::update_role(DAid, new_role = "id")
#'
#'   if (isTRUE(normalize)) {
#'     xgboost_rec <- xgboost_rec |> recipes::step_normalize(recipes::all_numeric())
#'   }
#'
#'   xgboost_rec <- xgboost_rec |>
#'     recipes::step_nzv(recipes::all_numeric()) |>
#'     recipes::step_corr(recipes::all_numeric(), threshold = cor_threshold) |>
#'     recipes::step_impute_knn(recipes::all_numeric())
#'
#'   xgboost_spec <- parsnip::boost_tree(trees = 1000,
#'                                       tree_depth = tune::tune(),
#'                                       min_n = tune::tune(),
#'                                       loss_reduction = tune::tune(),
#'                                       sample_size = tune::tune(),
#'                                       mtry = tune::tune(),
#'                                       learn_rate = tune::tune()) |>
#'     parsnip::set_engine("xgboost") |>
#'     parsnip::set_mode("classification")
#'
#'   disease_pred <- train_set |> dplyr::select(-dplyr::any_of(c("Disease", "DAid", "Sex", "Age", "BMI", variable)))
#'
#'   xgboost_wf <- workflows::workflow() |>
#'     workflows::add_model(xgboost_spec) |>
#'     workflows::add_recipe(xgboost_rec)
#'
#'   xgboost_grid <- xgboost_wf |>
#'     workflows::extract_parameter_set_dials() |>
#'     dials::finalize(disease_pred) |>
#'     dials::grid_space_filling(size = grid_size)
#'
#'   roc_res <- yardstick::metric_set(yardstick::roc_auc)
#'
#'   set.seed(seed)
#'   ctrl <- tune::control_grid(save_pred = TRUE, parallel_over = "everything")
#'   xgboost_tune <- xgboost_wf |>
#'     tune::tune_grid(
#'       train_folds,
#'       grid = xgboost_grid,
#'       control = ctrl,
#'       metrics = roc_res
#'     )
#'
#'   if (hypopt_vis) {
#'     hypopt_plot <- xgboost_tune |>
#'       tune::collect_metrics() |>
#'       dplyr::filter(.metric == "roc_auc") |>
#'       dplyr::select(mean, mtry:sample_size) |>
#'       tidyr::pivot_longer(mtry:sample_size,
#'                           values_to = "value",
#'                           names_to = "parameter") |>
#'       ggplot2::ggplot(ggplot2::aes(value, mean, color = parameter)) +
#'       ggplot2::geom_point(alpha = 0.8, show.legend = FALSE) +
#'       ggplot2::facet_wrap(~parameter, scales = "free_x") +
#'       ggplot2::labs(x = NULL, y = "AUC")
#'
#'     return(list("xgboost_tune" = xgboost_tune,
#'                 "xgboost_wf" = xgboost_wf,
#'                 "train_set" = train_set,
#'                 "test_set" = test_set,
#'                 "hypopt_vis" = hypopt_plot))
#'   }
#'
#'   return(list("xgboost_tune" = xgboost_tune,
#'               "xgboost_wf" = xgboost_wf,
#'               "train_set" = train_set,
#'               "test_set" = test_set))
#' }
#'
#'
#' #' Fit the best model
#' #'
#' #' `finalfit()` fits the model that performed the best in hyperparameter optimization.
#' #'
#' #' @param train_set Training set.
#' #' @param tune_res Hyperparameter optimization results.
#' #' @param wf Workflow object.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with three elements:
#' #'  - final_elnet: Final model.
#' #'  - best_elnet: Best hyperparameters from hyperparameter optimization.
#' #'  - final_wf: Final workflow object.
#' #' @keywords internal
#' finalfit <- function(train_set,
#'                      tune_res,
#'                      wf,
#'                      seed = 123) {
#'
#'   best <- tune_res |>
#'     tune::select_best(metric = "roc_auc") |>
#'     dplyr::select(-.config)
#'
#'   final_wf <- tune::finalize_workflow(wf, best)
#'
#'   final <- final_wf |>
#'     parsnip::fit(train_set)
#'
#'   return(list("final" = final,
#'               "best" = best,
#'               "final_wf" = final_wf))
#' }
#'
#'
#' #' Test the best model
#' #'
#' #' `testfit()` tests the best model on the test set and calculate metrics.
#' #' It calculates the accuracy, sensitivity, specificity, AUC, confusion matrix,
#' #' and ROC curve.
#' #'
#' #' @param train_set Training set.
#' #' @param test_set Testing set.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param case Case to predict.
#' #' @param finalfit_res Results from `elnet_finalfit()`.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned. Default is NULL.
#' #' @param type Type of regularization. Default is "lasso". Other options are "ridge" and "elnet".
#' #' @param seed Seed for reproducibility. Default is 123.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #'
#' #' @return A list with two elements:
#' #'  - metrics: A list with 5 metrics:
#' #'   - accuracy: Accuracy of the model.
#' #'   - sensitivity: Sensitivity of the model.
#' #'   - specificity: Specificity of the model.
#' #'   - auc: AUC of the model.
#' #'   - conf_matrix: Confusion matrix of the model.
#' #'   - roc_curve: ROC curve of the model.
#' #'  - mixture: Mixture of lasso and ridge regularization.
#' #'
#' #' @details In random forest models, mixture is returned as NULL.
#' #' @keywords internal
#' testfit <- function(train_set,
#'                     test_set,
#'                     variable = "Disease",
#'                     case,
#'                     finalfit_res,
#'                     exclude_cols = NULL,
#'                     type = "lasso",
#'                     seed = 123,
#'                     palette = NULL) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   set.seed(seed)
#'   splits <- rsample::make_splits(train_set, test_set)
#'
#'   preds <- tune::last_fit(finalfit_res$final_wf,
#'                           splits,
#'                           metrics = yardstick::metric_set(yardstick::roc_auc))
#'   res <- stats::predict(finalfit_res$final, new_data = test_set)
#'
#'   res <- dplyr::bind_cols(res, test_set |> dplyr::select(!!Variable))
#'
#'   accuracy <- res |>
#'     yardstick::accuracy(!!Variable, .pred_class)
#'
#'   sensitivity <- res |>
#'     yardstick::sensitivity(!!Variable, .pred_class)
#'
#'   specificity <- res |>
#'     yardstick::specificity(!!Variable, .pred_class)
#'
#'   if (is.null(names(palette)) && !is.null(palette)) {
#'     disease_color <- get_hpa_palettes()[[palette]][[case]]
#'   } else if (!is.null(palette)) {
#'     disease_color <- palette
#'   } else {
#'     disease_color <- "black"
#'   }
#'
#'   selected_point <- tibble::tibble(x = 1 - specificity$.estimate,
#'                                    y = sensitivity$.estimate)
#'   roc <- preds |>
#'     tune::collect_predictions(summarize = F) |>
#'     yardstick::roc_curve(truth = !!Variable, .pred_0) |>
#'     ggplot2::ggplot(ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
#'     ggplot2::geom_path(colour = disease_color, linewidth = 2) +
#'     ggplot2::geom_point(data = selected_point, ggplot2::aes(x = x, y = y), size = 2, shape = 4, colour = "black") +
#'     ggplot2::geom_abline(lty = 3) +
#'     ggplot2::coord_equal() +
#'     theme_hpa()
#'
#'   auc <- preds |>
#'     tune::collect_metrics()
#'
#'   cm <- res |>
#'     yardstick::conf_mat(!!Variable, .pred_class)
#'
#'   if (type == "elnet") {
#'     mixture <- finalfit_res$best$mixture
#'   } else if (type == "lasso") {
#'     mixture <- 1
#'   } else if (type == "ridge") {
#'     mixture <- 0
#'   } else {
#'     mixture <- NA
#'   }
#'
#'   return(list("metrics" = list("accuracy" = round(accuracy$.estimate, 2),
#'                               "sensitivity" = round(sensitivity$.estimate, 2),
#'                               "specificity" = round(specificity$.estimate, 2),
#'                               "auc" = round(auc$.estimate, 2),
#'                               "conf_matrix" = cm,
#'                               "roc_curve" = roc),
#'               "mixture" = mixture))
#' }
#'
#'
#' #' Create subtitle for variable importance plot
#' #'
#' #' `generate_subtitle()` generates a subtitle for the variable importance plot.
#' #'
#' #' @param features A tibble with features and their model importance.
#' #' @param accuracy Accuracy of the model.
#' #' @param sensitivity Sensitivity of the model.
#' #' @param specificity Specificity of the model.
#' #' @param auc AUC of the model.
#' #' @param mixture Mixture of lasso and ridge regularization. In random forest models it is NULL.
#' #' @param subtitle Vector of subtitle elements to include in the plot.
#' #'
#' #' @return The plot subtitle as character vector.
#' #' @keywords internal
#' generate_subtitle <- function(features,
#'                               accuracy,
#'                               sensitivity,
#'                               specificity,
#'                               auc,
#'                               mixture,
#'                               subtitle = c("accuracy",
#'                                            "sensitivity",
#'                                            "specificity",
#'                                            "auc",
#'                                            "features",
#'                                            "top-features",
#'                                            "mixture")) {
#'
#'   subtitle_parts <- c()
#'
#'   if ("accuracy" %in% subtitle) {
#'     subtitle_parts <- c(subtitle_parts, paste0('accuracy = ', round(accuracy, 2), '    '))
#'   }
#'
#'   if ("sensitivity" %in% subtitle) {
#'     subtitle_parts <- c(subtitle_parts, paste0('sensitivity = ', round(sensitivity, 2), '    '))
#'   }
#'
#'   if ("specificity" %in% subtitle) {
#'     subtitle_parts <- c(subtitle_parts, paste0('specificity = ', round(specificity, 2), '    '))
#'   }
#'
#'   if ("auc" %in% subtitle) {
#'     subtitle_parts <- c(subtitle_parts, paste0('AUC = ', round(auc, 2), '    '))
#'   }
#'
#'   if (length(subtitle_parts) > 0) {
#'     subtitle_parts <- c(subtitle_parts, '\n')
#'   }
#'
#'   if ("features" %in% subtitle) {
#'     subtitle_parts <- c(subtitle_parts, paste0('Features = ', nrow(features), '    '))
#'   }
#'
#'   if ("top-features" %in% subtitle) {
#'     subtitle_parts <- c(subtitle_parts, paste0('top-features = ',
#'                                                nrow(features |> dplyr::filter(Scaled_Importance >= 50)),
#'                                                '    '))
#'   }
#'
#'   if ("mixture" %in% subtitle) {
#'     subtitle_parts <- c(subtitle_parts, paste0('Lasso/Ridge ratio = ', round(mixture, 2), '    '))
#'   }
#'
#'   subtitle <- paste(subtitle_parts, collapse = '')
#'
#'   return(subtitle)
#' }
#'
#'
#' #' Plot feature variable importance
#' #'
#' #' `plot_var_imp()` collects the features and their model importance.
#' #' It scales their importance and plots it against them.
#' #'
#' #' @param finalfit_res Results from `finalfit()`.
#' #' @param case Case to predict.
#' #' @param accuracy Accuracy of the model.
#' #' @param sensitivity Sensitivity of the model.
#' #' @param specificity Specificity of the model.
#' #' @param auc AUC of the model.
#' #' @param mixture Mixture of lasso and ridge regularization.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #' @param vline Whether to add a vertical line at 50% importance. Default is TRUE.
#' #' @param subtitle Vector of subtitle elements to include in the plot.
#' #' @param yaxis_names Whether to add y-axis names to the plot. Default is FALSE.
#' #'
#' #' @return A list with two elements:
#' #'  - features: A tibble with features and their model importance.
#' #'  - var_imp_plot: Variable importance plot.
#' #' @keywords internal
#' plot_var_imp <- function (finalfit_res,
#'                           case,
#'                           accuracy,
#'                           sensitivity,
#'                           specificity,
#'                           auc,
#'                           mixture,
#'                           palette = NULL,
#'                           vline = TRUE,
#'                           subtitle = c("accuracy",
#'                                        "sensitivity",
#'                                        "specificity",
#'                                        "auc",
#'                                        "features",
#'                                        "top-features",
#'                                        "mixture"),
#'                           yaxis_names = FALSE) {
#'
#'   features <- finalfit_res$final |>
#'     workflows::extract_fit_parsnip() |>
#'     vip::vi() |>
#'     dplyr::mutate(
#'       Importance = abs(Importance),
#'       Variable = forcats::fct_reorder(Variable, Importance)
#'     ) |>
#'     dplyr::arrange(dplyr::desc(Importance)) |>
#'     dplyr::mutate(Scaled_Importance = scales::rescale(Importance, to = c(0, 100))) |>
#'     dplyr::filter(Scaled_Importance > 0)
#'
#'   subtitle_text <- generate_subtitle(features, accuracy, sensitivity, specificity, auc, mixture, subtitle)
#'
#'   # Prepare palettes
#'   pals <- get_hpa_palettes()
#'   if (!is.null(palette) && is.null(names(palette))) {
#'     pal <- pals[palette]
#'     pal <- unlist(pals[[palette]])
#'   } else if (!is.null(palette)) {
#'     pal <- palette
#'   } else {
#'     pal <- "red3"
#'   }
#'
#'   var_imp_plot <- features |>
#'     ggplot2::ggplot(ggplot2::aes(x = Scaled_Importance, y = Variable)) +
#'     ggplot2::geom_col(ggplot2::aes(fill = ifelse(Scaled_Importance > 50, case, NA))) +
#'     ggplot2::labs(y = NULL) +
#'     ggplot2::scale_x_continuous(breaks = c(0, 100), expand = c(0, 0)) +  # Keep x-axis tick labels at 0 and 100
#'     ggplot2::scale_fill_manual(values = pal, na.value = "grey50") +
#'     ggplot2::ggtitle(label = paste0(case, ''),
#'                      subtitle = subtitle_text) +
#'     ggplot2::xlab('Importance') +
#'     ggplot2::ylab('Features') +
#'     ggplot2::theme_classic() +
#'     ggplot2::theme(legend.position = "none")
#'
#'   if (isFALSE(yaxis_names)) {
#'     var_imp_plot <- var_imp_plot +
#'       ggplot2::theme(axis.text.y = ggplot2::element_blank(),
#'                      axis.ticks.y = ggplot2::element_blank())
#'   }
#'
#'   if (isTRUE(vline)) {
#'     var_imp_plot <- var_imp_plot +
#'       ggplot2::geom_vline(xintercept = 50, linetype = 'dashed', color = 'black')
#'   }
#'
#'   return(list("features" = features,
#'               "var_imp_plot" = var_imp_plot))
#' }
#'
#'
#' #' Fit logistic regression model for single predictors
#' #'
#' #' `lreg_fit()` fits a logistic regression model for a single predictor and calculates
#' #' the ROC AUC, accuracy, sensitivity, and specificity. It also performs cross-validation
#' #' and plots the ROC curve.
#' #'
#' #' @param olink_data Olink data.
#' #' @param metadata Metadata.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param case The case group.
#' #' @param control The control groups.
#' #' @param wide Whether the data is wide format. Default is TRUE.
#' #' @param strata Whether to stratify the data. Default is TRUE.
#' #' @param balance_groups Whether to balance the groups. Default is TRUE.
#' #' @param only_female Vector of diseases.
#' #' @param only_male Vector of diseases.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned.
#' #' @param ratio Ratio of training data to test data. Default is 0.75.
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param normalize Whether to normalize numeric data to have a standard deviation of one and a mean of zero. Default is TRUE.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #' @param points Whether to add points to the boxplot. Default is TRUE.
#' #' @param boxplot_xaxis_names Whether to add x-axis names to the boxplot. Default is FALSE.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with two elements:
#' #' \itemize{
#' #' \item fit_res: A list with 4 elements:
#' #' \itemize{
#' #'  \item lreg_wf: Workflow object.
#' #'  \item train_set: Training set.
#' #'  \item test_set: Testing set.
#' #'  \item final: Fitted model.
#' #'  }
#' #' \item metrics: A list with the model metrics:
#' #' \itemize{
#' #'  \item accuracy: Accuracy of the model.
#' #'  \item sensitivity: Sensitivity of the model.
#' #'  \item specificity: Specificity of the model.
#' #'  \item auc: AUC of the model.
#' #'  \item conf_matrix: Confusion matrix of the model.
#' #'  \item roc_curve: ROC curve of the model.
#' #'  }
#' #' }
#' #' @export
#' #'
#' #' @details
#' #' This model should be used with data that contain a single predictor. If the data
#' #' contains multiple predictors, prefer using the `do_rreg()` or `do_rf()` functions.
#' #'
#' #' @examples
#' #' # Data with single predictor
#' #' test_data <- example_data |> dplyr::filter(Assay == "ADA")
#' #'
#' #' # Run model
#' #' do_lreg(test_data,
#' #'         example_metadata,
#' #'         variable = "Disease",
#' #'         case = "AML",
#' #'         control = "CLL",
#' #'         wide = FALSE,
#' #'         ncores = 1,
#' #'         palette = "cancers12")
#' do_lreg <- function(olink_data,
#'                     metadata,
#'                     variable = "Disease",
#'                     case,
#'                     control,
#'                     wide = TRUE,
#'                     strata = TRUE,
#'                     balance_groups = TRUE,
#'                     only_female = NULL,
#'                     only_male = NULL,
#'                     exclude_cols = "Sex",
#'                     ratio = 0.75,
#'                     cor_threshold = 0.9,
#'                     normalize = TRUE,
#'                     cv_sets = 5,
#'                     ncores = 4,
#'                     palette = NULL,
#'                     points = TRUE,
#'                     boxplot_xaxis_names = FALSE,
#'                     seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   # Prepare datasets
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   join_data <- wide_data |>
#'     dplyr::left_join(metadata |> dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex", variable)))) |>
#'     dplyr::filter(!!Variable %in% c(case, control))
#'
#'   # Prepare sets and groups
#'   data_split <- split_data(join_data, variable, strata, ratio, seed)
#'   if (isTRUE(balance_groups)) {
#'     train_data <- make_groups(data_split$train_set,
#'                               variable,
#'                               case,
#'                               c(case, control),
#'                               only_female,
#'                               only_male,
#'                               seed)
#'     test_data <- make_groups(data_split$test_set,
#'                              variable,
#'                              case,
#'                              c(case, control),
#'                              only_female,
#'                              only_male,
#'                              seed)
#'   } else {
#'     train_data <- data_split$train_set
#'     test_data <- data_split$test_set
#'   }
#'   message("Sets and groups are ready. Model fitting is starting...")
#'   lreg_res <- lreg_fit(train_data,
#'                        test_data,
#'                        variable,
#'                        case,
#'                        cor_threshold,
#'                        cv_sets,
#'                        ncores,
#'                        exclude_cols,
#'                        palette,
#'                        seed)
#'
#'   protein <- wide_data |> dplyr::select(-DAid) |> names()
#'   boxplot_res <- plot_protein_boxplot(join_data,
#'                                       variable,
#'                                       protein,
#'                                       case,
#'                                       points,
#'                                       xaxis_names = boxplot_xaxis_names,
#'                                       palette)
#'
#'   lreg_res <- c(lreg_res, list("boxplot_res" = boxplot_res))
#'
#'   return(lreg_res)
#'
#' }
#'
#'
#' #' Regularized classification model pipeline
#' #'
#' #' `do_rreg()` runs the regularized classification model pipeline. It splits the
#' #' data into training and test sets, creates class-balanced case-control groups,
#' #' and fits the model. It also performs hyperparameter optimization, fits the best
#' #' model, tests it, and plots useful the feature variable importance.
#' #'
#' #' @param olink_data Olink data.
#' #' @param metadata Metadata.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param case The case group.
#' #' @param control The control groups.
#' #' @param wide Whether the data is wide format. Default is TRUE.
#' #' @param strata Whether to stratify the data. Default is TRUE.
#' #' @param balance_groups Whether to balance the groups. Default is TRUE.
#' #' @param only_female Vector of diseases that are female specific. Default is NULL.
#' #' @param only_male Vector of diseases that are male specific. Default is NULL.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned. Default is "Sex".
#' #' @param ratio Ratio of training data to test data. Default is 0.75.
#' #' @param type Type of regularization. Default is "lasso". Other options are "ridge" and "elnet".
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the hyperparameter optimization grid. Default is 10.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #' @param vline Whether to add a vertical line at 50% importance. Default is TRUE.
#' #' @param subtitle Vector of subtitle elements to include in the plot. Default is a list with all.
#' #' @param varimp_yaxis_names Whether to add y-axis names to the variable importance plot. Default is FALSE.
#' #' @param nfeatures Number of top features to include in the boxplot. Default is 9.
#' #' @param points Whether to add points to the boxplot. Default is TRUE.
#' #' @param boxplot_xaxis_names Whether to add x-axis names to the boxplot. Default is FALSE.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with results for each disease. The list contains:
#' #'  - hypopt_res: Hyperparameter optimization results.
#' #'  - finalfit_res: Final model fitting results.
#' #'  - testfit_res: Test model fitting results.
#' #'  - var_imp_res: Variable importance results.
#' #' @export
#' #'
#' #' @details If the data contain missing values, KNN imputation will be applied.
#' #' If no check for feature correlation is preferred, set `cor_threshold` to 1.
#' #'
#' #' @examples
#' #' do_rreg(example_data,
#' #'         example_metadata,
#' #'         case = "AML",
#' #'         control = c("CLL", "MYEL"),
#' #'         balance_groups = TRUE,
#' #'         wide = FALSE,
#' #'         type = "elnet",
#' #'         palette = "cancers12",
#' #'         cv_sets = 5,
#' #'         grid_size = 10,
#' #'         ncores = 1)
#' do_rreg <- function(olink_data,
#'                     metadata,
#'                     variable = "Disease",
#'                     case,
#'                     control,
#'                     wide = TRUE,
#'                     strata = TRUE,
#'                     balance_groups = TRUE,
#'                     only_female = NULL,
#'                     only_male = NULL,
#'                     exclude_cols = "Sex",
#'                     ratio = 0.75,
#'                     type = "lasso",
#'                     cor_threshold = 0.9,
#'                     cv_sets = 5,
#'                     grid_size = 10,
#'                     ncores = 4,
#'                     hypopt_vis = TRUE,
#'                     palette = NULL,
#'                     vline = TRUE,
#'                     subtitle = c("accuracy",
#'                                  "sensitivity",
#'                                  "specificity",
#'                                  "auc",
#'                                  "features",
#'                                  "top-features",
#'                                  "mixture"),
#'                     varimp_yaxis_names = FALSE,
#'                     nfeatures = 9,
#'                     points = TRUE,
#'                     boxplot_xaxis_names = FALSE,
#'                     seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   # Prepare datasets
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   join_data <- wide_data |>
#'     dplyr::left_join(metadata |> dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex", variable)))) |>
#'     dplyr::filter(!!Variable %in% c(case, control))
#'
#'   # Prepare sets and groups
#'   data_split <- split_data(join_data, variable, strata, ratio, seed)
#'   if (isTRUE(balance_groups)) {
#'     train_list <- make_groups(data_split$train_set,
#'                               variable,
#'                               case,
#'                               c(case, control),
#'                               only_female,
#'                               only_male,
#'                               seed)
#'     test_list <- make_groups(data_split$test_set,
#'                              variable,
#'                              case,
#'                              c(case, control),
#'                              only_female,
#'                              only_male,
#'                              seed)
#'   } else {
#'     train_list <- data_split$train_set
#'     test_list <- data_split$test_set
#'   }
#'   message("Sets and groups are ready. Model fitting is starting...")
#'
#'   # Run model
#'   message(paste0("Classification model for ", case, " as case is starting..."))
#'   hypopt_res <- elnet_hypopt(train_list,
#'                              test_list,
#'                              variable,
#'                              case,
#'                              type,
#'                              cor_threshold,
#'                              cv_sets,
#'                              grid_size,
#'                              ncores,
#'                              hypopt_vis,
#'                              exclude_cols,
#'                              seed)
#'
#'   finalfit_res <- finalfit(hypopt_res$train_set,
#'                            hypopt_res$elnet_tune,
#'                            hypopt_res$elnet_wf,
#'                            seed)
#'
#'   testfit_res <- testfit(hypopt_res$train_set,
#'                          hypopt_res$test_set,
#'                          variable,
#'                          case,
#'                          finalfit_res,
#'                          exclude_cols,
#'                          type,
#'                          seed,
#'                          palette)
#'
#'   var_imp_res <- plot_var_imp(finalfit_res,
#'                               case,
#'                               testfit_res$metrics$accuracy,
#'                               testfit_res$metrics$sensitivity,
#'                               testfit_res$metrics$specificity,
#'                               testfit_res$metrics$auc,
#'                               testfit_res$mixture,
#'                               palette = palette,
#'                               vline = vline,
#'                               subtitle = subtitle,
#'                               yaxis_names = varimp_yaxis_names)
#'
#'   top_features <- var_imp_res$features |>
#'     dplyr::arrange(dplyr::desc(Scaled_Importance)) |>
#'     dplyr::select(Variable) |>
#'     dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
#'     utils::head(nfeatures)
#'   proteins <- top_features[["Variable"]]
#'
#'   boxplot_res <- plot_protein_boxplot(join_data,
#'                                       variable,
#'                                       proteins,
#'                                       case,
#'                                       points,
#'                                       xaxis_names = boxplot_xaxis_names,
#'                                       palette = palette)
#'
#'   return(list("hypopt_res" = hypopt_res,
#'               "finalfit_res" = finalfit_res,
#'               "testfit_res" = testfit_res,
#'               "var_imp_res" = var_imp_res,
#'               "boxplot_res" = boxplot_res))
#' }
#'
#'
#' #' Random forest classification model pipeline
#' #'
#' #' `do_rf()` runs the random forest classification model pipeline. It splits the
#' #' data into training and test sets, creates class-balanced case-control groups,
#' #' and fits the model. It also performs hyperparameter optimization, fits the best
#' #' model, tests it, and plots useful the feature variable importance.
#' #'
#' #' @param olink_data Olink data.
#' #' @param metadata Metadata.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param case The case group.
#' #' @param control The control groups.
#' #' @param wide Whether the data is wide format. Default is TRUE.
#' #' @param strata Whether to stratify the data. Default is TRUE.
#' #' @param balance_groups Whether to balance the groups. Default is TRUE.
#' #' @param only_female Vector of diseases that are female specific. Default is NULL.
#' #' @param only_male Vector of diseases that are male specific. Default is NULL.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned. Default is "Sex".
#' #' @param ratio Ratio of training data to test data. Default is 0.75.
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param normalize Whether to normalize numeric data to have a standard deviation of one and a mean of zero. Default is TRUE.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the hyperparameter optimization grid. Default is 10.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #' @param vline Whether to add a vertical line at 50% importance. Default is TRUE.
#' #' @param subtitle Vector of subtitle elements to include in the plot. Default is a list with all.
#' #' @param varimp_yaxis_names Whether to add y-axis names to the plot. Default is FALSE.
#' #' @param nfeatures Number of top features to include in the boxplot. Default is 9.
#' #' @param points Whether to add points to the boxplot. Default is TRUE.
#' #' @param boxplot_xaxis_names Whether to add x-axis names to the boxplot. Default is FALSE.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with results for each disease. The list contains:
#' #'  - hypopt_res: Hyperparameter optimization results.
#' #'  - finalfit_res: Final model fitting results.
#' #'  - testfit_res: Test model fitting results.
#' #'  - var_imp_res: Variable importance results.
#' #' @export
#' #'
#' #' @details If the data contain missing values, KNN imputation will be applied.
#' #' If no check for feature correlation is preferred, set `cor_threshold` to 1.
#' #'
#' #' @examples
#' #' do_rf(example_data,
#' #'       example_metadata,
#' #'       case = "AML",
#' #'       control = c("CLL", "MYEL"),
#' #'       balance_groups = TRUE,
#' #'       wide = FALSE,
#' #'       palette = "cancers12",
#' #'       cv_sets = 5,
#' #'       grid_size = 10,
#' #'       ncores = 1)
#' do_rf <- function(olink_data,
#'                   metadata,
#'                   variable = "Disease",
#'                   case,
#'                   control,
#'                   wide = TRUE,
#'                   strata = TRUE,
#'                   balance_groups = TRUE,
#'                   only_female = NULL,
#'                   only_male = NULL,
#'                   exclude_cols = "Sex",
#'                   ratio = 0.75,
#'                   cor_threshold = 0.9,
#'                   normalize = TRUE,
#'                   cv_sets = 5,
#'                   grid_size = 10,
#'                   ncores = 4,
#'                   hypopt_vis = TRUE,
#'                   palette = NULL,
#'                   vline = TRUE,
#'                   subtitle = c("accuracy",
#'                                "sensitivity",
#'                                "specificity",
#'                                "auc",
#'                                "features",
#'                                "top-features"),
#'                   varimp_yaxis_names = FALSE,
#'                   nfeatures = 9,
#'                   points = TRUE,
#'                   boxplot_xaxis_names = FALSE,
#'                   seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   # Prepare datasets
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   join_data <- wide_data |>
#'     dplyr::left_join(metadata |> dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex", variable)))) |>
#'     dplyr::filter(!!Variable %in% c(case, control))
#'
#'   # Prepare sets and groups
#'   data_split <- split_data(join_data, variable, strata, ratio, seed)
#'   if (isTRUE(balance_groups)) {
#'     train_list <- make_groups(data_split$train_set,
#'                               variable,
#'                               case,
#'                               c(case, control),
#'                               only_female,
#'                               only_male,
#'                               seed)
#'     test_list <- make_groups(data_split$test_set,
#'                              variable,
#'                              case,
#'                              c(case, control),
#'                              only_female,
#'                              only_male,
#'                              seed)
#'   } else {
#'     train_list <- data_split$train_set
#'     test_list <- data_split$test_set
#'   }
#'   message("Sets and groups are ready. Model fitting is starting...")
#'
#'   # Run model
#'   message(paste0("Classification model for ", case, " as case is starting..."))
#'   hypopt_res <- rf_hypopt(train_list,
#'                           test_list,
#'                           variable,
#'                           case,
#'                           cor_threshold,
#'                           normalize,
#'                           cv_sets,
#'                           grid_size,
#'                           ncores,
#'                           hypopt_vis,
#'                           exclude_cols,
#'                           seed)
#'
#'   finalfit_res <- finalfit(hypopt_res$train_set,
#'                            hypopt_res$rf_tune,
#'                            hypopt_res$rf_wf,
#'                            seed)
#'
#'   testfit_res <- testfit(hypopt_res$train_set,
#'                          hypopt_res$test_set,
#'                          variable,
#'                          case,
#'                          finalfit_res,
#'                          exclude_cols,
#'                          type = "other",
#'                          seed,
#'                          palette)
#'
#'   var_imp_res <- plot_var_imp(finalfit_res,
#'                               case,
#'                               testfit_res$metrics$accuracy,
#'                               testfit_res$metrics$sensitivity,
#'                               testfit_res$metrics$specificity,
#'                               testfit_res$metrics$auc,
#'                               testfit_res$mixture,
#'                               palette = palette,
#'                               vline = vline,
#'                               subtitle = subtitle,
#'                               yaxis_names = varimp_yaxis_names)
#'
#'   top_features <- var_imp_res$features |>
#'     dplyr::arrange(dplyr::desc(Scaled_Importance)) |>
#'     dplyr::select(Variable) |>
#'     dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
#'     utils::head(nfeatures)
#'   proteins <- top_features[["Variable"]]
#'
#'   boxplot_res <- plot_protein_boxplot(join_data,
#'                                       variable,
#'                                       proteins,
#'                                       case,
#'                                       points,
#'                                       xaxis_names = boxplot_xaxis_names,
#'                                       palette)
#'
#'   return(list("hypopt_res" = hypopt_res,
#'               "finalfit_res" = finalfit_res,
#'               "testfit_res" = testfit_res,
#'               "var_imp_res" = var_imp_res,
#'               "boxplot_res" = boxplot_res))
#' }
#'
#'
#' #' XGBoost classification model pipeline
#' #'
#' #' `do_xgboost()` runs the XGBoost classification model pipeline. It splits the
#' #' data into training and test sets, creates class-balanced case-control groups,
#' #' and fits the model. It also performs hyperparameter optimization, fits the best
#' #' model, tests it, and plots useful the feature variable importance.
#' #'
#' #' @param olink_data Olink data.
#' #' @param metadata Metadata.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param case The case group.
#' #' @param control The control groups.
#' #' @param wide Whether the data is wide format. Default is TRUE.
#' #' @param strata Whether to stratify the data. Default is TRUE.
#' #' @param balance_groups Whether to balance the groups. Default is TRUE.
#' #' @param only_female Vector of diseases.
#' #' @param only_male Vector of diseases.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned.
#' #' @param ratio Ratio of training data to test data. Default is 0.75.
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param normalize Whether to normalize numeric data to have a standard deviation of one and a mean of zero. Default is TRUE.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the hyperparameter optimization grid. Default is 50.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #' @param vline Whether to add a vertical line at 50% importance. Default is TRUE.
#' #' @param subtitle Vector of subtitle elements to include in the plot. Default is a list with all.
#' #' @param varimp_yaxis_names Whether to add y-axis names to the plot. Default is FALSE.
#' #' @param nfeatures Number of top features to include in the boxplot. Default is 9.
#' #' @param points Whether to add points to the boxplot. Default is TRUE.
#' #' @param boxplot_xaxis_names Whether to add x-axis names to the boxplot. Default is FALSE.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with results for each disease. The list contains:
#' #'  - hypopt_res: Hyperparameter optimization results.
#' #'  - finalfit_res: Final model fitting results.
#' #'  - testfit_res: Test model fitting results.
#' #'  - var_imp_res: Variable importance results.
#' #'  - boxplot_res: Boxplot results.
#' #' @export
#' #'
#' #' @examples
#' #' do_xgboost(example_data,
#' #'            example_metadata,
#' #'            case = "AML",
#' #'            control = c("CLL", "MYEL"),
#' #'            balance_groups = TRUE,
#' #'            wide = FALSE,
#' #'            palette = "cancers12",
#' #'            cv_sets = 5,
#' #'            grid_size = 10,
#' #'            ncores = 1)
#' do_xgboost <- function(olink_data,
#'                        metadata,
#'                        variable = "Disease",
#'                        case,
#'                        control,
#'                        wide = TRUE,
#'                        strata = TRUE,
#'                        balance_groups = TRUE,
#'                        only_female = NULL,
#'                        only_male = NULL,
#'                        exclude_cols = "Sex",
#'                        ratio = 0.75,
#'                        cor_threshold = 0.9,
#'                        normalize = TRUE,
#'                        cv_sets = 5,
#'                        grid_size = 50,
#'                        ncores = 4,
#'                        hypopt_vis = TRUE,
#'                        palette = NULL,
#'                        vline = TRUE,
#'                        subtitle = c("accuracy",
#'                                     "sensitivity",
#'                                     "specificity",
#'                                     "auc",
#'                                     "features",
#'                                     "top-features"),
#'                        varimp_yaxis_names = FALSE,
#'                        nfeatures = 9,
#'                        points = TRUE,
#'                        boxplot_xaxis_names = FALSE,
#'                        seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   # Prepare datasets
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   join_data <- wide_data |>
#'     dplyr::left_join(metadata |> dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex", variable)))) |>
#'     dplyr::filter(!!Variable %in% c(case, control))
#'
#'   # Prepare sets and groups
#'   data_split <- split_data(join_data, variable, strata, ratio, seed)
#'   if (isTRUE(balance_groups)) {
#'     train_list <- make_groups(data_split$train_set,
#'                               variable,
#'                               case,
#'                               c(case, control),
#'                               only_female,
#'                               only_male,
#'                               seed)
#'     test_list <- make_groups(data_split$test_set,
#'                              variable,
#'                              case,
#'                              c(case, control),
#'                              only_female,
#'                              only_male,
#'                              seed)
#'   } else {
#'     train_list <- data_split$train_set
#'     test_list <- data_split$test_set
#'   }
#'   message("Sets and groups are ready. Model fitting is starting...")
#'
#'   # Run model
#'   message(paste0("Classification model for ", case, " as case is starting..."))
#'   hypopt_res <- xgboost_hypopt(train_list,
#'                                test_list,
#'                                variable,
#'                                case,
#'                                cor_threshold,
#'                                normalize,
#'                                cv_sets,
#'                                grid_size,
#'                                ncores,
#'                                hypopt_vis,
#'                                exclude_cols,
#'                                seed)
#'
#'   finalfit_res <- finalfit(hypopt_res$train_set,
#'                            hypopt_res$xgboost_tune,
#'                            hypopt_res$xgboost_wf,
#'                            seed)
#'
#'   testfit_res <- testfit(hypopt_res$train_set,
#'                          hypopt_res$test_set,
#'                          variable,
#'                          case,
#'                          finalfit_res,
#'                          exclude_cols,
#'                          type = "other",
#'                          seed,
#'                          palette)
#'
#'   var_imp_res <- plot_var_imp(finalfit_res,
#'                               case,
#'                               testfit_res$metrics$accuracy,
#'                               testfit_res$metrics$sensitivity,
#'                               testfit_res$metrics$specificity,
#'                               testfit_res$metrics$auc,
#'                               testfit_res$mixture,
#'                               palette = palette,
#'                               vline = vline,
#'                               subtitle = subtitle,
#'                               yaxis_names = varimp_yaxis_names)
#'
#'   top_features <- var_imp_res$features |>
#'     dplyr::arrange(dplyr::desc(Scaled_Importance)) |>
#'     dplyr::select(Variable) |>
#'     dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
#'     utils::head(nfeatures)
#'   proteins <- top_features[["Variable"]]
#'
#'   boxplot_res <- plot_protein_boxplot(join_data,
#'                                       variable,
#'                                       proteins,
#'                                       case,
#'                                       points,
#'                                       xaxis_names = boxplot_xaxis_names,
#'                                       palette = palette)
#'
#'   return(list("hypopt_res" = hypopt_res,
#'               "finalfit_res" = finalfit_res,
#'               "testfit_res" = testfit_res,
#'               "var_imp_res" = var_imp_res,
#'               "boxplot_res" = boxplot_res))
#' }
#'
#'
#' #' Plot features summary visualizations
#' #'
#' #' `plot_features_summary()` plots the number of proteins and the number of top
#' #' proteins for each disease in a barplot. It also plots the upset plot of the
#' #' top or all protein features, as well as a summary line plot of the model
#' #' performance metrics.
#' #'
#' #' @param ml_results A list of classification models results.
#' #' @param importance Importance threshold for top features. Default is 50.
#' #' @param upset_top_features Whether to plot the upset plot for the top features. Default is FALSE.
#' #' @param case_palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #' @param feature_type_palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is "all-features" = "pink" and "top-features" = "darkblue".
#' #' @param verbose If the function should print the different sets of features for each disease. Default is TRUE.
#' #'
#' #' @return A list with two elements:
#' #'   - features_barplot: Barplot of the number of proteins and top proteins for each disease.
#' #'   - upset_plot_features: Upset plot of the top or all proteins.
#' #'   - metrics_barplot: Barplot of the model metrics for each disease.
#' #'   - features_df: A tibble with the proteins for each combination of cases.
#' #'   - features_list: A list with the proteins for each combination of cases.
#' #' @export
#' #'
#' #' @examples
#' #' # Run the elastic net model pipeline for 3 different cases
#' #' res_aml <- do_rreg(example_data,
#' #'                    example_metadata,
#' #'                    case = "AML",
#' #'                    control = c("BRC", "PRC"),
#' #'                    wide = FALSE,
#' #'                    only_female = "BRC",
#' #'                    only_male = "PRC",
#' #'                    cv_sets = 2,
#' #'                    grid_size = 1,
#' #'                    ncores = 1)
#' #'
#' #' res_brc <- do_rreg(example_data,
#' #'                    example_metadata,
#' #'                    case = "BRC",
#' #'                    control = c("BRC", "AML"),
#' #'                    wide = FALSE,
#' #'                    only_female = "BRC",
#' #'                    only_male = "PRC",
#' #'                    cv_sets = 2,
#' #'                    grid_size = 1,
#' #'                    ncores = 1)
#' #'
#' #' res_prc <- do_rreg(example_data,
#' #'                    example_metadata,
#' #'                    case = "PRC",
#' #'                    control = c("BRC", "AML"),
#' #'                    wide = FALSE,
#' #'                    only_female = "BRC",
#' #'                    only_male = "PRC",
#' #'                    cv_sets = 2,
#' #'                    grid_size = 1,
#' #'                    ncores = 1)
#' #'
#' #' # Combine the results
#' #' res <- list("AML" = res_aml,
#' #'             "BRC" = res_brc,
#' #'             "PRC" = res_prc)
#' #'
#' #' # Plot features summary visualizations
#' #' plot_features_summary(res)
#' plot_features_summary <- function(ml_results,
#'                                   importance = 50,
#'                                   upset_top_features = FALSE,
#'                                   case_palette = NULL,
#'                                   feature_type_palette = c("all-features" = "pink",
#'                                                            "top-features" = "darkblue"),
#'                                   verbose = TRUE) {
#'
#'   barplot_data <- lapply(names(ml_results), function(case) {
#'
#'     features <- ml_results[[case]]$var_imp_res$features |>
#'       dplyr::mutate(Category = case) |>
#'       dplyr::select(Category, Variable) |>
#'       dplyr::rename(Assay = Variable) |>
#'       dplyr::group_by(Category) |>
#'       dplyr::summarise(Count = dplyr::n()) |>
#'       dplyr::ungroup() |>
#'       dplyr::mutate(Type = "all-features")
#'
#'     top_features <- ml_results[[case]]$var_imp_res$features |>
#'       dplyr::mutate(Category = case) |>
#'       dplyr::filter(Scaled_Importance >= importance) |>
#'       dplyr::select(Category, Variable) |>
#'       dplyr::rename(Assay = Variable) |>
#'       dplyr::group_by(Category) |>
#'       dplyr::summarise(Count = dplyr::n()) |>
#'       dplyr::ungroup() |>
#'       dplyr::mutate(Type = "top-features")
#'
#'     features_data <- rbind(features, top_features)
#'   })
#'
#'   barplot_data <- do.call(rbind, barplot_data)
#'
#'   features_barplot <- barplot_data |>
#'     ggplot2::ggplot(ggplot2::aes(x = Category, y = Count, fill = Type)) +
#'     ggplot2::geom_bar(stat = "identity", position = "dodge") +
#'     ggplot2::labs(x = "", y = "Number of protein", fill = "Feature type") +
#'     theme_hpa(angled = T) +
#'     ggplot2::theme(legend.position = "top",
#'                    legend.title = ggplot2::element_text(face = "bold"))
#'
#'   if (is.null(names(feature_type_palette)) && !is.null(feature_type_palette)) {
#'     features_barplot <- features_barplot + scale_fill_hpa(feature_type_palette)
#'   } else if (!is.null(feature_type_palette)) {
#'     features_barplot <- features_barplot + ggplot2::scale_fill_manual(values = feature_type_palette)
#'   }
#'
#'   metrics_data <- lapply(names(ml_results), function(case) {
#'     metrics <- tibble::tibble(
#'       metric = c("Accuracy", "Sensitivity", "Specificity", "AUC"),
#'       value = c(ml_results[[case]]$testfit_res$metrics$accuracy,
#'                 ml_results[[case]]$testfit_res$metrics$sensitivity,
#'                 ml_results[[case]]$testfit_res$metrics$specificity,
#'                 ml_results[[case]]$testfit_res$metrics$auc)
#'     ) |>
#'       dplyr::mutate(Category = case)
#'   })
#'
#'   metrics_data <- do.call(rbind, metrics_data)
#'
#'   metrics_lineplot <- metrics_data |>
#'     ggplot2::ggplot(ggplot2::aes(x = Category,
#'                                  y = value,
#'                                  color = metric,
#'                                  group = metric)) +
#'     ggplot2::geom_line() +
#'     ggplot2::geom_point(size = 2) +
#'     ggplot2::labs(x = "", y = "Value", color = "Metric") +
#'     theme_hpa(angled = TRUE) +
#'     ggplot2::theme(legend.position = "top",
#'                    legend.title = ggplot2::element_text(face = "bold")) +
#'     ggplot2::scale_color_manual(values = c("Accuracy" = "darkred",
#'                                            "Sensitivity" = "darkblue",
#'                                            "Specificity" = "darkgreen",
#'                                            "AUC" = "purple3"))
#'
#'   upset_features <- lapply(names(ml_results), function(case) {
#'
#'     if (upset_top_features == T) {
#'       upset_features <- ml_results[[case]]$var_imp_res$features |>
#'         dplyr::filter(Scaled_Importance >= importance) |>
#'         dplyr::pull(Variable)
#'     } else {
#'       upset_features <- ml_results[[case]]$var_imp_res$features |>
#'         dplyr::pull(Variable)
#'     }
#'
#'   })
#'   names(upset_features) <- names(ml_results)
#'
#'   # Prepare palettes
#'   if (is.null(names(case_palette)) && !is.null(case_palette)) {
#'     pal <- get_hpa_palettes()[[case_palette]]
#'   } else if (!is.null(case_palette)) {
#'     pal <- case_palette
#'   } else {
#'     pal <- rep("black", length(names(ml_results)))
#'     names(pal) <- names(ml_results)
#'   }
#'   feature_names <- names(ml_results)
#'   ordered_colors <- pal[feature_names]
#'   frequencies <- sapply(upset_features, length)
#'   ordered_feature_names <- names(sort(frequencies, decreasing = TRUE))
#'   ordered_colors <- ordered_colors[ordered_feature_names]
#'
#'   upset <- UpSetR::fromList(upset_features)
#'   features <- extract_protein_list(upset, upset_features)
#'
#'   if (verbose) {
#'     print(features$proteins_list)
#'   }
#'
#'   upset_plot_features <- UpSetR::upset(upset,
#'                                        sets = ordered_feature_names,
#'                                        order.by = "freq",
#'                                        nsets = length(ordered_feature_names),
#'                                        sets.bar.color = ordered_colors)
#'
#'   return(list("features_barplot" = features_barplot,
#'               "upset_plot_features" = upset_plot_features,
#'               "metrics_lineplot" = metrics_lineplot,
#'               "features_df" = features$proteins_df,
#'               "features_list" = features$proteins_list))
#' }
#'
#'
#' #' Regularized multiclassification model pipeline
#' #'
#' #' `do_rreg_multi()` runs the regularized multiclassification model pipeline. It splits the
#' #' data into training and test sets, creates class-balanced case-control groups,
#' #' and fits the model. It performs hyperparameter optimization and fits the best
#' #' model. It also plots the ROC curve and the AUC barplot for each class.
#' #'
#' #' @param olink_data Olink data.
#' #' @param metadata Metadata.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param wide Whether the data is wide format. Default is TRUE.
#' #' @param strata Whether to stratify the data. Default is TRUE.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned.
#' #' @param ratio Ratio of training data to test data. Default is 0.75.
#' #' @param type Type of regularization. Default is "lasso". Other options are "ridge" and "elnet".
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the hyperparameter optimization grid. Default is 10.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #' @param vline Whether to add a vertical line at 50% importance. Default is TRUE.
#' #' @param varimp_yaxis_names Whether to add y-axis names to the variable importance plot. Default is FALSE.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with the following elements:
#' #' - hypopt_res: Hyperparameter optimization results.
#' #' - finalfit_res: Final model fitting results.
#' #' - roc_curve: ROC curve plot.
#' #' - auc: AUC values for each class.
#' #' - auc_barplot: AUC barplot.
#' #' - var_imp_res: Variable importance results.
#' #' @export
#' #'
#' #' @details If the data contain missing values, KNN imputation will be applied.
#' #' If no check for feature correlation is preferred, set `cor_threshold` to 1.
#' #' It will filter out rows that contain NAs in Disease.
#' #'
#' #' @examples
#' #' do_rreg_multi(example_data,
#' #'               example_metadata,
#' #'               wide = FALSE,
#' #'               palette = "cancers12",
#' #'               cv_sets = 5,
#' #'               grid_size = 5,
#' #'               ncores = 1)
#' do_rreg_multi <- function(olink_data,
#'                           metadata,
#'                           variable = "Disease",
#'                           wide = TRUE,
#'                           strata = TRUE,
#'                           exclude_cols = "Sex",
#'                           ratio = 0.75,
#'                           type = "lasso",
#'                           cor_threshold = 0.9,
#'                           cv_sets = 5,
#'                           grid_size = 10,
#'                           ncores = 4,
#'                           hypopt_vis = TRUE,
#'                           palette = NULL,
#'                           vline = TRUE,
#'                           varimp_yaxis_names = FALSE,
#'                           seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   # Prepare datasets
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   nrows_before <- nrow(wide_data)
#'   join_data <- wide_data |>
#'     dplyr::left_join(metadata |> dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex", variable)))) |>
#'     dplyr::filter(!is.na(!!Variable))
#'
#'   nrows_after <- nrow(join_data)
#'   if (nrows_before != nrows_after){
#'     warning(paste0(nrows_before - nrows_after,
#'                    " rows were removed because they contain NAs in ", variable, "! They either contain NAs or data did not match metadata."))
#'   }
#'
#'   cases <- unique(join_data[[variable]])
#'
#'   # Prepare sets and groups
#'   data_split <- split_data(join_data, variable, strata, ratio, seed)
#'   train_list <- data_split$train_set
#'   test_list <- data_split$test_set
#'
#'   message("Sets are ready. Multiclassification model fitting is starting...")
#'
#'   # Run model
#'   hypopt_res <- elnet_hypopt_multi(train_list,
#'                                    test_list,
#'                                    variable,
#'                                    type,
#'                                    cor_threshold,
#'                                    cv_sets,
#'                                    grid_size,
#'                                    ncores,
#'                                    hypopt_vis,
#'                                    exclude_cols,
#'                                    seed)
#'
#'   finalfit_res <- finalfit(hypopt_res$train_set,
#'                            hypopt_res$elnet_tune,
#'                            hypopt_res$elnet_wf,
#'                            seed)
#'
#'   splits <- rsample::make_splits(hypopt_res$train_set, hypopt_res$test_set)
#'   last_fit <- tune::last_fit(finalfit_res$final_wf,
#'                              splits,
#'                              metrics = yardstick::metric_set(yardstick::roc_auc))
#'
#'   preds <- last_fit |>
#'     tune::collect_predictions()
#'
#'   pred_cols <- grep("^\\.pred_", names(preds), value = TRUE)
#'   preds <- preds |>
#'     dplyr::rowwise() |>
#'     dplyr::mutate(
#'       .pred_class = names(preds)[which.max(dplyr::c_across(dplyr::all_of(pred_cols)))]
#'     ) |>
#'     dplyr::ungroup() |>
#'     dplyr::mutate(.pred_class = sub("^\\.pred_", "", .pred_class))
#'
#'   unique_classes <- preds |>
#'     dplyr::pull(!!Variable) |>
#'     unique()
#'   pred_cols <- paste0(".pred_", unique_classes)
#'
#'   roc_curve <- preds |>
#'     yardstick::roc_curve(truth = !!Variable,
#'                          !!!rlang::syms(pred_cols)) |>
#'     ggplot2::ggplot(ggplot2::aes(x = 1 - specificity,
#'                                  y = sensitivity,
#'                                  color = .level)) +
#'     ggplot2::geom_path(linewidth = 1) +
#'     ggplot2::geom_abline(lty = 3) +
#'     ggplot2::coord_equal() +
#'     ggplot2::facet_wrap(~ .level) +
#'     theme_hpa() +
#'     ggplot2::theme(legend.position = "none",
#'                    axis.text = ggplot2::element_text(size = 10))
#'
#'   # Calculate AUC
#'   auc_macro <- preds |>
#'     dplyr::group_by(.pred_class) |>
#'     yardstick::roc_auc(truth = !!Variable,
#'                        !!!rlang::syms(pred_cols),
#'                        estimator = "macro_weighted")
#'
#'   # Variable importance plot
#'   var_imp_res <- plot_var_imp(finalfit_res,
#'                               "Multiclassification",
#'                               NULL,
#'                               NULL,
#'                               NULL,
#'                               NULL,
#'                               NULL,
#'                               palette = NULL,
#'                               vline = vline,
#'                               subtitle = c("features",
#'                                            "top-features"),
#'                               yaxis_names = varimp_yaxis_names)
#'
#'   # AUC barplot
#'   barplot <- ggplot2::ggplot(auc_macro,
#'                              ggplot2::aes(x = stats::reorder(.pred_class, -.estimate),
#'                                           y = .estimate,
#'                                           fill = .pred_class)) +
#'     ggplot2::geom_bar(stat = "identity") +
#'     ggplot2::labs(x = "", y = "AUC") +
#'     ggplot2::ylim(0, 1) +
#'     theme_hpa(angled = T) +
#'     ggplot2::theme(legend.position = "none")
#'
#'   # Prepare palettes
#'   if (!is.null(palette) && is.null(names(palette))) {
#'     roc_curve <- roc_curve + scale_color_hpa(palette)
#'     barplot <- barplot + scale_fill_hpa(palette)
#'   } else if (!is.null(palette)) {
#'     roc_curve <- roc_curve + ggplot2::scale_color_manual(values = palette)
#'     barplot <- barplot + ggplot2::scale_fill_manual(values = palette)
#'   } else {
#'     palette <- rep("black", length(cases))
#'     roc_curve <- roc_curve + ggplot2::scale_color_manual(values = palette)
#'     barplot <- barplot + ggplot2::scale_fill_manual(values = palette)
#'   }
#'
#'   auc_macro <- auc_macro |>
#'     dplyr::select(.pred_class, .estimate) |>
#'     dplyr::rename(AUC = .estimate, !!Variable := .pred_class)
#'
#'   return(list("hypopt_res" = hypopt_res,
#'               "finalfit_res" = finalfit_res,
#'               "roc_curve" = roc_curve,
#'               "auc" = auc_macro,
#'               "auc_barplot" = barplot,
#'               "var_imp_res" = var_imp_res))
#' }
#'
#'
#' #' Random forest multiclassification model pipeline
#' #'
#' #' `do_rf_multi()` runs the random forest multiclassification model pipeline. It splits the
#' #' data into training and test sets, creates class-balanced case-control groups,
#' #' and fits the model. It performs hyperparameter optimization and fits the best
#' #' model. It also plots the ROC curve and the AUC barplot for each class.
#' #'
#' #' @param olink_data Olink data.
#' #' @param metadata Metadata.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param wide Whether the data is wide format. Default is TRUE.
#' #' @param strata Whether to stratify the data. Default is TRUE.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned.
#' #' @param ratio Ratio of training data to test data. Default is 0.75.
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param normalize Whether to normalize numeric data to have a standard deviation of one and a mean of zero. Default is TRUE.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the hyperparameter optimization grid. Default is 10.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #' @param vline Whether to add a vertical line at 50% importance. Default is TRUE.
#' #' @param varimp_yaxis_names Whether to add y-axis names to the variable importance plot. Default is FALSE.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with the following elements:
#' #' - hypopt_res: Hyperparameter optimization results.
#' #' - finalfit_res: Final model fitting results.
#' #' - roc_curve: ROC curve plot.
#' #' - auc: AUC values for each class.
#' #' - auc_barplot: AUC barplot.
#' #' - var_imp_res: Variable importance results.
#' #' @export
#' #'
#' #' @details If the data contain missing values, KNN imputation will be applied.
#' #' If no check for feature correlation is preferred, set `cor_threshold` to 1.
#' #' It will filter out rows that contain NAs in Disease.
#' #'
#' #' @examples
#' #' do_rf_multi(example_data,
#' #'             example_metadata,
#' #'             wide = FALSE,
#' #'             palette = "cancers12",
#' #'             cv_sets = 5,
#' #'             grid_size = 5,
#' #'             ncores = 1)
#' do_rf_multi <- function(olink_data,
#'                         metadata,
#'                         variable = "Disease",
#'                         wide = TRUE,
#'                         strata = TRUE,
#'                         exclude_cols = "Sex",
#'                         ratio = 0.75,
#'                         cor_threshold = 0.9,
#'                         normalize = TRUE,
#'                         cv_sets = 5,
#'                         grid_size = 10,
#'                         ncores = 4,
#'                         hypopt_vis = TRUE,
#'                         palette = NULL,
#'                         vline = TRUE,
#'                         varimp_yaxis_names = FALSE,
#'                         seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   # Prepare datasets
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   nrows_before <- nrow(wide_data)
#'   join_data <- wide_data |>
#'     dplyr::left_join(metadata |> dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex", variable)))) |>
#'     dplyr::filter(!is.na(!!Variable))
#'
#'   nrows_after <- nrow(join_data)
#'   if (nrows_before != nrows_after){
#'     warning(paste0(nrows_before - nrows_after,
#'                    " rows were removed because they contain NAs in ", variable, "! They either contain NAs or data did not match metadata."))
#'   }
#'
#'   cases <- unique(join_data[[variable]])
#'
#'   # Prepare sets and groups
#'   data_split <- split_data(join_data, variable, strata, ratio, seed)
#'   train_list <- data_split$train_set
#'   test_list <- data_split$test_set
#'
#'   message("Sets are ready. Multiclassification model fitting is starting...")
#'
#'   # Run model
#'   hypopt_res <- rf_hypopt_multi(train_list,
#'                                 test_list,
#'                                 variable,
#'                                 cor_threshold,
#'                                 normalize,
#'                                 cv_sets,
#'                                 grid_size,
#'                                 ncores,
#'                                 hypopt_vis,
#'                                 exclude_cols,
#'                                 seed)
#'
#'   finalfit_res <- finalfit(hypopt_res$train_set,
#'                            hypopt_res$rf_tune,
#'                            hypopt_res$rf_wf,
#'                            seed)
#'
#'   splits <- rsample::make_splits(hypopt_res$train_set, hypopt_res$test_set)
#'   last_fit <- tune::last_fit(finalfit_res$final_wf,
#'                              splits,
#'                              metrics = yardstick::metric_set(yardstick::roc_auc))
#'
#'   preds <- last_fit |>
#'     tune::collect_predictions()
#'
#'   pred_cols <- grep("^\\.pred_", names(preds), value = TRUE)
#'   preds <- preds |>
#'     dplyr::rowwise() |>
#'     dplyr::mutate(
#'       .pred_class = names(preds)[which.max(dplyr::c_across(dplyr::all_of(pred_cols)))]
#'     ) |>
#'     dplyr::ungroup() |>
#'     dplyr::mutate(.pred_class = sub("^\\.pred_", "", .pred_class))
#'
#'   unique_classes <- preds |>
#'     dplyr::pull(!!Variable) |>
#'     unique()
#'   pred_cols <- paste0(".pred_", unique_classes)
#'
#'   roc_curve <- preds |>
#'     yardstick::roc_curve(truth = !!Variable,
#'                          !!!rlang::syms(pred_cols)) |>
#'     ggplot2::ggplot(ggplot2::aes(x = 1 - specificity,
#'                                  y = sensitivity,
#'                                  color = .level)) +
#'     ggplot2::geom_path(linewidth = 1) +
#'     ggplot2::geom_abline(lty = 3) +
#'     ggplot2::coord_equal() +
#'     ggplot2::facet_wrap(~ .level) +
#'     theme_hpa() +
#'     ggplot2::theme(legend.position = "none",
#'                    axis.text = ggplot2::element_text(size = 10))
#'
#'   # Calculate AUC
#'   auc_macro <- preds |>
#'     dplyr::group_by(.pred_class) |>
#'     yardstick::roc_auc(truth = !!Variable,
#'                        !!!rlang::syms(pred_cols),
#'                        estimator = "macro_weighted")
#'
#'   # Variable importance plot
#'   var_imp_res <- plot_var_imp(finalfit_res,
#'                               "Multiclassification",
#'                               NULL,
#'                               NULL,
#'                               NULL,
#'                               NULL,
#'                               NULL,
#'                               palette = NULL,
#'                               vline = vline,
#'                               subtitle = c("features",
#'                                            "top-features"),
#'                               yaxis_names = varimp_yaxis_names)
#'
#'   # AUC barplot``
#'   barplot <- ggplot2::ggplot(auc_macro,
#'                              ggplot2::aes(x = stats::reorder(.pred_class, -.estimate),
#'                                           y = .estimate,
#'                                           fill = .pred_class)) +
#'     ggplot2::geom_bar(stat = "identity") +
#'     ggplot2::labs(x = "", y = "AUC") +
#'     ggplot2::ylim(0, 1) +
#'     theme_hpa(angled = T) +
#'     ggplot2::theme(legend.position = "none")
#'
#'   # Prepare palettes
#'   if (!is.null(palette) && is.null(names(palette))) {
#'     roc_curve <- roc_curve + scale_color_hpa(palette)
#'     barplot <- barplot + scale_fill_hpa(palette)
#'   } else if (!is.null(palette)) {
#'     roc_curve <- roc_curve + ggplot2::scale_color_manual(values = palette)
#'     barplot <- barplot + ggplot2::scale_fill_manual(values = palette)
#'   } else {
#'     palette <- rep("black", length(cases))
#'     roc_curve <- roc_curve + ggplot2::scale_color_manual(values = palette)
#'     barplot <- barplot + ggplot2::scale_fill_manual(values = palette)
#'   }
#'
#'   auc_macro <- auc_macro |>
#'     dplyr::select(.pred_class, .estimate) |>
#'     dplyr::rename(AUC = .estimate, !!Variable := .pred_class)
#'
#'   return(list("hypopt_res" = hypopt_res,
#'               "finalfit_res" = finalfit_res,
#'               "roc_curve" = roc_curve,
#'               "auc" = auc_macro,
#'               "auc_barplot" = barplot,
#'               "var_imp_res" = var_imp_res))
#' }
#'
#'
#' #' XGBoost multiclassification model pipeline
#' #'
#' #' `do_xgboost_multi()` runs the XGBoost multiclassification model pipeline. It splits the
#' #' data into training and test sets, creates class-balanced case-control groups,
#' #' and fits the model. It performs hyperparameter optimization and fits the best
#' #' model. It also plots the ROC curve and the AUC barplot for each class.
#' #'
#' #' @param olink_data Olink data.
#' #' @param metadata Metadata.
#' #' @param variable The variable to predict. Default is "Disease".
#' #' @param wide Whether the data is wide format. Default is TRUE.
#' #' @param strata Whether to stratify the data. Default is TRUE.
#' #' @param exclude_cols Columns to exclude from the data before the model is tuned.
#' #' @param ratio Ratio of training data to test data. Default is 0.75.
#' #' @param cor_threshold Threshold of absolute correlation values. This will be used to remove the minimum number of features so that all their resulting absolute correlations are less than this value.
#' #' @param normalize Whether to normalize numeric data to have a standard deviation of one and a mean of zero. Default is TRUE.
#' #' @param cv_sets Number of cross-validation sets. Default is 5.
#' #' @param grid_size Size of the hyperparameter optimization grid. Default is 50.
#' #' @param ncores Number of cores to use for parallel processing. Default is 4.
#' #' @param hypopt_vis Whether to visualize hyperparameter optimization results. Default is TRUE.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #' @param vline Whether to add a vertical line at 50% importance. Default is TRUE.
#' #' @param varimp_yaxis_names Whether to add y-axis names to the variable importance plot. Default is FALSE.
#' #' @param seed Seed for reproducibility. Default is 123.
#' #'
#' #' @return A list with the following elements:
#' #'  - hypopt_res: Hyperparameter optimization results.
#' #'  - finalfit_res: Final model fitting results.
#' #'  - roc_curve: ROC curve plot.
#' #'  - auc: AUC values for each class.
#' #'  - auc_barplot: AUC barplot.
#' #'  - var_imp_res: Variable importance results.
#' #' @export
#' #'
#' #' @details If the data contain missing values, KNN imputation will be applied.
#' #' If no check for feature correlation is preferred, set `cor_threshold` to 1.
#' #' It will filter out rows that contain NAs in Disease.
#' #'
#' #' @examples
#' #' do_xgboost_multi(example_data,
#' #'                  example_metadata,
#' #'                  wide = FALSE,
#' #'                  palette = "cancers12",
#' #'                  cv_sets = 5,
#' #'                  grid_size = 5,
#' #'                  ncores = 1)
#' do_xgboost_multi <- function(olink_data,
#'                              metadata,
#'                              variable = "Disease",
#'                              wide = TRUE,
#'                              strata = TRUE,
#'                              exclude_cols = "Sex",
#'                              ratio = 0.75,
#'                              cor_threshold = 0.9,
#'                              normalize = TRUE,
#'                              cv_sets = 5,
#'                              grid_size = 50,
#'                              ncores = 4,
#'                              hypopt_vis = TRUE,
#'                              palette = NULL,
#'                              vline = TRUE,
#'                              varimp_yaxis_names = FALSE,
#'                              seed = 123) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   # Prepare datasets
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   nrows_before <- nrow(wide_data)
#'   join_data <- wide_data |>
#'     dplyr::left_join(metadata |> dplyr::select(dplyr::any_of(c("DAid", "Disease", "Sex", variable)))) |>
#'     dplyr::filter(!is.na(!!Variable))
#'
#'   nrows_after <- nrow(join_data)
#'   if (nrows_before != nrows_after){
#'     warning(paste0(nrows_before - nrows_after,
#'                    " rows were removed because they contain NAs in ", variable, "! They either contain NAs or data did not match metadata."))
#'   }
#'
#'   cases <- unique(join_data[[variable]])
#'
#'   # Prepare sets and groups
#'   data_split <- split_data(join_data, variable, strata, ratio, seed)
#'   train_list <- data_split$train_set
#'   test_list <- data_split$test_set
#'
#'   message("Sets are ready. Multiclassification model fitting is starting...")
#'
#'   # Run model
#'   hypopt_res <- xgboost_hypopt_multi(train_list,
#'                                      test_list,
#'                                      variable,
#'                                      cor_threshold,
#'                                      normalize,
#'                                      cv_sets,
#'                                      grid_size,
#'                                      ncores,
#'                                      hypopt_vis,
#'                                      exclude_cols,
#'                                      seed)
#'
#'   finalfit_res <- finalfit(hypopt_res$train_set,
#'                            hypopt_res$xgboost_tune,
#'                            hypopt_res$xgboost_wf,
#'                            seed)
#'
#'   splits <- rsample::make_splits(hypopt_res$train_set, hypopt_res$test_set)
#'   last_fit <- tune::last_fit(finalfit_res$final_wf,
#'                              splits,
#'                              metrics = yardstick::metric_set(yardstick::roc_auc))
#'
#'   preds <- last_fit |>
#'     tune::collect_predictions()
#'
#'   pred_cols <- grep("^\\.pred_", names(preds), value = TRUE)
#'   preds <- preds |>
#'     dplyr::rowwise() |>
#'     dplyr::mutate(
#'       .pred_class = names(preds)[which.max(dplyr::c_across(dplyr::all_of(pred_cols)))]
#'     ) |>
#'     dplyr::ungroup() |>
#'     dplyr::mutate(.pred_class = sub("^\\.pred_", "", .pred_class))
#'
#'   unique_classes <- preds |>
#'     dplyr::pull(!!Variable) |>
#'     unique()
#'   pred_cols <- paste0(".pred_", unique_classes)
#'
#'   roc_curve <- preds |>
#'     yardstick::roc_curve(truth = !!Variable,
#'                          !!!rlang::syms(pred_cols)) |>
#'     ggplot2::ggplot(ggplot2::aes(x = 1 - specificity,
#'                                  y = sensitivity,
#'                                  color = .level)) +
#'     ggplot2::geom_path(linewidth = 1) +
#'     ggplot2::geom_abline(lty = 3) +
#'     ggplot2::coord_equal() +
#'     ggplot2::facet_wrap(~ .level) +
#'     theme_hpa() +
#'     ggplot2::theme(legend.position = "none",
#'                    axis.text = ggplot2::element_text(size = 10))
#'
#'   # Calculate AUC
#'   auc_macro <- preds |>
#'     dplyr::group_by(.pred_class) |>
#'     yardstick::roc_auc(truth = !!Variable,
#'                        !!!rlang::syms(pred_cols),
#'                        estimator = "macro_weighted")
#'
#'   # Variable importance plot
#'   var_imp_res <- plot_var_imp(finalfit_res,
#'                               "Multiclassification",
#'                               NULL,
#'                               NULL,
#'                               NULL,
#'                               NULL,
#'                               NULL,
#'                               palette = NULL,
#'                               vline = vline,
#'                               subtitle = c("features",
#'                                            "top-features"),
#'                               yaxis_names = varimp_yaxis_names)
#'
#'   # AUC barplot``
#'   barplot <- ggplot2::ggplot(auc_macro,
#'                              ggplot2::aes(x = stats::reorder(.pred_class, -.estimate),
#'                                           y = .estimate,
#'                                           fill = .pred_class)) +
#'     ggplot2::geom_bar(stat = "identity") +
#'     ggplot2::labs(x = "", y = "AUC") +
#'     ggplot2::ylim(0, 1) +
#'     theme_hpa(angled = T) +
#'     ggplot2::theme(legend.position = "none")
#'
#'   # Prepare palettes
#'   if (!is.null(palette) && is.null(names(palette))) {
#'     roc_curve <- roc_curve + scale_color_hpa(palette)
#'     barplot <- barplot + scale_fill_hpa(palette)
#'   } else if (!is.null(palette)) {
#'     roc_curve <- roc_curve + ggplot2::scale_color_manual(values = palette)
#'     barplot <- barplot + ggplot2::scale_fill_manual(values = palette)
#'   } else {
#'     palette <- rep("black", length(cases))
#'     roc_curve <- roc_curve + ggplot2::scale_color_manual(values = palette)
#'     barplot <- barplot + ggplot2::scale_fill_manual(values = palette)
#'   }
#'
#'   auc_macro <- auc_macro |>
#'     dplyr::select(.pred_class, .estimate) |>
#'     dplyr::rename(AUC = .estimate, !!Variable := .pred_class)
#'
#'   return(list("hypopt_res" = hypopt_res,
#'               "finalfit_res" = finalfit_res,
#'               "roc_curve" = roc_curve,
#'               "auc" = auc_macro,
#'               "auc_barplot" = barplot,
#'               "var_imp_res" = var_imp_res))
#' }
