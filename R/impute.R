#' utils::globalVariables(c("Categories", "NA_percentage"))
#' #' Summary of missing values
#' #'
#' #' `na_search()` provides a summary of missing values in a dataset. It allows the user to
#' #' specify the metadata columns to include in the summary and the color palette to use for
#' #' the heatmap annotations.
#' #'
#' #' @param olink_data The Olink dataset.
#' #' @param metadata The metadata dataset.
#' #' @param wide If TRUE, the data is in wide format.
#' #' @param metadata_cols The metadata columns to include in the summary.
#' #' @param palette The color palettes to use for the heatmap annotations (check examples bellow).
#' #' @param x_labels If TRUE, show x-axis labels.
#' #' @param y_labels If TRUE, show y-axis labels.
#' #' @param show_heatmap If TRUE, show the heatmap.
#' #'
#' #' @return A list containing the summary of missing values and a heatmap.
#' #' @export
#' #'
#' #' @details When using continuous metadata variables, consider converted them to
#' #' categorical by binning them into categories before passing them to the function.
#' #' This will make the heatmap more informative and easier to interpret.
#' #' Also when coloring annotations, the user can use custom palettes or the
#' #' Human Protein Atlas (HPA) palettes. It is not required to provide a palette
#' #' for all annotations, but when a palette is provided, it must be in correct
#' #' format (check examples bellow).
#' #'
#' #' @examples
#' #' # Use custom palettes for coloring annotations
#' #' palette = list(Sex = c(M = "blue", F = "pink"))
#' #' na_res <- na_search(example_data,
#' #'                     example_metadata,
#' #'                     wide = FALSE,
#' #'                     metadata_cols = c("Age", "Sex"),
#' #'                     palette = palette,
#' #'                     show_heatmap = FALSE)
#' #'
#' #' # Use HPA palettes for coloring annotations
#' #' palette = list(Disease = get_hpa_palettes()$cancers12, Sex = get_hpa_palettes()$sex_hpa)
#' #' na_res <- na_search(example_data,
#' #'                     example_metadata,
#' #'                     wide = FALSE,
#' #'                     metadata_cols = c("Disease", "Sex"),
#' #'                     palette = palette,
#' #'                     show_heatmap = FALSE)
#' #'
#' #' # Pre-bin a continuous variable
#' #' metadata <- example_metadata
#' #' metadata$Age_bin <- cut(metadata$Age,
#' #'                         breaks = c(0, 20, 40, 60, 80, 120),
#' #'                         labels = c("0-20", "21-40", "41-60", "61-80", "81+"),
#' #'                         right = FALSE)
#' #'
#' #' palette = list(Disease = get_hpa_palettes()$cancers12)
#' #'
#' #' na_search(example_data,
#' #'           metadata,
#' #'           wide = FALSE,
#' #'           metadata_cols = c("Age_bin", "Disease"),
#' #'           palette = palette)
#' na_search <- function(olink_data,
#'                       metadata,
#'                       wide = TRUE,
#'                       metadata_cols = NULL,
#'                       palette = NULL,
#'                       x_labels = FALSE,
#'                       y_labels = FALSE,
#'                       show_heatmap = TRUE) {
#'   # Prepare data
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   if (!all(metadata_cols %in% colnames(metadata))) {
#'     message("Some category columns provided do not exist in the dataset.")
#'   }
#'
#'   long_data <- wide_data |>
#'     tidyr::pivot_longer(cols = -DAid, names_to = "Assay", values_to = "NPX", values_drop_na = FALSE)
#'
#'   join_data <- long_data |>
#'     dplyr::select(DAid, Assay, NPX) |>
#'     dplyr::left_join(metadata |> dplyr::select(dplyr::any_of(c(metadata_cols, "DAid"))), by = "DAid")
#'
#'   # Calculate NA percentages
#'   na_data <- join_data |>
#'     dplyr::group_by(dplyr::across(dplyr::all_of(c(metadata_cols, "Assay")))) |>
#'     dplyr::mutate(NA_percentage = mean(is.na(NPX)) * 100) |>
#'     dplyr::ungroup() |>
#'     dplyr::mutate(Categories = paste(!!!rlang::syms(metadata_cols), sep = "_")) |>
#'     dplyr::select(dplyr::any_of(c(metadata_cols, "Categories", "Assay", "NA_percentage"))) |>
#'     unique()
#'
#'   # Create heatmap
#'   if (x_labels == FALSE) {
#'     x_labs <- c("")
#'   } else {
#'     x_labs <- NULL
#'   }
#'   if (y_labels == FALSE) {
#'     y_labs <- c("")
#'   } else {
#'     y_labs <- NULL
#'   }
#'   na_heatmap <- tidyheatmaps::tidyheatmap(na_data,
#'                                           rows = Categories,
#'                                           columns = Assay,
#'                                           values = NA_percentage,
#'                                           annotation_row = metadata_cols,
#'                                           annotation_colors = palette,
#'                                           cluster_rows = TRUE,
#'                                           cluster_cols = TRUE,
#'                                           show_selected_row_labels = y_labs,
#'                                           show_selected_col_labels = x_labs,
#'                                           treeheight_row = 20,
#'                                           treeheight_col = 20,
#'                                           silent = isFALSE(show_heatmap))
#'
#'   return(list("na_data" = na_data, "na_heatmap" = na_heatmap))
#' }
#'
#' #' Impute via Median
#' #'
#' #' `impute_median()` imputes missing values in a dataset using the median of each column.
#' #' It allows the user to exclude certain columns from imputation and can also display the
#' #' percentage of missing values in each column before imputation.
#' #'
#' #' @param olink_data The input dataset.
#' #' @param wide If TRUE, the data is in wide format.
#' #' @param exclude_cols The columns to exclude from imputation.
#' #' @param show_na_percentage If TRUE, the percentage of missing values in each column is displayed.
#' #'
#' #' @return The imputed dataset.
#' #' @export
#' #'
#' #' @details This is the fastest but usually least accurate imputation method.
#' #' @examples
#' #' # Data before imputation
#' #' test_data <- example_data |>
#' #'   dplyr::select(DAid, Assay, NPX) |>
#' #'   tidyr::pivot_wider(names_from = "Assay", values_from = "NPX") |>
#' #'   dplyr::slice_head(n = 100)
#' #' test_data
#' #'
#' #' # Data after imputation
#' #' impute_median(test_data)
#' impute_median <- function(olink_data,
#'                           wide = TRUE,
#'                           exclude_cols = c("DAid", "Disease"),
#'                           show_na_percentage = TRUE) {
#'
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'   data_in <- wide_data |>
#'     dplyr::select(-dplyr::any_of(exclude_cols))
#'
#'   if (isTRUE(show_na_percentage)) {
#'     na_percentages <- calc_na_percentage_col(data_in)
#'     print(na_percentages)
#'   }
#'
#'   recipe <- recipes::recipe(~ ., data = data_in) |>
#'     recipes::step_impute_median(recipes::all_predictors())
#'
#'   imputed_data <- recipe |>
#'     recipes::prep() |>
#'     recipes::bake(new_data = NULL)
#'
#'   cols <- wide_data |>
#'     dplyr::select(dplyr::any_of(exclude_cols))
#'   imputed_data <- dplyr::bind_cols(cols, imputed_data)
#'
#'   return(imputed_data)
#' }
#'
#'
#' #' Impute via k-nearest neighbors
#' #'
#' #' `impute_knn()` imputes missing values in a dataset using the k-nearest neighbors method.
#' #' It allows the user to exclude certain columns from imputation and can also display the
#' #' percentage of missing values in each column before imputation. The user can also specify
#' #' the number of neighbors to consider for imputation.
#' #'
#' #' @param olink_data The input dataset.
#' #' @param wide If TRUE, the data is in wide format.
#' #' @param k The number of neighbors to consider for imputation.
#' #' @param exclude_cols The columns to exclude from imputation.
#' #' @param show_na_percentage If TRUE, the percentage of missing values in each column is displayed.
#' #'
#' #' @return The imputed dataset.
#' #' @export
#' #'
#' #' @examples
#' #' # Data before imputation
#' #' test_data <- example_data |>
#' #'   dplyr::select(DAid, Assay, NPX) |>
#' #'   tidyr::pivot_wider(names_from = "Assay", values_from = "NPX") |>
#' #'   dplyr::slice_head(n = 100)
#' #' test_data
#' #'
#' #' # Data after imputation
#' #' impute_knn(test_data, k = 3)
#' impute_knn <- function(olink_data,
#'                        wide = TRUE,
#'                        k = 5,
#'                        exclude_cols = c("DAid", "Disease"),
#'                        show_na_percentage = TRUE) {
#'
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'   data_in <- wide_data |>
#'     dplyr::select(-dplyr::any_of(exclude_cols))
#'
#'   if (isTRUE(show_na_percentage)) {
#'     na_percentages <- calc_na_percentage_col(data_in)
#'     print(na_percentages)
#'   }
#'
#'   recipe <- recipes::recipe(~ ., data = data_in) |>
#'     recipes::step_impute_knn(recipes::all_predictors(), neighbors = k)
#'
#'   imputed_data <- recipe |>
#'     recipes::prep() |>
#'     recipes::bake(new_data = NULL)
#'
#'   cols <- wide_data |>
#'     dplyr::select(dplyr::any_of(exclude_cols))
#'   imputed_data <- dplyr::bind_cols(cols, imputed_data)
#'
#'   return(imputed_data)
#' }
#'
#'
#' #' Impute via missForest
#' #'
#' #' `impute_missForest()` imputes missing values in a dataset using the `missForest` method.
#' #' It allows the user to exclude certain columns from imputation and can also display the
#' #' percentage of missing values in each column before imputation. The user can also specify
#' #' the maximum number of iterations, the number of trees to grow, the type of parallelization
#' #' ("no", "variables", or "forests"), as well as the number of cores to use for parallelization.
#' #'
#' #' @param olink_data The input dataset.
#' #' @param wide If TRUE, the data is in wide format.
#' #' @param maxiter The maximum number of iterations.
#' #' @param ntree  The number of trees to grow.
#' #' @param parallelize The type of parallelization to use. Options are "no", "variables", or "forests".
#' #' @param ncores The number of cores to use for parallelization.
#' #' @param exclude_cols The columns to exclude from imputation.
#' #' @param show_na_percentage If TRUE, the percentage of missing values in each column is displayed.
#' #'
#' #' @return The imputed dataset.
#' #' @export
#' #'
#' #' @examples
#' #' # Data before imputation
#' #' test_data <- example_data |>
#' #'   dplyr::select(DAid, Assay, NPX) |>
#' #'   tidyr::pivot_wider(names_from = "Assay", values_from = "NPX") |>
#' #'   dplyr::slice_head(n = 100)
#' #' test_data
#' #'
#' #' # Data after imputation
#' #' impute_missForest(test_data, maxiter = 1, ntree = 50, parallelize = "no")
#' impute_missForest <- function(olink_data,
#'                               wide = TRUE,
#'                               maxiter = 10,
#'                               ntree = 100,
#'                               parallelize = "variables",
#'                               ncores = 4,
#'                               exclude_cols = c("DAid", "Disease"),
#'                               show_na_percentage = TRUE) {
#'
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'   data_in <- wide_data |>
#'     dplyr::select(-dplyr::any_of(exclude_cols))
#'
#'   if (isTRUE(show_na_percentage)) {
#'     na_percentages <- calc_na_percentage_col(data_in)
#'     print(na_percentages)
#'   }
#'   if (parallelize == "no") {
#'     ncores <- 1
#'   } else {
#'     doParallel::registerDoParallel(cores = ncores)
#'   }
#'   set.seed(123)
#'   data_in <- as.data.frame(data_in)  # Convert to data frame for missForest
#'   imputed_data <- missForest::missForest(data_in, maxiter = maxiter, ntree = ntree,
#'                                          verbose = T, parallelize = parallelize)$ximp
#'   imputed_data <- tibble::as_tibble(imputed_data)
#'
#'   cols <- wide_data |>
#'     dplyr::select(dplyr::any_of(exclude_cols))
#'   imputed_data <- dplyr::bind_cols(cols, imputed_data)
#'
#'   return(imputed_data)
#' }
#'
#'
#' #' Impute via MICE
#' #'
#' #' `impute_mice()` imputes missing values in a dataset using the MICE algorithm.
#' #' It allows the user to exclude certain columns from imputation and can also display the
#' #' percentage of missing values in each column before imputation. The user can also specify
#' #' the number of imputed datasets to create, the maximum number of iterations, and the imputation
#' #' method to use.
#' #'
#' #' @param olink_data The input dataset.
#' #' @param wide If TRUE, the data is in wide format.
#' #' @param m The number of imputed datasets to create. Default is 5.
#' #' @param maxit The maximum number of iterations. Default is 5.
#' #' @param method The imputation method to use. Type `methods(mice::mice)` after `library(mice)` for all options. Default is "pmm".
#' #' @param exclude_cols The columns to exclude from imputation.
#' #' @param show_na_percentage If TRUE, the percentage of missing values in each column is displayed.
#' #'
#' #' @return The imputed dataset.
#' #' @export
#' #'
#' #' @examples
#' #' # Data before imputation
#' #' test_data <- example_data |>
#' #'   dplyr::select(DAid, Assay, NPX) |>
#' #'   tidyr::pivot_wider(names_from = "Assay", values_from = "NPX") |>
#' #'   dplyr::slice_head(n = 100)
#' #' test_data
#' #'
#' #' # Data after imputation
#' #' impute_mice(test_data)
#' impute_mice <- function(olink_data,
#'                         wide = TRUE,
#'                         m = 5,
#'                         maxit = 5,
#'                         method = "pmm",
#'                         exclude_cols = c("DAid", "Disease"),
#'                         show_na_percentage = TRUE) {
#'
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'   data_in <- wide_data |>
#'     dplyr::select(-dplyr::any_of(exclude_cols))
#'
#'   if (isTRUE(show_na_percentage)) {
#'     na_percentages <- calc_na_percentage_col(data_in)
#'     print(na_percentages)
#'   }
#'
#'   data_in <- mice::mice(data_in, m = m, maxit = maxit, method = method, seed = 123, printFlag = F)
#'   imputed_data <- mice::complete(data_in)
#'
#'   cols <- wide_data |>
#'     dplyr::select(dplyr::any_of(exclude_cols))
#'   imputed_data <- dplyr::bind_cols(cols, imputed_data)
#'
#'   return(imputed_data)
#' }
