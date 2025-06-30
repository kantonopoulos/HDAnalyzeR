#' Calculate NA percentage
#'
#' `calculate_na_percentage()` calculates the percentage of missing values in each column of a dataset.
#'
#' @param dat A dataset.
#'
#' @return A tibble containing the column names and their respective NA percentages.
#' @keywords internal
calculate_na_percentage <- function(dat) {
  tibble::tibble(
    Variable = colnames(dat),
    NA_percentage = sapply(dat, function(col) mean(is.na(col)) * 100)
  )
}


#' Heatmap summary of missing values
#'
#' `hd_na_search()` provides a visual summary of missing values in a dataset as an annotated heatmap.
#' It allows the user to specify the metadata columns to include in the summary and the color palette
#' to use for the heatmap annotations.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.
#' @param metadata A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.
#' @param annotation_vars The metadata columns to include in the summary.
#' @param palette A list of color palettes for the heatmap annotations. The names of the list should match the column names in `annotation_vars`. Default is NULL.
#' @param x_labels If TRUE, show x-axis labels. Default is FALSE.
#' @param y_labels If TRUE, show y-axis labels. Default is FALSE.
#'
#' @return A list containing the summary of missing values and the heatmap visualization.
#' @export
#'
#' @details When using continuous metadata variables, they are automatically binned
#' into categories of 5 bins to make the heatmap more informative and easier to interpret.
#' If the user wants to use a different number of bins, they can bin the data before
#' using the `hd_bin_columns()` function and its `bins` argument.
#'
#' Also, when coloring annotations, the user can use custom palettes or the
#' Human Protein Atlas (HPA) palettes. It is not required to provide a palette
#' for all annotations, but when a palette is provided, it must be in correct
#' format (check examples bellow).
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Use custom palettes for coloring annotations
#' palette = list(Sex = c(M = "blue", F = "pink"))
#' na_res <- hd_na_search(hd_object,
#'                        annotation_vars = c("Age", "Sex"),
#'                        palette = palette)
#' na_res$na_heatmap
#'
#' # Use a mix of custom and HPA palettes for coloring annotations
#' palette = list(Disease = "cancers12", Sex = c(M = "blue", F = "pink"))
#' na_res <- hd_na_search(hd_object,
#'                        annotation_vars = c("Disease", "Sex"),
#'                        palette = palette)
#' na_res$na_heatmap
hd_na_search <- function(dat,
                         metadata = NULL,
                         annotation_vars = NULL,
                         palette = NULL,
                         x_labels = FALSE,
                         y_labels = FALSE) {
  # Prepare data
  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data.")
    }
    wide_data <- dat[["data"]]
    metadata <- dat[["metadata"]]
    sample_id <- dat[["sample_id"]]
    var_name <- dat[["var_name"]]
    value_name <- dat[["value_name"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
    var_name <- "Features"
    value_name <- "Values"
  }

  check_numeric <- check_numeric_columns(wide_data)

  if (is.null(metadata)) {
    stop("The 'metadata' argument or slot of the HDAnalyzeR object is empty. Please provide the metadata.")
  }
  if (!all(annotation_vars %in% colnames(metadata))) {
    message("Some category columns provided do not exist in the dataset.")
  }

  annotation_vars_type <- sapply(metadata |>
                                   dplyr::select(dplyr::any_of(c(annotation_vars, sample_id))),
                                 hd_detect_vartype)
  metadata <- hd_bin_columns(metadata |>
                               dplyr::select(dplyr::any_of(c(annotation_vars, sample_id))),
                             annotation_vars_type)

  long_data <- wide_data |>
    tidyr::pivot_longer(cols = -dplyr::all_of(sample_id),
                        names_to = var_name,
                        values_to = value_name,
                        values_drop_na = FALSE)

  join_data <- long_data |>
    dplyr::select(dplyr::all_of(c(sample_id, var_name, value_name))) |>
    dplyr::left_join(metadata |>
                       dplyr::select(dplyr::any_of(c(annotation_vars, sample_id))),
                     by = sample_id)

  # Calculate NA percentages
  na_data <- join_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(annotation_vars, var_name)))) |>
    dplyr::mutate(NA_percentage = mean(is.na(!!rlang::sym(value_name))) * 100) |>
    dplyr::ungroup() |>
    dplyr::mutate(Categories = paste(!!!rlang::syms(annotation_vars), sep = "_")) |>
    dplyr::select(dplyr::any_of(c(annotation_vars, "Categories", var_name, "NA_percentage"))) |>
    unique()

  if (max(na_data[["NA_percentage"]]) == 0) {
    stop("There are no missing values in the dataset!")
  }

  # Create heatmap
  if (x_labels == FALSE) {
    x_labs <- c("")
  } else {
    x_labs <- NULL
  }
  if (y_labels == FALSE) {
    y_labs <- c("")
  } else {
    y_labs <- NULL
  }

  # Prepare palettes
  for (i in seq_along(palette)) {
    if (isTRUE(palette[[i]] %in% names(hd_palettes()))) {
      palette[[names(palette[i])]] <- hd_palettes()[[palette[[i]]]]
    }
  }

  na_heatmap <- ggplotify::as.ggplot(
    tidyheatmaps::tidyheatmap(na_data,
                              rows = !!rlang::sym("Categories"),
                              columns = !!rlang::sym(var_name),
                              values = !!rlang::sym("NA_percentage"),
                              annotation_row = annotation_vars,
                              annotation_colors = palette,
                              cluster_rows = TRUE,
                              cluster_cols = TRUE,
                              show_selected_row_labels = x_labs,
                              show_selected_col_labels = y_labs,
                              treeheight_row = 20,
                              treeheight_col = 20,
                              silent = TRUE))

  return(list("na_data" = na_data, "na_heatmap" = na_heatmap))
}


#' Omit missing values
#'
#' `hd_omit_na()` removes rows with missing values from a dataset. It allows the user to
#' specify the columns to consider for the removal of missing values. If no columns are
#' provided, the function removes rows with missing values in any column.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.
#' @param columns The columns to consider for the removal of missing values.
#'
#' @return The dataset without the rows containing missing values.
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#' hd_object$data
#'
#' # Data after removing missing values
#' res <- hd_omit_na(hd_object)
#' res$data
#'
#' # Data after removing missing values in specific columns
#' res <- hd_omit_na(hd_object, columns = "AARSD1")
#' res$data
hd_omit_na <- function(dat, columns = NULL){

  # Prepare data
  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data.")
    }
    wide_data <- dat[["data"]]
  } else {
    wide_data <- dat
  }

  if (is.null(columns)) {
    imputed_data <- wide_data[stats::complete.cases(wide_data), ]
  } else {
    missing_columns <- setdiff(columns, colnames(wide_data))
    if (length(missing_columns) > 0) {
       stop("The following columns are not in the dataset: ", paste(missing_columns, collapse = ", "))
    }
    imputed_data <- wide_data[!rowSums(is.na(wide_data[columns])), ]
  }

  if (inherits(dat, "HDAnalyzeR")) {
    dat[["data"]] <- imputed_data
    return(dat)
  } else {
    return(imputed_data)
  }
}


#' Impute via Median
#'
#' `hd_impute_median()` imputes missing values in a dataset using the median of each column.
#' It can also display the percentage of missing values in each column before imputation.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.
#' @param verbose If TRUE, the percentage of missing values in each column is displayed.
#'
#' @return The imputed dataset.
#' @details This is the fastest but usually least accurate imputation method.
#'
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#' hd_object$data
#'
#' # Data after imputation
#' res <- hd_impute_median(hd_object)
#' res$data
hd_impute_median <- function(dat, verbose = TRUE) {

  # Prepare data
  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data.")
    }
    wide_data <- dat[["data"]]
    sample_id <- dat[["sample_id"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
  }

  check_numeric <- check_numeric_columns(wide_data)

  data_in <- wide_data |>
    dplyr::select(-dplyr::any_of(sample_id))

  if (isTRUE(verbose)) {
    na_percentages <- calculate_na_percentage(data_in) |>
      dplyr::filter(!!rlang::sym("NA_percentage") > 0)
    message(na_percentages)
  }

  recipe <- recipes::recipe(~ ., data = data_in) |>
    recipes::step_impute_median(recipes::all_predictors())

  imputed_data <- recipe |>
    recipes::prep() |>
    recipes::bake(new_data = NULL)

  cols <- wide_data |>
    dplyr::select(dplyr::any_of(sample_id))

  imputed_data <- dplyr::bind_cols(cols, imputed_data)

  if (inherits(dat, "HDAnalyzeR")) {
    dat[["data"]] <- imputed_data
    return(dat)
  } else {
    return(imputed_data)
  }
}


#' Impute via k-nearest neighbors
#'
#' `hd_impute_knn()` imputes missing values in a dataset using the k-nearest neighbors method.
#' It can also display the percentage of missing values in each column before imputation.
#' The user can also specify the number of neighbors to consider for imputation.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.
#' @param k The number of neighbors to consider for imputation. Default is 5.
#' @param seed The seed to be used in the imputation. Default is 123.
#' @param verbose If TRUE, the percentage of missing values in each column is displayed.
#'
#' @return The imputed dataset.
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#' hd_object$data
#'
#' # Data after imputation
#' res <- hd_impute_knn(hd_object, k = 3)
#' res$data
hd_impute_knn <- function(dat, k = 5, seed = 123, verbose = TRUE) {

  # Prepare data
  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data.")
    }
    wide_data <- dat[["data"]]
    sample_id <- dat[["sample_id"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
  }

  check_numeric <- check_numeric_columns(wide_data)

  data_in <- wide_data |>
    dplyr::select(-dplyr::any_of(sample_id))

  if (isTRUE(verbose)) {
    na_percentages <- calculate_na_percentage(data_in) |>
      dplyr::filter(!!rlang::sym("NA_percentage") > 0)
    message(na_percentages)
  }

  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  recipe <- recipes::recipe(~ ., data = data_in) |>
    recipes::step_impute_knn(recipes::all_predictors(), neighbors = k)

  imputed_data <- recipe |>
    recipes::prep() |>
    recipes::bake(new_data = NULL)

  cols <- wide_data |>
    dplyr::select(dplyr::any_of(sample_id))

  imputed_data <- dplyr::bind_cols(cols, imputed_data)

  if (inherits(dat, "HDAnalyzeR")) {
    dat[["data"]] <- imputed_data
    return(dat)
  } else {
    return(imputed_data)
  }
}


#' Impute via missForest
#'
#' `impute_missForest()` imputes missing values in a dataset using the `missForest` method.
#' It can also display the percentage of missing values in each column before imputation.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.
#' @param maxiter The maximum number of iterations.
#' @param ntree  The number of trees to grow.
#' @param parallelize If "no", the imputation is done in a single core. If "variables", the imputation is done in parallel for each variable. If "forest", the imputation is done in parallel for each tree. For more information, check the `missForest` documentation.
#' @param seed The seed to be used in the imputation. Default is 123.
#' @param verbose If TRUE, the percentage of missing values in each column is displayed.
#'
#' @return The imputed dataset.
#' @details This is the slowest and more complex imputation method. If KNN works fine, it is
#' recommended to use it instead of `missForest`. In case of large datasets, it is recommended
#' to parallelize the imputation. However, the user must have the `doParallel` package installed
#' and register a cluster before running the function. An example of how to parallelize the
#' imputation is provided in the examples section.
#'
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#' hd_object$data
#'
#' # Data after imputation
#' res <- hd_impute_missForest(hd_object, maxiter = 1, ntree = 50)
#' res$data
#'
#' \dontrun{
#' # Parallelize the imputation
#' library(doParallel)  # Load the doParallel package
#' cl <- makeCluster(4)  # Create a cluster with 4 cores
#' registerDoParallel(cl)  # Register the cluster
#' res <- hd_impute_missForest(hd_object, maxiter = 1, ntree = 50, parallelize = "forests")
#' }
hd_impute_missForest <- function(dat, maxiter = 10, ntree = 100, parallelize = "no", seed = 123, verbose = TRUE) {

  # Prepare data
  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data.")
    }
    wide_data <- dat[["data"]]
    sample_id <- dat[["sample_id"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
  }

  check_numeric <- check_numeric_columns(wide_data)

  data_in <- wide_data |>
    dplyr::select(-dplyr::any_of(sample_id))

  if (isTRUE(verbose)) {
    na_percentages <- calculate_na_percentage(data_in) |>
      dplyr::filter(!!rlang::sym("NA_percentage") > 0)
    message(na_percentages)
  }

  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  data_in <- as.data.frame(data_in)  # Convert to data frame for missForest
  imputed_data <- missForest::missForest(data_in,
                                         maxiter = maxiter,
                                         ntree = ntree,
                                         verbose = verbose,
                                         parallelize = parallelize)$ximp
  imputed_data <- tibble::as_tibble(imputed_data)

  cols <- wide_data |>
    dplyr::select(dplyr::any_of(sample_id))

  imputed_data <- dplyr::bind_cols(cols, imputed_data)

  if (inherits(dat, "HDAnalyzeR")) {
    dat[["data"]] <- imputed_data
    return(dat)
  } else {
    return(imputed_data)
  }
}
