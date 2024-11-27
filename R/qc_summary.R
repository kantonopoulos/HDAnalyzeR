#' Check the column types of the dataset
#'
#' `check_col_types()` checks the column types of the input dataset and
#' returns the counts of each class.
#'
#' @param dat The input dataset.
#' @param unique_threshold The threshold to consider a numeric variable as categorical. Default is 5.
#'
#' @return A table with the counts of each class in the dataset.
#' @keywords internal
check_col_types <- function(dat, unique_threshold = 5) {
  # Get the classes of all columns
  col_classes <- lapply(dat, function(column) hd_detect_vartype(column, unique_threshold = unique_threshold))

  # Summarize the counts of each class
  class_summary <- table(unlist(col_classes))

  return(class_summary)
}

#' Calculate the percentage of NAs in each column of the dataset
#'
#' `calc_na_percentage_col()` calculates the percentage of NAs in each column of the input dataset.
#' It filters out the columns with 0% missing data and returns the rest in descending order.
#'
#' @param dat The input dataset.
#'
#' @return A tibble with the column names and the percentage of NAs in each column.
#' @keywords internal
calc_na_percentage_col <- function(dat) {

  na_percentage <- dat |>
    dplyr::summarise_all(~ round(sum(is.na(.)) / dplyr::n() * 100, 1)) |>
    tidyr::gather(key = "column", value = "na_percentage") |>
    dplyr::filter(!!rlang::sym("na_percentage") > 0) |>  # Filter out columns with no NAs
    dplyr::arrange(dplyr::desc(!!rlang::sym("na_percentage")))

  return(na_percentage)
}


#' Calculate the percentage of NAs in each row of the dataset
#'
#' `calc_na_percentage_row()` calculates the percentage of NAs in each row of the input dataset.
#' It filters out the rows with 0% missing data and returns the rest in descending order.
#'
#' @param dat The input dataset.
#' @param sample_id The name of the column containing the sample IDs.
#'
#' @return A tibble with the DAids and the percentage of NAs in each row.
#' @keywords internal
calc_na_percentage_row <- function(dat, sample_id) {

  na_percentage <- dat |>
    dplyr::rowwise() |>
    dplyr::mutate(na_percentage = round(sum(is.na(dplyr::across(dplyr::everything())))/ncol(dat) * 100, 1)) |>
    dplyr::ungroup() |>
    dplyr::filter(!!rlang::sym("na_percentage") > 0) |>
    dplyr::arrange(dplyr::desc(!!rlang::sym("na_percentage"))) |>
    dplyr::select(dplyr::any_of(c(sample_id, "na_percentage")))

  return(na_percentage)
}


#' Print the summary of the quality control results
#'
#' `print_summary()` prints the summary of the quality control results of the
#' input dataset. It includes the number of samples and variables, the counts of
#' each class, the percentage of NAs in each column and row, the normality test
#' results, the protein-protein correlations above a certain threshold, and the
#' correlation heatmap.
#'
#' @param sample_n The number of samples.
#' @param var_n The number of variables.
#' @param class_summary A table with the counts of each class in the dataframe.
#' @param na_percentage_col A tibble with the column names and the percentage of NAs in each column.
#' @param na_percentage_row A tibble with the DAids and the percentage of NAs in each row.
#' @param cor_results A tibble with the filtered protein pairs and their correlation values.
#' @param cor_threshold The reporting protein-protein correlation threshold.
#'
#' @return NULL
#' @keywords internal
print_summary <- function(sample_n,
                          var_n,
                          class_summary,
                          na_percentage_col,
                          na_percentage_row = NULL,
                          cor_results = NULL,
                          cor_threshold = 0.8) {

  print("Summary:")
  print("Note: In case of long output, only the first 10 rows are shown. To see the rest display the object with view()")
  print(paste0("Number of samples: ", sample_n))
  print(paste0("Number of variables: ", var_n))
  print("--------------------------------------")
  for (class_name in names(class_summary)) {
    print(paste(class_name, ":", class_summary[class_name]))
  }
  print("--------------------------------------")
  print("NA percentage in each column:")
  print(na_percentage_col)
  print("--------------------------------------")
  if (!is.null(na_percentage_row)) {
    print("NA percentage in each row:")
    print(na_percentage_row)
    print("--------------------------------------")
  }
  if (!is.null(cor_results)) {
    print(paste0("Protein-protein correlations above ", cor_threshold, ":"))
    print(cor_results)
    print("--------------------------------------")
  }

  invisible(NULL)
}


#' Create the missing value distribution
#'
#' `plot_missing_values()` creates a histogram of the missing value distribution.
#'
#' @param missing_values A tibble with the column/row names and the percentage of NAs in each column/row.
#' @param yaxis_name The name of the y-axis.
#'
#' @return A histogram of the missing value distribution.
#' @keywords internal
plot_missing_values <- function(missing_values, yaxis_name) {

  na_histogram <- missing_values |>
    ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym("na_percentage"))) +
    ggplot2::geom_histogram() +
    ggplot2::labs(x = "Missing value percentage", y = yaxis_name) +
    theme_hd()

  return(na_histogram)
}

#' Plot summary visualization for Sex, Age and BMI metadata
#'
#' `plot_metadata_summary()` creates three plots:
#'    - Two ridge plots for the Age and BMI distributions.
#'    - A bar plot for the number of samples per Sex.
#'
#' @param metadata A dataset containing the metadata information with the sample ID as the first column.
#' @param sample_id The name of the column containing the sample IDs.
#' @param variable The name of the column containing the different classes.
#' @param palette A list of color palettes for the plots. The names of the list should match the column names in the metadata. Default is NULL.
#' @param unique_threshold The threshold to consider a numeric variable as categorical. Default is 5.
#'
#' @return A list containing plots and sample counts.
#' @keywords internal
plot_metadata_summary <- function(metadata, sample_id, variable, palette = NULL, unique_threshold = 5) {

  col_classes <- lapply(metadata |> dplyr::select(-rlang::sym(sample_id)),
                        function(column) hd_detect_vartype(column, unique_threshold = unique_threshold))

  plot_list <- list()
  for (col in names(col_classes)) {
    Variable <- rlang::sym(col)
    Class_var <- rlang::sym(variable)
    if (col_classes[[col]] == "continuous"){
      dist_plot <- metadata |>
        ggplot2::ggplot(ggplot2::aes(x = !!Variable, y = !!Class_var, fill = !!Class_var)) +
        ggridges::geom_density_ridges(alpha = 0.7, scale = 0.9) +
        ggplot2::labs(x = col, y = variable) +
        theme_hd() +
        ggplot2::theme(legend.position = "none")

      if (!is.null(palette[[col]])){
        dist_plot <- apply_palette(dist_plot, palette[[col]], type = "fill")
      }

      plot_list[[col]] <- dist_plot
    }

    if (col_classes[[col]] == "categorical"){

      barplot <- metadata |>
        dplyr::count(!!Class_var, !!Variable) |>
        ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym("n"), y = !!Class_var, fill = !!Variable)) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::labs(x = "Number of samples", y = variable) +
        theme_hd() +
        ggplot2::theme()

      if (!is.null(palette[[col]])){
        barplot <- apply_palette(barplot, palette[[col]], type = "fill")
      }

      if (col != variable){
        plot_list[[col]] <- barplot
      }
    }
  }

  return(plot_list)
}


#' Summarize the quality control results of Olink data
#'
#' `qc_summary_data()` summarizes the quality control results of the input dataset.
#' It can handles both long and wide dataframes. The function checks the column types,
#' calculates the percentage of NAs in each column and row, performs a normality test,
#' calculates the protein-protein correlations, and creates a heatmap of the correlations.
#' The user can specify the reporting protein-protein correlation threshold.
#'
#' @param wide_data A dataset in wide format and sample_id as its first column.
#' @param sample_id The name of the column containing the sample IDs.
#' @param unique_threshold The threshold to consider a numeric variable as categorical. Default is 5.
#' @param cor_threshold The threshold to consider a protein-protein correlation as high. Default is 0.8.
#' @param cor_method The method to calculate the correlation. Default is "pearson".
#' @param verbose Whether to print the summary. Default is TRUE.
#'
#' @return A list containing the qc summery of data
#' @keywords internal
qc_summary_data <- function(wide_data, sample_id, unique_threshold = 5, cor_threshold = 0.8, cor_method = "pearson", verbose = TRUE) {

  wide_data <- wide_data |>
    dplyr::select(rlang::sym(sample_id), dplyr::where(is.numeric))
  sample_n <- nrow(wide_data)
  protein_n <- ncol(wide_data)
  class_summary <- check_col_types(wide_data, unique_threshold)
  na_percentage_col <- calc_na_percentage_col(wide_data)
  na_col_dist <- plot_missing_values(na_percentage_col, "Number of Features")
  na_percentage_row <- calc_na_percentage_row(wide_data, sample_id)
  na_row_dist <- plot_missing_values(na_percentage_row, "Number of Samples")
  cor <- hd_plot_cor_heatmap(wide_data |> dplyr::select(-rlang::sym(sample_id)),
                             threshold = cor_threshold,
                             method = cor_method)
  cor_matrix <- cor[["cor_matrix"]]
  cor_results <- cor[["cor_results"]]
  p <- cor[["cor_heatmap"]]

  if (isTRUE(verbose)) {
    print_summary(sample_n,
                  protein_n,
                  class_summary,
                  na_percentage_col,
                  na_percentage_row,
                  cor_results,
                  cor_threshold)
  }

  return(list("na_percentage_col" = na_percentage_col,
              "na_col_hist" = na_col_dist,
              "na_percentage_row" = na_percentage_row,
              "na_row_hist" = na_row_dist,
              "cor_matrix" = cor_matrix,
              "cor_results" = cor_results,
              "cor_heatmap" = p))
}


#' Summarize the quality control results of metadata
#'
#' `qc_summary_metadata()` summarizes the quality control results of the metadata dataframe.
#' It checks the column types, calculates the percentage of NAs in each column and row,
#' and creates summary visualizations for categorical and numeric variables.
#'
#' @param metadata A dataset containing the metadata information with the sample ID as the first column.
#' @param sample_id The name of the column containing the sample IDs.
#' @param variable The name of the column containing the different classes.
#' @param palette A list of color palettes for the plots. The names of the list should match the column names in the metadata. Default is NULL.
#' @param unique_threshold The threshold to consider a numeric variable as categorical. Default is 5.
#' @param verbose Whether to print the summary. Default is TRUE.
#'
#' @return A list of the qc summary of data
#' @keywords internal
qc_summary_metadata <- function(metadata, sample_id, variable, palette = NULL, unique_threshold = 5, verbose = TRUE) {

  sample_n <- nrow(metadata)
  var_n <- ncol(metadata)
  class_summary <- check_col_types(metadata)
  na_percentage_col <- calc_na_percentage_col(metadata)
  na_col_dist <- plot_missing_values(na_percentage_col, "Number of Metadata Variables")
  na_percentage_row <- calc_na_percentage_row(metadata, sample_id)
  na_row_dist <- plot_missing_values(na_percentage_row, "Number of Metadata Samples")

  if (isTRUE(verbose)) {
    print_summary(sample_n, var_n, class_summary, na_percentage_col, na_percentage_row)
  }

  metadata_plot <- plot_metadata_summary(metadata, sample_id, variable, palette, unique_threshold)

  na_list <- list("na_percentage_col" = na_percentage_col,
                  "na_col_hist" = na_col_dist,
                  "na_percentage_row" = na_percentage_row,
                  "na_row_hist" = na_row_dist)

  res_list <- c(na_list, metadata_plot)

  return(res_list)
}


#' Summarize the quality control results of the input dataset
#'
#' `hd_qc_summary()` summarizes the quality control results of the input dataset.
#' It returns general information about the datasets, missing value information,
#' protein-protein correlations, and metadata summary visualizations.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample_id as its first column.
#' @param metadata A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.
#' @param variable The name of the column containing the different classes (for example the column that contains your case and control groups).
#' @param palette A list of color palettes for the plots. The names of the list should match the column names in the metadata. Default is NULL.
#' @param unique_threshold The threshold to consider a numeric variable as categorical. Default is 5.
#' @param cor_threshold The threshold to consider a protein-protein correlation as high. Default is 0.8.
#' @param cor_method The method to calculate the correlation. Default is "pearson".
#' @param verbose Whether to print the summary. Default is TRUE.
#'
#' @return A list containing the qc summary of data and metadata.
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data,
#'                            example_metadata |> dplyr::select(-Sample))
#'
#' # Run the quality control summary
#' hd_qc_summary(hd_object,
#'               variable = "Disease",
#'               palette = list(Disease = "cancers12", Sex = "sex"),
#'               cor_threshold = 0.7,
#'               verbose = FALSE)
hd_qc_summary <- function(dat, metadata, variable, palette = NULL, unique_threshold = 5, cor_threshold = 0.8, cor_method = "pearson", verbose = TRUE) {
  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data to run the PCA analysis.")
    }
    wide_data <- dat[["data"]]
    sample_id <- dat[["sample_id"]]
    metadata <- dat[["metadata"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
    var_name <- "Features"
  }
  check_numeric <- check_numeric_columns(wide_data)

  data_summary <- qc_summary_data(wide_data, sample_id, unique_threshold, cor_threshold, cor_method, verbose)
  metadata_summary <- qc_summary_metadata(metadata, sample_id, variable, palette, unique_threshold, verbose)

  qc_object <- list("data_summary" = data_summary, "metadata_summary" = metadata_summary)
  class(qc_object) <- "hd_qc"

  return(qc_object)
}
