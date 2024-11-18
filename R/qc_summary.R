#' utils::globalVariables(c("DAid", "Assay", "NPX", "adj.P.Val", "Age", "BMI", "n", "na_percentage"))
#' #' Check the column types of the dataset
#' #'
#' #' `check_col_types()` checks the column types of the input dataset and
#' #' returns the counts of each class.
#' #'
#' #' @param df The input dataset.
#' #'
#' #' @return A table with the counts of each class in the dataset.
#' #' @keywords internal
#' check_col_types <- function(df) {
#'   # Get the classes of all columns
#'   col_classes <- lapply(df, class)
#'
#'   # Summarize the counts of each class
#'   class_summary <- table(unlist(col_classes))
#'
#'   return(class_summary)
#' }
#'
#' #' Calculate the percentage of NAs in each column of the dataset
#' #'
#' #' `calc_na_percentage_col()` calculates the percentage of NAs in each column of the input dataset.
#' #' It filters out the columns with 0% missing data and returns the rest in descending order.
#' #'
#' #' @param df The input dataset.
#' #'
#' #' @return A tibble with the column names and the percentage of NAs in each column.
#' #' @keywords internal
#' calc_na_percentage_col <- function(df) {
#'
#'   na_percentage <- df |>
#'     dplyr::summarise_all(~ round(sum(is.na(.)) / dplyr::n() * 100, 1)) |>
#'     tidyr::gather(key = "column", value = "na_percentage") |>
#'     dplyr::filter(na_percentage > 0) |>  # Filter out columns with no NAs
#'     dplyr::arrange(dplyr::desc(na_percentage))
#'
#'   return(na_percentage)
#' }
#'
#'
#' #' Calculate the percentage of NAs in each row of the dataset
#' #'
#' #' `calc_na_percentage_row()` calculates the percentage of NAs in each row of the input dataset.
#' #' It filters out the rows with 0% missing data and returns the rest in descending order.
#' #'
#' #' @param df The input dataset.
#' #'
#' #' @return A tibble with the DAids and the percentage of NAs in each row.
#' #' @keywords internal
#' calc_na_percentage_row <- function(df) {
#'
#'   na_percentage <- df |>
#'     dplyr::rowwise() |>
#'     dplyr::mutate(na_percentage = round(sum(is.na(dplyr::across(dplyr::everything())))/ncol(df) * 100, 1)) |>
#'     dplyr::ungroup() |>
#'     dplyr::filter(na_percentage > 0) |>
#'     dplyr::arrange(dplyr::desc(na_percentage)) |>
#'     dplyr::select(DAid, na_percentage)
#'
#'   return(na_percentage)
#' }
#'
#'
#' #' Check normality of the data
#' #'
#' #' `check_normality()` checks the normality of the input dataset using the Shapiro-Wilk test.
#' #' It performs the test and returns the p-values, adjusted p-values, and
#' #' normality status for each protein.
#' #'
#' #' @param df The input dataset.
#' #'
#' #' @details If the number of rows is greater than 5000, a random sample of 5000 rows is taken.
#' #' Shapiro-Wilk test is not working properly with large datasets.
#' #'
#' #' @return A tibble with the protein names, p-values, adjusted p-values, and normality status.
#' #' @keywords internal
#' check_normality <- function(df) {
#'
#'   if (nrow(df) >= 5000) {
#'     df <- df |> dplyr::sample_n(5000)
#'   }
#'
#'   # Perform Shapiro-Wilk test
#'   p_values <- lapply(df |>
#'                        dplyr::select(-dplyr::any_of(c("DAid"))),
#'                      function(column) {
#'     stats::shapiro.test(column)$p.value
#'   })
#'
#'   adj.P.Val <- stats::p.adjust(unlist(p_values), method = "BH")
#'
#'   # Determine normality based on adjusted p-values
#'   normality <- adj.P.Val > 0.05
#'
#'   normality_results <- tibble::tibble(
#'     Protein = names(p_values),
#'     p_value = unlist(p_values),
#'     adj.P.Val = adj.P.Val,
#'     is_normal = normality
#'   ) |>
#'     dplyr::arrange((adj.P.Val))
#'
#'   return(normality_results)
#' }
#'
#'
#' #' Print the summary of the quality control results
#' #'
#' #' `print_summary()` prints the summary of the quality control results of the
#' #' input dataset. It includes the number of samples and variables, the counts of
#' #' each class, the percentage of NAs in each column and row, the normality test
#' #' results, the protein-protein correlations above a certain threshold, and the
#' #' correlation heatmap.
#' #'
#' #' @param sample_n The number of samples.
#' #' @param var_n The number of variables.
#' #' @param class_summary A table with the counts of each class in the dataframe.
#' #' @param na_percentage_col A tibble with the column names and the percentage of NAs in each column.
#' #' @param na_percentage_row A tibble with the DAids and the percentage of NAs in each row.
#' #' @param normality_results  A tibble with the protein names, p-values, adjusted p-values, and normality status.
#' #' @param cor_results A tibble with the filtered protein pairs and their correlation values.
#' #' @param heatmap A heatmap of protein-protein correlations.
#' #' @param threshold The reporting protein-protein correlation threshold.
#' #'
#' #' @return NULL
#' #' @keywords internal
#' print_summary <- function(sample_n,
#'                           var_n,
#'                           class_summary,
#'                           na_percentage_col,
#'                           na_percentage_row,
#'                           normality_results = FALSE,
#'                           cor_results = FALSE,
#'                           heatmap = FALSE,
#'                           threshold = FALSE) {
#'
#'   print("Summary:")
#'   print("Note: In case of long output, only the first 10 rows are shown. To see the rest display the object with view()")
#'   print(paste0("Number of samples: ", sample_n))
#'   print(paste0("Number of variables: ", var_n))
#'   print("--------------------------------------")
#'   for (class_name in names(class_summary)) {
#'     print(paste(class_name, ":", class_summary[class_name]))
#'   }
#'   print("--------------------------------------")
#'   print("NA percentage in each column:")
#'   print(na_percentage_col)
#'   print("--------------------------------------")
#'   print("NA percentage in each row:")
#'   print(na_percentage_row)
#'   print("--------------------------------------")
#'   if (!isFALSE(normality_results)) {
#'     print("Normality test results:")
#'     print(normality_results)
#'     print("--------------------------------------")
#'   }
#'   if (!isFALSE(cor_results)) {
#'     print(paste0("Protein-protein correlations above ", threshold, ":"))
#'     print(cor_results)
#'     print("--------------------------------------")
#'   }
#'   if (!isFALSE(heatmap)) {
#'     print("Correlation heatmap:")
#'     print(heatmap)
#'     print("--------------------------------------")
#'   }
#'
#'   invisible(NULL)
#' }
#'
#'
#' #' Create the missing value distribution
#' #'
#' #' `plot_missing_values()` creates a histogram of the missing value distribution.
#' #'
#' #' @param missing_values A tibble with the column/row names and the percentage of NAs in each column/row.
#' #' @param yaxis_name The name of the y-axis.
#' #'
#' #' @return A histogram of the missing value distribution.
#' #' @keywords internal
#' plot_missing_values <- function(missing_values, yaxis_name) {
#'
#'   na_histogram <- missing_values |>
#'     ggplot2::ggplot(ggplot2::aes(x = na_percentage)) +
#'     ggplot2::geom_histogram() +
#'     ggplot2::labs(x = "Missing value percentage", y = yaxis_name) +
#'     theme_hpa()
#'
#'   return(na_histogram)
#' }
#'
#' #' Plot summary visualization for Sex, Age and BMI metadata
#' #'
#' #' `plot_metadata_summary()` creates three plots:
#' #'    - Two ridge plots for the Age and BMI distributions.
#' #'    - A bar plot for the number of samples per Sex.
#' #'
#' #' @param metadata The metadata dataframe.
#' #' @param categorical The categorical variables to summarize. Default is "Sex".
#' #' @param numerical The numerical variables to summarize. Default is "Age".
#' #' @param disease_palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`.
#' #' @param categ_palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is "sex_hpa".
#' #'
#' #' @return A list containing plots and sample counts.
#' #' @keywords internal
#' plot_metadata_summary <- function(metadata,
#'                                   categorical = "Sex",
#'                                   numerical = "Age",
#'                                   disease_palette = NULL,
#'                                   categ_palette = "sex_hpa") {
#'
#'   plot_list <- list()
#'   counts_list <- list()
#'
#'   for (col in numerical) {
#'     Variable <- rlang::sym(col)
#'     dist_plot <- metadata |>
#'       ggplot2::ggplot(ggplot2::aes(x = !!Variable, y = Disease, fill = Disease)) +
#'       ggridges::geom_density_ridges(alpha = 0.7, scale = 0.9) +
#'       ggplot2::labs(x = col, y = "Disease") +
#'       theme_hpa() +
#'       ggplot2::theme(legend.position = "none")
#'
#'     if (is.null(names(disease_palette)) && !is.null(disease_palette)) {
#'       dist_plot <- dist_plot + scale_fill_hpa(disease_palette)
#'     } else if (!is.null(disease_palette)) {
#'       dist_plot <- dist_plot + ggplot2::scale_fill_manual(values = disease_palette)
#'     }
#'
#'     plot_list[[paste0("distplot_", col)]] <- dist_plot
#'   }
#'
#'   for (col in categorical) {
#'     Variable <- rlang::sym(col)
#'     counts <- metadata |>
#'       dplyr::count(Disease, !!Variable)
#'     message(paste0(col, " contains:"))
#'     print(counts)
#'     barplot <- counts |>
#'       ggplot2::ggplot(ggplot2::aes(x = n, y = Disease, fill = !!Variable)) +
#'       ggplot2::geom_bar(stat = "identity", position = "stack") +
#'       ggplot2::labs(x = "Number of samples", y = "") +
#'       theme_hpa()
#'
#'     if (is.null(names(categ_palette)) && !is.null(categ_palette)) {
#'       barplot <- barplot + scale_fill_hpa(categ_palette)
#'     } else if (!is.null(categ_palette)) {
#'       barplot <- barplot + ggplot2::scale_fill_manual(values = categ_palette)
#'     }
#'
#'     counts_list[[paste0("count_", col)]] <- counts
#'     plot_list[[paste0("barplot_", col)]] <- barplot
#'   }
#'
#'   res_list <- c(plot_list, counts_list)
#'   return(res_list)
#' }
#'
#'
#' #' Summarize the quality control results of Olink data
#' #'
#' #' `qc_summary_data()` summarizes the quality control results of the input dataset.
#' #' It can handles both long and wide dataframes. The function checks the column types,
#' #' calculates the percentage of NAs in each column and row, performs a normality test,
#' #' calculates the protein-protein correlations, and creates a heatmap of the correlations.
#' #' The user can specify the reporting protein-protein correlation threshold.
#' #'
#' #' @param df The input dataset.
#' #' @param wide Whether the input dataset is in wide format. Default is TRUE.
#' #' @param threshold The reporting protein-protein correlation threshold. Default is 0.8.
#' #' @param cor_method The correlation method. Default is "pearson".
#' #' @param report Whether to print the summary. Default is TRUE.
#' #'
#' #' @details The correlation method is Pearson and the normality test is Shapiro-Wilk.
#' #' If the wide dataset contains more than 5000 rows, a random sample of 5000 rows
#' #' is taken to assess normality as it is a requirement from Shapiro-Wilk test.
#' #'
#' #' @return A list containing the following elements:
#' #'   - na_percentage_col: A tibble with the column names and the percentage of NAs in each column.
#' #'   - na_percentage_row: A tibble with the DAids and the percentage of NAs in each row.
#' #'   - normality_results: A tibble with the protein names, p-values, adjusted p-values, and normality status.
#' #'   - cor_matrix: A matrix of protein-protein correlations.
#' #'   - cor_results: A tibble with the filtered protein pairs and their correlation values.
#' #'   - heatmap: A heatmap of protein-protein correlations.
#' #' @export
#' #'
#' #' @examples
#' #' qc_res <- qc_summary_data(example_data, wide = FALSE, threshold = 0.7)
#' #'
#' #' qc_res$heatmap
#' qc_summary_data <- function(df, wide = TRUE, threshold = 0.8, cor_method = "pearson", report = TRUE) {
#'
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(df)
#'   } else {
#'     wide_data <- df
#'   }
#'   wide_data <- wide_data |>
#'     dplyr::select(DAid, dplyr::where(is.numeric))
#'   sample_n <- nrow(wide_data)
#'   protein_n <- ncol(wide_data) - 1
#'   class_summary <- check_col_types(wide_data)
#'   na_percentage_col <- calc_na_percentage_col(wide_data)
#'   na_col_dist <- plot_missing_values(na_percentage_col, "Number of Assays")
#'   na_percentage_row <- calc_na_percentage_row(wide_data)
#'   na_row_dist <- plot_missing_values(na_percentage_row, "Number of Samples")
#'   normality_results <- check_normality(wide_data)
#'   cor <- create_corr_heatmap(wide_data |> dplyr::select(-dplyr::any_of(c("DAid"))),
#'                              threshold = threshold,
#'                              method = cor_method,
#'                              show_heatmap = report)
#'   cor_matrix <- cor$cor_matrix
#'   cor_results <- cor$cor_results
#'   p <- cor$cor_plot
#'
#'   if (isTRUE(report)) {
#'     print_summary(sample_n,
#'                   protein_n,
#'                   class_summary,
#'                   na_percentage_col,
#'                   na_percentage_row,
#'                   normality_results,
#'                   cor_results,
#'                   p,
#'                   threshold)
#'   }
#'
#'   return(list("na_percentage_col" = na_percentage_col,
#'               "na_col_dist" = na_col_dist,
#'               "na_percentage_row" = na_percentage_row,
#'               "na_row_dist" = na_row_dist,
#'               "normality_results" = normality_results,
#'               "cor_matrix" = cor_matrix,
#'               "cor_results" = cor_results,
#'               "heatmap" = p))
#' }
#'
#'
#' #' Summarize the quality control results of metadata
#' #'
#' #' `qc_summary_metadata()` summarizes the quality control results of the metadata dataframe.
#' #' It checks the column types, calculates the percentage of NAs in each column and row,
#' #' and creates summary visualizations for user selected categorical and numeric variables.
#' #'
#' #' @param metadata The metadata dataframe.
#' #' @param categorical The categorical variables to summarize. Default is "Sex".
#' #' @param numerical The numeric variables to summarize. Default is "Age".
#' #' @param disease_palette The color palette for the different diseases. If it is a character, it should be one of the palettes from `get_hpa_palettes()`.
#' #' @param categ_palette The categorical color palette. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is "sex_hpa".
#' #' @param report Whether to print the summary. Default is TRUE.
#' #'
#' #' @return A list containing the following elements:
#' #'   - na_percentage_col: A tibble with the column names and the percentage of NAs in each column.
#' #'   - na_percentage_row: A tibble with the DAids and the percentage of NAs in each row.
#' #'   - Several distribution and barplots, as well as the counts of samples.
#' #' @export
#' #'
#' #' @examples
#' #' qc_res <- qc_summary_metadata(example_metadata, disease_palette = "cancers12")
#' #'
#' #' # Metadata distributions
#' #' qc_res$barplot_Sex
#' #' qc_res$distplot_Age
#' qc_summary_metadata <- function(metadata,
#'                                 categorical = "Sex",
#'                                 numerical = "Age",
#'                                 disease_palette = NULL,
#'                                 categ_palette = "sex_hpa",
#'                                 report = TRUE) {
#'
#'   sample_n <- nrow(metadata)
#'   var_n <- ncol(metadata) - 1
#'   class_summary <- check_col_types(metadata)
#'   na_percentage_col <- calc_na_percentage_col(metadata)
#'   na_col_dist <- plot_missing_values(na_percentage_col, "Number of Columns")
#'   na_percentage_row <- calc_na_percentage_row(metadata)
#'   na_row_dist <- plot_missing_values(na_percentage_row, "Number of Rows")
#'
#'   if (isTRUE(report)) {
#'     print_summary(sample_n, var_n, class_summary, na_percentage_col, na_percentage_row)
#'   }
#'
#'   metadata_plot <- plot_metadata_summary(metadata,
#'                                          categorical,
#'                                          numerical,
#'                                          disease_palette,
#'                                          categ_palette)
#'
#'   na_list <- list("na_percentage_col" = na_percentage_col,
#'                   "na_col_dist" = na_col_dist,
#'                   "na_percentage_row" = na_percentage_row,
#'                   "na_row_dist" = na_row_dist)
#'
#'   res_list <- c(na_list, metadata_plot)
#'
#'   return(res_list)
#' }
