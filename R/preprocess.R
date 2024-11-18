#' utils::globalVariables(c("PlateID", "Cohort", "Assay_Warning", "Exclude Sample"))
#' #' Replace specific values with NA
#' #'
#' #' `replace_with_na()` replaces the specified values in the input vector with NAs.
#' #'
#' #' @param df_in The input dataframe.
#' #' @param replace_w_na The values to replace with NA. Default is c(0, "0", "", "Unknown", "unknown", "none", NA, "na").
#' #'
#' #' @return The tibble with the specified values replaced with NA.
#' #' @keywords internal
#' replace_with_na <- function(df_in, replace_w_na = c(0, "0", "", "Unknown", "unknown", "none", NA, "na")) {
#'   df_out <- df_in |>
#'     dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(. %in% replace_w_na, NA, .)))
#'
#'   return(df_out)
#' }
#'
#'
#' #' Preprocess data
#' #'
#' #' `clean_data()` preprocesses the data by filtering out rows based on the specified criteria.
#' #'   - It keeps only the specified columns.
#' #'   - It keeps only the data of the specified plates and assays.
#' #'   - It can remove all samples with Assay_Warning != "PASS".
#' #'   - It removes rows with NAs in the DAid and NPX columns.
#' #'   - It replaces the specified values with NA.
#' #'
#' #' @param df_in The input dataframe.
#' #' @param keep_cols The columns to keep in the output dataframe.
#' #' @param filter_plates The plates to exclude.
#' #' @param filter_assays The assays to filter out.
#' #' @param filter_assay_warning If TRUE, only the rows with Assay_Warning == "PASS" are kept. Default is FALSE.
#' #' @param remove_na_cols The columns to check for NAs and remove respective rows. Defaults is c("DAid", "NPX").
#' #' @param replace_w_na The values to replace with NA. Default is c(0, "0", "", "Unknown", "unknown", "none", NA, "na").
#' #'
#' #' @return The preprocessed dataframe.
#' #' @export
#' #'
#' #' @examples
#' #' # Unprocessed data
#' #' example_data
#' #'
#' #' # Preprocessed data
#' #' clean_data(example_data, filter_plates = c("Plate1", "Plate2"), filter_assay_warning = TRUE)
#' clean_data <- function(df_in,
#'                        keep_cols = c("DAid", "Assay", "NPX"),
#'                        filter_plates = NULL,
#'                        filter_assays = NULL,
#'                        filter_assay_warning = FALSE,
#'                        remove_na_cols = c("DAid", "NPX"),
#'                        replace_w_na = c(0, "0", "", "Unknown", "unknown", "none", NA, "na")) {
#'
#'   df_out <- df_in |>
#'     dplyr::filter(if ("PlateID" %in% colnames(df_in)) {
#'                     !(PlateID %in% filter_plates)
#'                   } else {
#'                     TRUE
#'                   }) |>
#'     dplyr::filter(if (filter_assay_warning == T && "Assay_Warning" %in% colnames(df_in)) {
#'                     (Assay_Warning == "PASS")
#'                   } else {
#'                     TRUE
#'                   }) |>
#'     dplyr::filter(!(Assay %in% filter_assays)) |>
#'     dplyr::select(dplyr::any_of(keep_cols))
#'
#'     if (!is.null(replace_w_na)) {
#'       df_out <- replace_with_na(df_out, replace_w_na)
#'     }
#'
#'     if (!is.null(remove_na_cols)) {
#'       rows_before <- nrow(df_out)
#'       df_out <- df_out |>
#'         dplyr::filter(dplyr::if_any(dplyr::any_of(remove_na_cols), ~!is.na(.)))
#'       rows_after <- nrow(df_out)
#'       if (rows_before != rows_after) {
#'         message("Removed ", rows_before - rows_after, " rows with NAs based on ", remove_na_cols)
#'       }
#'     }
#'
#'     return(df_out)
#' }
#'
#'
#' Preprocess metadata
#'
#' `clean_metadata()` preprocesses the metadata by filtering out rows based on the specified criteria.
#'   - It keeps only the specified columns.
#'   - It keeps only the data of the specified cohort.
#'   - It removes rows with NAs in the DAid and Disease columns.
#' #'   - It replaces the specified values with NA.
#' #'
#' #' @param df_in The input metadata.
#' #' @param keep_cols The columns to keep in the output metadata.
#' #' @param cohort The cohort to keep.
#' #' @param remove_na_cols The columns to check for NAs and remove respective rows. Defaults is c("DAid", "Disease").
#' #' @param replace_w_na The values to replace with NA. Default is c("Unknown", "unknown", "none", NA, "na").
#' #'
#' #' @return The preprocessed metadata.
#' #' @export
#' #'
#' #' @examples
#' #' # Unprocessed metadata
#' #' example_metadata
#' #'
#' #' # Preprocessed metadata
#' #' clean_metadata(example_metadata)
#' clean_metadata <- function(df_in,
#'                            keep_cols = c("DAid", "Disease", "Sex", "Age", "BMI"),
#'                            cohort = NULL,
#'                            remove_na_cols = c("DAid", "Disease"),
#'                            replace_w_na = c("Unknown", "unknown", "none", NA, "na")) {
#'
#'   df_out <- df_in |>
#'     dplyr::filter(if ("Cohort" %in% colnames(df_in)) {
#'       is.null(cohort) | Cohort %in% cohort
#'     } else {
#'       TRUE
#'     }) |> dplyr::select(dplyr::any_of(keep_cols))
#'
#'   if (!is.null(replace_w_na)) {
#'     df_out <- replace_with_na(df_out, replace_w_na)
#'   }
#'
#'   if (!is.null(remove_na_cols)) {
#'     rows_before <- nrow(df_out)
#'     df_out <- df_out |>
#'       dplyr::filter(dplyr::if_any(dplyr::any_of(remove_na_cols), ~!is.na(.)))
#'     rows_after <- nrow(df_out)
#'     if (rows_before != rows_after) {
#'       message("Removed ", rows_before - rows_after, " rows with NAs based on ", remove_na_cols)
#'     }
#'   }
#'
#'   return(df_out)
#' }
