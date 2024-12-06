#' Remove batch effects
#'
#' `remove_batch_effects()` removes batch effects from the data using the limma package.
#' It converts the dataframe into matrix and transposes it to get it ready for limma.
#' It removes the batch effects and then converts the data back to normal format.
#'
#' @param wide_data A tibble containing the data to be normalized. The data should be in wide format.
#' @param metadata A tibble containing the metadata information.
#' @param sample_id The column containing the sample ID information.
#' @param batch The metadata column containing the batch information.
#' @param batch2 The metadata column containing the second batch information. Default is NULL.
#'
#' @return A tibble containing the data without batch effects.
#' @export
#'
#' @keywords internal
remove_batch_effects <- function(wide_data,
                                 metadata,
                                 sample_id,
                                 batch,
                                 batch2 = NULL) {

  batch <- wide_data |>
    dplyr::left_join(metadata |>
                       dplyr::select(dplyr::any_of(c(sample_id, batch))),
                     by = sample_id) |>
    dplyr::pull(batch)
  if (!is.null(batch2)) {
    batch2 <- wide_data |>
      dplyr::left_join(metadata |>
                         dplyr::select(dplyr::any_of(c(sample_id, batch2))),
                       by = sample_id) |>
      dplyr::pull(batch2)
  }

  # Prepare the data for limma
  mat_data <- as.matrix(wide_data |> dplyr::select(-!!rlang::sym(sample_id)))
  transposed_mat <- t(mat_data)

  # Remove batch effects
  no_batch_effects_res <- limma::removeBatchEffect(transposed_mat, batch=batch, batch2=batch2)
  transposed_no_batch_effects <- t(no_batch_effects_res)
  no_batch_effects <- tibble::as_tibble(transposed_no_batch_effects)

  return(no_batch_effects)
}


#' Normalize data and remove batch effects
#'
#' `hd_run_normalization()` normalizes the data by scaling them and removing their batch effects.
#' It first converts the data to wide format if they are not already. It then removes
#' the batch effects and scales or centers the data. To remove batch effects, it uses the
#' `remove_batch_effects()`, that utilizes limma package. For scaling, it uses the `scale()`
#' from base R.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample_id as its first column.
#' @param metadata A dataset containing the metadata information. If a HDAnalyzeR object is provided, this parameter is not needed.
#' @param center A logical value indicating whether to center the data. Default is TRUE.
#' @param scale A logical value indicating whether to scale the data. Default is TRUE.
#' @param batch The metadata column containing the batch information. In order to correct for batch effects, this parameter should be provided. Default is NULL.
#' @param batch2 The metadata column containing the second batch information if available. Default is NULL.
#'
#' @return A tibble containing the normalized data.
#' @details
#' You can read more about the scaling and centering methods in the documentation of the `scale()` function in the `base` package,
#' as well as about the method for removing batch effects in the documentation of the `removeBatchEffect()` function in the `limma` package.
#'
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Center data
#' hd_run_normalization(hd_object, center = TRUE, scale = FALSE)
#'
#' # Center and scale data (z-score scaling)
#' hd_run_normalization(hd_object, center = TRUE, scale = TRUE)
#'
#' # Center, scale and remove batch effects
#' hd_run_normalization(hd_object, batch = "Cohort")
hd_run_normalization <- function(dat,
                                 metadata = NULL,
                                 center = TRUE,
                                 scale = TRUE,
                                 batch = NULL,
                                 batch2 = NULL) {

  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The HDAnalyzeR object does not contain any data.")
    }
    wide_data <- dat[["data"]]
    metadata <- dat[["metadata"]]
  } else {
    wide_data <- dat
  }
  id_col <- wide_data[1]
  check_numeric <- check_numeric_columns(wide_data)

  # Remove batch effects
  if (!is.null(batch)) {
    data_wo_batch_effects <- remove_batch_effects(wide_data,
                                                  metadata,
                                                  colnames(id_col),
                                                  batch = batch,
                                                  batch2 = batch2)
  } else {
    data_wo_batch_effects <- wide_data |> dplyr::select(-colnames(id_col))
  }

  # Scale the data
  scaled_data <- tibble::as_tibble(scale(data_wo_batch_effects, center = center, scale = scale))
  names(scaled_data) <- names(data_wo_batch_effects)

  scaled_data <- dplyr::bind_cols(id_col, scaled_data)

  return(scaled_data)
}
