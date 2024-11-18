#' #' Remove batch effects
#' #'
#' #' `remove_batch_effects()` removes batch effects from the data using the limma package.
#' #' It converts the dataframe into matrix and transposes it to get it ready for limma.
#' #' It removes the batch effects and then converts the data back to normal format.
#' #'
#' #' @param wide_data A tibble containing the data to be normalized. The data should be in wide format.
#' #' @param metadata A tibble containing the metadata information.
#' #' @param batch The metadata column containing the batch information.
#' #' @param batch2 The metadata column containing the second batch information. Default is NULL.
#' #'
#' #' @return A tibble containing the data without batch effects.
#' #' @export
#' #'
#' #' @keywords internal
#' remove_batch_effects <- function(wide_data,
#'                                  metadata,
#'                                  batch,
#'                                  batch2 = NULL
#'                                  ) {
#'
#'   batch <- wide_data |>
#'     dplyr::left_join(metadata |>
#'                        dplyr::select(dplyr::any_of(c("DAid", batch))),
#'                      by = "DAid") |>
#'     dplyr::pull(batch)
#'   if (!is.null(batch2)) {
#'     batch2 <- wide_data |>
#'       dplyr::left_join(metadata |>
#'                          dplyr::select(DAid, batch2),
#'                        by = "DAid") |>
#'       dplyr::pull(batch2)
#'   }
#'
#'   # Prepare the data for limma
#'   mat_data <- as.matrix(wide_data |> dplyr::select(-DAid))
#'   transposed_mat <- t(mat_data)
#'
#'   # Remove batch effects
#'   no_batch_effects_res <- limma::removeBatchEffect(transposed_mat, batch=batch, batch2=batch2)
#'   transposed_no_batch_effects <- t(no_batch_effects_res)
#'   no_batch_effects <- tibble::as_tibble(transposed_no_batch_effects)
#'
#'   return(no_batch_effects)
#' }
#'
#'
#' #' Normalize data and remove batch effects
#' #'
#' #' `normalize_data()` normalizes the data by scaling them and removing their batch effects.
#' #' It first converts the data to wide format if they are not already. It then removes
#' #' the batch effects and scales or centers the data. To remove batch effects, it uses the
#' #' `remove_batch_effects()`, that utilizes limma package. For scaling, it uses the `scale()`
#' #' from base R.
#' #'
#' #' @param olink_data A dataset containing Olink data to be normalized.
#' #' @param metadata A dataset containing the metadata information.
#' #' @param wide A logical value indicating whether the data is in wide format. Default is TRUE.
#' #' @param center A logical value indicating whether to center the data. Default is TRUE.
#' #' @param scale A logical value indicating whether to scale the data. Default is TRUE.
#' #' @param batch The metadata column containing the batch information. In order to correct for batch effects, this parameter should be provided. Default is NULL.
#' #' @param batch2 The metadata column containing the second batch information. Default is NULL.
#' #' @param return_long A logical value indicating whether to return the data in long format. Default is FALSE.
#' #' @param save A logical value indicating whether to save the data. Default is FALSE.
#' #' @param file_name The name of the file to be saved. Default is "normalized_data".
#' #'
#' #' @return A tibble containing the normalized data.
#' #' @export
#' #'
#' #' @examples
#' #' # Non-normalized data
#' #' example_data |>
#' #'   dplyr::select(DAid, Assay, NPX) |>
#' #'   tidyr::pivot_wider(names_from = "Assay", values_from = "NPX")
#' #'
#' #' # Center data
#' #' normalize_data(example_data, example_metadata, wide = FALSE, center = TRUE, scale = FALSE)
#' #'
#' #' # Center and scale data (z-score scaling)
#' #' normalize_data(example_data, example_metadata, wide = FALSE, center = TRUE, scale = TRUE)
#' #'
#' #' # Center, scale and remove batch effects
#' #' normalize_data(example_data, example_metadata, wide = FALSE, batch = "Cohort")
#' normalize_data <- function(olink_data,
#'                            metadata = NULL,
#'                            wide = TRUE,
#'                            center = TRUE,
#'                            scale = TRUE,
#'                            batch = NULL,
#'                            batch2 = NULL,
#'                            return_long = FALSE,
#'                            save = FALSE,
#'                            file_name = "normalized_data"
#'                            ) {
#'
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   # Prepare the data for scaling
#'   id_col <- wide_data |> dplyr::pull(DAid)
#'
#'   # Remove batch effects
#'   if (!is.null(batch)) {
#'     data_wo_batch_effects <- remove_batch_effects(wide_data, metadata, batch = batch, batch2 = batch2)
#'   } else {
#'     data_wo_batch_effects <- wide_data |> dplyr::select(-DAid)
#'   }
#'
#'   # Scale the data
#'   scaled_data <- tibble::as_tibble(scale(data_wo_batch_effects, center = center, scale = scale))
#'   names(scaled_data) <- names(data_wo_batch_effects)
#'
#'   # Prepare data to be returned
#'   scaled_data$DAid <- id_col
#'   scaled_data <- scaled_data |> dplyr::relocate(DAid, dplyr::everything())
#'
#'   if (isTRUE(return_long)) {
#'     scaled_data <- scaled_data |> tidyr::pivot_longer(cols = -DAid, names_to = "Assay", values_to = "NPX")
#'   }
#'
#'   if (isTRUE(save)) {
#'     dir_name <- create_dir("data/processed/data_metadata", date = T)
#'     save_df(scaled_data, dir_name, file_name, "rda")
#'   }
#'
#'   return(scaled_data)
#' }
#'
