# # Test remove_batch_effects ----------------------------------------------------
# test_that("remove_batch_effects removes batch effects", {
#   # Example based on limma example (https://rdrr.io/bioc/limma/man/removeBatchEffect.html)
#   test_data <- as.data.frame(t(matrix(rnorm(10*9), 10, 9)))
#   daid <- c(1:9)
#   batch <- c("A","A","A","B","B","B","C","C","C")
#   test_data <- test_data |>
#     dplyr::mutate(dplyr::across(1:3, ~ . + 5)) |>
#     dplyr::mutate(DAid = daid)
#   test_metadata <- tibble::tibble(DAid = daid, Batch = batch)
#   result <- remove_batch_effects(test_data, test_metadata, "Batch")
#   test_data <- test_data |> dplyr::select(-DAid)
#   expected <- tibble::as_tibble(t(limma::removeBatchEffect(t(test_data), batch)))
#   expect_equal(result, expected)
#   # It gives warning for the `name` as my function expects a tibble with names and not a matrix like the example
# })
#
#
# # Test normalize_data ---------------------------------------------------------
# test_that("normalize_data normalizes data properly", {
#   first_3_unique_assays <- example_data |>
#     dplyr::distinct(Assay) |>
#     dplyr::slice(1:3) |>
#     dplyr::pull(Assay)
#   first_3_unique_samples <- example_data |>
#     dplyr::distinct(DAid) |>
#     dplyr::slice(1:3) |>
#     dplyr::pull(DAid)
#
#   test_data <- example_data |>
#     dplyr::filter(Assay %in% first_3_unique_assays) |>
#     dplyr::filter(DAid %in% first_3_unique_samples)
#
#   result <- normalize_data(test_data, wide = FALSE) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
#
#   expected <- tibble::tibble(
#     DAid = c("DA00001", "DA00002"),
#     AARSD1 = c(0.71, -0.71),
#     ABL1 = c(0.71, -0.71),
#     ACAA1 = c(0.71, -0.71)
#   )
#   attr(expected$AARSD1, 'scaled:center') <- attr(result$AARSD1, 'scaled:center')
#   attr(expected$AARSD1, 'scaled:scale') <- attr(result$AARSD1, 'scaled:scale')
#   attr(expected$ABL1, 'scaled:center') <- attr(result$ABL1, 'scaled:center')
#   attr(expected$ABL1, 'scaled:scale') <- attr(result$ABL1, 'scaled:scale')
#   attr(expected$ACAA1, 'scaled:center') <- attr(result$ACAA1, 'scaled:center')
#   attr(expected$ACAA1, 'scaled:scale') <- attr(result$ACAA1, 'scaled:scale')
#
#   expect_equal(result, expected)
# })
#
#
# test_that("normalize_data normalizes data properly and returns long data", {
#   first_3_unique_assays <- example_data |>
#     dplyr::distinct(Assay) |>
#     dplyr::slice(1:3) |>
#     dplyr::pull(Assay)
#   first_3_unique_samples <- example_data |>
#     dplyr::distinct(DAid) |>
#     dplyr::slice(1:3) |>
#     dplyr::pull(DAid)
#
#   test_data <- example_data |>
#     dplyr::filter(Assay %in% first_3_unique_assays) |>
#     dplyr::filter(DAid %in% first_3_unique_samples)
#
#   result <- normalize_data(test_data, wide = FALSE, return_long = TRUE) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
#
#   expected <- tibble::tibble(
#     DAid = c("DA00001", "DA00001", "DA00001", "DA00002", "DA00002", "DA00002"),
#     Assay = c("AARSD1", "ABL1", "ACAA1", "AARSD1", "ABL1", "ACAA1"),
#     NPX = c(0.71, 0.71, 0.71, -0.71, -0.71, -0.71)
#   )
#
#   expect_equal(result, expected)
# })
#
#
# test_that("normalize_data normalizes data and removes batch effects properly", {
#   result <- normalize_data(example_data, example_metadata, wide = FALSE, batch = "Cohort")
#
#   test_data <- example_data |>
#     dplyr::select(DAid, Assay, NPX) |>
#     tidyr::pivot_wider(names_from = Assay, values_from = NPX)
#   batch <- test_data |>
#     dplyr::left_join(example_metadata |> dplyr::select(DAid, Cohort), by = "DAid") |>
#     dplyr::pull("Cohort")
#   id_col <- test_data |> dplyr::pull(DAid)
#   test_data <- test_data |> dplyr::select(-DAid)
#
#   no_batch_effects_res <- limma::removeBatchEffect(t(as.matrix(test_data)), batch=batch)
#   expected <- tibble::as_tibble(t(no_batch_effects_res))
#   expected <- tibble::as_tibble(scale(expected, center = T, scale = T))
#   names(expected) <- names(test_data)
#   expected$DAid <- id_col
#   expected <- expected |> dplyr::select(DAid, dplyr::everything())
#   expect_equal(result, expected)
# })
