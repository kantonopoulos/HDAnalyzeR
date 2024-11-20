utils::globalVariables(c(":="))
#' Fix PC names
#'
#' `fix_pca_names()` fixes the names of the PCs in the PCA results. If number of PCs is higher than 10,
#' the function will remove the zeros in front of the PC names (for example PC01 -> PC1).
#'
#' @param pca_res A tibble with the PCA results. Created by `hd_run_pca()`.
#' @param components The number of PCs to be calculated.
#' @param by_sample If TRUE, points represent samples. If FALSE, points represent features.
#' @param sample_id The name of the column in the data that contains the sample IDs.
#' @param var_name The name of the column in the data that contains the feature IDs.
#' @param type The type of analysis. Default is "pca".
#'
#' @return A tibble with the PCA results with fixed PC names.
#' @keywords internal
fix_components_names <- function(pca_res, components, by_sample, sample_id, var_name, type = "pca") {
  if (type == "pca") {
    pc_names <- paste0("PC", 1:components)
  } else if (type == "umap") {
    pc_names <- paste0("UMAP", 1:components)
  }
  if (isTRUE(by_sample)) {
    col_names <- c(sample_id, pc_names)
  } else {
    col_names <- c(var_name, pc_names)
  }

  colnames(pca_res) <- col_names

  return(pca_res)
}


#' Run PCA analysis
#'
#' `hd_run_pca()` runs a PCA analysis on the provided data. If data contain missing values,
#' the function imputes them using the k-nearest neighbors algorithm (k = 5). The number of PCs
#' to be calculated is defined by the user. The function returns a tibble with the PCA results.
#'
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and its first column is the sample ID.
#' @param components The number of PCs to be calculated. Default is 10.
#' @param by_sample If TRUE, points represent samples. If FALSE, points represent features. Default is TRUE.
#' @param seed The seed to be used in the PCA analysis. Default is 123.
#'
#' @return A list with the PCA results, PCA loadings and explained variance.
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run the PCA analysis
#' hd_run_pca(hd_object, components = 5, by_sample = TRUE, seed = 123)
#'
#' # Run the PCA analysis by feature
#' hd_run_pca(hd_object, components = 5, by_sample = FALSE, seed = 123)
hd_run_pca <- function(dat,
                       components = 10,
                       by_sample = TRUE,
                       seed = 123) {

  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data to run the PCA analysis.")
    }
    wide_data <- dat[["data"]]
    sample_id <- dat[["sample_id"]]
    var_name <- dat[["var_name"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
    var_name <- "Features"
  }

  if (isFALSE(by_sample)) {
    transposed_data <- wide_data |> tibble::column_to_rownames(var = sample_id)
    wide_data <- tibble::as_tibble(cbind(nms = names(transposed_data), t(transposed_data))) |>
      dplyr::rename(!!var_name := !!rlang::sym("nms")) |>
      dplyr::mutate(dplyr::across(-!!var_name, as.numeric))
  }

  if (components > ncol(wide_data)-1) {
    stop(paste("The number of PCs to be calculated is higher than the number of features in the data.",
               ncol(wide_data)-1,
               "PCs will be used."))
  }

  set.seed(seed)

  pca_rec <- recipes::recipe( ~ ., data = wide_data) |>
    recipes::update_role(1, new_role = "id")  |>
    recipes::step_normalize(recipes::all_predictors()) |>
    recipes::step_impute_knn(recipes::all_predictors(), neighbors = 5) |>
    recipes::step_pca(recipes::all_predictors(), num_comp = components)

  pca_prep <- recipes::prep(pca_rec)

  pca_loadings <- broom::tidy(pca_prep, 3) |> dplyr::select(-!!rlang::sym("id"))

  pca_variance <- broom::tidy(pca_prep, number = 3, type = "variance") |>
    dplyr::filter(!!rlang::sym("terms") %in% c("percent variance", "cumulative percent variance")) |>
    dplyr::filter(!!rlang::sym("component") >= 1 & !!rlang::sym("component") <= components) |>
    tidyr::pivot_wider(names_from = !!rlang::sym("terms"), values_from = !!rlang::sym("value")) |>
    dplyr::rename(percent_variance = !!rlang::sym("percent variance"),
                  cumulative_percent_variance = !!rlang::sym("cumulative percent variance")) |>
    dplyr::select(-!!rlang::sym("id"))

  pca_res <-  recipes::juice(pca_prep)
  # Fix PC names if number of PCs is higher than 10
  pca_res <- fix_components_names(pca_res, components, by_sample, sample_id, var_name, type = "pca")

  pca_object <- list("pca_res" = pca_res,
                     "pca_loadings" = pca_loadings,
                     "pca_variance" = pca_variance,
                     "by_sample" = by_sample)
  class(pca_object) <- "hd_pca"

  return(pca_object)
}


#' Plot PCA loadings
#'
#' `plot_loadings()` plots the PCA loadings for the top n features and first m PCs.
#' n and m are defined by the user. The contribution direction of the features
#' is indicated by the color of the bars.
#'
#' @param pca_object A PCA object containing the PCA loadings. Created by `hd_run_pca()`.
#' @param displayed_pcs The number of PCs to be displayed. Default is 6.
#' @param displayed_features The number of features to be displayed. Default is 15.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run the PCA analysis and create the loadings plot
#' pca_object <- hd_run_pca(hd_object, components = 5, by_sample = TRUE, seed = 123) |>
#'   hd_plot_pca_loadings()
#' pca_object$pca_loadings_plot
hd_plot_pca_loadings <- function(pca_object, displayed_pcs = 6, displayed_features = 15) {

  loadings_plot <- pca_object[["pca_loadings"]] |>
    dplyr::filter(!!rlang::sym("component") %in% paste0("PC", 1:displayed_pcs)) |>
    dplyr::group_by(!!rlang::sym("component")) |>
    dplyr::top_n(displayed_features, abs(!!rlang::sym("value"))) |>
    dplyr::ungroup() |>
    dplyr::mutate(terms = tidytext::reorder_within(!!rlang::sym("terms"),
                                                   abs(!!rlang::sym("value")),
                                                   !!rlang::sym("component"))) |>
    dplyr::mutate(positive = factor((!!rlang::sym("value") > 0), levels = c(TRUE, FALSE))) |>
    ggplot2::ggplot(ggplot2::aes(abs(!!rlang::sym("value")), !!rlang::sym("terms"), fill = !!rlang::sym("positive"))) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#C03830", "FALSE" = "#317EC2"),
                               labels=c("Positive", "Negative")) +
    ggplot2::facet_wrap( ~ component, scales = "free_y") +
    tidytext::scale_y_reordered() +
    ggplot2::labs(x = "Absolute Value of Contribution",
                  y = NULL, fill = "Sign") +
    theme_hd()

  pca_object[["pca_loadings_plot"]] <- loadings_plot

  return(pca_object)
}


#' Plot PCA variance
#'
#' `plot_explained_variance()` plots the explained variance and cumulative explained variance of the PCs.
#'
#' @param pca_object A PCA object containing the PCA variance. Created by `hd_run_pca()`.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run the PCA analysis and create the variance plot
#' pca_object <- hd_run_pca(hd_object, components = 5, by_sample = TRUE, seed = 123) |>
#'   hd_plot_pca_variance()
#' pca_object$pca_variance_plot
hd_plot_pca_variance <- function(pca_object) {

  variance_plot <- ggplot2::ggplot(
    pca_object[["pca_variance"]],
    ggplot2::aes(x = factor(!!rlang::sym("component"), levels = !!rlang::sym("component")))
    ) +
    ggplot2::geom_bar(ggplot2::aes(y = !!rlang::sym("percent_variance"),
                                   fill = "Individual Variance"),
                      stat = "identity") +
    ggplot2::geom_line(ggplot2::aes(y = !!rlang::sym("cumulative_percent_variance"),
                                    group = 1,
                                    color = "Cumulative Variance")) +
    ggplot2::geom_point(ggplot2::aes(y = !!rlang::sym("cumulative_percent_variance"),
                                     color = "Cumulative Variance")) +
    ggplot2::geom_text(ggplot2::aes(y = !!rlang::sym("cumulative_percent_variance"),
                       label = paste0(round(!!rlang::sym("cumulative_percent_variance")), "%")),
              vjust = -0.5,
              size = 3.5) +
    ggplot2::labs(x = "Components", y = "% Explained Variance") +
    ggplot2::scale_fill_manual(name = "", values = c("Individual Variance" = "#317EC2")) +
    ggplot2::scale_color_manual(name = "", values = c("Cumulative Variance" = "#C03830")) +
    theme_hd() +
    ggplot2::theme(legend.position = "top")

  pca_object[["pca_variance_plot"]] <- variance_plot

  return(pca_object)
}


#' Extract information from the HDAnalyzeR and PCA or UMAP object
#'
#' `prepare_plot_data()` extracts the PCA or UMAP results and metadata information from the respective objects.
#'
#' @param dim_object A PCA or UMAP object containing the results of the dimensionality reduction analysis. Created by `hd_run_pca()` or `hd_run_umap()`.
#' @param metadata An HDAnalyzeR object or a dataset in wide format and its first column is the sample ID.
#' @param color The name of the column that contains the variable to be used to plot the points color.
#' @param x The name of the column in `dim_object` that contains the x-axis values.
#' @param y The name of the column in `dim_object` that contains the y-axis values.
#'
#' @return A tibble with the PCA or UMAP results and metadata information if available.
#' @keywords internal
prepare_plot_data <- function(dim_object, metadata, color, x, y) {
  # Extract PCA or UMAP results
  if (inherits(dim_object, "hd_pca")) {
    dim_res <- dim_object[["pca_res"]]
  } else if (inherits(dim_object, "hd_umap")) {
    dim_res <- dim_object[["umap_res"]]
  }
  by_sample <- dim_object[["by_sample"]]

  # Handle metadata and color argument
  if (inherits(metadata, "HDAnalyzeR")) {
    sample_id <- metadata[["sample_id"]]
    metadata <- metadata[["metadata"]]
    if (!is.null(color)) {
      if (is.null(metadata)) {
        stop("The 'metadata' slot of the HDAnalyzeR object is empty. Please provide the metadata to plot the points color.")
      }
    }
  } else {
    sample_id <- colnames(metadata)[1]
  }

  if (!is.null(color) && !color %in% colnames(metadata) && !color %in% colnames(dim_res)) {
    stop("The column name provided in 'color' does not exist.")
  }

  if (isTRUE(by_sample)) {
    if (!is.null(metadata)) {
      dim_res <- dim_res |>
        dplyr::left_join(metadata |>
                           dplyr::select(dplyr::any_of(c(sample_id, color))),
                         by = sample_id)
    }
  }

  return(dim_res)
}


#' Plot sample data points in a two-dimensional plane
#'
#' `plot_points()` plots the sample data points in a two-dimensional plane.
#' The points can be plotted in the PCx/PCy or UMAP1/UMAP2 space.
#'
#' @param dim_res A tibble with the results of the dimensionality reduction analysis.
#' @param x The name of the column in `dim_res` that contains the x-axis values.
#' @param y The name of the column in `dim_res` that contains the y-axis values.
#' @param color The name of the column in `dim_res` that contains the variable to be used to plot the points color.
#'
#' @return A ggplot object
#' @keywords internal
plot_points <- function(dim_res, x, y, color = NULL) {
  if (!is.null(color)) {
    dim_plot <- dim_res |>
      ggplot2::ggplot(ggplot2::aes(!!rlang::sym(x), !!rlang::sym(y))) +
      ggplot2::geom_point(ggplot2::aes(color = !!rlang::sym(color)), alpha = 0.7, size = 2) +
      ggplot2::labs(Color = color) +
      theme_hd()
  } else {
    dim_plot <- dim_res |>
      ggplot2::ggplot(ggplot2::aes(!!rlang::sym(x), !!rlang::sym(y))) +
      ggplot2::geom_point(alpha = 0.7, size = 2) +
      theme_hd()
  }

  return(dim_plot)
}


#' Prepare PCA loadings to be plotted on the 2D plane
#'
#' `plot_loadings()` prepares the PCA loadings to be plotted on the 2D plane.
#'
#' @param dim_object A PCA object containing the PCA loadings. Created by `hd_run_pca()`.
#' @param plot_loadings The component to be plotted. Default is NULL.
#' @param nloadings The number of loadings to be plotted. Default is 5.
#'
#' @return A tibble with the PCA loadings to be plotted.
#' @keywords internal
plot_loadings <- function(dim_object, plot_loadings, nloadings) {
  pca_loadings <- dim_object[["pca_loadings"]]

  if (nloadings > nrow(pca_loadings |> dplyr::filter(!!rlang::sym("component") == plot_loadings))) {
    message("The number of loadings to be plotted is higher than the number of loadings available. All loadings will be plotted.")
    nloadings <- nrow(pca_loadings |> dplyr::filter(!!rlang::sym("component") == plot_loadings))
  }

  pca_loadings <- pca_loadings |>
    dplyr::filter(!!rlang::sym("component") == plot_loadings) |>
    dplyr::arrange(dplyr::desc(abs(!!rlang::sym("value")))) |>
    utils::head(nloadings)

  return(pca_loadings)
}


#' Add component variance to the axis labels
#'
#' `add_axis_variance()` adds the explained variance of the components to the axis labels.
#'
#' @param dim_object A PCA object containing the PCA variance. Created by `hd_run_pca()`.
#' @param dim_plot A ggplot object with the data points on the 2D plane.
#' @param x The name of the column in `dim_res` that contains the x-axis values.
#' @param y The name of the column in `dim_res` that contains the y-axis values.
#'
#' @return A ggplot object
#' @keywords internal
add_axis_variance <- function(dim_object, dim_plot, x, y) {
  pca_variance <- dim_object[["pca_variance"]]
  variance_vec <- pca_variance |> dplyr::pull(!!rlang::sym("percent_variance"))

  x_num <- as.numeric(sub("PC", "", x))
  y_num <- as.numeric(sub("PC", "", y))

  dim_plot <- dim_plot + ggplot2::labs(x = paste0(x, " (", round(variance_vec[x_num], 1), "%)"),
                                       y = paste0(y, " (", round(variance_vec[y_num], 1), "%)"))

  return(dim_plot)
}

#' Plot PCA or UMAP results on a 2D plane
#'
#' `hd_plot_dim()` plots the sample data points on a 2D plane.
#' The points can be plotted in the PCx/PCy or UMAP1/UMAP2 space.
#'
#' @param dim_object A PCA or UMAP object containing the results of the dimensionality reduction analysis. Created by `hd_run_pca()` or `hd_run_umap()`.
#' @param metadata An HDAnalyzeR object or a dataset in wide format and its first column is the sample ID.
#' @param x The name of the column in `dim_res` that contains the x-axis values.
#' @param y The name of the column in `dim_res` that contains the y-axis values.
#' @param color The name of the column in `dim_res` that contains the variable to be used to plot the points color.
#' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`.
#' @param plot_loadings The component to be plotted. Default is NULL.
#' @param nloadings The number of loadings to be plotted. Default is 5.
#' @param axis_variance If TRUE, the explained variance of the components is added to the axis labels. Default is TRUE.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run the PCA analysis
#' pca_object <- hd_run_pca(hd_object, components = 5, by_sample = TRUE, seed = 123) |>
#'   hd_plot_dim(hd_object, x = "PC1", y = "PC2", color = "Disease", palette = "cancers12")
hd_plot_dim <- function(dim_object,
                        metadata,
                        x,
                        y,
                        color = NULL,
                        palette = NULL,
                        plot_loadings = NULL,
                        nloadings = 5,
                        axis_variance = TRUE) {

  # Prepare data for plotting
  dim_res <- prepare_plot_data(dim_object, metadata, color, x, y)

  # Create basic plot with points
  dim_plot <- plot_points(dim_res, x, y, color)

  # Add loadings if PCA
  if (!is.null(plot_loadings) && inherits(dim_object, "hd_pca")) {
    pca_loadings <- plot_loadings(dim_object, plot_loadings, nloadings)
    dim_plot <- dim_plot +
      ggplot2::geom_segment(data = pca_loadings,
                            ggplot2::aes(x = 0,
                                         y = 0,
                                         xend = !!rlang::sym("value"),
                                         yend = !!rlang::sym("value"))) +
      ggrepel::geom_text_repel(data = pca_loadings,
                               ggplot2::aes(x = !!rlang::sym("value"),
                                            y = !!rlang::sym("value"),
                                            label = !!rlang::sym("terms")),
                               size = 3,
                               color = "black")
  }

  # Add variance labels if PCA
  if (axis_variance && inherits(dim_object, "hd_pca")) {
    dim_plot <- add_axis_variance(dim_object, dim_plot, x, y)
  }

  # Apply color palette if provided
  dim_plot <- apply_palette(dim_plot, palette, type = "color")

  # Save plot in the appropriate object slot
  if (inherits(dim_object, "hd_pca")) {
    dim_object[["pca_plot"]] <- dim_plot
  } else if (inherits(dim_object, "hd_umap")) {
    dim_object[["umap_plot"]] <- dim_plot
  }

  return(dim_object)
}


#' Run PCA analysis and plot the results
#'
#' `hd_auto_pca()` runs a PCA analysis on the provided data and plots the PCA results.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and its first column is the sample ID.
#' @param components The number of PCs to be calculated. Default is 10.
#' @param by_sample If TRUE, points represent samples. If FALSE, points represent features. Default is TRUE.
#' @param plot_x The name of the column in `dim_res` that contains the x-axis values. Default is "PC1".
#' @param plot_y The name of the column in `dim_res` that contains the y-axis values. Default is "PC2".
#' @param plot_color The name of the column in `dim_res` that contains the variable to be used to plot the points color. Default is NULL.
#' @param plot_palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#'
#' @return A list with the PCA results and PCA plots.
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run the PCA analysis
#' hd_auto_pca(hd_object, components = 20, plot_color = "Disease", plot_palette = "cancers12")
hd_auto_pca <- function(dat, components = 10, by_sample = TRUE, plot_x = "PC1", plot_y = "PC2", plot_color = NULL, plot_palette = NULL) {

  pca_object <- hd_run_pca(dat, components = components, by_sample = by_sample) |>
    hd_plot_pca_loadings(displayed_pcs = 6, displayed_features = 15) |>
    hd_plot_pca_variance() |>
    hd_plot_dim(dat, x = plot_x, y = plot_y, color = plot_color, palette = plot_palette)

  return(pca_object)
}


#' Run UMAP analysis
#'
#' `hd_run_umap()` runs a UMAP analysis on the provided data. If data contain missing values,
#' the function imputes them using the k-nearest neighbors algorithm (k = 5). The number of components
#' to be calculated is defined by the user. The function returns a tibble with the UMAP results.
#'
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and its first column is the sample ID.
#' @param components The number of components to be calculated. Default is 10.
#' @param by_sample If TRUE, points represent samples. If FALSE, points represent features. Default is TRUE.
#' @param seed The seed to be used in the UMAP analysis. Default is 123.
#'
#' @return A list with the UMAP results.
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run the PCA analysis
#' hd_run_umap(hd_object, components = 5, by_sample = TRUE, seed = 123)
#'
#' # Run the PCA analysis by feature
#' hd_run_umap(hd_object, components = 5, by_sample = FALSE, seed = 123)
hd_run_umap <- function(dat,
                        by_sample = TRUE,
                        components = 2,
                        seed = 123) {

  # Ensure 'umap' package is loaded
  if (!requireNamespace("umap", quietly = TRUE)) {
    stop("The 'umap' package is required but not installed. Please install it using install.packages('umap').")
  }

  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data to run the PCA analysis.")
    }
    wide_data <- dat[["data"]]
    sample_id <- dat[["sample_id"]]
    var_name <- dat[["var_name"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
    var_name <- "Features"
  }

  if (isFALSE(by_sample)) {
    var_name <- rlang::sym(var_name)
    transposed_data <- wide_data |> tibble::column_to_rownames(var = sample_id)
    wide_data <- tibble::as_tibble(cbind(nms = names(transposed_data), t(transposed_data))) |>
      dplyr::rename(!!var_name := !!rlang::sym("nms")) |>
      dplyr::mutate(dplyr::across(-!!var_name, as.numeric))
  }

  if (components > ncol(wide_data)-1) {
    message(paste("The number of UMAPs to be calculated is higher than the number of features in the data.",
                  ncol(wide_data)-1,
                  "UMAPs will be used."))
  }

  set.seed(seed)

  umap_rec <- recipes::recipe( ~ ., data = wide_data) |>
    recipes::update_role(1, new_role = "id")  |>
    recipes::step_normalize(recipes::all_predictors()) |>
    recipes::step_impute_knn(recipes::all_predictors(), neighbors = 5) |>
    embed::step_umap(recipes::all_predictors(), num_comp = components)

  umap_prep <- recipes::prep(umap_rec)

  umap_res <-  recipes::juice(umap_prep)

  # Fix UMAPs names if number of UMAPs is higher than 10
  umap_res <- fix_components_names(umap_res, components, by_sample, sample_id, var_name, type = "umap")

  umap_object <- list("umap_res" = umap_res, "by_sample" = by_sample)
  class(umap_object) <- "hd_umap"

  return(umap_object)
}


#' Run UMAP analysis and plot the results
#'
#' `hd_auto_umap()` runs a UMAP analysis on the provided data and plots the UMAP results.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and its first column is the sample ID.
#' @param by_sample If TRUE, points represent samples. If FALSE, points represent features. Default is TRUE.
#' @param plot_x The name of the column in `dim_res` that contains the x-axis values. Default is "PC1".
#' @param plot_y The name of the column in `dim_res` that contains the y-axis values. Default is "PC2".
#' @param plot_color The name of the column in `dim_res` that contains the variable to be used to plot the points color. Default is NULL.
#' @param plot_palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#'
#' @return A list with the UMAP results and UMAP plots.
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run the UMAP analysis
#' hd_auto_umap(hd_object, plot_color = "Disease", plot_palette = "cancers12")
hd_auto_umap <- function(dat, by_sample = TRUE, plot_x = "UMAP1", plot_y = "UMAP2", plot_color = NULL, plot_palette = NULL) {

  umap_object <- hd_run_umap(dat, by_sample = by_sample) |>
    hd_plot_dim(dat, x = plot_x, y = plot_y, color = plot_color, palette = plot_palette)

  return(umap_object)
}
