#' utils::globalVariables(c("terms", "value", "component", "positive", "var",
#'                          "cumulative_variance", "cumulative percent variance",
#'                          "id", "percent variance", "nms", "PC"))
#' #' Plot PCA loadings
#' #'
#' #' `plot_loadings()` plots the PCA loadings for the top n features and first m PCs.
#' #' n and m are defined by the user. The contribution direction of the features
#' #' is indicated by the color of the bars.
#' #'
#' #' @param tidied_res A tibble with the results of the PCA analysis.
#' #' @param npcs The number of PCs to be plotted. Default is 4.
#' #' @param nproteins The number of proteins to be plotted. Default is 8.
#' #'
#' #' @return A PCA loadings ggplot object.
#' #' @keywords internal
#' plot_loadings <- function(tidied_res, npcs = 4, nproteins = 8) {
#'
#'   p <- tidied_res |>
#'     dplyr::filter(component %in% paste0("PC", 1:npcs)) |>
#'     dplyr::group_by(component) |>
#'     dplyr::top_n(nproteins, abs(value)) |>
#'     dplyr::ungroup() |>
#'     dplyr::mutate(terms = tidytext::reorder_within(terms, abs(value), component)) |>
#'     dplyr::mutate(positive = factor((value > 0), levels = c(TRUE, FALSE))) |>
#'     ggplot2::ggplot(ggplot2::aes(abs(value), terms, fill = positive)) +
#'     ggplot2::geom_col() +
#'     ggplot2::scale_fill_manual(values = c("TRUE" = "red3", "FALSE" = "darkblue")) +
#'     ggplot2::facet_wrap( ~ component, scales = "free_y") +
#'     tidytext::scale_y_reordered() +
#'     ggplot2::labs(x = "Absolute value of contribution",
#'                   y = NULL, fill = "Positive?") +
#'     theme_hpa()
#'
#'   return(p)
#' }
#'
#'
#' #' Plot explained variance and cumulative explained variance
#' #'
#' #' `plot_explained_variance()` plots the explained variance and cumulative explained variance.
#' #'
#' #' @param explained_variance A tibble with the explained variance and cumulative explained variance.
#' #'
#' #' @return A ggplot object
#' #' @keywords internal
#' plot_explained_variance <- function(explained_variance) {
#'
#'   variance_plot <- ggplot2::ggplot(
#'     explained_variance,
#'     ggplot2::aes(x = factor(component, levels = component))
#'     ) +
#'     ggplot2::geom_bar(ggplot2::aes(y = `percent variance`,
#'                                    fill = "Individual Variance"),
#'                       stat = "identity") +
#'     ggplot2::geom_line(ggplot2::aes(y = `cumulative percent variance`,
#'                                     group = 1,
#'                                     color = "Cumulative Variance")) +
#'     ggplot2::geom_point(ggplot2::aes(y = `cumulative percent variance`,
#'                                      color = "Cumulative Variance")) +
#'     ggplot2::geom_text(ggplot2::aes(y = `cumulative percent variance`,
#'                        label = paste0(round(`cumulative percent variance`), "%")),
#'               vjust = -0.5,
#'               size = 3.5) +
#'     ggplot2::labs(x = "Components", y = "% Explained Variance") +
#'     ggplot2::scale_fill_manual(name = "", values = c("Individual Variance" = "darkblue")) +
#'     ggplot2::scale_color_manual(name = "", values = c("Cumulative Variance" = "red3")) +
#'     theme_hpa() +
#'     ggplot2::theme(legend.position = "top")
#'
#'   return(variance_plot)
#' }
#' #' Plot sample data points in a two-dimensional plane.
#' #'
#' #' `plot_dim_reduction()` plots the sample data points in a two-dimensional plane.
#' #' The points can be plotted in the PC1/PC2 or UMAP1/UMAP2 space.
#' #'
#' #' @param res A tibble with the results of the dimensionality reduction analysis.
#' #' @param x The name of the column in `res` that contains the x-axis values.
#' #' @param y The name of the column in `res` that contains the y-axis values.
#' #' @param metadata A tibble with metadata information to be added to the plot. It will be used for coloring the points.
#' #' @param color The name of the column in `metadata` that contains the variable to be used to plot the points color.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`.
#' #' @param loadings If TRUE, the PCA loadings are plotted on the 2 dimensional plot. Default is FALSE.
#' #' @param variance_explained A vector with the explained variance for each PC. Default is NULL.
#' #' @param loadings_data A tibble with the PCA loadings. Default is NULL.
#' #' @param assay If TRUE, each point is an assay and not a sample. Default is FALSE.
#' #'
#' #' @return A ggplot object
#' #' @keywords internal
#' plot_dim_reduction <- function(res,
#'                                x,
#'                                y,
#'                                metadata,
#'                                color,
#'                                palette,
#'                                loadings = FALSE,
#'                                variance_explained = NULL,
#'                                loadings_data = NULL,
#'                                assay = FALSE) {
#'
#'   if (!is.null(metadata)) {
#'     p <- res |>
#'       dplyr::left_join(metadata, by = "DAid") |>
#'       ggplot2::ggplot(ggplot2::aes(!!rlang::sym(x), !!rlang::sym(y))) +
#'       ggplot2::geom_point(ggplot2::aes(color = !!rlang::sym(color)),
#'                           alpha = 0.7,
#'                           size = 2) +
#'       ggplot2::labs(Color = color) +
#'       theme_hpa()
#'   } else if (isTRUE(assay)) {
#'     p <- res |>
#'       ggplot2::ggplot(ggplot2::aes(!!rlang::sym(x), !!rlang::sym(y))) +
#'       ggplot2::geom_point(ggplot2::aes(color = !!rlang::sym(color)),
#'                           alpha = 0.7,
#'                           size = 2) +
#'       ggplot2::labs(Color = color) +
#'       theme_hpa()
#'   }
#'   else {
#'     p <- res |>
#'       ggplot2::ggplot(ggplot2::aes(!!rlang::sym(x), !!rlang::sym(y))) +
#'       ggplot2::geom_point(alpha = 0.7, size = 2) +
#'       theme_hpa()
#'   }
#'
#'   if (isTRUE(loadings)) {
#'     loadings_data <- loadings_data |>
#'       dplyr::filter(PC %in% c("PC1", "PC01")) |>
#'       dplyr::arrange(dplyr::desc(abs(Value))) |>
#'       utils::head(5)
#'
#'     p <- p +
#'       ggplot2::geom_segment(data = loadings_data,
#'                             ggplot2::aes(x = 0, y = 0, xend = Value, yend = Value)) +
#'       ggrepel::geom_text_repel(data = loadings_data,
#'                                ggplot2::aes(x = Value, y = Value, label = Assay),
#'                                size = 3,
#'                                color = "black")
#'   }
#'
#'   if (!is.null(variance_explained)) {
#'     x_num <- as.numeric(sub("PC", "", x))
#'     y_num <- as.numeric(sub("PC", "", y))
#'     p <- p + ggplot2::labs(x = paste0(x, " (", round(variance_explained[x_num], 1), "%)"),
#'                            y = paste0(y, " (", round(variance_explained[y_num], 1), "%)"))
#'   }
#'   if (!is.null(palette)) {
#'     if (is.null(names(palette))) {
#'       p <- p + scale_color_hpa(palette)
#'     } else {
#'       p <- p + ggplot2::scale_color_manual(values = palette)
#'     }
#'   }
#'
#'   return(p)
#' }
#'
#'
#' #' Run PCA analysis
#' #'
#' #' `do_pca()` runs a PCA analysis on the provided data. The function can visualize
#' #' the sample points on the first and second PC plane as well as the PCA loadings
#' #' and the explained variance. It can also save the plots in the results directory.
#' #'
#' #' @param olink_data A tibble with the data to be used in the PCA analysis.
#' #' @param metadata A tibble with metadata information to be used in the PCA plots. Default is NULL.
#' #' @param pcs The number of PCs to be calculated. Default is 5.
#' #' @param color The name of the column in `metadata` that contains the variable.
#' #' to be used to plot the points color. Default is "Disease".
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`.
#' #' @param wide If TRUE, the data is assumed to be in wide format. Default is TRUE.
#' #' @param assay If TRUE, each point is an assay and not a sample. Default is FALSE.
#' #' @param impute If TRUE, the data is imputed before the PCA analysis. Default is TRUE.
#' #' @param plots If TRUE, the function creates plots of the PCA results. Default is TRUE.
#' #' @param x The component to be plotted on the x-axis. Default is "PC1".
#' #' @param y The component to be plotted on the y-axis. Default is "PC2".
#' #' @param npcs The number of PCs to be plotted. Default is 4.
#' #' @param nproteins The number of proteins to be plotted. Default is 8.
#' #' @param loadings If TRUE, the PCA loadings are plotted on the 2 dimensional plot. Default is FALSE.
#' #' @param save If TRUE, the plots are saved in the results directory. Default is FALSE.
#' #'
#' #' @return A list with the PCA results and, if requested, the PCA plots.
#' #'   - pca_res: A tibble with the PCA results.
#' #'   - loadings: A tibble with the PCA loadings.
#' #'   - pca_plot: A ggplot object with the data points on the 1st and 2nd PCs plane.
#' #'   - loadings_plot: A PCA loadings ggplot object.
#' #'   - variance_plot: A ggplot object with the explained variance and cumulative explained variance.
#' #' @export
#' #'
#' #' @examples
#' #' do_pca(example_data,
#' #'        metadata = example_metadata,
#' #'        pcs = 8,
#' #'        wide = FALSE,
#' #'        color = "Disease",
#' #'        palette = "cancers12")
#' do_pca <- function(olink_data,
#'                    metadata = NULL,
#'                    pcs = 5,
#'                    color = "Disease",
#'                    palette = NULL,
#'                    wide = TRUE,
#'                    assay = FALSE,
#'                    impute = TRUE,
#'                    plots = TRUE,
#'                    x = "PC1",
#'                    y = "PC2",
#'                    npcs = 4,
#'                    nproteins = 8,
#'                    loadings = FALSE,
#'                    save = FALSE) {
#'
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   if (isTRUE(assay)) {
#'     transposed_data <- wide_data |> tibble::column_to_rownames(var = "DAid")
#'     wide_data <- tibble::as_tibble(cbind(nms = names(transposed_data), t(transposed_data))) |>
#'       dplyr::rename(Assay = nms) |>
#'       dplyr::mutate(dplyr::across(-Assay, as.numeric))
#'   }
#'
#'   set.seed(123)
#'   if (isTRUE(impute)) {
#'     pca_rec <- recipes::recipe( ~ ., data = wide_data) |>
#'       recipes::update_role(1, new_role = "id")  |>
#'       recipes::step_normalize(recipes::all_predictors()) |>
#'       recipes::step_impute_knn(recipes::all_predictors(), neighbors = 5) |>
#'       recipes::step_pca(recipes::all_predictors(), num_comp = pcs)
#'
#'     pca_prep <- recipes::prep(pca_rec)
#'
#'     tidied_pca <- broom::tidy(pca_prep, 3)
#'
#'     ntidy <- 3
#'   } else {
#'     pca_rec <- recipes::recipe( ~ ., data = wide_data) |>
#'       recipes::update_role(1, new_role = "id")  |>
#'       recipes::step_normalize(recipes::all_predictors()) |>
#'       recipes::step_pca(recipes::all_predictors())
#'
#'     pca_prep <- recipes::prep(pca_rec)
#'
#'     tidied_pca <- broom::tidy(pca_prep, 2)
#'
#'     ntidy <- 2
#'   }
#'
#'   loadings_data <- tidied_pca |>
#'     dplyr::rename(Assay = terms, Value = value, PC = component)
#'
#'   pca_res <-  recipes::juice(pca_prep)
#'
#'   # Extract the explained variance and calculate cumulative explained variance
#'   explained_variance <- broom::tidy(pca_prep, number = ntidy, type = "variance") |>
#'     dplyr::filter(terms %in% c("percent variance", "cumulative percent variance")) |>
#'     dplyr::filter(component >= 1 & component <= pcs) |>
#'     tidyr::pivot_wider(names_from = terms, values_from = value) |>
#'     dplyr::select(-id)
#'
#'   variance_explained <- explained_variance |>
#'     dplyr::pull(`percent variance`)
#'
#'   pc_names <- paste0("PC", 1:pcs)
#'   if (isFALSE(assay)) {
#'     col_names <- c("DAid", pc_names)
#'   } else {
#'     col_names <- c("Assay", pc_names)
#'   }
#'
#'   colnames(pca_res) <- col_names
#'
#'   # Visualize results
#'   if (isTRUE(plots)) {
#'     pca_plot <- plot_dim_reduction(pca_res,
#'                                    x,
#'                                    y,
#'                                    metadata,
#'                                    color,
#'                                    palette,
#'                                    loadings,
#'                                    variance_explained,
#'                                    loadings_data,
#'                                    assay)
#'     loadings_plot <- plot_loadings(tidied_pca, npcs, nproteins)
#'     variance_plot <- plot_explained_variance(explained_variance)
#'
#'     if (isTRUE(save)) {
#'       dir_name <- create_dir("results/pca_plots", date = T)
#'       ggplot2::ggsave(pca_plot, filename = paste0(dir_name, "/pca_plot.png"), width = 10, height = 8)
#'       ggplot2::ggsave(loadings_plot, filename = paste0(dir_name, "/loadings_plot.png"), width = 10, height = 8)
#'       ggplot2::ggsave(variance_plot, filename = paste0(dir_name, "/variance_plot.png"), width = 10, height = 8)
#'     }
#'     return(list("pca_res" = pca_res,
#'                 "loadings" = loadings_data,
#'                 "pca_plot" = pca_plot,
#'                 "loadings_plot" = loadings_plot,
#'                 "variance_plot" = variance_plot))
#'   } else {
#'     return(list("pca_res" = pca_res, "loadings" = loadings_data))
#'   }
#' }
#'
#'
#' #' Run UMAP analysis
#' #'
#' #' `do_umap()` runs a UMAP analysis on the provided data. The function can visualize
#' #' the sample points on the first and second UMAP plane. It can also save the plots
#' #' in the results directory.
#' #'
#' #' @param olink_data A tibble with the data to be used in the UMAP analysis.
#' #' @param metadata A tibble with metadata information to be used in the UMAP plots. Default is NULL.
#' #' @param color The name of the column in `metadata` that contains the variable.
#' #' to be used to plot the points color. Default is "Disease".
#' #' @param palette A vector with the colors to be used in the UMAP plots. Default is NULL.
#' #' @param wide If TRUE, the data is assumed to be in wide format. Default is TRUE.
#' #' @param assay If TRUE, each point is an assay and not a sample. Default is FALSE.
#' #' @param impute If TRUE, the data is imputed before the UMAP analysis. Default is TRUE.
#' #' @param plots If TRUE, the function creates plots of the UMAP results. Default is TRUE.
#' #' @param save If TRUE, the plots are saved in the results directory. Default is FALSE.
#' #'
#' #' @return A list with the UMAP results and, if requested, the UMAP plots.
#' #'   - umap_res: A tibble with the UMAP results.
#' #'   - umap_plot: A ggplot object with the data points on the 1st and 2nd UMAPs plane.
#' #' @export
#' #'
#' #' @examples
#' #' do_umap(example_data,
#' #'        metadata = example_metadata,
#' #'        wide = FALSE,
#' #'        color = "Sex",
#' #'        palette = "sex_hpa")
#' do_umap <- function(olink_data,
#'                     metadata = NULL,
#'                     color = "Disease",
#'                     palette = NULL,
#'                     wide = TRUE,
#'                     assay = FALSE,
#'                     impute = TRUE,
#'                     plots = TRUE,
#'                     save = FALSE) {
#'
#'   # Ensure 'umap' package is loaded
#'   if (!requireNamespace("umap", quietly = TRUE)) {
#'     stop("The 'umap' package is required but not installed. Please install it using install.packages('umap').")
#'   }
#'
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'   } else {
#'     wide_data <- olink_data
#'   }
#'
#'   if (isTRUE(assay)) {
#'     transposed_data <- wide_data |> tibble::column_to_rownames(var = "DAid")
#'     wide_data <- tibble::as_tibble(cbind(nms = names(transposed_data), t(transposed_data))) |>
#'       dplyr::rename(Assay = nms) |>
#'       dplyr::mutate(dplyr::across(-Assay, as.numeric))
#'   }
#'
#'   set.seed(123)
#'   if (isTRUE(impute)) {
#'     umap_rec <- recipes::recipe( ~ ., data = wide_data) |>
#'       recipes::update_role(1, new_role = "id")  |>
#'       recipes::step_normalize(recipes::all_predictors()) |>
#'       recipes::step_impute_knn(recipes::all_predictors(), neighbors = 5) |>
#'       embed::step_umap(recipes::all_predictors())
#'
#'     umap_prep <- recipes::prep(umap_rec)
#'
#'   } else {
#'     umap_rec <- recipes::recipe( ~ ., data = wide_data) |>
#'       recipes::update_role(1, new_role = "id")  |>
#'       recipes::step_normalize(recipes::all_predictors()) |>
#'       embed::step_umap(recipes::all_predictors())
#'
#'     umap_prep <- recipes::prep(umap_rec)
#'
#'   }
#'
#'   umap_res <-  recipes::juice(umap_prep)
#'
#'   if (isTRUE(plots)) {
#'     umap_plot <- plot_dim_reduction(umap_res,
#'                                     "UMAP1",
#'                                     "UMAP2",
#'                                     metadata,
#'                                     color,
#'                                     palette,
#'                                     assay = assay)
#'
#'     if (isTRUE(save)) {
#'       dir_name <- create_dir("results/umap_plots", date = T)
#'       ggplot2::ggsave(umap_plot, filename = paste0(dir_name, "/umap_plot.png"), width = 10, height = 8)
#'     }
#'     return(
#'       list(
#'         "umap_res" = umap_res,
#'         "umap_plot" = umap_plot
#'       )
#'     )
#'   } else {
#'     return("umap_res" = umap_res)
#'   }
#' }
