#' Apply palette to plot
#'
#' `apply_palette` applies the color palette to the plot. It checks if the palette is a valid
#' palette from the Human Protein Atlas (HPA) or a custom palette.
#'
#' @param plot The plot to apply the palette.
#' @param palette The color palette to apply. It can be either a character with the name of the
#' palette from the HPA or a custom palette (for example `c("red", "blue")`).
#' @param type The type of palette to apply. Default is "color". Other option is "fill".
#'
#' @return The plot with the selected palette.
#' @keywords internal
apply_palette <- function(plot, palette, type = "color") {

  if (type == "color") {
    if (!is.null(palette)) {
      if (is.null(names(palette))) {
        if (!is.null(hd_palettes()[[palette]])) {
          plot <- plot + scale_color_hd(palette)
        } else {
          stop("The color palette provided is not valid. Please provide one of the palettes from 'hd_palettes()' or a valid custom palette.")
        }
      } else {
        plot <- plot + ggplot2::scale_color_manual(values = palette)
      }
    }
  } else {
    if (!is.null(palette)) {
      if (is.null(names(palette))) {
        if (!is.null(hd_palettes()[[palette]])) {
          plot <- plot + scale_fill_hd(palette)
        } else {
          stop("The color palette provided is not valid. Please provide one of the palettes from 'hd_palettes()' or a valid custom palette.")
        }
      } else {
        plot <- plot + ggplot2::scale_fill_manual(values = palette)
      }
    }
  }

  return(plot)
}

#' utils::globalVariables(c("Value"))
#' #' Plot protein boxplots
#' #'
#' #' `plot_protein_boxplot()` plots boxplots for the specified proteins in the dataset.
#' #' It annotates the boxplot with color for the selected case
#' #' It is also possible to add points to the boxplot.
#' #'
#' #' @param join_data The dataset with the wide Olink data joined with the metadata.
#' #' @param variable The variable that will be used as `x` and `fill.`
#' #' @param proteins The proteins to include in the boxplot.
#' #' @param case The case to annotate.
#' #' @param points Whether to add points to the boxplot.
#' #' @param xaxis_names Whether to show the x-axis names. Default is FALSE.
#' #' @param palette The color palette to use. Default is "red3" for the annotated case
#' #'
#' #' @return The boxplot panel with the selected proteins.
#' #' @export
#' #'
#' #' @examples
#' #' # Prepare the data
#' #' wide_data <- widen_data(example_data)
#' #' join_data <- wide_data |>
#' #'   dplyr::left_join(example_metadata |> dplyr::select(DAid, Disease, Sex))
#' #'
#' #' # Boxplots for AARSD1 and ABL1 in AML
#' #' plot_protein_boxplot(join_data,
#' #'                      proteins = c("AARSD1", "ABL1"),
#' #'                      case = "AML",
#' #'                      palette = "cancers12")
#' plot_protein_boxplot <- function(join_data,
#'                                  variable = "Disease",
#'                                  proteins,
#'                                  case,
#'                                  points = TRUE,
#'                                  xaxis_names = FALSE,
#'                                  palette = NULL) {
#'   Variable <- rlang::sym(variable)
#'   # Prepare palettes
#'   pals <- get_hpa_palettes()
#'   if (!is.null(palette) && is.null(names(palette))) {
#'     pal <- pals[palette]
#'     pal <- unlist(pals[[palette]])
#'   } else if (!is.null(palette)) {
#'     pal <- palette
#'   } else {
#'     pal <- "black"
#'   }
#'
#'   long_data <- join_data |>
#'     dplyr::select(!!Variable, dplyr::all_of(proteins)) |>
#'     tidyr::pivot_longer(cols = !dplyr::any_of(c(variable)),
#'                         names_to = "Protein",
#'                         values_to = "NPX")
#'
#'   long_data$Protein <- factor(long_data$Protein, levels = proteins, labels = proteins)
#'   long_data[[variable]] <- as.factor(long_data[[variable]])
#'
#'   # Create boxplot
#'   boxplot <- long_data |>
#'     ggplot2::ggplot(ggplot2::aes(x = !!Variable, y = NPX)) +
#'     ggplot2::geom_boxplot(outlier.shape = NA) +
#'     ggplot2::geom_boxplot(data = dplyr::filter(long_data, !!Variable == case),
#'                           ggplot2::aes(fill = !!Variable),
#'                           alpha = 0.5,
#'                           show.legend = FALSE,
#'                           outlier.shape = NA)
#'
#'   if (isTRUE(points)) {
#'     boxplot <- boxplot +
#'       ggplot2::geom_point(data = dplyr::filter(long_data, !!Variable != case),
#'                           position = ggplot2::position_jitter(width = 0.1),
#'                           color = 'grey',
#'                           alpha = 0.3)
#'
#'     if (!is.null(palette)) {
#'       boxplot <- boxplot +
#'         ggplot2::geom_point(data = dplyr::filter(long_data, !!Variable == case),
#'                             ggplot2::aes(fill = !!Variable),
#'                             position = ggplot2::position_jitter(width = 0.1),
#'                             color = pal[case],
#'                             alpha = 0.5,
#'                             show.legend = FALSE)
#'     } else {
#'       boxplot <- boxplot +
#'         ggplot2::geom_point(data = dplyr::filter(long_data, !!Variable == case),
#'                             ggplot2::aes(fill = !!Variable),
#'                             position = ggplot2::position_jitter(width = 0.1),
#'                             alpha = 0.5,
#'                             show.legend = FALSE)
#'     }
#'   }
#'   boxplot_panel <- boxplot
#'   boxplot_panel <- boxplot +
#'     ggplot2::theme(legend.position = 'none') +
#'     ggplot2::scale_fill_manual(values = pal) +
#'     ggplot2::xlab('') +
#'     ggplot2::theme_classic() +
#'     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
#'
#'   if (isFALSE(xaxis_names)) {
#'     boxplot_panel <- boxplot_panel +
#'       ggplot2::theme(axis.text.x = ggplot2::element_blank())
#'   }
#'
#'   boxplot_panel <- boxplot_panel +
#'     ggplot2::facet_wrap(~ Protein, scale="free_y")
#'
#'   return(boxplot_panel)
#' }
#'
#'
#' #' Plot a scatter plot with regression line
#' #'
#' #' `plot_scatter_with_regression` plots a scatter plot with a linear regression line.
#' #' It is possible to add the standard error of the regression line, as well as the
#' #' R-squared and p-value.
#' #'
#' #' @param plot_data The wide dataset containing the data to plot as cols.
#' #' @param x The column name of the x-axis variable.
#' #' @param y The column name of the y-axis variable.
#' #' @param se Whether to add the standard error of the regression line. Default is FALSE.
#' #' @param line_color The color of the regression line.
#' #' @param r_2 Whether to add the R-squared and p-value to the plot. Default is TRUE.
#' #'
#' #' @return The scatter plot with the regression line.
#' #' @export
#' #'
#' #' @examples
#' #' # Prepare the data
#' #' wide_data <- widen_data(example_data)
#' #'
#' #' # Scatter plot for AARSD1 and ABL1
#' #' plot_scatter_with_regression(wide_data, "AARSD1", "ABL1", line_color = "red3")
#' plot_scatter_with_regression <- function(plot_data,
#'                                          x,
#'                                          y,
#'                                          se = FALSE,
#'                                          line_color = "black",
#'                                          r_2 = TRUE) {
#'   # Fit the linear model
#'   formula <- stats::as.formula(paste(y, "~", x))
#'   model <- stats::lm(formula, data = plot_data)
#'
#'   # Get the R-squared and p-value
#'   summary_model <- summary(model)
#'   r_squared <- summary_model$r.squared
#'   p_val <- stats::coef(summary_model)[2, 4]
#'
#'   # Create the plot
#'   x <- rlang::sym(x)
#'   y <- rlang::sym(y)
#'   scatter <- plot_data |>
#'     ggplot2::ggplot(ggplot2::aes(x = !!x, y = !!y)) +
#'     ggplot2::geom_point() +
#'     ggplot2::geom_smooth(method = "lm", se = se, color = line_color)
#'
#'   if (isTRUE(r_2)) {
#'     scatter <- scatter +
#'       ggplot2::annotate("text",
#'                         x = Inf,
#'                         y = Inf,
#'                         label = paste("R2 =", round(r_squared, 2), "\nP =", format.pval(round(p_val, 4))),
#'                         hjust = 1.1,
#'                         vjust = 2,
#'                         size = 5,
#'                         color = "black") +
#'     ggplot2::theme_classic()
#'   } else {
#'     scatter <- scatter +
#'       ggplot2::theme_classic()
#'   }
#'
#'   return(scatter)
#' }
#'
#'
#' #' Plot a summary heatmap of the combined differential expression and classification models results
#' #'
#' #' `plot_biomarkers_summary_heatmap` plots a summary heatmap of the combined differential
#' #' expression and classification models results. The heatmap shows the log2 fold change
#' #' and adjusted p-value of the differential expression results, and the scaled importance
#' #' and sign of the classification models results. The heatmap is ordered and the selected
#' #' assays are based on the specified control group.
#' #'
#' #' @param de_results A list of differential expression results.
#' #' @param ml_results A list of classification models results.
#' #' @param order_by The control group to order the heatmap.
#' #' @param pval_lim The p-value limit to filter the differential expression results of the `order_by` group.
#' #' @param logfc_lim The log2 fold change limit to filter the differential expression results  of the `order_by` group.
#' #'
#' #' @return The summary heatmap of the combined differential expression and classification models results.
#' #' @export
#' #'
#' #' @details It is very important the de_results and ml_results are in the same order
#' #' and in the right format (see examples).
#' #'
#' #' @examples
#' #' # Prepare differential expression results
#' #' de_results_amlbrc <- do_limma(example_data,
#' #'                               example_metadata,
#' #'                               case = "AML",
#' #'                               control = c("BRC"),
#' #'                               wide = FALSE,
#' #'                               only_female = "BRC")
#' #'
#' #' de_results_amlcll <- do_limma(example_data,
#' #'                               example_metadata,
#' #'                               case = "AML",
#' #'                               control = c("CLL"),
#' #'                               wide = FALSE,
#' #'                               only_female = "BRC")
#' #'
#' #' de_results_amllungc <- do_limma(example_data,
#' #'                                 example_metadata,
#' #'                                 case = "AML",
#' #'                                 control = c("LUNGC"),
#' #'                                 wide = FALSE,
#' #'                                 only_female = "BRC")
#' #'
#' #' # Combine the results
#' #' res_de <- list("BRC" = de_results_amlbrc,
#' #'                "CLL" = de_results_amlcll,
#' #'                "LUNGC" = de_results_amllungc)
#' #'
#' #' res_amlbrc <- do_rreg(example_data,
#' #'                       example_metadata,
#' #'                       case = "AML",
#' #'                       type = 'elnet',
#' #'                       control = c("BRC"),
#' #'                       wide = FALSE,
#' #'                       only_female = "BRC",
#' #'                       cv_sets = 2,
#' #'                       grid_size = 1,
#' #'                       ncores = 1)
#' #'
#' #' res_amlcll <- do_rreg(example_data,
#' #'                       example_metadata,
#' #'                       case = "AML",
#' #'                       type = 'elnet',
#' #'                       control = c("CLL"),
#' #'                       wide = FALSE,
#' #'                       only_female = "BRC",
#' #'                       cv_sets = 2,
#' #'                       grid_size = 1,
#' #'                       ncores = 1)
#' #'
#' #' res_amllungc <- do_rreg(example_data,
#' #'                         example_metadata,
#' #'                         case = "AML",
#' #'                         control = c("LUNGC"),
#' #'                         type = 'elnet',
#' #'                         wide = FALSE,
#' #'                         only_female = "BRC",
#' #'                         cv_sets = 2,
#' #'                         grid_size = 1,
#' #'                         ncores = 1)
#' #'
#' #' # Combine the results
#' #' res_ml <- list("BRC" = res_amlbrc,
#' #'                "CLL" = res_amlcll,
#' #'                "LUNGC" = res_amllungc)
#' #'
#' #' # Create the summary heatmap
#' #' plot_biomarkers_summary_heatmap(res_de, res_ml, order_by = "BRC")
#' plot_biomarkers_summary_heatmap <- function(de_results,
#'                                             ml_results,
#'                                             order_by=NULL,
#'                                             pval_lim = 0.05,
#'                                             logfc_lim = 0) {
#'
#'   res_de <- de_results[[order_by]]$de_results |>
#'     dplyr::filter(abs(logFC) > logfc_lim, adj.P.Val < pval_lim) |>
#'     dplyr::select(Assay, logFC, adj.P.Val)
#'
#'   assays <- res_de$Assay
#'
#'   res_plot <- tibble::tibble()
#'   for (i in 1:length(de_results)) {
#'
#'     res_de <- de_results[[i]]$de_results |>
#'       dplyr::filter(Assay %in% assays) |>
#'       dplyr::select(Assay, logFC, adj.P.Val)
#'
#'     res_ml <- ml_results[[i]]$var_imp_res$features |>
#'       dplyr::rename(Assay = Variable) |>
#'       dplyr::filter(Assay %in% assays) |>
#'       dplyr::select(Assay, Scaled_Importance, Sign)
#'
#'     res_combined <- res_de |>
#'       dplyr::left_join(res_ml, by = c("Assay")) |>
#'       dplyr::mutate(control_group = names(de_results)[[i]]) |>
#'       dplyr::arrange(logFC)
#'
#'     res_plot <- rbind(res_plot, res_combined)
#'     }
#'
#'   summary_heatmap <- res_plot |>
#'     dplyr::mutate(Assay = factor(Assay, levels = res_plot |>
#'                                    dplyr::filter(control_group == order_by) |>
#'                                    dplyr::arrange(logFC) |>
#'                                    dplyr::pull(Assay))) |>
#'     ggplot2::ggplot(ggplot2::aes(x = Assay, y = control_group)) +
#'     ggplot2::geom_tile(ggplot2::aes(fill = logFC), color = "white") +
#'     ggplot2::geom_point(ggplot2::aes(size = Scaled_Importance, color = Sign)) +
#'     ggplot2::geom_point(ggplot2::aes(size = Scaled_Importance), shape = 1, colour = "black") +
#'     ggplot2::geom_text(ggplot2::aes(label = ifelse(adj.P.Val < pval_lim, "*", "")), color = "black", size = 2) +
#'     ggplot2::scale_fill_gradient2(low = "cornflowerblue", mid = "white", high = "brown4", midpoint = 0, name = "Log2 FC") +
#'     ggplot2::scale_color_manual(values = c("POS" = "cornflowerblue", "NEG" = "brown4"), name = "Sign", na.translate = FALSE) +
#'     ggplot2::scale_size(name = "Scaled Importance") +
#'     ggplot2::labs(x = "Protein (Assay)", y = "Control Group") +
#'     ggplot2::theme_minimal() +
#'     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
#'                    axis.text.y = ggplot2::element_text(),
#'                    axis.title.x = ggplot2::element_text(face = "bold", size = 12),
#'                    axis.title.y = ggplot2::element_text(face = "bold", size = 12),
#'                    panel.grid.major = ggplot2::element_blank(),
#'                    panel.grid.minor = ggplot2::element_blank(),
#'                    legend.position = "top",
#'                    legend.box = "horizontal",
#'                    legend.title = ggplot2::element_text(size = 10),
#'                    legend.text = ggplot2::element_text(size = 9))
#'
#'   return(summary_heatmap)
#' }
