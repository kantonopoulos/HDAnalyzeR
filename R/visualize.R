#' Apply palette to plot
#'
#' `apply_palette` applies the color palette to the plot. It checks if the palette is a valid
#' palette from the Human Protein Atlas (HPA) or a custom palette.
#'
#' @param plot The plot to apply the palette.
#' @param palette The color palette to apply. It can be either a character with the name of the
#' palette from the HPA or a custom palette (for example `c("M" = "red", "F" = "blue")`).
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
        plot <- plot + ggplot2::scale_color_manual(values = palette, na.value = "grey50")
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
        plot <- plot + ggplot2::scale_fill_manual(values = palette, na.value = "grey50")
      }
    }
  }

  return(plot)
}


#' Plot feature boxplots
#'
#' `hd_plot_feature_boxplot()` plots boxplots for the specified features in the dataset.
#' It annotates the boxplot with color for the selected case
#' It is also possible to add points to the boxplot.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample_id as its first column.
#' @param metadata A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.
#' @param variable The name of the column containing the case and control groups. Default is "Disease".
#' @param features The features to include in the boxplot. They should be columns of the data.
#' @param case The case class.
#' @param type The type of boxplot to plot. Default is "case_vs_all". Other option is "case_vs_control".
#' @param points Whether to add points to the boxplot.
#' @param x_labels Whether to show the x-axis labels.
#' @param yaxis_title The title of the y-axis. Default is "NPX".
#' @param palette The color palette for the classes. If it is a character, it should be one of the palettes from `hd_palettes()`. Default is NULL.
#'
#' @return The boxplot panel of the selected features
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Boxplots for AARSD1 and ABL1 in AML vs all other classes
#' hd_plot_feature_boxplot(hd_object,
#'                         variable = "Disease",
#'                         features = c("AARSD1", "ABL1"),
#'                         case = "AML",
#'                         palette = "cancers12")
#'
#' # Boxplots for AARSD1 and ABL1 in AML vs joint control group
#' hd_plot_feature_boxplot(hd_object,
#'                         variable = "Disease",
#'                         features = c("AARSD1", "ABL1"),
#'                         case = "AML",
#'                         type = "case_vs_control",
#'                         palette = "cancers12")
hd_plot_feature_boxplot <- function(dat,
                                    metadata = NULL,
                                    variable = "Disease",
                                    features,
                                    case,
                                    type = "case_vs_all",
                                    points = TRUE,
                                    x_labels = TRUE,
                                    yaxis_title = "NPX",
                                    palette = NULL) {

  Variable <- rlang::sym(variable)
  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data to run the DE analysis.")
    }
    wide_data <- dat[["data"]]
    metadata <- dat[["metadata"]]
    sample_id <- dat[["sample_id"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
  }

  if (isFALSE(variable %in% colnames(metadata))) {
    stop("The variable is not be present in the metadata.")
  }

  join_data <- wide_data |>
    dplyr::left_join(metadata |>
                       dplyr::select(dplyr::all_of(c(sample_id, variable))),
                     by = sample_id)

  if (type == "case_vs_control") {
    join_data <- join_data |>
      dplyr::mutate(!!Variable := ifelse(!!Variable == case, case, "Control"))
  }

  # Prepare palettes
  pals <- hd_palettes()
  if (!is.null(palette) && is.null(names(palette))) {
    pal <- unlist(pals[[palette]])
  } else if (!is.null(palette)) {
    pal <- palette
  } else {
    pal <- "black"
  }

  long_data <- join_data |>
    dplyr::select(!!Variable, dplyr::all_of(features)) |>
    tidyr::pivot_longer(cols = !dplyr::any_of(c(variable)),
                        names_to = "Features",
                        values_to = yaxis_title)

  long_data$Features <- factor(long_data$Features, levels = features, labels = features)
  long_data[[variable]] <- as.factor(long_data[[variable]])

  # Create boxplot
  boxplot <- long_data |>
    ggplot2::ggplot(ggplot2::aes(x = !!Variable, y = !!rlang::sym(yaxis_title))) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_boxplot(data = dplyr::filter(long_data, !!Variable == case),
                          ggplot2::aes(fill = !!Variable),
                          alpha = 0.5,
                          show.legend = FALSE,
                          outlier.shape = NA)

  if (isTRUE(points)) {
    boxplot <- boxplot +
      ggplot2::geom_point(data = dplyr::filter(long_data, !!Variable != case),
                          position = ggplot2::position_jitter(width = 0.1),
                          color = 'grey',
                          alpha = 0.3)

    if (!is.null(palette)) {
      boxplot <- boxplot +
        ggplot2::geom_point(data = dplyr::filter(long_data, !!Variable == case),
                            ggplot2::aes(fill = !!Variable),
                            position = ggplot2::position_jitter(width = 0.1),
                            color = pal[case],
                            alpha = 0.5,
                            show.legend = FALSE)
    } else {
      boxplot <- boxplot +
        ggplot2::geom_point(data = dplyr::filter(long_data, !!Variable == case),
                            ggplot2::aes(fill = !!Variable),
                            position = ggplot2::position_jitter(width = 0.1),
                            alpha = 0.5,
                            show.legend = FALSE)
    }
  }

  boxplot_panel <- boxplot +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::scale_fill_manual(values = pal) +
    theme_hd() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90))

  if (isFALSE(x_labels)) {
    boxplot_panel <- boxplot_panel +
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
  }

  boxplot_panel <- boxplot_panel +
    ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym("Features")), scale="free_y")

  return(boxplot_panel)
}


#' Visualize scatter plot with regression line
#'
#' `hd_plot_regression` plots a scatter plot with a linear regression line.
#' It is possible to add the standard error of the regression line, as well as the
#' R-squared and p-value.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample_id as its first column.
#' @param metadata A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.
#' @param metadata_cols The metadata columns to plot. Default is NULL.
#' @param x The column name of the x-axis variable.
#' @param y The column name of the y-axis variable.
#' @param se Whether to add the standard error of the regression line. Default is FALSE.
#' @param line_color The color of the regression line.
#' @param r_2 Whether to add the R-squared and p-value to the plot. Default is TRUE.
#'
#' @return The scatter plot with the regression line.
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Scatter plot for AARSD1 and ABL1
#' hd_plot_regression(hd_object,
#'                    x = "AARSD1",
#'                    y = "ABL1",
#'                    se = TRUE,
#'                    line_color = "red3")
#'
#' # Scatter plot for AARSD1 and Age (metadata variable)
#' hd_plot_regression(hd_object,
#'                    metadata_cols = "Age",
#'                    x = "AARSD1",
#'                    y = "Age",
#'                    line_color = "red3",
#'                    r_2 = FALSE)
hd_plot_regression <- function(dat,
                               metadata = NULL,
                               metadata_cols = NULL,
                               x,
                               y,
                               se = FALSE,
                               line_color = "black",
                               r_2 = TRUE) {
  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data to run the DE analysis.")
    }
    wide_data <- dat[["data"]]
    metadata <- dat[["metadata"]]
    sample_id <- dat[["sample_id"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
  }


  join_data <- wide_data |>
    dplyr::left_join(metadata |>
                       dplyr::select(dplyr::all_of(c(sample_id, metadata_cols))),
                     by = sample_id)

  # Fit the linear model
  formula <- stats::as.formula(paste(y, "~", x))
  model <- stats::lm(formula, data = join_data)

  # Get the R-squared and p-value
  summary_model <- summary(model)
  r_squared <- summary_model$r.squared
  p_val <- stats::coef(summary_model)[2, 4]

  # Create the plot
  x <- rlang::sym(x)
  y <- rlang::sym(y)
  scatter <- join_data |>
    ggplot2::ggplot(ggplot2::aes(x = !!x, y = !!y)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = se, color = line_color)

  if (isTRUE(r_2)) {
    scatter <- scatter +
      ggplot2::annotate("text",
                        x = Inf,
                        y = 0,
                        label = paste("R^2 =", round(r_squared, 2), "\nP =", format.pval(round(p_val, 4))),
                        hjust = 1.1,
                        vjust = 0,
                        size = 5,
                        color = "black") +
    theme_hd()
  } else {
    scatter <- scatter + theme_hd()
  }

  return(scatter)
}


#' Plot a summary heatmap of the combined differential expression and classification models results
#'
#' `hd_plot_features_heatmap` plots a summary heatmap of the combined differential
#' expression and classification models results. The heatmap shows the log2 fold change
#' and adjusted p-value of the differential expression results, and the scaled importance
#' and sign of the classification models results. The heatmap is ordered and the selected
#' assays are based on the specified control group.
#'
#' @param de_results A list of differential expression results.
#' @param model_results A list of classification models results.
#' @param order_by The control group to order the heatmap.
#' @param pval_lim The p-value limit to filter the differential expression results of the `order_by` group.
#' @param logfc_lim The log2 fold change limit to filter the differential expression results  of the `order_by` group.
#'
#' @return The summary heatmap of the combined differential expression and classification models results.
#' @export
#'
#' @details It is very important the de_results and model_results are in the same order
#' and in the right format (see examples).
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' de_results_myel <- hd_run_de_limma(hd_object, case = "AML", control = "MYEL")
#' de_results_lungc <- hd_run_de_limma(hd_object, case = "AML", control = "LUNGC")
#' de_results_gliom <- hd_run_de_limma(hd_object, case = "AML", control = "GLIOM")
#'
#' res_de <- list("MYEL" = de_results_myel,
#'                "LUNGC" = de_results_lungc,
#'                "GLIOM" = de_results_gliom)
#'
#'
#' # Run Classification models
#' # Split the data into training and test sets
#' hd_split <- hd_run_data_split(hd_object, variable = "Disease")
#'
#' # Run the regularized regression model pipeline
#' model_results_myel <- hd_run_rreg(hd_split,
#'                                   variable = "Disease",
#'                                   case = "AML",
#'                                   control = "MYEL",
#'                                   grid_size = 2,
#'                                   cv_sets = 2)
#'
#' model_results_lungc <- hd_run_rreg(hd_split,
#'                                    variable = "Disease",
#'                                    case = "AML",
#'                                    control = "LUNGC",
#'                                    grid_size = 2,
#'                                    cv_sets = 2)
#'
#' model_results_gliom <- hd_run_rreg(hd_split,
#'                                    variable = "Disease",
#'                                    case = "AML",
#'                                    control = "GLIOM",
#'                                    grid_size = 2,
#'                                    cv_sets = 2)
#'
#' # The models are in the same order as the DE results
#' res_model <- list("MYEL" = model_results_myel,
#'                   "LUNGC" = model_results_lungc,
#'                   "GLIOM" = model_results_gliom)
#'
#'
#' # Create the summary heatmap
#' hd_plot_features_heatmap(res_de, res_model, order_by = "MYEL")
hd_plot_features_heatmap <- function(de_results,
                                     model_results,
                                     order_by = NULL,
                                     pval_lim = 0.05,
                                     logfc_lim = 0) {

  res_de <- de_results[[order_by]][["de_res"]] |>
    dplyr::filter(abs(!!rlang::sym("logFC")) > logfc_lim,
                  !!rlang::sym("adj.P.Val") < pval_lim) |>
    dplyr::select(!!rlang::sym("Feature"),
                  !!rlang::sym("logFC"),
                  !!rlang::sym("adj.P.Val"))

  assays <- res_de[["Feature"]]

  res_plot <- tibble::tibble()
  for (i in 1:length(de_results)) {

    res_de <- de_results[[i]][["de_res"]] |>
      dplyr::filter(!!rlang::sym("Feature") %in% assays) |>
      dplyr::select(!!rlang::sym("Feature"),
                    !!rlang::sym("logFC"),
                    !!rlang::sym("adj.P.Val"))

    res_model <- model_results[[i]][["features"]] |>
      dplyr::rename(Feature = !!rlang::sym("Variable")) |>
      dplyr::filter(!!rlang::sym("Feature") %in% assays) |>
      dplyr::select(!!rlang::sym("Feature"),
                    !!rlang::sym("Scaled_Importance"),
                    !!rlang::sym("Sign"))

    res_combined <- res_de |>
      dplyr::left_join(res_model, by = c("Feature")) |>
      dplyr::mutate(control_group = names(de_results)[[i]]) |>
      dplyr::arrange(!!rlang::sym("logFC"))

    res_plot <- rbind(res_plot, res_combined)
    }

  summary_heatmap <- res_plot |>
    dplyr::mutate(
      Feature = factor(!!rlang::sym("Feature"), levels = res_plot |>
                         dplyr::filter(!!rlang::sym("control_group") == order_by) |>
                         dplyr::arrange(!!rlang::sym("logFC")) |>
                         dplyr::pull(!!rlang::sym("Feature")))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym("Feature"), y = !!rlang::sym("control_group"))) +
    ggplot2::geom_tile(ggplot2::aes(fill = !!rlang::sym("logFC")), color = "white") +
    ggplot2::geom_point(ggplot2::aes(size = !!rlang::sym("Scaled_Importance"), color = !!rlang::sym("Sign"))) +
    ggplot2::geom_point(ggplot2::aes(size = !!rlang::sym("Scaled_Importance")), shape = 1, colour = "black") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(!!rlang::sym("adj.P.Val") < pval_lim, "*", "")), color = "black", size = 3) +
    ggplot2::scale_fill_gradient2(low = "#317EC2", mid = "white", high = "#C03830", midpoint = 0, name = "Log2 FC") +
    ggplot2::scale_color_manual(values = c("NEG" = "#317EC2", "POS" = "#C03830"), name = "Sign", na.translate = FALSE) +
    ggplot2::scale_size(name = "Importance") +
    ggplot2::labs(x = "Feature", y = "Control Group") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.text.y = ggplot2::element_text(),
                   axis.title.x = ggplot2::element_text(face = "bold", size = 12),
                   axis.title.y = ggplot2::element_text(face = "bold", size = 12),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.box = "horizontal",
                   legend.title = ggplot2::element_text(size = 10),
                   legend.text = ggplot2::element_text(size = 9))

  return(summary_heatmap)
}
