#' Weighted gene co-expression network analysis
#'
#' `hd_wgcna` performs a weighted gene co-expression network analysis (WGCNA) on the provided data.
#' The user can specify the power parameter for the analysis or the function will select an optimal
#' power value based on the data. The function returns a list containing the WGCNA object, the power
#' and the power plots (in case optimization is performed).
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.
#' @param power The power parameter for the WGCNA analysis as an integer between 1 and 30. If NULL, the function will select an optimal power value. Default is NULL.
#'
#' @returns A list containing the results of the WGCNA.
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Perform WGCNA analysis
#' wgcna_res <- hd_wgcna(hd_object)
#'
#' # Access the WGCNA results
#' wgcna_res$wgcna$colors
#' head(wgcna_res$wgcna$MEs)
#' wgcna_res$power
#' wgcna_res$power_plots
hd_wgcna <- function(dat, power = NULL) {

  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data to run the DE analysis.")
    }
    wide_data <- dat[["data"]]
    sample_id <- dat[["sample_id"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
  }

  # Check power input
  if (!is.null(power) && !is.numeric(power)) {
    stop("The 'power' parameter must be a numeric value.")
  } else if (!is.null(power) && (power < 1 || power > 30)) {
    stop("The 'power' parameter must be an integer between 1 and 30.")
  }

  # Imputation of missing values
  dat_impute <- hd_impute_knn(wide_data, k = 5, verbose = FALSE) |>
    tibble::column_to_rownames(sample_id)

  matrix_dat <- dat_impute |> as.matrix()
  bicor = WGCNA::bicor  # Set bicor namespace to avoid error in WGCNA::blockwiseModules

  # Perform WGCNA
  if (is.null(power)) {
    power_check <- WGCNA::pickSoftThreshold(data = matrix_dat, dataIsExpr = TRUE, verbose = 0)
    power_plts <- power_check$fitIndices |>
      dplyr::rename_with(tolower) |>
      dplyr::select(1, 2, 5:7) |>
      tidyr::pivot_longer(cols = -power) |>
      ggplot2::ggplot(ggplot2::aes(x = power, y = value)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(group = name)) +
      ggplot2::geom_vline(xintercept = power_check$powerEstimate, colour = "red", linetype = "dotted") +
      ggplot2::facet_wrap(~ name, scales = "free_y") +
      theme_hd() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0.5))

    wgcna_obj <- WGCNA::blockwiseModules(datExpr = matrix_dat,
                                         corType = "bicor",
                                         power = power_check$powerEstimate,
                                         verbose = 0,
                                         saveTOMs = FALSE)

    wgcna <- list("wgcna" = wgcna_obj, "power" = power_check$powerEstimate, "power_plots" = power_plts)
    class(wgcna) <- "hd_wgcna"

    return(wgcna)
  } else {
    wgcna <- WGCNA::blockwiseModules(datExpr = matrix_dat,
                                     corType = "bicor",
                                     power = power,
                                     verbose = 0,
                                     saveTOMs = FALSE)

    wgcna <- list("wgcna" = wgcna_obj, "power" = power)
    class(wgcna) <- "hd_wgcna"

    return(wgcna)
  }
}



#' Plot WGCNA results
#'
#' `hd_plot_wgcna` generates useful visualizations for the results of the WGCNA analysis.
#' The function generates a heatmap of proteins and their adjacency, a heatmap of module
#' eigengene (MEs) adjacency, and heatmaps of predictive power score (PPS) between MEs and
#' metadata.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.
#' @param metadata A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.
#' @param wgcna The WGCNA analysis results obtained from `hd_wgcna()`.
#' @param clinical_vars A character vector containing the names of the clinical variables to be used in the predictive power score analysis.
#'
#' @returns The input object enriched with the plots.
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Perform WGCNA analysis
#' wgcna_res <- hd_wgcna(hd_object)
#'
#' # Plot WGCNA results
#' wgcna_res <- hd_plot_wgcna(hd_object,
#'                            wgcna = wgcna_res,
#'                            clinical_vars = c("Disease", "Sex", "Age", "BMI"))
#'
#' # Access the plots
#' wgcna_res$tom_heatmap
#' wgcna_res$me_adjacency
#' wgcna_res$pps
#' wgcna_res$me_pps_heatmap
#' wgcna_res$var_pps_heatmap
hd_plot_wgcna <- function(dat, metadata = NULL, wgcna, clinical_vars = NULL) {

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

  # Imputation of missing values
  dat_impute <- hd_impute_knn(wide_data, k = 5, verbose = FALSE) |>
    tibble::column_to_rownames(sample_id)

  # Heatmap of proteins and their adjacency
  # Make topology overlap matrix
  tom <- WGCNA::TOMsimilarityFromExpr(dat_impute, power = wgcna[["power"]])

  # Get dissimilarity and transform for plotting
  plot_tom <- (1 - tom) ** 10
  rownames(plot_tom) <- colnames(dat_impute)
  colnames(plot_tom) <- colnames(dat_impute)
  diag(plot_tom) <- NA

  # Use modules and their colours to annotate columns
  annot_df <- data.frame(module = wgcna$wgcna$colors, Assay = names(wgcna$wgcna$colors))
  annot_col <- list(module = unique(wgcna$wgcna$colors) |> setNames(unique(wgcna$wgcna$colors)))

  # Prepare dataset and plot heatmap
  plot_tom <- as.data.frame(as.table(plot_tom)) |>
    dplyr::left_join(annot_df, by = c("Var2" = "Assay")) |>
    dplyr::rename(Module = module)

  tom_hm <- ggplotify::as.ggplot(
    tidyheatmaps::tidyheatmap(plot_tom,
                              rows = Var1,
                              columns = Var2,
                              values = Freq,
                              colors = rev(hcl.colors(100)),
                              annotation_col = Module,
                              annotation_colors = annot_col,
                              show_rownames = FALSE,
                              show_colnames = FALSE,
                              cluster_rows = wgcna$wgcna$dendrograms[[1]],
                              cluster_cols = wgcna$wgcna$dendrograms[[1]],
                              color_legend_min = -1,
                              color_legend_max = 1,
                              treeheight_row = 0,
                              silent = TRUE))


  # Module eigengene (MEs) adjacency (defined as (1 + cor) / 2)
  me_adj_dat <- wgcna$wgcna$MEs |>
    cor()

  # Prepare dataset and plot heatmap
  me_adj_dat <- as.data.frame(as.table(me_adj_dat)) |>
    dplyr::mutate(Freq = (Freq + 1)/2)

  me_adj <- ggplotify::as.ggplot(
    tidyheatmaps::tidyheatmap(me_adj_dat,
                              rows = Var1,
                              columns = Var2,
                              values = Freq,
                              colors = hcl.colors(10),
                              legend_breaks = seq(0, 1, length.out = 11),
                              border_color = "white",
                              color_legend_min = 0,
                              color_legend_max = 1,
                              display_numbers = TRUE,
                              number_color = "black",
                              fontsize_number = 11,
                              silent = TRUE))


  # Predictive power score between MEs and metadata
  me_names <- colnames(wgcna$wgcna$MEs)
  vartype <- sapply(metadata |>
                      dplyr::select(dplyr::all_of(c(clinical_vars))),
                    hd_detect_vartype)

  # Input df to ppsr::score function
  pps_in <- wgcna$wgcna$MEs |>
    tibble::rownames_to_column(sample_id) |>
    dplyr::left_join(metadata |>
                       dplyr::select(dplyr::all_of(c(sample_id, clinical_vars))),
                     by = sample_id)

  for (var in names(vartype)) {
    if (vartype[[var]] == "categorical") {
      # Convert categorical variables to factors
      pps_in[[var]] <- as.factor(pps_in[[var]])
    } else if (vartype[[var]] == "continuous") {
      # Convert continuous variables to ordered factors
      #next
      pps_in[[var]] <- factor(pps_in[[var]], ordered = TRUE)
    }
  }

  # ME as predictor for variables
  me_pps <- lapply(me_names, \(x) {
    lapply(clinical_vars, \(y) {
      sc <- ppsr::score(pps_in, x = x, y = y) |> tibble::as_tibble()
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  # Variables as predictors for MEs
  var_pps <- lapply(clinical_vars, \(x) {
    lapply(me_names, \(y) {
      sc <- ppsr::score(pps_in, x = x, y = y) |> tibble::as_tibble()
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  # Make heatmaps
  me_pps_heatmap <- me_pps |>
    dplyr::select(x, y, pps) |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, fill = pps)) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = "Predictor", y = "Outcome", fill = "PPS") +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1)) +
    ggplot2::theme_minimal()

  var_pps_heatmap <- var_pps |>
    dplyr::select(x, y, pps) |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, fill = pps)) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = "Predictor", y = "Outcome", fill = "PPS") +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1)) +
    ggplot2::theme_minimal()

  wgcna[["tom_heatmap"]] <- tom_hm
  wgcna[["me_adjacency"]] <- me_adj
  wgcna[["pps"]] <- dplyr::bind_rows(me_pps, var_pps)
  wgcna[["me_pps_heatmap"]] <- me_pps_heatmap
  wgcna[["var_pps_heatmap"]] <- var_pps_heatmap

  return(wgcna)
}
