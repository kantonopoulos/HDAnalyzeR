utils::globalVariables(c(":=", "sig.label"))
#' Differential expression analysis with limma
#'
#' `limma_de()` performs differential expression analysis using limma package.
#' It can correct the results for metadata columns like Sex, Age, or BMI.
#' The output tibble includes the logFC, p-values, as well as the FDR adjusted p-values.
#' The function removes the NAs from the columns that are used to correct for.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample_id as its first column.
#' @param metadata A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.
#' @param variable The name of the column containing the case and control groups or a continuous variable.
#' @param case The case group. In case of a continuous variable, it must be NULL.
#' @param control The control groups. If NULL, it will be set to all other unique values of the variable that are not the case. In case of a continuous variable, it must be NULL.
#' @param correct The variables to correct the results with. Default is NULL.
#'
#' @return An object with the DE results.
#' @details
#' In case of a continuous variable or if you are correcting based on a continuous variable,
#' the variable should be numeric and contain at least 6 unique variables.
#'
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' hd_run_de_limma(hd_object, case = "AML")
#'
#' # Run differential expression analysis for AML vs CLL and MYEL and correct for metadata variables
#' hd_run_de_limma(hd_object,
#'                 case = "AML",
#'                 control = c("CLL", "MYEL"),
#'                 correct = c("Sex", "Age", "BMI"))
#'
#' # Run differential expression analysis for continuous variable
#' hd_run_de_limma(hd_object, variable = "Age", case = NULL, correct = c("Sex"))
hd_run_de_limma <- function(dat,
                            metadata = NULL,
                            variable = "Disease",
                            case,
                            control = NULL,
                            correct = NULL) {

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

  # If control is NULL, set it to the unique values of the variable that are not the case
  if (is.null(control)){
    control <- setdiff(unique(metadata[[variable]]), case)
  }
  variable_type <- hd_detect_vartype(metadata[[variable]], unique_threshold = 2)

  join_data <- wide_data |>
    dplyr::left_join(metadata |>
                       dplyr::select(dplyr::all_of(c(sample_id, variable, correct))),
                     by = sample_id) |>
    dplyr::filter(!!Variable %in% c(case, control))

  nrows_before <- nrow(join_data)

  join_data <- join_data |>
    dplyr::filter(!dplyr::if_any(dplyr::all_of(c(variable, correct)), is.na))  # Remove NAs from columns in formula

  if (variable_type == "categorical") {
    join_data <- join_data |>
      dplyr::mutate(!!Variable := ifelse(!!Variable == case, "1_Case", "0_Control"))
  }

  nrows_after <- nrow(join_data)
  if (nrows_before != nrows_after){
    warning(paste0(nrows_before - nrows_after,
                   " rows were removed because they contain NAs in ",
                   variable,
                   " or ",
                   paste(correct, collapse = ", "),
                   "!"))
  }

  # Design a model
  if (variable_type == "categorical") {
    formula <- paste("~0 + as.factor(", variable, ")")
  } else {
    formula <- paste("~0 +" , variable)
  }

  if (!is.null(correct)) {
    var_types <- sapply(metadata |>
                          dplyr::select(dplyr::all_of(c(correct))),
                        hd_detect_vartype)
    for (i in correct) {
      if (var_types[i] == "categorical") {
        cofactor = paste("as.factor(", i, ")")
      } else {
        cofactor = i
      }
      formula <- paste(formula, "+", cofactor)
    }
  }

  design <- stats::model.matrix(stats::as.formula(formula), data = join_data)
  if (variable_type == "categorical") {
    cols <- c("control", "case", correct)
    cols <- cols[!is.null(cols)]
    colnames(design) <- paste(cols)
    contrast <- limma::makeContrasts(Diff = case - control, levels = design)
  }

  # Fit linear model to each protein assay
  data_fit <- join_data |>
    dplyr::select(-dplyr::any_of(c(variable, correct))) |>
    tibble::column_to_rownames(sample_id) |>
    t()

  fit <- limma::lmFit(data_fit, design = design, method = "robust", maxit = 10000)

  # Apply empirical Bayes smoothing to the SE
  if (variable_type == "categorical") {
    contrast_fit <- limma::contrasts.fit(fit, contrast)
    ebays_fit <- limma::eBayes(contrast_fit)
  } else {
    ebays_fit <- limma::eBayes(fit)
  }

  # Extract DE results
  de_results <- limma::topTable(ebays_fit,
                                n = nrow(ebays_fit$p.value),
                                adjust.method = "fdr",
                                confint = TRUE)

  if (variable_type == "categorical") {
    de_res <- de_results |>
      tibble::as_tibble(rownames = "Feature") |>
      dplyr::mutate(!!Variable := case) |>
      dplyr::arrange(!!rlang::sym("adj.P.Val"))
  } else {
    de_res <- de_results |>
      tibble::as_tibble(rownames = "Feature") |>
      dplyr::rename(logFC = colnames(de_results)[1]) |>
      dplyr::arrange(!!rlang::sym("adj.P.Val"))
  }

  de_res <- list("de_res" = de_res)
  class(de_res) <- "hd_de"

  return(de_res)
}


#' Differential expression analysis with t-test
#'
#' `hd_run_de_ttest()` performs differential expression analysis using t-test.
#' It separates the data in case-control groups, checks for data normality and
#' perform a t-test or Wilcoxon test respectively. It also performs p value FDR adjustment.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format and sample_id as its first column.
#' @param metadata A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.
#' @param variable The name of the column containing the case and control groups.
#' @param case The case group.
#' @param control The control groups. If NULL, it will be set to all other unique values of the variable that are not the case.
#'
#' @return An object with the DE results.
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' hd_run_de_ttest(hd_object, case = "AML")
#'
#' # Run differential expression analysis for AML vs CLL
#' hd_run_de_ttest(hd_object, case = "AML", control = "CLL")
hd_run_de_ttest <- function(dat,
                            metadata = NULL,
                            variable = "Disease",
                            case,
                            control = NULL) {

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

  # If control is NULL, set it to the unique values of the variable that are not the case
  if (is.null(control)){
    control <- setdiff(unique(metadata[[variable]]), case)
  }

  join_data <- wide_data |>
    dplyr::left_join(metadata |>
                       dplyr::select(dplyr::all_of(c(sample_id, variable))),
                     by = sample_id) |>
    dplyr::filter(!!Variable %in% c(case, control))

  nrows_before <- nrow(join_data)

  join_data <- join_data |>
    dplyr::filter(!dplyr::if_any(dplyr::all_of(c(variable)), is.na))  # Remove NAs from columns in formula

  nrows_after <- nrow(join_data)
  if (nrows_before != nrows_after){
    warning(paste0(nrows_before - nrows_after,
                   " rows were removed because they contain NAs in ",
                   variable,
                   "!"))
  }

  de_res <- matrix(nrow=0, ncol=5)
  colnames(de_res) <- c("Feature", "logFC", "t", "P.Value", variable)

  # Run statistical test for each assay
  de_res_list <- lapply(names(wide_data[-1]), function(assay) {
    case_group <- join_data |>
      dplyr::filter(!!Variable == case) |>
      dplyr::pull(!!rlang::sym(assay))

    control_group <- join_data |>
      dplyr::filter(!!Variable %in% control) |>
      dplyr::pull(!!rlang::sym(assay))


    test_res <- stats::t.test(case_group, control_group)

    t.val <- test_res[["statistic"]]
    p.val <- test_res[["p.value"]]
    difference <- mean(case_group, na.rm = TRUE) - mean(control_group, na.rm = TRUE)

    de_res <- rbind(de_res, c(assay, difference, t.val, p.val, case))
  })

  combined_de_res <- do.call(rbind, de_res_list)
  combined_de_res <- as.data.frame(combined_de_res) |>
    dplyr::mutate(P.Value = as.numeric(!!rlang::sym("P.Value")),
                  logFC = as.numeric(!!rlang::sym("logFC")))
  combined_de_res[["adj.P.Val"]] <- stats::p.adjust(combined_de_res$P.Value, method = "fdr")
  de_res <- combined_de_res |>
    dplyr::arrange(!!rlang::sym("adj.P.Val")) |>
    dplyr::relocate(!!Variable, .after = !!rlang::sym("adj.P.Val"))

  de_res <- list("de_res" = tibble::as_tibble(de_res))
  class(de_res) <- "hd_de"

  return(de_res)
}


#' Create volcano plots
#'
#' `plot_volcano()` creates volcano plots for the differential expression results.
#' It colors and labels the top up and down regulated proteins.
#'
#' @param de_object The differential expression object. Created by `hd_run_de_limma()`.
#' @param pval_lim The p-value limit for significance. Default is 0.05.
#' @param logfc_lim The logFC limit for significance. Default is 0.
#' @param top_up_prot The number of top up regulated proteins to label on the plot. Default is 10.
#' @param top_down_prot The number of top down regulated proteins to label on the plot. Default is 5.
#' @param palette The color palette for the plot. It should be one of the palettes from `hd_palettes()` or a custom palette. Default is "diff_exp".
#' @param title The title of the plot or NULL for no title.
#' @param report_nproteins If the number of significant proteins should be reported in the subtitle. Default is TRUE.
#' @param user_defined_proteins A vector with the protein names to label on the plot. Default is NULL.
#'
#' @return An object with the DE results and the volcano plot.
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' de_results <- hd_run_de_limma(hd_object, case = "AML")
#'
#' # Create a volcano plot
#' hd_plot_volcano(de_results)
hd_plot_volcano <- function(de_object,
                            pval_lim = 0.05,
                            logfc_lim = 0,
                            top_up_prot = 10,
                            top_down_prot = 5,
                            palette = "diff_exp",
                            title = NULL,
                            report_nproteins = TRUE,
                            user_defined_proteins = NULL) {

  if (!inherits(de_object, "hd_de")) {
    stop("The input object is not a differential expression object.")
  }
  if (is.null(de_object[["de_res"]]) | is.null(de_object[["de_res"]][["adj.P.Val"]])){
    stop("The input object does not contain the differential expression results.")
  }

  top.sig.down <- de_object[["de_res"]] |>
    dplyr::filter(!!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") < -logfc_lim) |>
    dplyr::arrange(!!rlang::sym("adj.P.Val")) |>
    dplyr::pull(!!rlang::sym("Feature"))

  top.sig.up <- de_object[["de_res"]] |>
    dplyr::filter(!!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") > logfc_lim) |>
    dplyr::arrange(!!rlang::sym("adj.P.Val")) |>
    dplyr::pull(!!rlang::sym("Feature"))

  if (is.null(user_defined_proteins)) {
    top.sig.prot <- c(top.sig.up[1:top_up_prot], top.sig.down[1:top_down_prot])
  } else {
    top.sig.prot <- user_defined_proteins
  }

  tab <- de_object[["de_res"]] |>
    dplyr::mutate(
      sig.label = ifelse(!!rlang::sym("Feature") %in% top.sig.prot, "top significance", 0),
      sig = dplyr::case_when(
        !!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") < -logfc_lim ~ "significant down",
        !!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") > logfc_lim ~ "significant up",
        TRUE ~ "not significant"
      )
    )

  num.sig.up <- length(top.sig.up)
  num.sig.down <- length(top.sig.down)

  p <- de_object[["de_res"]] |>
    dplyr::mutate(sig = dplyr::case_when(
      !!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") < -logfc_lim ~ "significant down",
      !!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") > logfc_lim ~ "significant up",
      TRUE ~ "not significant"
    )) |>
    ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym("logFC"),
                                 y = -log10(!!rlang::sym("adj.P.Val")),
                                 color = !!rlang::sym("sig"),
                                 label = !!rlang::sym("Feature"))) +
    ggplot2::geom_point(size = 1, alpha = 0.4) +
    ggrepel::geom_text_repel(data = subset(tab, sig.label == "top significance"), show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = -log10(pval_lim), linetype = 'dashed') +
    ggplot2::geom_vline(xintercept = logfc_lim, linetype = 'dashed') +
    ggplot2::geom_vline(xintercept = -logfc_lim, linetype = 'dashed') +
    ggplot2::labs(color = "Significance")

  # Title and subtitle
  title_text <- ""
  if (!is.null(title)) {
    title_text = title
  }

  if (isTRUE(report_nproteins)) {
    if (title_text != "") {
      title_text = paste0(title_text,
                          "\nNum significant up = ", num.sig.up,
                          "\nNum significant down = ", num.sig.down)
    } else {
      title_text = paste0("Num significant up = ", num.sig.up,
                          "\nNum significant down = ", num.sig.down)
    }
  }

  if (title_text != "") {
    p <- p + ggplot2::ggtitle(label = paste0(title_text, ""))
  }

  # Set palette
  p <- apply_palette(p, palette, type = "color")

  de_object[["volcano_plot"]] <- p + theme_hd() + ggplot2::theme(legend.position = "none")

  return(de_object)
}


#' Extract protein lists from the upset data
#'
#' `extract_protein_list()` extracts the protein lists from the upset data.
#' It creates a list with the proteins for each combination of diseases.
#' It also creates a tibble with the proteins for each combination of diseases.
#'
#' @param upset_data A tibble with the upset data.
#' @param proteins A list with the protein lists for each disease.
#'
#' @return A list with the following elements:
#'  - proteins_list: A list with the proteins for each combination of diseases.
#'  - proteins_df: A tibble with the proteins for each combination of diseases.
#' @keywords internal
extract_protein_list <- function(upset_data, proteins) {
  combinations <- as.data.frame(upset_data)
  proteins_list <- list()

  for (i in 1:nrow(combinations)) {
    combo <- combinations[i, ]
    set_names <- names(combo)[combo == 1]
    set_name <- paste(set_names, collapse = "&")
    protein_set <- Reduce(intersect, proteins[set_names])
    proteins_list[[set_name]] <- protein_set
  }

  proteins_df <- do.call(rbind, lapply(names(proteins_list), function(set_name) {
    tibble::tibble(
      "Shared_in" = set_name,
      "up/down" = ifelse(grepl("down", deparse(substitute(proteins_list))), "down", "up"),
      "Feature" = unique(unlist(proteins_list[[set_name]]))
    )
  }))

  proteins_df <- proteins_df |>
    dplyr::mutate(Priority = stringr::str_count(!!rlang::sym("Shared_in"), "&"))

  proteins_df <- proteins_df |>
    dplyr::arrange(!!rlang::sym("Feature"), dplyr::desc(!!rlang::sym("Priority"))) |>
    dplyr::group_by(!!rlang::sym("Feature")) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-!!rlang::sym("Priority"))

  return(list("proteins_list" = proteins_list, "proteins_df" = proteins_df))
}


#' Plot summary visualizations for the differential expression results
#'
#' `plot_de_summary()` creates summary visualizations for the differential expression results.
#' It plots a barplot with the number of significant proteins for each disease.
#' It also creates upset plots both for the significant up and down regulated proteins for each disease.
#'
#' @param de_results A list of differential expression results. It should be a list of objects created by `hd_run_de_limma()` with the classes as names. See the examples for more details.
#' @param variable The name of the column containing the case and control groups.
#' @param class_palette The color palette for the classes. If it is a character, it should be one of the palettes from `hd_palettes()`. Default is NULL.
#' @param diff_exp_palette The color palette for the differential expression. If it is a character, it should be one of the palettes from `hd_palettes()`. Default is "diff_exp".
#' @param pval_lim The p-value limit for significance. Default is 0.05.
#' @param logfc_lim The logFC limit for significance. Default is 0.
#'
#' @return A list with the DE summary plots and results
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' de_results_aml <- hd_run_de_limma(hd_object, case = "AML", control)
#' de_results_lungc <- hd_run_de_limma(hd_object, case = "LUNGC")
#' de_results_cll <- hd_run_de_limma(hd_object, case = "CLL")
#' de_results_myel <- hd_run_de_limma(hd_object, case = "MYEL")
#' de_results_gliom <- hd_run_de_limma(hd_object, case = "GLIOM")
#'
#' res <- list("AML" = de_results_aml,
#'             "LUNGC" = de_results_lungc,
#'             "CLL" = de_results_cll,
#'             "MYEL" = de_results_myel,
#'             "GLIOM" = de_results_gliom)
#'
#' # Plot summary visualizations
#' hd_plot_de_summary(res, class_palette = "cancers12")
hd_plot_de_summary <- function(de_results,
                               variable = "Disease",
                               class_palette = NULL,
                               diff_exp_palette = "diff_exp",
                               pval_lim = 0.05,
                               logfc_lim = 0) {

  Variable <- rlang::sym(variable)
  de_res_list <- list()
  for (i in 1:length(de_results)) {
    de_res_list[[i]] <- de_results[[i]][["de_res"]] |>
      dplyr::mutate(sig = dplyr::case_when(
        !!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") < -logfc_lim ~ "significant down",
        !!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") > logfc_lim ~ "significant up",
        TRUE ~ "not significant"
      )) |>
      dplyr::mutate(!!Variable := de_results[[i]][["de_res"]][[variable]])
  }

  barplot_data <- de_res_list |>
    dplyr::bind_rows() |>
    dplyr::mutate(sig = factor(!!rlang::sym("sig"), levels = c("not significant", "significant down", "significant up"))) |>
    dplyr::group_by(!!Variable, !!rlang::sym("sig")) |>
    dplyr::summarise(Count = dplyr::n()) |>
    dplyr::ungroup()

  de_barplot <- barplot_data |>
    ggplot2::ggplot(ggplot2::aes(x = !!Variable, y = !!rlang::sym("Count"), fill = !!rlang::sym("sig")), alpha = 0.4) +
    ggplot2::geom_bar(stat = "identity", position = "stack", colour="black") +
    ggplot2::labs(x = "", y = "Number of proteins", fill = "Significance") +
    theme_hd(angled = 90) +
    ggplot2::theme(legend.position = "top",
                   legend.title = ggplot2::element_text(face = "bold"))

  de_barplot <- apply_palette(de_barplot, diff_exp_palette, type = "fill")

  significant_proteins_up <- lapply(names(de_results), function(disease) {
    significant_proteins_up <- de_results[[disease]][["de_res"]] |>
      dplyr::mutate(sig = dplyr::case_when(
        !!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") < -logfc_lim ~ "significant down",
        !!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") > logfc_lim ~ "significant up",
        TRUE ~ "not significant"
      )) |>
      dplyr::filter(!!rlang::sym("sig") == "significant up") |>
      dplyr::pull(!!rlang::sym("Feature"))

  })
  names(significant_proteins_up) <- names(de_results)

  significant_proteins_down <- lapply(names(de_results), function(disease) {

    significant_proteins_down <- de_results[[disease]][["de_res"]] |>
      dplyr::mutate(sig = dplyr::case_when(
        !!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") < -logfc_lim ~ "significant down",
        !!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") > logfc_lim ~ "significant up",
        TRUE ~ "not significant"
      )) |>
      dplyr::filter(!!rlang::sym("sig") == "significant down") |>
      dplyr::pull(!!rlang::sym("Feature"))

  })
  names(significant_proteins_down) <- names(de_results)

  significant_proteins <- list("up" = significant_proteins_up, "down" = significant_proteins_down)

  # Prepare palettes
  if (is.null(names(class_palette)) && !is.null(class_palette)) {
    pal <- hd_palettes()[[class_palette]]
  } else if (!is.null(class_palette)) {
    pal <- class_palette
  } else {
    pal <- rep("black", length(names(de_results)))
    names(pal) <- names(de_results)
  }
  de_names <- names(significant_proteins_up)
  ordered_colors <- pal[de_names]
  frequencies_up <- sapply(significant_proteins_up, length)
  ordered_names_up <- names(sort(frequencies_up, decreasing = TRUE))
  ordered_colors_up <- ordered_colors[ordered_names_up]
  frequencies_down <- sapply(significant_proteins_down, length)
  ordered_names_down <- names(sort(frequencies_down, decreasing = TRUE))
  ordered_colors_down <- ordered_colors[ordered_names_down]

  # Create upset data and extract protein lists
  upset_up <- UpSetR::fromList(significant_proteins_up)
  upset_down <- UpSetR::fromList(significant_proteins_down)
  proteins_up <- extract_protein_list(upset_up, significant_proteins_up)
  proteins_down <- extract_protein_list(upset_down, significant_proteins_down)

  # Create upset plots
  upset_plot_up <- UpSetR::upset(upset_up,
                                 sets = ordered_names_up,
                                 order.by = "freq",
                                 nsets = length(ordered_names_up),
                                 sets.bar.color = ordered_colors_up)

  upset_plot_down <- UpSetR::upset(upset_down,
                                   sets = ordered_names_down,
                                   order.by = "freq",
                                   nsets = length(ordered_names_down),
                                   sets.bar.color = ordered_colors_down)

  return(list("de_barplot" = de_barplot,
              "upset_plot_up" = upset_plot_up,
              "upset_plot_down" = upset_plot_down,
              "proteins_df_up" = proteins_up$proteins_df,
              "proteins_df_down" = proteins_down$proteins_df,
              "proteins_list_up" = proteins_up$proteins_list,
              "proteins_list_down" = proteins_down$proteins_list))
}
