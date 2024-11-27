utils::globalVariables(":=")
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
#' @return A tibble with the differential expression results.
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
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data to run the PCA analysis.")
    }
    wide_data <- dat[["data"]]
    metadata <- dat[["metadata"]]
    sample_id <- dat[["sample_id"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
  }

  # If control is NULL, set it to the unique values of the variable that are not the case
  if (is.null(control)){
    control <- setdiff(unique(metadata[[variable]]), case)
  }
  variable_type <- hd_detect_vartype(metadata[[variable]])

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
#' @param subtitle The subtitle of the plot or NULL for no subtitle.
#'
#' @return A ggplot object with the volcano plot.
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
                            user_defined_proteins = NULL,
                            subtitle = NULL) {

  if (!inherits(de_object, "hd_de")) {
    stop("The input object is not a differential expression object.")
  }
  if (is.null(de_object[["de_res"]])){
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
    ggrepel::geom_text_repel(data = subset(tab, !!rlang::sym("sig.label") == "top significance"), show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = -log10(pval_lim), linetype = 'dashed') +
    ggplot2::geom_vline(xintercept = logfc_lim, linetype = 'dashed') +
    ggplot2::geom_vline(xintercept = -logfc_lim, linetype = 'dashed') +
    ggplot2::labs(color = "Significance")

  # Title and subtitle
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(label = paste0(title, ""))
  }

  subtitle_text <- ""
  if (!is.null(subtitle)) {
    subtitle_text = subtitle
  }

  if (isTRUE(report_nproteins)) {
    if (subtitle_text != "") {
      subtitle_text = paste0(subtitle_text,
                             "\nNum significant up = ", num.sig.up,
                             "\nNum significant down = ", num.sig.down)
    } else {
      subtitle_text = paste0("Num significant up = ", num.sig.up,
                             "\nNum significant down = ", num.sig.down)
    }
  }

  if (subtitle_text != "") {
    p <- p + ggplot2::ggtitle(label = paste0(title, ""), subtitle = subtitle_text)
  }

  # Set palette
  p <- apply_palette(p, palette, type = "color")

  de_object[["volcano_plot"]] <- p + theme_hd() + ggplot2::theme(legend.position = "none")

  return(de_object)
}


#' #' Extract protein lists from the upset data
#' #'
#' #' `extract_protein_list()` extracts the protein lists from the upset data.
#' #' It creates a list with the proteins for each combination of diseases.
#' #' It also creates a tibble with the proteins for each combination of diseases.
#' #'
#' #' @param upset_data A tibble with the upset data.
#' #' @param proteins A list with the protein lists for each disease.
#' #'
#' #' @return A list with the following elements:
#' #'  - proteins_list: A list with the proteins for each combination of diseases.
#' #'  - proteins_df: A tibble with the proteins for each combination of diseases.
#' #' @keywords internal
#' extract_protein_list <- function(upset_data, proteins) {
#'   combinations <- as.data.frame(upset_data)
#'   proteins_list <- list()
#'
#'   for (i in 1:nrow(combinations)) {
#'     combo <- combinations[i, ]
#'     set_names <- names(combo)[combo == 1]
#'     set_name <- paste(set_names, collapse = "&")
#'     protein_set <- Reduce(intersect, proteins[set_names])
#'     proteins_list[[set_name]] <- protein_set
#'   }
#'
#'   proteins_df <- do.call(rbind, lapply(names(proteins_list), function(set_name) {
#'     tibble::tibble(
#'       "Shared in" = set_name,
#'       "up/down" = ifelse(grepl("down", deparse(substitute(proteins_list))), "down", "up"),
#'       "Assay" = unique(unlist(proteins_list[[set_name]]))
#'     )
#'   }))
#'
#'   proteins_df <- proteins_df |>
#'     dplyr::mutate(Priority = stringr::str_count(`Shared in`, "&"))
#'
#'   proteins_df <- proteins_df |>
#'     dplyr::arrange(Assay, dplyr::desc(Priority)) |>
#'     dplyr::group_by(Assay) |>
#'     dplyr::slice(1) |>
#'     dplyr::ungroup() |>
#'     dplyr::select(-Priority)
#'
#'   return(list("proteins_list" = proteins_list, "proteins_df" = proteins_df))
#' }
#'
#'
#' #' Plot summary visualizations for the differential expression results
#' #'
#' #' `plot_de_summary()` creates summary visualizations for the differential expression results.
#' #' It plots a barplot with the number of significant proteins for each disease.
#' #' It also creates upset plots both for the significant up and down regulated proteins for each disease.
#' #'
#' #' @param de_results A list of differential expression results.
#' #' @param disease_palette The color palette for the disease. If it is a character, it should be one of the palettes from `hd_palettes()`. Default is NULL.
#' #' @param diff_exp_palette The color palette for the differential expression. If it is a character, it should be one of the palettes from `hd_palettes()`. Default is "diff_exp".
#' #' @param verbose If the function should print the different sets of significant proteins for each disease. Default is TRUE.
#' #'
#' #' @return A list containing the following plots:
#' #'   - de_barplot: A barplot with the number of significant proteins for each disease.
#' #'   - upset_plot_up: An upset plot with the significant up regulated proteins for each disease.
#' #'   - upset_plot_down: An upset plot with the significant down regulated proteins for each disease.
#' #'   - proteins_df_up: A tibble with the significant up regulated proteins for each combination of diseases.
#' #'   - proteins_df_down: A tibble with the significant down regulated proteins for each combination of diseases.
#' #'   - proteins_list_up: A list with the significant up regulated proteins for each combination of diseases.
#' #'   - proteins_list_down: A list with the significant down regulated proteins for each combination of diseases.
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' # Run differential expression analysis for 3 different cases
#' #' de_results_aml <- do_limma(example_data,
#' #'                            example_metadata,
#' #'                            case = "AML",
#' #'                            control = c("BRC", "PRC"),
#' #'                            wide = FALSE,
#' #'                            only_female = "BRC",
#' #'                            only_male = "PRC")
#' #'
#' #' de_results_brc <- do_limma(example_data,
#' #'                            example_metadata,
#' #'                            case = "BRC",
#' #'                            control = c("AML", "PRC"),
#' #'                            wide = FALSE,
#' #'                            only_female = "BRC",
#' #'                            only_male = "PRC")
#' #'
#' #' de_results_prc <- do_limma(example_data,
#' #'                            example_metadata,
#' #'                            case = "PRC",
#' #'                            control = c("AML", "BRC"),
#' #'                            wide = FALSE,
#' #'                            only_female = "BRC",
#' #'                            only_male = "PRC")
#' #'
#' #' # Combine the results
#' #' res <- list("AML" = de_results_aml,
#' #'             "BRC" = de_results_brc,
#' #'             "PRC" = de_results_prc)
#' #'
#' #' # Plot summary visualizations
#' #' plot_de_summary(res)
#' plot_de_summary <- function(de_results,
#'                             disease_palette = NULL,
#'                             diff_exp_palette = "diff_exp",
#'                             verbose = TRUE) {
#'   de_res_list <- list()
#'   for (i in 1:length(de_results)) {
#'     de_res_list[[i]] <- de_results[[i]]$de_results |>
#'       dplyr::mutate(Disease = de_results[[i]]$de_results$Disease)
#'   }
#'
#'   barplot_data <- de_res_list |>
#'     dplyr::bind_rows() |>
#'     dplyr::mutate(sig = factor(sig, levels = c("not significant", "significant down", "significant up"))) |>
#'     dplyr::group_by(Disease, sig) |>
#'     dplyr::summarise(Count = dplyr::n()) |>
#'     dplyr::ungroup()
#'
#'   de_barplot <- barplot_data |>
#'     ggplot2::ggplot(ggplot2::aes(x = Disease, y = Count, fill = sig)) +
#'     ggplot2::geom_bar(stat = "identity", position = "stack") +
#'     ggplot2::labs(x = "", y = "Number of proteins", fill = "Significance") +
#'     theme_hpa(angled = T) +
#'     ggplot2::theme(legend.position = "top",
#'                    legend.title = ggplot2::element_text(face = "bold"))
#'
#'   if (is.null(names(diff_exp_palette)) && !is.null(diff_exp_palette)) {
#'     de_barplot <- de_barplot + scale_fill_hpa(diff_exp_palette)
#'   } else if (!is.null(diff_exp_palette)) {
#'     de_barplot <- de_barplot + ggplot2::scale_fill_manual(values = diff_exp_palette)
#'   }
#'
#'   significant_proteins_up <- lapply(names(de_results), function(disease) {
#'     significant_proteins_up <- de_results[[disease]]$de_results |>
#'       dplyr::filter(sig == "significant up") |>
#'       dplyr::pull(Assay)
#'
#'   })
#'   names(significant_proteins_up) <- names(de_results)
#'
#'   significant_proteins_down <- lapply(names(de_results), function(disease) {
#'
#'     significant_proteins_down <- de_results[[disease]]$de_results |>
#'       dplyr::filter(sig == "significant down") |>
#'       dplyr::pull(Assay)
#'
#'   })
#'   names(significant_proteins_down) <- names(de_results)
#'
#'   significant_proteins <- list("up" = significant_proteins_up, "down" = significant_proteins_down)
#'
#'   # Prepare palettes
#'   if (is.null(names(disease_palette)) && !is.null(disease_palette)) {
#'     pal <- get_hpa_palettes()[[disease_palette]]
#'   } else if (!is.null(disease_palette)) {
#'     pal <- disease_palette
#'   } else {
#'     pal <- rep("black", length(names(de_results)))
#'     names(pal) <- names(de_results)
#'   }
#'   de_names <- names(significant_proteins_up)
#'   ordered_colors <- pal[de_names]
#'   frequencies_up <- sapply(significant_proteins_up, length)
#'   ordered_names_up <- names(sort(frequencies_up, decreasing = TRUE))
#'   print(ordered_names_up)
#'   ordered_colors_up <- ordered_colors[ordered_names_up]
#'   frequencies_down <- sapply(significant_proteins_down, length)
#'   ordered_names_down <- names(sort(frequencies_down, decreasing = TRUE))
#'   ordered_colors_down <- ordered_colors[ordered_names_down]
#'
#'   # Create upset data and extract protein lists
#'   upset_up <- UpSetR::fromList(significant_proteins_up)
#'   upset_down <- UpSetR::fromList(significant_proteins_down)
#'   proteins_up <- extract_protein_list(upset_up, significant_proteins_up)
#'   proteins_down <- extract_protein_list(upset_down, significant_proteins_down)
#'
#'   if (verbose) {
#'     print(proteins_up$proteins_df)
#'     print(proteins_down$proteins_df)
#'   }
#'
#'   # Create upset plots
#'   upset_plot_up <- UpSetR::upset(upset_up,
#'                                  sets = ordered_names_up,
#'                                  order.by = "freq",
#'                                  nsets = length(ordered_names_up),
#'                                  sets.bar.color = ordered_colors_up)
#'
#'   upset_plot_down <- UpSetR::upset(upset_down,
#'                                    sets = ordered_names_down,
#'                                    order.by = "freq",
#'                                    nsets = length(ordered_names_down),
#'                                    sets.bar.color = ordered_colors_down)
#'
#'   return(list("de_barplot" = de_barplot,
#'               "upset_plot_up" = upset_plot_up,
#'               "upset_plot_down" = upset_plot_down,
#'               "proteins_df_up" = proteins_up$proteins_df,
#'               "proteins_df_down" = proteins_down$proteins_df,
#'               "proteins_list_up" = proteins_up$proteins_list,
#'               "proteins_list_down" = proteins_down$proteins_list))
#' }
