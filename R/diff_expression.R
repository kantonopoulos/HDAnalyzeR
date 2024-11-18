#' utils::globalVariables(c("adj.P.Val", "P.Value", "logFC", "sig", "sig.label", "Sex",
#'                          "Disease", "case", "control", "is_normal", "Count", ":=",
#'                          "Shared in", "Priority"))
#' #' Differential expression analysis with limma
#' #'
#' #' `do_limma_de()` performs differential expression analysis using limma package.
#' #' It can correct the results for metadata columns like Sex, Age, or BMI.
#' #' The output tibble includes the logFC, p-values, as well as the FDR adjusted p-values.
#' #' The function removes the NAs from the columns that are used to correct for.
#' #'
#' #' @param join_data A tibble with the Olink data in wide format joined with metadata.
#' #' @param variable The variable of interest that includes the case and control groups.
#' #' @param case The case group.
#' #' @param control The control groups.
#' #' @param correct The variables to correct the results with. Default c("Sex", "Age").
#' #' @param correct_type The type of the variables to correct the results with. Default c("factor", "numeric").
#' #' @param only_female The female specific diseases. Default is NULL.
#' #' @param only_male The male specific diseases. Default is NULL.
#' #' @param pval_lim The p-value limit of significance. Default is 0.05.
#' #' @param logfc_lim The logFC limit of significance. Default is 0.
#' #'
#' #' @return A tibble with the differential expression results.
#' #' @keywords internal
#' do_limma_de <- function(join_data,
#'                         variable = "Disease",
#'                         case,
#'                         control,
#'                         correct = c("Sex", "Age"),
#'                         correct_type = c("factor", "numeric"),
#'                         only_female = NULL,
#'                         only_male = NULL,
#'                         pval_lim = 0.05,
#'                         logfc_lim = 0) {
#'   Variable <- rlang::sym(variable)
#'
#'   if (variable == "Disease") {
#'     sex_specific <- FALSE
#'     # Filter for Sex if disease is Sex specific
#'     if(!is.null(only_female) & case %in% only_female) {
#'       join_data <- join_data |>
#'         dplyr::filter(Sex == "F")
#'       sex_specific <- TRUE
#'     } else {
#'       join_data <- join_data
#'     }
#'
#'     if(!is.null(only_male) & case %in% only_male) {
#'       join_data <- join_data |>
#'         dplyr::filter(Sex == "M")
#'       sex_specific <- TRUE
#'     } else {
#'       join_data <- join_data
#'     }
#'   }
#'
#'   nrows_before <- nrow(join_data)
#'
#'   join_data <- join_data |>
#'     dplyr::filter(!dplyr::if_any(dplyr::all_of(c(variable, correct)), is.na)) |>  # Remove NAs from columns in formula
#'     dplyr::filter(!!Variable %in% c(case, control)) |>
#'     dplyr::mutate(!!Variable := ifelse(!!Variable == case, "1_Case", "0_Control"))
#'
#'   nrows_after <- nrow(join_data)
#'   if (nrows_before != nrows_after){
#'     warning(paste0(nrows_before - nrows_after,
#'                    " rows were removed because they contain NAs in ",
#'                    variable,
#'                    " or ",
#'                    paste(correct, collapse = ", "),
#'                    "!"))
#'   }
#'
#'   # Design a model
#'   formula <- paste("~0 + as.factor(", variable, ")")
#'
#'   if (!is.null(correct)) {
#'     for (i in 1:length(correct)) {
#'       if (correct_type[i] == "factor") {
#'         if (correct[i] == "Sex" && sex_specific == TRUE) {
#'           join_data <- join_data |>
#'             dplyr::select(-Sex)
#'           next
#'         } else {
#'           cofactor = paste("as.factor(", correct[i], ")")
#'         }
#'       } else {
#'         cofactor = correct[i]
#'       }
#'       formula <- paste(formula, "+", cofactor)
#'     }
#'   }
#'
#'   if (c("Sex") %in% correct && sex_specific == TRUE) {
#'     correct <- correct[!correct == 'Sex']
#'   }
#'
#'   design <- stats::model.matrix(stats::as.formula(formula), data = join_data)
#'   cols <- c("control", "case", correct)
#'   cols <- cols[!is.null(cols)]
#'   colnames(design) <- paste(cols)
#'   contrast <- limma::makeContrasts(Diff = case - control, levels = design)
#'
#'   # Fit linear model to each protein assay
#'   data_fit <- join_data |>
#'     dplyr::select(-dplyr::any_of(c(variable, "Sex", correct))) |>
#'     tibble::column_to_rownames("DAid") |>
#'     t()
#'
#'   fit <- limma::lmFit(data_fit, design = design, method = "robust", maxit = 10000)
#'   contrast_fit <- limma::contrasts.fit(fit, contrast)
#'
#'   # Apply empirical Bayes smoothing to the SE
#'   ebays_fit <- limma::eBayes(contrast_fit)
#'
#'   # Extract DE results
#'   de_results <- limma::topTable(ebays_fit,
#'                                 n = nrow(ebays_fit$p.value),
#'                                 adjust.method = "fdr",
#'                                 confint = TRUE)
#'
#'   de_res <- de_results |>
#'     tibble::as_tibble(rownames = "Assay") |>
#'     dplyr::mutate(!!Variable := case) |>
#'     dplyr::mutate(sig = dplyr::case_when(
#'       adj.P.Val < pval_lim & logFC < -logfc_lim ~ "significant down",
#'       adj.P.Val < pval_lim & logFC > logfc_lim ~ "significant up",
#'       T ~ "not significant")
#'     ) |>
#'     dplyr::arrange(adj.P.Val)
#'
#'   return(de_res)
#' }
#'
#'
#' #' Differential expression analysis with limma for continuous variable
#' #'
#' #' This function performs differential expression analysis using limma for a continuous variable.
#' #' The output dataframe includes the logFC, the p-values, as well as the adjusted p-values with FDR.
#' #' The function removes the NAs from the columns that are used to correct for.
#' #'
#' #' @param join_data A tibble with the Olink data in wide format joined with metadata.
#' #' @param variable The variable of interest.
#' #' @param correct The variables to correct the results with. Default is c("Sex").
#' #' @param correct_type The type of the variables to correct the results with. Default is c("factor").
#' #' @param pval_lim The p-value limit for significance. Default is 0.05.
#' #' @param logfc_lim The logFC limit for significance. Default is 0.
#' #'
#' #' @return A tibble with the differential expression results.
#' #' @keywords internal
#' do_limma_continuous_de <- function(join_data,
#'                                    variable,
#'                                    correct = c("Sex"),
#'                                    correct_type = c("factor"),
#'                                    pval_lim = 0.05,
#'                                    logfc_lim = 0) {
#'   nrows_before <- nrow(join_data)
#'
#'   join_data <- join_data |>
#'     dplyr::filter(!dplyr::if_any(dplyr::all_of(c(variable, correct)), is.na))  # Remove NAs from columns in formula
#'
#'   nrows_after <- nrow(join_data)
#'   if (nrows_before != nrows_after){
#'     warning(paste0(nrows_before - nrows_after,
#'                    " rows were removed because they contain NAs in ",
#'                    variable,
#'                    " or ",
#'                    paste(correct, collapse = ", "),
#'                    "!"))
#'   }
#'
#'   # Design a model
#'   formula <- paste("~0 +" , variable)
#'
#'   if (!is.null(correct)) {
#'     for (i in 1:length(correct)) {
#'       if (correct_type[i] == "factor") {
#'         cofactor = paste("as.factor(", correct[i], ")")
#'       } else {
#'         cofactor = correct[i]
#'       }
#'       formula <- paste(formula, "+", cofactor)
#'     }
#'   }
#'   design <- stats::model.matrix(stats::as.formula(formula), data = join_data)
#'
#'   # Fit linear model to each protein assay
#'   data_fit <- join_data |>
#'     dplyr::select(-dplyr::any_of(c(variable, correct))) |>
#'     tibble::column_to_rownames("DAid") |>
#'     t()
#'
#'   fit <- limma::lmFit(data_fit, design = design, method = "robust", maxit = 10000)
#'
#'
#'   # Apply empirical Bayes smoothing to the SE
#'   ebays_fit <- limma::eBayes(fit)
#'
#'   # Extract DE results
#'   de_results <- limma::topTable(ebays_fit,
#'                                 n = nrow(ebays_fit$p.value),
#'                                 adjust.method = "fdr",
#'                                 confint = TRUE)
#'
#'   de_res <- de_results |>
#'     tibble::as_tibble(rownames = "Assay") |>
#'     dplyr::rename(logFC = colnames(de_results)[1]) |>
#'     dplyr::mutate(sig = dplyr::case_when(
#'       adj.P.Val < pval_lim & logFC < -logfc_lim ~ "significant down",
#'       adj.P.Val < pval_lim & logFC > logfc_lim ~ "significant up",
#'       T ~ "not significant")
#'     ) |>
#'     dplyr::arrange(adj.P.Val)
#'
#'   return(de_res)
#' }
#'
#'
#' #' Differential expression analysis with t-test
#' #'
#' #' `do_ttest_de()` performs differential expression analysis using t-test.
#' #' It separates the data in case-control groups, checks for data normality and
#' #' perform a t-test or Wilcoxon test respectively. It also performs p value FDR adjustment.
#' #'
#' #' @param long_data A tibble with the Olink data in long format and the Sex column from metadata.
#' #' @param variable The variable of interest that includes the case and control groups.
#' #' @param case The case group.
#' #' @param control The control groups.
#' #' @param assays The assays to run the differential expression analysis on.
#' #' @param only_female The female specific diseases. Default is NULL.
#' #' @param only_male The male specific diseases. Default is NULL.
#' #' @param pval_lim The p-value limit of significance. Default is 0.05.
#' #' @param logfc_lim The logFC limit of significance. Default is 0.
#' #'
#' #' @return A tibble with the differential expression results.
#' #' @keywords internal
#' do_ttest_de <- function(long_data,
#'                         variable = "Disease",
#'                         case,
#'                         control,
#'                         assays,
#'                         normality_res,
#'                         only_female = NULL,
#'                         only_male = NULL,
#'                         pval_lim = 0.05,
#'                         logfc_lim = 0) {
#'
#'   Variable <- rlang::sym(variable)
#'   de_res <- matrix(nrow=0, ncol=4)
#'   colnames(de_res) <- c("Assay", "P.Value", "logFC", variable)
#'
#'   # Filter for Sex if case is Sex specific
#'   if(!is.null(only_female) & case %in% only_female) {
#'     long_data <- long_data |>
#'       dplyr::filter(Sex == "F")
#'   } else if(!is.null(only_male) & case %in% only_male) {
#'     long_data <- long_data |>
#'       dplyr::filter(Sex == "M")
#'   } else {
#'     long_data <- long_data
#'   }
#'
#'   # Run statistical test for each assay
#'   i <- 0
#'   de_res_list <- lapply(assays, function(assay) {
#'     i <<- i + 1
#'     case_group <- long_data |>
#'       dplyr::filter(!!Variable == case, Assay == assay) |>
#'       dplyr::pull(NPX)
#'
#'     control_group <- long_data |>
#'       dplyr::filter(!!Variable %in% control, Assay == assay) |>
#'       dplyr::pull(NPX)
#'
#'     if (normality_res[i] == T) {
#'       test_res <- stats::t.test(case_group, control_group)
#'     } else {
#'       test_res <- stats::wilcox.test(case_group, control_group)
#'     }
#'
#'     p.val <- test_res$p.value
#'     difference <- mean(case_group, na.rm = T) - mean(control_group, na.rm = T)
#'
#'     de_res <- rbind(de_res, c(assay, p.val, difference, case))
#'
#'   })
#'
#'   combined_de_res <- do.call(rbind, de_res_list)
#'   combined_de_res <- as.data.frame(combined_de_res) |>
#'     dplyr::mutate(P.Value = as.numeric(P.Value),
#'                   logFC = as.numeric(logFC))
#'   combined_de_res$adj.P.Val <- stats::p.adjust(combined_de_res$P.Value, method = "fdr")
#'   de_res <- combined_de_res |>
#'     dplyr::mutate(sig = dplyr::case_when(
#'       adj.P.Val < pval_lim & logFC < -logfc_lim ~ "significant down",
#'       adj.P.Val < pval_lim & logFC > logfc_lim ~ "significant up",
#'       TRUE ~ "not significant"
#'     )) |>
#'     dplyr::arrange(adj.P.Val)
#'
#'   de_res <- tibble::as_tibble(de_res)
#'
#'   return(de_res)
#' }
#'
#'
#' #' Create volcano plots
#' #'
#' #' `plot_volcano()` creates volcano plots for the differential expression results.
#' #' It colors and labels the top up and down regulated proteins.
#' #'
#' #' @param de_result The differential expression results.
#' #' @param pval_lim The p-value limit for significance. Default is 0.05.
#' #' @param logfc_lim The logFC limit for significance. Default is 0.
#' #' @param top_up_prot The number of top up regulated proteins to label on the plot. Default is 40.
#' #' @param top_down_prot The number of top down regulated proteins to label on the plot. Default is 10.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is "diff_exp".
#' #' @param title The title of the plot or NULL for no title.
#' #' @param report_nproteins If the number of significant proteins should be reported in the subtitle. Default is TRUE.
#' #' @param subtitle The subtitle of the plot or NULL for no subtitle.
#' #'
#' #' @return A ggplot object with the volcano plot.
#' #' @keywords internal
#' plot_volcano <- function(de_result,
#'                          pval_lim = 0.05,
#'                          logfc_lim = 0,
#'                          top_up_prot = 40,
#'                          top_down_prot = 10,
#'                          palette = "diff_exp",
#'                          title = NULL,
#'                          report_nproteins = TRUE,
#'                          user_defined_proteins = NULL,
#'                          subtitle = NULL) {
#'
#'   top.sig.down <- de_result |>
#'     dplyr::filter(adj.P.Val < pval_lim & logFC < -logfc_lim) |>
#'     dplyr::arrange(adj.P.Val) |>
#'     dplyr::pull(Assay)
#'
#'   top.sig.up <- de_result |>
#'     dplyr::filter(adj.P.Val < pval_lim & logFC > logfc_lim) |>
#'     dplyr::arrange(adj.P.Val) |>
#'     dplyr::pull(Assay)
#'
#'   if (is.null(user_defined_proteins)) {
#'     top.sig.prot <- c(top.sig.up[1:top_up_prot], top.sig.down[1:top_down_prot])
#'   } else {
#'     top.sig.prot <- user_defined_proteins
#'   }
#'
#'   tab <- de_result |>
#'     dplyr::mutate(sig.label = ifelse(Assay %in% top.sig.prot, "top significance", 0))
#'
#'   num.sig.up <- length(top.sig.up)
#'   num.sig.down <- length(top.sig.down)
#'
#'   p <- de_result |>
#'     ggplot2::ggplot(ggplot2::aes(x = logFC,
#'                                  y = -log10(adj.P.Val),
#'                                  color = sig,
#'                                  label = Assay)) +
#'     ggplot2::geom_point(size = 1, alpha = 0.4)+
#'     ggrepel::geom_text_repel(data = subset(tab, sig.label == "top significance"), show.legend = FALSE) +
#'     ggplot2::geom_hline(yintercept = -log10(pval_lim), linetype = 'dashed') +
#'     ggplot2::geom_vline(xintercept = logfc_lim, linetype = 'dashed') +
#'     ggplot2::geom_vline(xintercept = -logfc_lim, linetype = 'dashed') +
#'     ggplot2::labs(color = "Significance")
#'
#'   # Title and subtitle
#'   if (!is.null(title)) {
#'     p <- p + ggplot2::ggtitle(label = paste0(title, ""))
#'   }
#'
#'   subtitle_text <- ""
#'   if (!is.null(subtitle)) {
#'     subtitle_text = subtitle
#'   }
#'
#'   if (isTRUE(report_nproteins)) {
#'     if (subtitle_text != "") {
#'       subtitle_text = paste0(subtitle_text,
#'                              "\nNum significant up = ", num.sig.up,
#'                              "\nNum significant down = ", num.sig.down)
#'     } else {
#'       subtitle_text = paste0("Num significant up = ", num.sig.up,
#'                              "\nNum significant down = ", num.sig.down)
#'     }
#'   }
#'
#'   if (subtitle_text != "") {
#'     p <- p + ggplot2::ggtitle(label = paste0(title, ""), subtitle = subtitle_text)
#'   }
#'
#'   # Set palette
#'   if (is.null(names(palette))) {
#'     p <- p + scale_color_hpa(palette)
#'   } else {
#'     p <- p + ggplot2::scale_color_manual(values = palette)
#'   }
#'
#'   return(p + theme_hpa() + ggplot2::theme(legend.position = "none"))
#' }
#'
#'
#' #' Run differential expression analysis with limma
#' #'
#' #' `do_limma()` performs differential expression analysis using limma package.
#' #' It can correct the results for metadata columns like Sex, Age, or BMI.
#' #' The output tibble includes the logFC, p-values, as well as the FDR adjusted p-values.
#' #' The function removes the NAs from the columns that are used to correct for.
#' #' It can generate and save volcano plots.
#' #'
#' #' @param olink_data A tibble with the Olink data in wide format.
#' #' @param metadata A tibble with the metadata.
#' #' @param variable The variable of interest that includes the case and control groups.
#' #' @param case The case group.
#' #' @param control The control groups.
#' #' @param correct The variables to correct the results with. Default c("Sex", "Age").
#' #' @param correct_type The type of the variables to correct the results with. Default c("factor", "numeric", "numeric").
#' #' @param wide If the data is in wide format. Default is TRUE.
#' #' @param only_female The female specific diseases. Default is NULL.
#' #' @param only_male The male specific diseases. Default is NULL.
#' #' @param volcano Generate volcano plots. Default is TRUE.
#' #' @param pval_lim The p-value limit of significance. Default is 0.05.
#' #' @param logfc_lim The logFC limit of significance. Default is 0.
#' #' @param top_up_prot The number of top up regulated proteins to label on the plot. Default is 40.
#' #' @param top_down_prot The number of top down regulated proteins to label on the plot. Default is 10.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is "diff_exp".
#' #' @param report_nproteins If the number of significant proteins should be reported in the subtitle. Default is TRUE.
#' #' @param user_defined_proteins A list with the user defined proteins to label on the plot. Default is NULL.
#' #' @param subtitle The subtitle of the plot or NULL for no subtitle.
#' #' @param save Save the volcano plots. Default is FALSE.
#' #'
#' #' @return A list with the differential expression results and volcano plots.
#' #'   - de_results: A list with the differential expression results.
#' #'   - volcano_plots: A list with the volcano plots.
#' #' @export
#' #'
#' #' @details For sex-specific diseases, there will be no correction for Sex.
#' #' This is performed automatically by the function. It will also filter out
#' #' rows with NA values in any of the columns that are used for correction,
#' #' either the `variable` or in `correct`. The `user_defined_proteins` overrides
#' #' the `top_up_prot` and `top_down_prot` arguments.
#' #'
#' #' @examples
#' #' de_results <- do_limma(example_data,
#' #'                        example_metadata,
#' #'                        case = "AML",
#' #'                        control = c("CLL", "MYEL"),
#' #'                        wide = FALSE)
#' #'
#' #' # Results for AML
#' #' de_results$de_results
#' #'
#' #' # Volcano plot for AML
#' #' de_results$volcano_plot
#' do_limma <- function(olink_data,
#'                      metadata,
#'                      variable = "Disease",
#'                      case,
#'                      control,
#'                      correct = c("Sex", "Age"),
#'                      correct_type = c("factor", "numeric"),
#'                      wide = TRUE,
#'                      only_female = NULL,
#'                      only_male = NULL,
#'                      volcano = TRUE,
#'                      pval_lim = 0.05,
#'                      logfc_lim = 0,
#'                      top_up_prot = 40,
#'                      top_down_prot = 10,
#'                      palette = "diff_exp",
#'                      report_nproteins = TRUE,
#'                      user_defined_proteins = NULL,
#'                      subtitle = NULL,
#'                      save = FALSE) {
#'
#'   message(paste0("Comparing ", case, " with ", paste(control, collapse = ", "), "."))
#'   Variable <- rlang::sym(variable)
#'   # Prepare Olink data and merge them with metadata
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'
#'     nrows_before <- nrow(wide_data)
#'     join_data <- wide_data |>
#'       dplyr::left_join(
#'         metadata |> dplyr::select(dplyr::any_of(c("DAid", variable, "Sex", correct))),
#'         by = "DAid") |>
#'       dplyr::filter(!is.na(!!Variable))
#'
#'     nrows_after <- nrow(join_data)
#'     if (nrows_before != nrows_after){
#'       warning(paste0(nrows_before - nrows_after,
#'                      " rows were removed because data did not match metadata and NAs were created in ",
#'                      variable,
#'                      "!"))
#'     }
#'   } else {
#'     nrows_before <- nrow(olink_data)
#'     join_data <- olink_data |>
#'       dplyr::left_join(metadata |>
#'                          dplyr::select(dplyr::any_of(c("DAid", variable, "Sex", correct))),
#'                        by = "DAid") |>
#'       dplyr::filter(!is.na(!!Variable))
#'
#'     nrows_after <- nrow(join_data)
#'     if (nrows_before != nrows_after){
#'       warning(paste0(nrows_before - nrows_after,
#'                      " rows were removed because data did not match metadata and NAs were created in ",
#'                      variable,
#'                      "!"))
#'     }
#'   }
#'
#'   # Run differential expression analysis
#'   de_results <- do_limma_de(join_data,
#'                             variable,
#'                             case,
#'                             control,
#'                             correct,
#'                             correct_type,
#'                             only_female,
#'                             only_male,
#'                             pval_lim,
#'                             logfc_lim)
#'
#'   # Generate (and save) volcano plots
#'   if (volcano) {
#'
#'     volcano_plot <- plot_volcano(de_results,
#'                                  pval_lim,
#'                                  logfc_lim,
#'                                  top_up_prot,
#'                                  top_down_prot,
#'                                  palette,
#'                                  case,
#'                                  report_nproteins,
#'                                  user_defined_proteins,
#'                                  subtitle)
#'
#'     if (isTRUE(save)) {
#'       dir_name <- create_dir("results/volcano_plots", date = T)
#'       ggplot2::ggsave(volcano_plot,
#'                       filename = paste0(dir_name, "/", case, "_volcano.png"),
#'                       width = 10,
#'                       height = 8)
#'     }
#'     return(list("de_results" = de_results, "volcano_plot" = volcano_plot))
#'   }
#'   return(de_results)
#' }
#'
#'
#' #' Run differential expression analysis with limma for continuous variable
#' #'
#' #' This function runs differential expression analysis using limma for a continuous variable.
#' #' It can generate and save volcano plots.
#' #'
#' #' @param olink_data A tibble with the Olink data in wide format.
#' #' @param metadata A tibble with the metadata.
#' #' @param variable The variable of interest.
#' #' @param correct The variables to correct the results with. Default is c("Sex").
#' #' @param correct_type The type of the variables to correct the results with. Default is c("factor").
#' #' @param wide If the data is in wide format. Default is TRUE.
#' #' @param volcano Generate volcano plots. Default is TRUE.
#' #' @param pval_lim The p-value limit for significance. Default is 0.05.
#' #' @param logfc_lim The logFC limit for significance. Default is 0.
#' #' @param top_up_prot The number of top up regulated proteins to label on the plot. Default is 40.
#' #' @param top_down_prot The number of top down regulated proteins to label on the plot. Default is 10.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is "diff_exp".
#' #' @param report_nproteins If the number of significant proteins should be reported in the subtitle. Default is TRUE.
#' #' @param user_defined_proteins A list with the user defined proteins to label on the plot. Default is NULL.
#' #' @param subtitle The subtitle of the plot or NULL for no subtitle.
#' #' @param save Save the volcano plots. Default is FALSE.
#' #'
#' #' @return A list with the differential expression results and volcano plots.
#' #'   - de_results: A list with the differential expression results.
#' #'   - volcano_plot: A list with the volcano plots.
#' #' @export
#' #'
#' #' @details
#' #' It will filter out rows with NA values in any of the columns that are used for
#' #' correction, either the `variable` or in `correct`. The `user_defined_proteins` overrides
#' #' the `top_up_prot` and `top_down_prot` arguments.
#' #'
#' #' @examples
#' #' do_limma_continuous(example_data, example_metadata, "Age", wide = FALSE)
#' do_limma_continuous <- function(olink_data,
#'                                 metadata,
#'                                 variable,
#'                                 correct = c("Sex"),
#'                                 correct_type = c("factor"),
#'                                 wide = TRUE,
#'                                 volcano = TRUE,
#'                                 pval_lim = 0.05,
#'                                 logfc_lim = 0,
#'                                 top_up_prot = 40,
#'                                 top_down_prot = 10,
#'                                 palette = "diff_exp",
#'                                 report_nproteins = TRUE,
#'                                 user_defined_proteins = NULL,
#'                                 subtitle = NULL,
#'                                 save = FALSE) {
#'
#'   Variable <- rlang::sym(variable)
#'
#'   # Prepare Olink data and merge them with metadata
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'
#'     nrows_before <- nrow(wide_data)
#'     join_data <- wide_data |>
#'       dplyr::left_join(
#'         metadata |> dplyr::select(dplyr::any_of(c("DAid", variable, correct))),
#'         by = "DAid") |>
#'       dplyr::filter(!is.na(!!Variable))
#'
#'     nrows_after <- nrow(join_data)
#'     if (nrows_before != nrows_after){
#'       warning(paste0(nrows_before - nrows_after,
#'                      " rows were removed because data did not match metadata and NAs were created in ",
#'                      variable,
#'                      "!"))
#'     }
#'   } else {
#'     nrows_before <- nrow(olink_data)
#'     join_data <- olink_data |>
#'       dplyr::left_join(metadata |>
#'                          dplyr::select(dplyr::any_of(c("DAid", variable, correct))),
#'                        by = "DAid") |>
#'       dplyr::filter(!is.na(!!Variable))
#'
#'     nrows_after <- nrow(join_data)
#'     if (nrows_before != nrows_after){
#'       warning(paste0(nrows_before - nrows_after,
#'                      " rows were removed because data did not match metadata and NAs were created in ",
#'                      variable,
#'                      "!"))
#'     }
#'   }
#'
#'   # Run differential expression analysis
#'   de_results <- do_limma_continuous_de(join_data,
#'                                        variable,
#'                                        correct,
#'                                        correct_type,
#'                                        pval_lim,
#'                                        logfc_lim)
#'
#'   # Generate (and save) volcano plots
#'   if (volcano) {
#'     volcano_plot <- plot_volcano(de_results,
#'                                  pval_lim,
#'                                  logfc_lim,
#'                                  top_up_prot,
#'                                  top_down_prot,
#'                                  palette,
#'                                  variable,
#'                                  report_nproteins,
#'                                  user_defined_proteins,
#'                                  subtitle)
#'
#'     if (isTRUE(save)) {
#'       dir_name <- create_dir("results/volcano_plot", date = T)
#'       for (i in 1:length(levels)) {
#'         ggplot2::ggsave(volcano_plot[[i]], filename = paste0(dir_name, "/", levels[i], "_volcano.png"), width = 10, height = 8)
#'       }
#'     }
#'     return(list("de_results" = de_results, "volcano_plot" = volcano_plot))
#'   }
#'   return(de_results)
#' }
#'
#'
#' #' Run differential expression analysis with t-test
#' #'
#' #' `do_ttest()` performs differential expression analysis using t-test.
#' #' It separates the data in case-control groups, checks for data normality and
#' #' perform a t-test or Wilcoxon test respectively. It also performs p value FDR adjustment.
#' #' It can generate and save volcano plots.
#' #'
#' #' @param olink_data A tibble with the Olink data in wide format.
#' #' @param metadata A tibble with the metadata.
#' #' @param variable The variable of interest that includes the case and control groups.
#' #' @param case The case group.
#' #' @param control The control groups.
#' #' @param wide If the data is in wide format. Default is TRUE.
#' #' @param only_female The female specific diseases. Default is NULL.
#' #' @param only_male The male specific diseases. Default is NULL.
#' #' @param volcano Generate volcano plots. Default is TRUE.
#' #' @param pval_lim The p-value limit of significance. Default is 0.05.
#' #' @param logfc_lim The logFC limit of significance. Default is 0.
#' #' @param top_up_prot The number of top up regulated proteins to label on the plot. Default is 40.
#' #' @param top_down_prot The number of top down regulated proteins to label on the plot. Default is 10.
#' #' @param palette The color palette for the plot. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is "diff_exp".
#' #' @param report_nproteins If the number of significant proteins should be reported in the subtitle. Default is TRUE.
#' #' @param user_defined_proteins A list with the user defined proteins to label on the plot. Default is NULL.
#' #' @param subtitle The subtitle of the plot or NULL for no subtitle.
#' #' @param save Save the volcano plots. Default is FALSE.
#' #'
#' #' @return A list with the differential expression results and volcano plots.
#' #'   - de_results: A list with the differential expression results.
#' #'   - volcano_plots: A list with the volcano plots.
#' #' @export
#' #'
#' #' @details
#' #' It will filter out rows with NA values in any of the columns that are used for
#' #' correction, either the `variable` or in `correct`. The `user_defined_proteins`
#' #' overrides the `top_up_prot` and `top_down_prot` arguments.
#' #'
#' #' @examples
#' #' de_results <- do_ttest(example_data,
#' #'                        example_metadata,
#' #'                        case = "AML",
#' #'                        control = c("CLL", "MYEL"),
#' #'                        wide = FALSE)
#' #'
#' #' # Results for AML
#' #' de_results$de_results
#' #'
#' #' # Volcano plot for AML
#' #' de_results$volcano_plot
#' do_ttest <- function(olink_data,
#'                      metadata,
#'                      variable = "Disease",
#'                      case,
#'                      control,
#'                      wide = TRUE,
#'                      only_female = NULL,
#'                      only_male = NULL,
#'                      volcano = TRUE,
#'                      pval_lim = 0.05,
#'                      logfc_lim = 0,
#'                      top_up_prot = 40,
#'                      top_down_prot = 10,
#'                      palette = "diff_exp",
#'                      report_nproteins = TRUE,
#'                      user_defined_proteins = NULL,
#'                      subtitle = NULL,
#'                      save = FALSE) {
#'
#'   Variable <- rlang::sym(variable)
#'   # Prepare Olink data and merge them with metadata
#'   if (isFALSE(wide)) {
#'     wide_data <- widen_data(olink_data)
#'
#'     nrows_before <- nrow(wide_data)
#'     join_data <- wide_data |>
#'       dplyr::left_join(
#'         metadata |> dplyr::select(dplyr::any_of(c("DAid", variable, "Sex"))),
#'         by = "DAid") |>
#'       dplyr::filter(!is.na(!!Variable))
#'
#'     nrows_after <- nrow(join_data)
#'     if (nrows_before != nrows_after){
#'       warning(paste0(nrows_before - nrows_after,
#'                      " rows were removed because data did not match metadata and NAs were created in ",
#'                      variable,
#'                      "!"))
#'     }
#'   } else {
#'     nrows_before <- nrow(olink_data)
#'     join_data <- olink_data |>
#'       dplyr::left_join(metadata |>
#'                          dplyr::select(dplyr::any_of(c("DAid", variable, "Sex"))),
#'                        by = "DAid") |>
#'       dplyr::filter(!is.na(!!Variable))
#'
#'     nrows_after <- nrow(join_data)
#'     if (nrows_before != nrows_after){
#'       warning(paste0(nrows_before - nrows_after,
#'                      " rows were removed because data did not match metadata and NAs were created in ",
#'                      variable,
#'                      "!"))
#'     }
#'   }
#'
#'   # Run differential expression analysis
#'   long_data <- join_data |>
#'     dplyr::select(-dplyr::any_of(c("Age", "BMI"))) |>
#'     tidyr::pivot_longer(!dplyr::any_of(c(variable, "DAid", "Disease", "Sex")), names_to = "Assay", values_to = "NPX")
#'
#'   assays <- unique(long_data$Assay)
#'
#'   normality_res <- check_normality(
#'     join_data |>
#'       dplyr::select(-dplyr::any_of(c(variable, "DAid", "Disease", "Sex", "Age", "BMI")))
#'   ) |>
#'     dplyr::pull(is_normal)
#'
#'   de_results <- do_ttest_de(long_data,
#'                             variable,
#'                             case,
#'                             control,
#'                             assays,
#'                             normality_res,
#'                             only_female,
#'                             only_male,
#'                             pval_lim,
#'                             logfc_lim)
#'
#'   # Generate (and save) volcano plots
#'   if (volcano) {
#'     volcano_plot <- plot_volcano(de_results,
#'                                  pval_lim,
#'                                  logfc_lim,
#'                                  top_up_prot,
#'                                  top_down_prot,
#'                                  palette,
#'                                  case,
#'                                  report_nproteins,
#'                                  user_defined_proteins,
#'                                  subtitle)
#'
#'     if (isTRUE(save)) {
#'       dir_name <- create_dir("results/volcano_plots", date = T)
#'       ggplot2::ggsave(volcano_plot,
#'                       filename = paste0(dir_name, "/", case, "_volcano.png"),
#'                       width = 10,
#'                       height = 8)
#'     }
#'     return(list("de_results" = de_results, "volcano_plot" = volcano_plot))
#'   }
#'   return(de_results)
#' }
#'
#'
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
#' #' @param disease_palette The color palette for the disease. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is NULL.
#' #' @param diff_exp_palette The color palette for the differential expression. If it is a character, it should be one of the palettes from `get_hpa_palettes()`. Default is "diff_exp".
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
