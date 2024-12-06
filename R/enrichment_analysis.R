#' Convert gene names to ENTREZID
#'
#' `gene_to_entrezid()` converts gene names to ENTREZID using the `org.Hs.eg.db` database.
#'
#' @param gene_list A character vector containing the gene names.
#' @param background A character vector containing the background genes if any.
#'
#' @return A list containing the gene list and the background with ENTREZID.
#' @keywords internal
gene_to_entrezid <- function(gene_list, background = NULL){
  # From gene name to ENTREZID
  gene_conversion <- clusterProfiler::bitr(gene_list,
                                           fromType = "SYMBOL",
                                           toType = "ENTREZID",
                                           OrgDb = org.Hs.eg.db::org.Hs.eg.db)

  gene_list <- gene_conversion |> dplyr::pull(!!rlang::sym("ENTREZID")) |> unique()

  if (!is.null(background)) {
    background <- clusterProfiler::bitr(background,
                                        fromType = "SYMBOL",
                                        toType = "ENTREZID",
                                        OrgDb = org.Hs.eg.db::org.Hs.eg.db)

    background <- background |> dplyr::pull(!!rlang::sym("ENTREZID")) |> unique()
  }

  return(list("gene_list" = gene_list, "background" = background))
}

#' Run over-representation analysis
#'
#' `hd_run_ora()` performs over-representation analysis (ORA) using the clusterProfiler package.
#'
#' @param gene_list A character vector containing the gene names. These can be differentially expressed proteins or selected protein features from classification models.
#' @param database The database to perform the ORA. It can be either "GO", "KEGG", or "Reactome".
#' @param ontology The ontology to use when database = "GO". It can be "BP" (Biological Process), "CC" (Cellular Component), or "MF" (Molecular Function). In the case of KEGG and Reactome, this parameter is ignored.
#' @param background A character vector containing the background genes.
#' @param pval_lim The p-value threshold to consider a term as significant.
#'
#' @return A list containing the results of the ORA.
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' de_results <- hd_run_de_limma(hd_object, case = "AML")
#'
#' # Extract the up-regulated proteins for AML
#' sig_up_proteins_aml <- de_results$de_res |>
#'   dplyr::filter(adj.P.Val < 0.05 & logFC > 0.5) |>
#'   dplyr::pull(Feature)
#'
#' # Perform ORA with `GO` database and `BP` ontology
#' hd_run_ora(sig_up_proteins_aml, database = "GO", ontology = "BP")
hd_run_ora <- function(gene_list,
                       database = c("GO", "Reactome", "KEGG"),
                       ontology = c("BP", "CC", "MF"),
                       background = NULL,
                       pval_lim = 0.05) {
  database <- match.arg(database)

  # if (is.null(background)) {
  #   message("No background provided. When working with proteomics data it is recommended to use background.")
  # }

  # Ensure 'clusterProfiler' package is loaded
  if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
    stop("The 'clusterProfiler' package is required but not installed. Please install it using BiocManager::install('clusterProfiler').")
  }

  conversion <- gene_to_entrezid(gene_list, background)
  gene_list <- conversion[["gene_list"]]
  background <- conversion[["background"]]

  if (database == "KEGG") {

    # Perform KEGG enrichment analysis - Under development
    enrichment <- clusterProfiler::enrichKEGG(gene = gene_list,
                                              organism = "hsa",
                                              keyType = "ncbi-geneid",
                                              pvalueCutoff = pval_lim,
                                              universe = background)

  } else if (database == "GO") {

    # Ensure 'org.Hs.eg.db' package is loaded
    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
      stop("The 'org.Hs.eg.db' package is required but not installed. Please install it using BiocManager::install('org.Hs.eg.db').")
    }

    # Perform GO enrichment analysis
    enrichment <- clusterProfiler::enrichGO(gene = gene_list,
                                            OrgDb = org.Hs.eg.db::org.Hs.eg.db,
                                            ont = ontology,
                                            pvalueCutoff = pval_lim,
                                            universe = background)

  } else if (database == "Reactome") {

    # Ensure 'ReactomePA' package is loaded
    if (!requireNamespace("ReactomePA", quietly = TRUE)) {
      stop("The 'ReactomePA' package is required but not installed. Please install it using BiocManager::install('ReactomePA').")
    }

    # Perform Reactome enrichment analysis
    enrichment <- ReactomePA::enrichPathway(gene = gene_list,
                                            organism = "human",
                                            pvalueCutoff = pval_lim,
                                            universe = background)


  }

  if (!any(enrichment@result$p.adjust < pval_lim)) {
    message("No significant terms found.")
    return(NULL)
  }

  enrichment <- list("gene_list" = gene_list, "background" = background, "enrichment" = enrichment)
  class(enrichment) <- "hd_enrichment"

  if (is.null(background)) {
    enrichment[["background"]] <- NULL
  }
  return(enrichment)
}


#' Generate visualizations for the over-representation analysis
#'
#' `hd_plot_ora()` generates useful visualizations for the results of the
#' over-representation analysis.
#'
#' @param enrichment The enrichment results obtained from `hd_run_ora()`.
#'
#' @return The input object enriched with the plots.
#'
#' @details
#' When KEGG database is used, a cnetplot is generated with ENTREZIDs instead of gene names.
#' For GO and Reactome databases the ENTREZIDs are converted to gene names.
#'
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' de_results <- hd_run_de_limma(hd_object, case = "AML")
#'
#' # Extract the up-regulated proteins for AML
#' sig_up_proteins_aml <- de_results$de_res |>
#'   dplyr::filter(adj.P.Val < 0.05 & logFC > 0.5) |>
#'   dplyr::pull(Feature)
#'
#' # Perform ORA with `GO` database and `BP` ontology
#' enrichment <- hd_run_ora(sig_up_proteins_aml, database = "GO", ontology = "BP")
#'
#' # Plot the results
#' hd_plot_ora(enrichment)
hd_plot_ora <- function(enrichment) {

  # Visualize results
  dot_plot <- clusterProfiler::dotplot(enrichment[["enrichment"]])

  # Ensure 'enrichplot' package is loaded
  if (!requireNamespace("enrichplot", quietly = TRUE)) {
    stop("The 'enrichplot' package is required but not installed. Please install it using install.packages('enrichplot').")
  }
  tree_plot_data <- enrichplot::pairwise_termsim(enrichment[["enrichment"]])
  tree_plot <- enrichplot::treeplot(tree_plot_data)

  if (enrichment[["enrichment"]]@ontology == "KEGG") {
    cnet_plot <- clusterProfiler::cnetplot(enrichment[["enrichment"]], categorySize = "pvalue")
  } else {
    enrichment_transformed <- clusterProfiler::setReadable(enrichment[["enrichment"]], OrgDb = org.Hs.eg.db::org.Hs.eg.db)
    cnet_plot <- clusterProfiler::cnetplot(enrichment_transformed, categorySize = "pvalue")
  }

  enrichment[["dotplot"]] <- dot_plot
  enrichment[["tree_plot"]] <- tree_plot
  enrichment[["cnetplot"]] <- cnet_plot

  return(enrichment)
}


#' utils::globalVariables(c("ENTREZID"))
#' #' Perform over-representation analysis
#' #'
#' #' `do_ora()` performs over-representation analysis (ORA) using the clusterProfiler package.
#' #'
#' #' @param protein_list A character vector containing the protein names.
#' #' @param database The database to perform the ORA. It can be either "GO" or "Reactome".
#' #' @param background A character vector containing the background genes.
#' #' @param pval_lim The p-value threshold to consider a term as significant.
#' #'
#' #' @return A list containing the results of the ORA.
#' #' @export
#' #'
#' #' @details The ontology option used when database = "GO" is "BP" (Biological Process).
#' #' When Olink data is used, it is recommended to provide a protein list as background.
#' #'
#' #' @examples
#' #' # Perform Differential Expression Analysis
#' #' control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC")
#' #' de_res <- do_limma(example_data,
#' #'                    example_metadata,
#' #'                    case = "AML",
#' #'                    control = control,
#' #'                    wide = FALSE)
#' #'
#' #' # Extract the up-regulated proteins for AML
#' #' sig_up_proteins_aml <- de_res$de_results |>
#' #'   dplyr::filter(sig == "significant up") |>
#' #'   dplyr::pull(Assay)
#' #'
#' #' # Perform ORA with GO database
#' #' do_ora(sig_up_proteins_aml, database = "GO")
#' do_ora <- function(protein_list,
#'                    database = c("GO", "Reactome"),
#'                    background = NULL,
#'                    pval_lim = 0.05) {
#'   database <- match.arg(database)
#'
#'   if (is.null(background)) {
#'     message("No background provided. When working with Olink data it is recommended to use background.")
#'   }
#'
#'   # From gene name to ENTREZID
#'   protein_conversion <- clusterProfiler::bitr(protein_list,
#'                                            fromType = "SYMBOL",
#'                                            toType = "ENTREZID",
#'                                            OrgDb = org.Hs.eg.db::org.Hs.eg.db)
#'
#'   protein_list <- protein_conversion |> dplyr::pull(ENTREZID) |> unique()
#'
#'   if (!is.null(background)) {
#'     background <- clusterProfiler::bitr(background,
#'                                         fromType = "SYMBOL",
#'                                         toType = "ENTREZID",
#'                                         OrgDb = org.Hs.eg.db::org.Hs.eg.db)
#'
#'     background <- background |> dplyr::pull(ENTREZID) |> unique()
#'   }
#'
#'   if (database == "KEGG") {
#'     # Perform KEGG enrichment analysis - Under development
#'     # enrichment <- clusterProfiler::enrichKEGG(gene = protein_list,
#'     #                                           organism = "hsa",
#'     #                                           pvalueCutoff = pval_lim,
#'     #                                           universe = background)
#'   } else if (database == "GO") {
#'     # Perform GO enrichment analysis
#'     enrichment <- clusterProfiler::enrichGO(gene = protein_list,
#'                                             OrgDb = org.Hs.eg.db::org.Hs.eg.db,
#'                                             ont = "BP",
#'                                             pvalueCutoff = pval_lim,
#'                                             universe = background)
#'   } else if (database == "Reactome") {
#'     # Perform Reactome enrichment analysis
#'     enrichment <- ReactomePA::enrichPathway(gene = protein_list,
#'                                             organism = "human",
#'                                             pvalueCutoff = pval_lim,
#'                                             universe = background)
#'   }
#'
#'   if (!any(enrichment@result$p.adjust < pval_lim)) {
#'     message("No significant terms found.")
#'     return(NULL)
#'   }
#'
#'   return(enrichment)
#' }
#'
#'
#' #' Plot the results of the over-representation analysis
#' #'
#' #' `plot_ora()` produces useful plots to visualize the results of the
#' #' over-representation analysis.
#' #'
#' #' @param enrichment The results of the over-representation analysis.
#' #' @param protein_list A character vector containing the protein names. It should be the same as the one used in `do_ora()`.
#' #' @param pval_lim The p-value threshold to consider a term as significant.
#' #' @param ncateg The number of categories to show in the plots.
#' #' @param fontsize The font size for the plots.
#' #'
#' #' @return A list containing the plots.
#' #' @export
#' #'
#' #' @examples
#' #' # Perform Differential Expression Analysis
#' #' control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC")
#' #' de_res <- do_limma(example_data,
#' #'                    example_metadata,
#' #'                    case = "AML",
#' #'                    control = control,
#' #'                    wide = FALSE)
#' #'
#' #' # Extract the up-regulated proteins for AML
#' #' sig_up_proteins_aml <- de_res$de_results |>
#' #'   dplyr::filter(sig == "significant up") |>
#' #'   dplyr::pull(Assay)
#' #'
#' #' # Perform ORA with GO database
#' #' enrichment <- do_ora(sig_up_proteins_aml, database = "GO")
#' #'
#' #' # Plot the results
#' #' plot_ora(enrichment, sig_up_proteins_aml, pval_lim = 0.05, ncateg = 5)
#' plot_ora <- function(enrichment,
#'                      protein_list,
#'                      pval_lim = 0.05,
#'                      ncateg = 10,
#'                      fontsize = 10) {
#'
#'   # From gene name to ENTREZID
#'   protein_conversion <- clusterProfiler::bitr(protein_list,
#'                                               fromType = "SYMBOL",
#'                                               toType = "ENTREZID",
#'                                               OrgDb = org.Hs.eg.db::org.Hs.eg.db)
#'
#'   protein_list <- protein_conversion |> dplyr::pull(ENTREZID) |> unique()
#'
#'   # Visualize results
#'   dotplot <- clusterProfiler::dotplot(enrichment,
#'                                       showCategory = ncateg,
#'                                       font.size = fontsize)
#'
#'   barplot <- barplot(enrichment,
#'                      drop = TRUE,
#'                      showCategory = ncateg,
#'                      font.size = fontsize)
#'
#'   enrichment <- clusterProfiler::setReadable(enrichment, OrgDb = org.Hs.eg.db::org.Hs.eg.db)
#'   cnetplot <- clusterProfiler::cnetplot(enrichment,
#'                                         showCategory = ncateg,
#'                                         categorySize = "pvalue",
#'                                         color.params = list(foldChange = protein_list),
#'                                         cex.params = list(category_label = (fontsize + 2)/12,
#'                                                           gene_label = (fontsize)/12))
#'
#'   return(list("dotplot" = dotplot,
#'               "barplot" = barplot,
#'               "cnetplot" = cnetplot))
#' }
#'
#'
#' #' Perform gene set enrichment analysis
#' #'
#' #' This function performs gene set enrichment analysis (GSEA) using the clusterProfiler package.
#' #'
#' #' @param de_results A tibble containing the results of a differential expression analysis.
#' #' @param database The database to perform the GSEA. It can be either "GO" or "Reactome".
#' #' @param pval_lim The p-value threshold to consider a term as significant.
#' #'
#' #' @return A list containing the results of the GSEA.
#' #' @export
#' #'
#' #' @details The ontology option used when database = "GO" is "ALL".
#' #'
#' #' @examples
#' #' # Run Differential Expression Analysis and extract results
#' #' control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC")
#' #' de_res <- do_limma(example_data,
#' #'                    example_metadata,
#' #'                    case = "AML",
#' #'                    control = control,
#' #'                    wide = FALSE)
#' #' de_results <- de_res$de_results
#' #'
#' #' # Run GSEA with Reactome database
#' #' do_gsea(de_results,
#' #'         database = "GO",
#' #'         pval_lim = 0.9)
#' #' # Remember that the data is artificial, this is why we use an absurdly high p-value cutoff
#' do_gsea <- function(de_results,
#'                     database = c("GO", "Reactome"),
#'                     pval_lim = 0.05) {
#'
#'   database <- match.arg(database)
#'
#'   # Prepare sorted_protein_list
#'   protein_list <- stats::setNames(de_results$logFC,
#'                                   de_results$Assay)
#'   sorted_protein_list <- sort(protein_list, decreasing = TRUE)
#'
#'   # From gene name to ENTREZID
#'   protein_conversion <- clusterProfiler::bitr(names(sorted_protein_list),
#'                                               fromType = "SYMBOL",
#'                                               toType = "ENTREZID",
#'                                               OrgDb = org.Hs.eg.db::org.Hs.eg.db)
#'
#'   protein_list <- stats::setNames(sorted_protein_list, protein_conversion$ENTREZID)
#'
#'   if (database == "KEGG") {
#'     # Perform GSEA for KEGG - Under development
#'     # enrichment <- clusterProfiler::gseKEGG(geneList = protein_list,
#'     #                                        organism = "hsa",
#'     #                                        pvalueCutoff = pval_lim,
#'     #                                        pAdjustMethod = "BH",
#'     #                                        minGSSize = 10,
#'     #                                        maxGSSize = 500)
#'   } else if (database == "GO") {
#'     # Perform GSEA for GO
#'     enrichment <- clusterProfiler::gseGO(geneList = protein_list,
#'                                          OrgDb = org.Hs.eg.db::org.Hs.eg.db,
#'                                          ont = "BP",
#'                                          pvalueCutoff = pval_lim,
#'                                          pAdjustMethod = "BH",
#'                                          minGSSize = 10,
#'                                          maxGSSize = 500)
#'   } else if (database == "Reactome") {
#'     # Perform GSEA for Reactome
#'     enrichment <- ReactomePA::gsePathway(protein_list,
#'                                          organism = "human",
#'                                          pvalueCutoff = pval_lim,
#'                                          pAdjustMethod = "BH",
#'                                          verbose = FALSE)
#'   }
#'
#'   if (!any(enrichment@result$p.adjust < pval_lim)) {
#'     message("No significant terms found.")
#'     return(NULL)
#'   }
#'
#'   return(enrichment)
#' }
#'
#'
#' #' Plot the results of the gene set enrichment analysis
#' #'
#' #' `plot_gsea()` produces useful plots to visualize the results of the
#' #' gene set enrichment analysis.
#' #'
#' #' @param enrichment The results of the gene set enrichment analysis.
#' #' @param de_results A tibble containing the results of a differential expression analysis. It should be the same as the one used in `do_gsea()`.
#' #' @param pval_lim The p-value threshold to consider a term as significant.
#' #' @param ncateg The number of categories to show in the plots.
#' #' @param fontsize The font size for the plots.
#' #'
#' #' @return A list containing the plots.
#' #' @export
#' #'
#' #' @examples
#' #' # Perform Differential Expression Analysis
#' #' control = c("BRC", "CLL", "CRC", "CVX", "ENDC", "GLIOM", "LUNGC", "LYMPH", "MYEL", "OVC", "PRC")
#' #' de_res <- do_limma(example_data,
#' #'                    example_metadata,
#' #'                    case = "AML",
#' #'                    control = control,
#' #'                    wide = FALSE)
#' #' de_results <- de_res$de_results
#' #'
#' #' # Run GSEA with Reactome database
#' #' enrichment <- do_gsea(de_results, database = "GO", pval_lim = 0.9)
#' #'
#' #' # Plot the results
#' #' plot_gsea(enrichment, de_results, pval_lim = 0.9, ncateg = 7, fontsize = 7)
#' #' # Remember that the data is artificial, this is why we use an absurdly high p-value cutoff
#' plot_gsea <- function(enrichment,
#'                       de_results,
#'                       pval_lim = 0.05,
#'                       ncateg = 10,
#'                       fontsize = 10) {
#'
#'   # Prepare sorted_protein_list
#'   protein_list <- stats::setNames(de_results$logFC,
#'                                   de_results$Assay)
#'   sorted_protein_list <- sort(protein_list, decreasing = TRUE)
#'
#'   # Visualize results
#'   dotplot <- clusterProfiler::dotplot(enrichment,
#'                                       showCategory = ncateg,
#'                                       font.size = fontsize,
#'                                       split=".sign") +
#'     ggplot2::facet_grid(.~.sign)
#'
#'   ridgeplot <- clusterProfiler::ridgeplot(enrichment, showCategory = ncateg) +
#'     ggplot2::labs(x = "enrichment distribution") +
#'     ggplot2::theme(axis.text.x = ggplot2::element_text(size = fontsize),
#'                    axis.text.y = ggplot2::element_text(size = fontsize),
#'                    text = ggplot2::element_text(size = fontsize))
#'
#'   gseaplot <- clusterProfiler::gseaplot(enrichment,
#'                                         by = "all",
#'                                         title = enrichment$Description[1],
#'                                         geneSetID = 1)
#'
#'   enrichment <- clusterProfiler::setReadable(enrichment, OrgDb = org.Hs.eg.db::org.Hs.eg.db)
#'   cnetplot <- clusterProfiler::cnetplot(enrichment,
#'                                         showCategory = ncateg,
#'                                         categorySize = "pvalue",
#'                                         color.params = list(foldChange = protein_list),
#'                                         cex.params = list(category_label = (fontsize + 2)/12,
#'                                                           gene_label = (fontsize)/12))
#'
#'   return(list("dotplot" = dotplot,
#'               "cnetplot" = cnetplot,
#'               "ridgeplot" = ridgeplot,
#'               "gseaplot" = gseaplot))
#' }
