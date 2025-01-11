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

#' Over-representation analysis
#'
#' `hd_ora()` performs over-representation analysis (ORA) using the clusterProfiler package.
#'
#' @param gene_list A character vector containing the gene names. These can be differentially expressed proteins or selected protein features from classification models.
#' @param database The database to perform the ORA. It can be either "GO", "KEGG", or "Reactome".
#' @param ontology The ontology to use when database = "GO". It can be "BP" (Biological Process), "CC" (Cellular Component), "MF" (Molecular Function), or "ALL". In the case of KEGG and Reactome, this parameter is ignored.
#' @param background A character vector containing the background genes.
#' @param pval_lim The p-value threshold to consider a term as significant in the enrichment analysis.
#'
#' @return A list containing the results of the ORA.
#'
#' @details
#' To perform the ORA, `clusterProfiler` package is used.
#' The `qvalueCutoff` is set to 1 by default to prioritize filtering by adjusted
#' p-values (p.adjust). This simplifies the workflow by ensuring a single, clear
#' significance threshold based on the false discovery rate (FDR). While q-values
#' are not used for filtering by default, they are still calculated and included
#' in the results for users who wish to apply additional criteria.
#' For more information, please refer to the `clusterProfiler` documentation.
#'
#' If you want to learn more about ORA, please refer to the following publications:
#' - Chicco D, Agapito G. Nine quick tips for pathway enrichment analysis. PLoS Comput Biol. 2022 Aug 11;18(8):e1010348. doi: 10.1371/journal.pcbi.1010348. PMID: 35951505; PMCID: PMC9371296. https://pmc.ncbi.nlm.nih.gov/articles/PMC9371296/
#' - https://yulab-smu.top/biomedical-knowledge-mining-book/enrichment-overview.html#gsea-algorithm
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' de_results <- hd_de_limma(hd_object, case = "AML")
#'
#' # Extract the up-regulated proteins for AML
#' sig_up_proteins_aml <- de_results$de_res |>
#'   dplyr::filter(adj.P.Val < 0.05 & logFC > 0) |>
#'   dplyr::pull(Feature)
#'
#' # Perform ORA with `GO` database and `BP` ontology
#' hd_ora(sig_up_proteins_aml, database = "GO", ontology = "BP")
hd_ora <- function(gene_list,
                   database = c("GO", "Reactome", "KEGG"),
                   ontology = c("BP", "CC", "MF", "ALL"),
                   background = NULL,
                   pval_lim = 0.05) {
  database <- match.arg(database)
  ontology <- match.arg(ontology)

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

    # Perform KEGG enrichment analysis
    enrichment <- clusterProfiler::enrichKEGG(gene = gene_list,
                                              organism = "hsa",
                                              keyType = "ncbi-geneid",
                                              pvalueCutoff = pval_lim,
                                              qvalueCutoff = 1,
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
                                            qvalueCutoff = 1,
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
                                            qvalueCutoff = 1,
                                            universe = background)


  }

  if (!any(enrichment@result[["p.adjust"]] < pval_lim)) {
    stop("No significant terms found.")
  }

  enrichment <- list("gene_list" = gene_list, "background" = background, "enrichment" = enrichment)
  class(enrichment) <- "hd_enrichment"

  if (is.null(background)) {
    enrichment[["background"]] <- NULL
  }
  return(enrichment)
}


#' Plot over-representation analysis
#'
#' `hd_plot_ora()` generates useful visualizations for the results of the
#' over-representation analysis.
#'
#' @param enrichment The enrichment results obtained from `hd_ora()`.
#' @param seed Seed for reproducibility. Default is 123.
#'
#' @return The input object enriched with the plots.
#'
#' @details
#' When KEGG database is used, a cnetplot is generated with ENTREZIDs instead of gene names.
#' For GO and Reactome databases the ENTREZIDs are converted to gene names.
#' If you get the "grid.Call(C_convert, x, as.integer(whatfrom), as.integer(whatto),  :
#' Viewport has zero dimension(s)" warning or error, try to increase the RStudio's viewer window size.
#'
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' de_results <- hd_de_limma(hd_object, case = "AML")
#'
#' # Extract the up-regulated proteins for AML
#' sig_up_proteins_aml <- de_results$de_res |>
#'   dplyr::filter(adj.P.Val < 0.05 & logFC > 0.5) |>
#'   dplyr::pull(Feature)
#'
#' # Perform ORA with `GO` database and `BP` ontology
#' enrichment <- hd_ora(sig_up_proteins_aml, database = "GO", ontology = "BP")
#'
#' # Plot the results
#' hd_plot_ora(enrichment)
hd_plot_ora <- function(enrichment, seed = 123) {

  set.seed(seed)

  # Visualize results
  dot_plot <- clusterProfiler::dotplot(enrichment[["enrichment"]])

  # Ensure 'enrichplot' package is loaded
  if (!requireNamespace("enrichplot", quietly = TRUE)) {
    stop("The 'enrichplot' package is required but not installed. Please install it using install.packages('enrichplot').")
  }
  tree_plot_data <- enrichplot::pairwise_termsim(enrichment[["enrichment"]])
  tree_plot <- enrichplot::treeplot(tree_plot_data)

  if (grepl("hsa", utils::head(enrichment[["enrichment"]]@result[["ID"]], 1))) {
    cnet_plot <- clusterProfiler::cnetplot(enrichment[["enrichment"]],
                                           cex.params = list(gene_label = 0.5, gene_node = 0.8),
                                           color.params = list(edge = TRUE))
  } else {
    enrichment_transformed <- clusterProfiler::setReadable(enrichment[["enrichment"]], OrgDb = org.Hs.eg.db::org.Hs.eg.db)
    cnet_plot <- clusterProfiler::cnetplot(enrichment_transformed,
                                           cex.params = list(gene_label = 0.5, gene_node = 0.8),
                                           color.params = list(edge = TRUE))
  }

  enrichment[["dotplot"]] <- dot_plot
  enrichment[["treeplot"]] <- tree_plot
  enrichment[["cnetplot"]] <- cnet_plot

  return(enrichment)
}


#' Gene set enrichment analysis
#'
#' `hd_gsea()` performs gene set enrichment analysis (GSEA) using the clusterProfiler package.
#'
#' @param de_results An `hd_de` object from `hd_de_limma()` or `hd_de_ttest()` or a tibble containing the results of a differential expression analysis.
#' @param database The database to perform the ORA. It can be either "GO", "KEGG", or "Reactome".
#' @param ontology The ontology to use when database = "GO". It can be "BP" (Biological Process), "CC" (Cellular Component), "MF" (Molecular Function), or "ALL". In the case of KEGG and Reactome, this parameter is ignored.
#' @param ranked_by The variable to rank the proteins. It can be "logFC", "both" which is the product of logFC and -log(adj.P.Val) or a custom sorting variable. It should be however a column in the DE results tibble (`de_results` argument).
#' @param pval_lim The p-value threshold to consider a term as significant in the enrichment analysis. Default is 0.05.
#'
#' @return A list containing the results of the GSEA.
#' @details
#' To perform the GSEA, `clusterProfiler` package is used. For more information, please
#' refer to the `clusterProfiler` documentation.
#'
#' If you want to learn more about GSEA, please refer to the following publications:
#' - Chicco D, Agapito G. Nine quick tips for pathway enrichment analysis. PLoS Comput Biol. 2022 Aug 11;18(8):e1010348. doi: 10.1371/journal.pcbi.1010348. PMID: 35951505; PMCID: PMC9371296. https://pmc.ncbi.nlm.nih.gov/articles/PMC9371296/
#' - https://yulab-smu.top/biomedical-knowledge-mining-book/enrichment-overview.html#gsea-algorithm
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' de_results <- hd_de_limma(hd_object, case = "AML")
#'
#' # Run GSEA with GO database
#' hd_gsea(de_results,
#'         database = "GO",
#'         ontology = "BP",
#'         ranked_by = "logFC",
#'         pval_lim = 0.9)
#' # Remember that the data is artificial, this is why we use an absurdly high p-value cutoff
#'
#' # Run GSEA with different ranking variable
#' hd_gsea(de_results,
#'         database = "GO",
#'         ontology = "BP",
#'         ranked_by = "both",
#'         pval_lim = 0.9)
hd_gsea <- function(de_results,
                    database = c("GO", "Reactome", "KEGG"),
                    ontology = c("BP", "CC", "MF", "ALL"),
                    ranked_by = "logFC",
                    pval_lim = 0.05) {

  database <- match.arg(database)
  ontology <- match.arg(ontology)

  if (class(de_results) == "hd_de") {
    de_results <- de_results$de_res
  }

  # Prepare sorted_protein_list
  if (ranked_by == "logFC") {
    gene_list <- stats::setNames(de_results[["logFC"]],
                                 de_results[["Feature"]])
  } else if (ranked_by == "both") {
    de_results <- de_results |>
      dplyr::mutate(both = logFC * -log(adj.P.Val))
    gene_list <- stats::setNames(de_results[["adj.P.Val"]],
                                 de_results[["Feature"]])
  } else {
    if (ranked_by %in% colnames(de_results)) {
      message(paste("The ranking will be done based on the", ranked_by, "variable."))
      gene_list <- stats::setNames(de_results[[ranked_by]],
                                   de_results[["Feature"]])
    } else {
      stop("The ranking variable provided is not valid. Please provide a valid variable.")
    }
  }
  sorted_gene_list <- sort(gene_list, decreasing = TRUE)

  if (length(sorted_gene_list) == 0) {
    stop("Gene list could not be sorted. Please check the input data.")
  }
  # Ensure 'clusterProfiler' package is loaded
  if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
    stop("The 'clusterProfiler' package is required but not installed. Please install it using BiocManager::install('clusterProfiler').")
  }

  conversion <- gene_to_entrezid(names(sorted_gene_list), NULL)
  gene_list <- stats::setNames(sorted_gene_list, conversion[["gene_list"]])

  if (database == "KEGG") {

    # Perform GSEA for KEGG
    enrichment <- clusterProfiler::gseKEGG(geneList = gene_list,
                                           organism = "hsa",
                                           pvalueCutoff = pval_lim,
                                           pAdjustMethod = "BH",
                                           minGSSize = 10,
                                           maxGSSize = 500)

  } else if (database == "GO") {

    # Ensure 'org.Hs.eg.db' package is loaded
    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
      stop("The 'org.Hs.eg.db' package is required but not installed. Please install it using BiocManager::install('org.Hs.eg.db').")
    }

    # Perform GSEA for GO
    enrichment <- clusterProfiler::gseGO(geneList = gene_list,
                                         OrgDb = org.Hs.eg.db::org.Hs.eg.db,
                                         ont = ontology,
                                         pvalueCutoff = pval_lim,
                                         pAdjustMethod = "BH",
                                         minGSSize = 10,
                                         maxGSSize = 500)

  } else if (database == "Reactome") {

    # Ensure 'ReactomePA' package is loaded
    if (!requireNamespace("ReactomePA", quietly = TRUE)) {
      stop("The 'ReactomePA' package is required but not installed. Please install it using BiocManager::install('ReactomePA').")
    }

    # Perform GSEA for Reactome
    enrichment <- ReactomePA::gsePathway(gene_list,
                                         organism = "human",
                                         pvalueCutoff = pval_lim,
                                         pAdjustMethod = "BH",
                                         verbose = FALSE)

  }

  if (!any(enrichment@result[["p.adjust"]] < pval_lim)) {
    stop("No significant terms found.")
  }

  enrichment <- list("gene_list" = gene_list, "enrichment" = enrichment)
  class(enrichment) <- "hd_enrichment"

  return(enrichment)
}


#' Plot gene set enrichment analysis
#'
#' `hd_plot_gsea()` produces useful plots to visualize the results of the
#' gene set enrichment analysis.
#'
#' @param enrichment The enrichment results obtained from `hd_gsea()`.
#' @param seed Seed for reproducibility. Default is 123.
#'
#' @return The input object enriched with the plots.
#'
#' @details
#' When KEGG database is used, a cnetplot is generated with ENTREZIDs instead of gene names.
#' For GO and Reactome databases the ENTREZIDs are converted to gene names.
#' If you get the "grid.Call(C_convert, x, as.integer(whatfrom), as.integer(whatto),  :
#' Viewport has zero dimension(s)" warning or error, try to increase the RStudio's viewer window size.
#'
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_object <- hd_initialize(example_data, example_metadata)
#'
#' # Run differential expression analysis for AML vs all others
#' de_results <- hd_de_limma(hd_object, case = "AML")
#'
#' # Run GSEA with Reactome database
#' enrichment <- hd_gsea(de_results,
#'                       database = "GO",
#'                       ontology = "BP",
#'                       ranked_by = "logFC",
#'                       pval_lim = 0.9)
#' # Remember that the data is artificial, this is why we use an absurdly high p-value cutoff
#'
#' # Plot the results
#' hd_plot_gsea(enrichment)
hd_plot_gsea <- function(enrichment, seed = 123) {

  set.seed(seed)

  # Visualize results
  dot_plot <- clusterProfiler::dotplot(enrichment[["enrichment"]], split=".sign") +
    ggplot2::facet_grid(.~.sign)

  gseaplot <- clusterProfiler::gseaplot(enrichment[["enrichment"]],
                                        by = "all",
                                        title = enrichment[["enrichment"]]@result[["Description"]][1],
                                        geneSetID = 1)

  if (grepl("hsa", utils::head(enrichment[["enrichment"]]@result[["ID"]], 1))) {
    cnet_plot <- clusterProfiler::cnetplot(enrichment[["enrichment"]],
                                           cex.params = list(gene_label = 0.5, gene_node = 0.8),
                                           color.params = list(edge = TRUE))
  } else {
    enrichment_transformed <- clusterProfiler::setReadable(enrichment[["enrichment"]], OrgDb = org.Hs.eg.db::org.Hs.eg.db)
    cnet_plot <- clusterProfiler::cnetplot(enrichment_transformed,
                                           cex.params = list(gene_label = 0.5, gene_node = 0.8),
                                           color.params = list(edge = TRUE))
  }

  ridgeplot <- clusterProfiler::ridgeplot(enrichment[["enrichment"]]) +
    ggplot2::labs(x = "Enrichment Distribution") +
    ggplot2::theme_minimal()

  enrichment[["dotplot"]] <- dot_plot
  enrichment[["gseaplot"]] <- gseaplot
  enrichment[["cnetplot"]] <- cnet_plot
  enrichment[["ridgeplot"]] <- ridgeplot

  return(enrichment)
}
