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
#' `hd_ora()` performs over-representation analysis (ORA) using the clusterProfiler package.
#'
#' @param gene_list A character vector containing the gene names. These can be differentially expressed proteins or selected protein features from classification models.
#' @param database The database to perform the ORA. It can be either "GO", "KEGG", or "Reactome".
#' @param ontology The ontology to use when database = "GO". It can be "BP" (Biological Process), "CC" (Cellular Component), "MF" (Molecular Function), or "ALL". In the case of KEGG and Reactome, this parameter is ignored.
#' @param background A character vector containing the background genes.
#' @param pval_lim_enrichment The p-value threshold to consider a term as significant in the enrichment analysis.
#'
#' @return A list containing the results of the ORA.
#'
#' @details
#' qvalueCutoff was set to be 4 times the pvalueCutoff.
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
#'   dplyr::filter(adj.P.Val < 0.05 & logFC > 0) |>
#'   dplyr::pull(Feature)
#'
#' # Perform ORA with `GO` database and `BP` ontology
#' hd_ora(sig_up_proteins_aml, database = "GO", ontology = "BP")
hd_ora <- function(gene_list,
                   database = c("GO", "Reactome", "KEGG"),
                   ontology = c("BP", "CC", "MF", "ALL"),
                   background = NULL,
                   pval_lim_enrichment = 0.05) {
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
                                              pvalueCutoff = pval_lim_enrichment,
                                              qvalueCutoff = pval_lim_enrichment*4,
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
                                            pvalueCutoff = pval_lim_enrichment,
                                            qvalueCutoff = pval_lim_enrichment*4,
                                            universe = background)

  } else if (database == "Reactome") {

    # Ensure 'ReactomePA' package is loaded
    if (!requireNamespace("ReactomePA", quietly = TRUE)) {
      stop("The 'ReactomePA' package is required but not installed. Please install it using BiocManager::install('ReactomePA').")
    }

    # Perform Reactome enrichment analysis
    enrichment <- ReactomePA::enrichPathway(gene = gene_list,
                                            organism = "human",
                                            pvalueCutoff = pval_lim_enrichment,
                                            qvalueCutoff = pval_lim_enrichment*4,
                                            universe = background)


  }

  if (!any(enrichment@result[["p.adjust"]] < pval_lim_enrichment)) {
    stop("No significant terms found.")
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
#' @param enrichment The enrichment results obtained from `hd_ora()`.
#' @param seed Seed for reproducibility. Default is 123.
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
    cnet_plot <- clusterProfiler::cnetplot(enrichment[["enrichment"]], categorySize = "pvalue", colorEdge = TRUE)
  } else {
    enrichment_transformed <- clusterProfiler::setReadable(enrichment[["enrichment"]], OrgDb = org.Hs.eg.db::org.Hs.eg.db)
    cnet_plot <- clusterProfiler::cnetplot(enrichment_transformed, categorySize = "pvalue", colorEdge = TRUE)
  }

  enrichment[["dotplot"]] <- dot_plot
  enrichment[["treeplot"]] <- tree_plot
  enrichment[["cnetplot"]] <- cnet_plot

  return(enrichment)
}


#' Perform gene set enrichment analysis
#'
#' `hd_gsea()` performs gene set enrichment analysis (GSEA) using the clusterProfiler package.
#'
#' @param de_results A tibble containing the results of a differential expression analysis.
#' @param database The database to perform the ORA. It can be either "GO", "KEGG", or "Reactome".
#' @param ontology The ontology to use when database = "GO". It can be "BP" (Biological Process), "CC" (Cellular Component), "MF" (Molecular Function), or "ALL". In the case of KEGG and Reactome, this parameter is ignored.
#' @param expression The type of differentially expressed proteins to consider in the analysis. It can be "both", "up", or "down".
#' @param ranked_by The metric to rank the proteins. It can be "logFC" or "adj.P.Val".
#' @param pval_lim The p-value threshold to consider a term as significant in the differential expression analysis. Default is 0.05.
#' @param logfc_lim The log fold change threshold to consider a term as significant in the differential expression analysis. Default is 0.
#' @param pval_lim_enrichment The p-value threshold to consider a term as significant in the enrichment analysis. Default is 0.05.
#'
#' @return A list containing the results of the GSEA.
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
#' hd_gsea(de_results,
#'         database = "GO",
#'         ontology = "BP",
#'         expression = "both",
#'         ranked_by = "logFC",
#'         pval_lim_enrichment = 0.9)
#' # Remember that the data is artificial, this is why we use an absurdly high p-value cutoff
hd_gsea <- function(de_results,
                    database = c("GO", "Reactome", "KEGG"),
                    ontology = c("BP", "CC", "MF", "ALL"),
                    expression = c("both", "up", "down"),
                    ranked_by = c("logFC", "adj.P.Val"),
                    pval_lim = 0.05,
                    logfc_lim = 0,
                    pval_lim_enrichment = 0.05) {

  database <- match.arg(database)
  ontology <- match.arg(ontology)
  expression <- match.arg(expression)
  ranked_by <- match.arg(ranked_by)

  if (expression == "both") {
    de_results <- de_results[["de_res"]] |>
      dplyr::filter(!!rlang::sym("adj.P.Val") < pval_lim & abs(!!rlang::sym("logFC")) > logfc_lim)
  } else if (expression == "up") {
    de_results <- de_results[["de_res"]] |>
      dplyr::filter(!!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") > logfc_lim)
  } else if (expression == "down") {
    de_results <- de_results[["de_res"]] |>
      dplyr::filter(!!rlang::sym("adj.P.Val") < pval_lim & !!rlang::sym("logFC") < -logfc_lim)
  }

  de_results <- de_results |> dplyr::mutate(logFC = abs(!!rlang::sym("logFC")))

  # Prepare sorted_protein_list
  if (ranked_by == "logFC") {
    gene_list <- stats::setNames(de_results[["logFC"]],
                                 de_results[["Feature"]])
  } else {
    gene_list <- stats::setNames(de_results[["adj.P.Val"]],
                                 de_results[["Feature"]])
  }
  sorted_gene_list <- sort(gene_list, decreasing = TRUE)

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
                                           pvalueCutoff = pval_lim_enrichment,
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
                                         pvalueCutoff = pval_lim_enrichment,
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
                                         pvalueCutoff = pval_lim_enrichment,
                                         pAdjustMethod = "BH",
                                         verbose = FALSE)

  }

  if (!any(enrichment@result[["p.adjust"]] < pval_lim_enrichment)) {
    stop("No significant terms found.")
  }

  enrichment <- list("gene_list" = gene_list, "enrichment" = enrichment)
  class(enrichment) <- "hd_enrichment"

  return(enrichment)
}


#' Plot the results of the gene set enrichment analysis
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
#'                       expression = "both",
#'                       ranked_by = "logFC",
#'                       pval_lim_enrichment = 0.9)
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
    cnet_plot <- clusterProfiler::cnetplot(enrichment[["enrichment"]], categorySize = "pvalue", colorEdge = TRUE)
  } else {
    enrichment_transformed <- clusterProfiler::setReadable(enrichment[["enrichment"]], OrgDb = org.Hs.eg.db::org.Hs.eg.db)
    cnet_plot <- clusterProfiler::cnetplot(enrichment_transformed, categorySize = "pvalue", colorEdge = TRUE)
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
