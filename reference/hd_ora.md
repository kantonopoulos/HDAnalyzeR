# Over-representation analysis

`hd_ora()` performs over-representation analysis (ORA) using the
clusterProfiler package.

## Usage

``` r
hd_ora(
  gene_list,
  database = c("GO", "Reactome", "KEGG"),
  ontology = c("BP", "CC", "MF", "ALL"),
  background = NULL,
  pval_lim = 0.05
)
```

## Arguments

- gene_list:

  A character vector containing the gene names. These can be
  differentially expressed proteins or selected protein features from
  classification models.

- database:

  The database to perform the ORA. It can be either "GO", "KEGG", or
  "Reactome".

- ontology:

  The ontology to use when database = "GO". It can be "BP" (Biological
  Process), "CC" (Cellular Component), "MF" (Molecular Function), or
  "ALL". In the case of KEGG and Reactome, this parameter is ignored.

- background:

  A character vector containing the background genes or a string with
  the name of the background gene list to use (use
  [`hd_show_backgrounds()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_show_backgrounds.md)
  to see available lists). If NULL, the full proteome is used as
  background.

- pval_lim:

  The p-value threshold to consider a term as significant in the
  enrichment analysis.

## Value

A list containing the results of the ORA.

## Details

When nothing passes the significance threshold the function warns and
returns the (empty) enrichment object instead of stopping, so that a
null result does not abort a longer pipeline.

To perform the ORA, `clusterProfiler` package is used. The
`qvalueCutoff` is set to 1 by default to prioritize filtering by
adjusted p-values (p.adjust). This simplifies the workflow by ensuring a
single, clear significance threshold based on the false discovery rate
(FDR). While q-values are not used for filtering by default, they are
still calculated and included in the results for users who wish to apply
additional criteria. For more information, please refer to the
`clusterProfiler` documentation.

If you want to learn more about ORA, please refer to the following
publications:

- Chicco D, Agapito G. Nine quick tips for pathway enrichment analysis.
  PLoS Comput Biol. 2022 Aug 11;18(8):e1010348. doi:
  10.1371/journal.pcbi.1010348. PMID: 35951505; PMCID: PMC9371296.
  https://pmc.ncbi.nlm.nih.gov/articles/PMC9371296/

- https://yulab-smu.top/biomedical-knowledge-mining-book/enrichment-overview.html#gsea-algorithm

## Examples

``` r
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Run differential expression analysis for AML vs all others
de_results <- hd_de_limma(hd_object, case = "AML")

# Extract the up-regulated proteins for AML
sig_up_proteins_aml <- de_results$de_res |>
  dplyr::filter(adj.P.Val < 0.05 & logFC > 0) |>
  dplyr::pull(Feature)

# Perform ORA with `GO` database and `BP` ontology
enrichment <- hd_ora(sig_up_proteins_aml, database = "GO", ontology = "BP")
#> No background gene list provided. For meaningful enrichment results, it is recommended to specify a relevant background list of genes (e.g., the full proteome or a set of genes that could be impacted in your experiment). The absence of a background may lead to misleading results in the over-representation analysis (ORA).
#> 'select()' returned 1:1 mapping between keys and columns

# Access the results
head(enrichment$enrichment@result)
#>                    ID                                   Description GeneRatio
#> GO:0060135 GO:0060135 maternal process involved in female pregnancy      3/22
#> GO:0001666 GO:0001666                           response to hypoxia      5/22
#> GO:0036293 GO:0036293           response to decreased oxygen levels      5/22
#> GO:0070482 GO:0070482                     response to oxygen levels      5/22
#> GO:0050900 GO:0050900                           leukocyte migration      5/22
#> GO:0007610 GO:0007610                                      behavior      5/22
#>              BgRatio RichFactor FoldEnrichment    zScore       pvalue
#> GO:0060135  30/18842 0.10000000       85.64545 15.863547 5.495587e-06
#> GO:0001666 278/18842 0.01798561       15.40386  8.272161 1.445914e-05
#> GO:0036293 287/18842 0.01742160       14.92081  8.125096 1.686023e-05
#> GO:0070482 312/18842 0.01602564       13.72523  7.749235 2.518994e-05
#> GO:0050900 352/18842 0.01420455       12.16555  7.229969 4.483268e-05
#> GO:0007610 400/18842 0.01250000       10.70568  6.708193 8.219694e-05
#>               p.adjust      qvalue                 geneID Count
#> GO:0060135 0.005429640 0.003447758          59272/181/285     3
#> GO:0001666 0.005552635 0.003525858 51129/405/1386/100/285     5
#> GO:0036293 0.005552635 0.003525858 51129/405/1386/100/285     5
#> GO:0070482 0.006221915 0.003950843 51129/405/1386/100/285     5
#> GO:0050900 0.008858938 0.005625322  9048/199/30817/25/566     5
#> GO:0007610 0.012004197 0.007622524   267/59272/100/25/181     5

# With a background gene list
enrichment <- hd_ora(sig_up_proteins_aml,
                     database = "GO",
                     ontology = "BP",
                     background = "olink_explore_ht")
#> 'select()' returned 1:1 mapping between keys and columns
#> 'select()' returned 1:1 mapping between keys and columns
#> Warning: 1.59% of input gene IDs are fail to map...

# Access the results
head(enrichment$enrichment@result)
#>                    ID                                   Description GeneRatio
#> GO:0060135 GO:0060135 maternal process involved in female pregnancy      3/22
#> GO:0007186 GO:0007186  G protein-coupled receptor signaling pathway      7/22
#> GO:0001666 GO:0001666                           response to hypoxia      5/22
#> GO:0036293 GO:0036293           response to decreased oxygen levels      5/22
#> GO:0070482 GO:0070482                     response to oxygen levels      5/22
#> GO:0071363 GO:0071363   cellular response to growth factor stimulus      7/22
#>             BgRatio RichFactor FoldEnrichment    zScore       pvalue   p.adjust
#> GO:0060135  14/4800 0.21428571      46.753247 11.632237 2.945175e-05 0.02056119
#> GO:0007186 240/4800 0.02916667       6.363636  5.784238 6.399185e-05 0.02056119
#> GO:0001666  99/4800 0.05050505      11.019284  6.834752 6.722575e-05 0.02056119
#> GO:0036293 102/4800 0.04901961      10.695187  6.715269 7.758939e-05 0.02056119
#> GO:0070482 109/4800 0.04587156      10.008340  6.454892 1.065791e-04 0.02228493
#> GO:0071363 267/4800 0.02621723       5.720123  5.384925 1.261411e-04 0.02228493
#>                qvalue                        geneID Count
#> GO:0060135 0.01762096                 59272/181/285     3
#> GO:0007186 0.01762096 9289/30817/100/25/566/181/976     7
#> GO:0001666 0.01762096        51129/405/1386/100/285     5
#> GO:0036293 0.01762096        51129/405/1386/100/285     5
#> GO:0070482 0.01909821        51129/405/1386/100/285     5
#> GO:0071363 0.01909821 9289/405/51742/1386/25/59/285     7
```
