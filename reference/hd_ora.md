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
#> Warning: coercing argument of type 'double' to logical

# Access the results
head(enrichment$enrichment@result)
#>                    ID                                   Description GeneRatio
#> GO:0050900 GO:0050900                           leukocyte migration      7/22
#> GO:0050926 GO:0050926             regulation of positive chemotaxis      3/22
#> GO:0071674 GO:0071674                    mononuclear cell migration      5/22
#> GO:0001666 GO:0001666                           response to hypoxia      5/22
#> GO:0036293 GO:0036293           response to decreased oxygen levels      5/22
#> GO:0060135 GO:0060135 maternal process involved in female pregnancy      3/22
#>              BgRatio RichFactor FoldEnrichment    zScore       pvalue
#> GO:0050900 397/18860 0.01763224       15.11564  9.713997 2.255163e-07
#> GO:0050926  26/18860 0.11538462       98.91608 17.073506 3.519968e-06
#> GO:0071674 235/18860 0.02127660       18.23985  9.088086 6.375236e-06
#> GO:0001666 317/18860 0.01577287       13.52165  7.683419 2.706145e-05
#> GO:0036293 330/18860 0.01515152       12.98898  7.508528 3.280154e-05
#> GO:0060135  60/18860 0.05000000       42.86364 11.099079 4.515298e-05
#>                p.adjust      qvalue                         geneID Count
#> GO:0050900 0.0002753554 0.000166882 100/566/9048/25/2683/199/30817     7
#> GO:0050926 0.0021489407 0.001302388                   566/285/9048     3
#> GO:0071674 0.0025947211 0.001572558           566/9048/25/2683/199     5
#> GO:0001666 0.0080101353 0.004854627         100/285/51129/405/1386     5
#> GO:0036293 0.0080101353 0.004854627         100/285/51129/405/1386     5
#> GO:0060135 0.0083253886 0.005045690                  285/181/59272     3

# With a background gene list
enrichment <- hd_ora(sig_up_proteins_aml,
                     database = "GO",
                     ontology = "BP",
                     background = "olink_explore_ht")
#> 'select()' returned 1:1 mapping between keys and columns
#> 'select()' returned 1:1 mapping between keys and columns
#> Warning: 1.51% of input gene IDs are fail to map...
#> Warning: coercing argument of type 'double' to logical

# Access the results
head(enrichment$enrichment@result)
#>                    ID                                   Description GeneRatio
#> GO:0050900 GO:0050900                           leukocyte migration      7/22
#> GO:0050926 GO:0050926             regulation of positive chemotaxis      3/22
#> GO:0007186 GO:0007186  G protein-coupled receptor signaling pathway      7/22
#> GO:0001666 GO:0001666                           response to hypoxia      5/22
#> GO:0040012 GO:0040012                      regulation of locomotion      9/22
#> GO:0060135 GO:0060135 maternal process involved in female pregnancy      3/22
#>             BgRatio RichFactor FoldEnrichment   zScore       pvalue   p.adjust
#> GO:0050900 210/4812 0.03333333       7.290909 6.317002 2.657681e-05 0.02384161
#> GO:0050926  19/4812 0.15789474      34.535885 9.925311 7.667855e-05 0.02384161
#> GO:0007186 256/4812 0.02734375       5.980824 5.549969 9.510644e-05 0.02384161
#> GO:0001666 113/4812 0.04424779       9.678198 6.325973 1.250600e-04 0.02384161
#> GO:0040012 479/4812 0.01878914       4.109698 4.860186 1.353602e-04 0.02384161
#> GO:0060135  23/4812 0.13043478      28.529644 8.968160 1.384894e-04 0.02384161
#>                qvalue                                geneID Count
#> GO:0050900 0.01972001        100/566/9048/25/2683/199/30817     7
#> GO:0050926 0.01972001                          566/285/9048     3
#> GO:0007186 0.01972001         100/566/9289/181/25/976/30817     7
#> GO:0001666 0.01972001                100/285/51129/405/1386     5
#> GO:0040012 0.01972001 100/566/9289/285/9048/25/59/51742/199     9
#> GO:0060135 0.01972001                         285/181/59272     3
```
