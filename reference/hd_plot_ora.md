# Plot over-representation analysis results

`hd_plot_ora()` generates useful visualizations for the results of the
over-representation analysis.

## Usage

``` r
hd_plot_ora(enrichment, seed = 123)
```

## Arguments

- enrichment:

  The enrichment results obtained from
  [`hd_ora()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_ora.md).

- seed:

  Seed for reproducibility. Default is 123.

## Value

The input object enriched with the plots.

## Details

When KEGG database is used, a cnetplot is generated with ENTREZIDs
instead of gene names. For GO and Reactome databases the ENTREZIDs are
converted to gene names. If you get the "grid.Call(C_convert, x,
as.integer(whatfrom), as.integer(whatto), : Viewport has zero
dimension(s)" warning or error, try to increase the RStudio's viewer
window size.

## Examples

``` r
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Run differential expression analysis for AML vs all others
de_results <- hd_de_limma(hd_object, case = "AML")

# Extract the up-regulated proteins for AML
sig_up_proteins_aml <- de_results$de_res |>
  dplyr::filter(adj.P.Val < 0.05 & logFC > 0.5) |>
  dplyr::pull(Feature)

# Perform ORA with `GO` database and `BP` ontology
enrichment <- hd_ora(sig_up_proteins_aml, database = "GO", ontology = "BP")
#> No background gene list provided. For meaningful enrichment results, it is recommended to specify a relevant background list of genes (e.g., the full proteome or a set of genes that could be impacted in your experiment). The absence of a background may lead to misleading results in the over-representation analysis (ORA).
#> 'select()' returned 1:1 mapping between keys and columns
#> Warning: coercing argument of type 'double' to logical

# Plot the results
enrichment <- hd_plot_ora(enrichment)
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
#> Warning: Possible problem with the clustering in the treeplot. Might not have enough significant results.

# Access the plots
enrichment$dotplot

enrichment$treeplot
#> NULL
enrichment$cnetplot
```
