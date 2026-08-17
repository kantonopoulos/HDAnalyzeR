# Rename a ranked gene list from gene symbols to ENTREZ identifiers

`rename_ranking_to_entrezid()` keeps each score attached to its own
gene.

## Usage

``` r
rename_ranking_to_entrezid(ranked_genes)
```

## Arguments

- ranked_genes:

  A named numeric vector, named by gene symbol.

## Value

The same scores, named by ENTREZ identifier and sorted decreasingly.

## Details

The symbol to identifier mapping drops genes it cannot resolve and can
return several identifiers for one symbol, so the scores have to be
matched by name. Renaming the vector positionally would silently attach
each score to the wrong gene as soon as a single symbol failed to map.
