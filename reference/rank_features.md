# Rank differential expression results for GSEA

`rank_features()` turns a table of differential expression results into
the named, decreasing vector that GSEA expects.

## Usage

``` r
rank_features(de_results, ranked_by = "logFC")
```

## Arguments

- de_results:

  A tibble of differential expression results with at least a `Feature`
  column.

- ranked_by:

  The column to rank by, `"logFC"`, or `"both"` for the product of the
  log fold change and `-log(adj.P.Val)`.

## Value

A named numeric vector sorted in decreasing order.
