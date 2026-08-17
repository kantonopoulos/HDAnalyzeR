# Check whether an enrichment run returned anything significant

`enrichment_is_empty()` reports whether a `clusterProfiler` result
object is missing, has no rows, or has no term below the significance
threshold.

## Usage

``` r
enrichment_is_empty(enrichment, pval_lim)
```

## Arguments

- enrichment:

  A `clusterProfiler` enrichment object, or `NULL`.

- pval_lim:

  The adjusted p-value threshold used for the run.

## Value

`TRUE` when there is nothing worth reporting, `FALSE` otherwise.

## Details

The adjusted p-values can contain `NA`s, so the comparison has to drop
them explicitly. Without `na.rm`, an all-`NA` column makes
[`any()`](https://rdrr.io/r/base/any.html) return `NA` and the
surrounding `if()` fails with a missing-value error instead of reporting
that nothing was enriched.
