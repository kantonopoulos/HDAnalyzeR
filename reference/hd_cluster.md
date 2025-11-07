# Cluster data

`hd_cluster()` takes a dataset and returns the same dataset ordered
according to the clustering method of the rows and columns. This dataset
can then be used to plot a heatmap with ggplot2 that is not having
clustering functionality.

## Usage

``` r
hd_cluster(
  dat,
  distance_method = "euclidean",
  clustering_method = "ward.D2",
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  normalize = TRUE
)
```

## Arguments

- dat:

  An HDAnalyzeR object or a dataset in wide format and sample ID as its
  first column.

- distance_method:

  The distance method to use. Default is "euclidean". Other options are
  "maximum", "manhattan", "canberra", "binary" or "minkowski".

- clustering_method:

  The clustering method to use. Default is "ward.D2". Other options are
  "ward.D", "single", "complete", "average" (= UPGMA), "mcquitty" (=
  WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC)

- cluster_rows:

  Whether to cluster rows. Default is TRUE.

- cluster_cols:

  Whether to cluster columns. Default is TRUE.

- normalize:

  A logical value indicating whether to normalize the data. Z-score
  normalization is applied using the
  [`hd_normalize()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_normalize.md)
  function. Default is TRUE.

## Value

A list with the dataset ordered according to the clustering of the rows
and columns and the hierarchical clustering object for rows and columns.

## Details

You can read more about the distance and clustering methods in the
documentation of the [`dist()`](https://rdrr.io/r/stats/dist.html) and
[`hclust()`](https://rdrr.io/r/stats/hclust.html) functions in the
`stats` package.

## Examples

``` r
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)

# Clustered data
hd_cluster(hd_object)
#> Warning: attributes are not identical across measure variables; they will be dropped
#> $cluster_res
#> # A tibble: 586 × 101
#>    DAid    AARSD1 ARHGAP1  AKR1B1  ATOX1 ANXA11  AKT1S1 ARHGEF12  AXIN1  ANXA3
#>    <fct>    <dbl>   <dbl>   <dbl>  <dbl>  <dbl>   <dbl>    <dbl>  <dbl>  <dbl>
#>  1 DA00029   4.04 -1.42   -2.24   -1.86  -3.11  -0.0624   -1.51  -0.960 -2.46 
#>  2 DA00273   2.36 -0.923  -0.801  -2.66  -3.30  -0.580    -2.15  -2.38  -2.22 
#>  3 DA00511   1.88 -1.56    0.144  -1.07  -1.39  -2.37     -2.08  -2.05  -1.78 
#>  4 DA00016   1.79 -0.835  -0.734  -2.36  -2.03  -2.03     -1.41  -1.33  -1.97 
#>  5 DA00267  NA    -0.281   0.0621 -1.29  -2.73  -1.47     -2.03  -1.20  -1.16 
#>  6 DA00494   1.98  0.0550 -1.09   -1.26  -1.58  -1.60     -0.568 -1.97  -1.18 
#>  7 DA00250   2.92 -0.0653 -0.676  -1.18  -3.54  -1.26     -1.50  -1.56  -2.04 
#>  8 DA00271   1.40 -0.0407  0.0906 -1.09  -1.50  -0.256    -1.19  -1.17  -0.228
#>  9 DA00418   1.79 -0.476  -0.387  -0.241 -0.473 -0.261    -1.38  -0.886 -1.64 
#> 10 DA00557   2.83  0.557  -0.963   0.119 -0.486  0.442    -0.601 -0.436 -0.155
#> # ℹ 576 more rows
#> # ℹ 91 more variables: ANXA4 <dbl>, AIF1 <dbl>, ATP6V1F <dbl>, AHCY <dbl>,
#> #   ATXN10 <dbl>, ACAA1 <dbl>, ACOX1 <dbl>, AKT3 <dbl>, ARSB <dbl>,
#> #   AIFM1 <dbl>, ATP5IF1 <dbl>, ANGPT1 <dbl>, APP <dbl>, ANKRD54 <dbl>,
#> #   AK1 <dbl>, ATG4A <dbl>, ADAM23 <dbl>, AMIGO2 <dbl>, ADAMTS8 <dbl>,
#> #   AGER <dbl>, ADGRG2 <dbl>, AOC3 <dbl>, AMBN <dbl>, ADCYAP1R1 <dbl>,
#> #   AOC1 <dbl>, ANGPTL3 <dbl>, APOH <dbl>, AGR3 <dbl>, ACP6 <dbl>, AMN <dbl>, …
#> 
#> $cluster_rows
#> 
#> Call:
#> stats::hclust(d = stats::dist(x, method = distance), method = method)
#> 
#> Cluster method   : ward.D2 
#> Distance         : euclidean 
#> Number of objects: 586 
#> 
#> 
#> $cluster_cols
#> 
#> Call:
#> stats::hclust(d = stats::dist(x, method = distance), method = method)
#> 
#> Cluster method   : ward.D2 
#> Distance         : euclidean 
#> Number of objects: 100 
#> 
#> 
#> attr(,"class")
#> [1] "hd_cluster"
```
