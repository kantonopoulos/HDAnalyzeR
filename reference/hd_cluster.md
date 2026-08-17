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
#> $cluster_res
#> # A tibble: 586 × 101
#>    DAid     ANGPT1    APP ARHGAP1  AKR1B1  ATOX1 ANXA11  AKT1S1 ARHGEF12   AXIN1
#>    <chr>     <dbl>  <dbl>   <dbl>   <dbl>  <dbl>  <dbl>   <dbl>    <dbl>   <dbl>
#>  1 DA00235 -0.953  -0.827  -0.537 -0.166  -0.543  0.244 -1.42     0.120  -0.0689
#>  2 DA00279 -1.45   -1.90    0.481  0.818  -0.369  1.22  -0.700    0.169  -0.313 
#>  3 DA00252 -1.04   -1.14    0.602 -1.39   -0.752 -0.776  0.137   -0.281   0.248 
#>  4 DA00384 -1.36   -1.14   -0.140 -0.837  -1.06  -0.683 -0.794   -1.13   -1.88  
#>  5 DA00342  0.0962 -0.170  -0.536 -1.10   -1.71  -0.428 -0.143    0.273  -0.376 
#>  6 DA00357 -0.327  -1.58   NA     NA       0.356 -0.494  0.284   -0.958  -0.779 
#>  7 DA00373 -0.0122 -0.379   0.186  0.494  -0.382 -0.387 -0.469   -0.344  -1.04  
#>  8 DA00582 -0.479  -1.06   -1.42  -1.06   -0.681 -0.674 -0.0985  -1.66   -0.187 
#>  9 DA00151 -1.84   -1.04    0.704  1.62   -0.213  0.149 -0.684   -1.20   -1.05  
#> 10 DA00244  0.263  -1.66    0.246 -0.0151 -0.621 -0.440 -1.75     0.0463 -0.560 
#> # ℹ 576 more rows
#> # ℹ 91 more variables: ANXA3 <dbl>, ANXA4 <dbl>, AIF1 <dbl>, ATP6V1F <dbl>,
#> #   AHCY <dbl>, ATXN10 <dbl>, ACAA1 <dbl>, ACOX1 <dbl>, AKT3 <dbl>, ARSB <dbl>,
#> #   AIFM1 <dbl>, ATP5IF1 <dbl>, ACE2 <dbl>, ALDH1A1 <dbl>, ACY1 <dbl>,
#> #   ADH4 <dbl>, AGXT <dbl>, AKR1C4 <dbl>, ALDH3A1 <dbl>, ACP5 <dbl>,
#> #   ANGPTL1 <dbl>, ANPEP <dbl>, AMFR <dbl>, ABL1 <dbl>, APBB1IP <dbl>,
#> #   ARHGAP25 <dbl>, APEX1 <dbl>, ARID4B <dbl>, AGR2 <dbl>, ANXA10 <dbl>, …
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
