# Cluster samples in k clusters

`hd_cluster_samples()` takes a dataset and returns the clustering of the
samples. The user can define the number of clusters or let the function
determine the optimal number of clusters using the gap statistic. If the
data contain missing values, the function imputes them using the
k-nearest neighbors algorithm (k = 5).

## Usage

``` r
hd_cluster_samples(
  dat,
  distance_method = "euclidean",
  clustering_method = "ward.D2",
  normalize = TRUE,
  k = NULL,
  k_max = 15,
  gap_b = 100,
  seed = 123,
  verbose = FALSE
)
```

## Arguments

- dat:

  An HDAnalyzeR object or a dataset in wide format and sample ID as its
  first column.

- distance_method:

  The distance method to use. Default is "euclidean".

- clustering_method:

  The clustering method to use. Default is "ward.D2".

- normalize:

  A logical value indicating whether to normalize the data. Z-score
  normalization is applied using the
  [`hd_normalize()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_normalize.md)
  function. Default is TRUE.

- k:

  The number of clusters to return. If NULL, the function determines the
  optimal number of clusters using the gap statistic.

- k_max:

  The maximum number of clusters to test. Default is 15. Valid only if k
  is NULL.

- gap_b:

  The number of times to repeat the clustering in the gap analysis.
  Default is 100. Valid only if k is NULL.

- seed:

  The seed to use for reproducibility. Default is 123. Valid only if k
  is NULL.

- verbose:

  A logical value indicating whether to print the progress of the gap
  analysis. Default is FALSE. Valid only if k is NULL.

## Value

A list with the cluster assignment.

## Examples

``` r
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)

# Clustered data
clustering <- hd_cluster_samples(hd_object, gap_b = 10)
#> Determining optimal number of clusters using gap statistic.
#> Optimal number of clusters:4

# Access the results
head(clustering[["cluster_res"]])
#> # A tibble: 6 × 2
#>   DAid    Cluster
#>   <chr>     <int>
#> 1 DA00001       1
#> 2 DA00002       2
#> 3 DA00003       3
#> 4 DA00004       4
#> 5 DA00005       1
#> 6 DA00006       2

# Clustered data with defined number of clusters
clustering <- hd_cluster_samples(hd_object, k = 7)
#> Using user-defined number of clusters:7

# Access the results
head(clustering[["cluster_res"]])
#> # A tibble: 6 × 2
#>   DAid    Cluster
#>   <chr>     <int>
#> 1 DA00001       1
#> 2 DA00002       2
#> 3 DA00003       3
#> 4 DA00004       4
#> 5 DA00005       1
#> 6 DA00006       5
```
