# Assess clusters

`hd_assess_clusters()` assesses the stability of the clusters by
bootstrapping the data. It calculates the mean Jaccard index for each
cluster and removes clusters with a mean Jaccard index below 0.5 or a
sample size below 10. The remaining clusters are renamed in order of
size.

## Usage

``` r
hd_assess_clusters(
  cluster_object,
  nrep = 100,
  ji_lim = 0.5,
  nsample_lim = 10,
  seed = 123,
  verbose = FALSE
)
```

## Arguments

- cluster_object:

  The clustering results obtained from
  [`hd_cluster_samples()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_cluster_samples.md).

- nrep:

  The number of bootstrap replicates. Default is 100.

- ji_lim:

  The minimum limit for the mean Jaccard index. Default is 0.5.

- nsample_lim:

  The minimum limit for the sample size. Default is 10.

- seed:

  The seed to use for reproducibility. Default is 123.

- verbose:

  A logical value indicating whether to print the progress of the
  bootstrapping. Default is FALSE.

## Value

A list with the updated cluster assignment and the stability assessment.

## Examples

``` r
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)

# Clustered data
clustering <- hd_cluster_samples(hd_object, gap_b = 10)
#> Determining optimal number of clusters using gap statistic.
#> Optimal number of clusters:4

# Assess clusters
clustering <- hd_assess_clusters(clustering, nrep = 20)
#> Cluster results updated with stability assessment. Low-quality clusters removed.

# Access the results
clustering$cluster_assessment
#> # A tibble: 4 × 4
#>   Cluster cluster_og     n Mean_ji
#>     <dbl>      <int> <int>   <dbl>
#> 1       4          4    70   0.557
#> 2       3          3    96   0.688
#> 3       2          1   112   0.741
#> 4       1          2   308   0.624
```
