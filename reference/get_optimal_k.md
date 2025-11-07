# Get optimal number of clusters

`get_optimal_k()` takes a dataset and returns the optimal number of
clusters based on the gap statistic.

## Usage

``` r
get_optimal_k(x, k_max = 15, b = 50, distance, method, verbose = FALSE)
```

## Arguments

- x:

  A numeric matrix or data frame.

- k_max:

  The maximum number of clusters to test. Default is 15.

- b:

  The number of times to repeat the clustering in the gap analysis.
  Default is 50.

- distance:

  The distance method to use.

- method:

  The clustering method to use.

- verbose:

  A logical value indicating whether to print the progress of the gap
  analysis. Default is FALSE.

## Value

A list with the gap statistic results and the optimal number of
clusters.
