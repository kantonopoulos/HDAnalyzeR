# Cluster data helper function

`cluster_help()` is a helper function that acts as a wrapper for the
[`hclust()`](https://rdrr.io/r/stats/hclust.html) function. It takes a
dataset and returns the clustering of the rows or columns.

## Usage

``` r
cluster_help(x, k = NULL, distance, method)
```

## Arguments

- x:

  A numeric matrix or data frame.

- k:

  The number of clusters to return. If NULL, the function simply returns
  the hierarchical clustering object.

- distance:

  The distance method to use.

- method:

  The clustering method to use.

## Value

A list with the cluster assignment.
