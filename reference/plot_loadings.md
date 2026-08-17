# Prepare PCA loadings to be plotted on the 2D plane

`plot_loadings()` prepares the PCA loadings to be plotted on the 2D
plane.

## Usage

``` r
plot_loadings(dim_object, plot_loadings, nloadings, x, y)
```

## Arguments

- dim_object:

  A PCA object containing the PCA loadings. Created by
  [`hd_pca()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_pca.md).

- plot_loadings:

  The component whose strongest features should be drawn.

- nloadings:

  The number of loadings to be plotted. Default is 5.

- x:

  The component on the x-axis.

- y:

  The component on the y-axis.

## Value

A tibble with one row per selected feature and one column per plotted
component, so that each arrow can be drawn from the origin to its
`(x, y)` loading.
