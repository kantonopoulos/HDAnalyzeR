# Prepare PCA loadings to be plotted on the 2D plane

`plot_loadings()` prepares the PCA loadings to be plotted on the 2D
plane.

## Usage

``` r
plot_loadings(dim_object, plot_loadings, nloadings)
```

## Arguments

- dim_object:

  A PCA object containing the PCA loadings. Created by
  [`hd_pca()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_pca.md).

- plot_loadings:

  The component to be plotted. Default is NULL.

- nloadings:

  The number of loadings to be plotted. Default is 5.

## Value

A tibble with the PCA loadings to be plotted.
