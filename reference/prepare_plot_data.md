# Extract information from the HDAnalyzeR and PCA or UMAP object

`prepare_plot_data()` extracts the PCA or UMAP results and metadata
information from the respective objects.

## Usage

``` r
prepare_plot_data(dim_object, metadata, color, x, y)
```

## Arguments

- dim_object:

  A PCA or UMAP object containing the results of the dimensionality
  reduction analysis. Created by
  [`hd_pca()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_pca.md)
  or
  [`hd_umap()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_umap.md).

- metadata:

  An HDAnalyzeR object or a dataset in wide format and sample ID as its
  first column.

- color:

  The name of the column that contains the variable to be used to plot
  the points color.

- x:

  The name of the column in `dim_object` that contains the x-axis
  values.

- y:

  The name of the column in `dim_object` that contains the y-axis
  values.

## Value

A tibble with the PCA or UMAP results and metadata information if
available.
