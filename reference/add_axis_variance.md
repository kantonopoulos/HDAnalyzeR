# Add component variance to the axis labels

`add_axis_variance()` adds the explained variance of the components to
the axis labels.

## Usage

``` r
add_axis_variance(dim_object, dim_plot, x, y)
```

## Arguments

- dim_object:

  A PCA object containing the PCA variance. Created by
  [`hd_pca()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_pca.md).

- dim_plot:

  A ggplot object with the data points on the 2D plane.

- x:

  The name of the column in `dim_res` that contains the x-axis values.

- y:

  The name of the column in `dim_res` that contains the y-axis values.

## Value

A ggplot object
