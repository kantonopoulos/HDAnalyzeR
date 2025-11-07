# Plot sample data points in a two-dimensional plane

`plot_points()` plots the sample data points in a two-dimensional plane.
The points can be plotted in the PCx/PCy or UMAP1/UMAP2 space.

## Usage

``` r
plot_points(dim_res, x, y, color = NULL)
```

## Arguments

- dim_res:

  A tibble with the results of the dimensionality reduction analysis.

- x:

  The name of the column in `dim_res` that contains the x-axis values.

- y:

  The name of the column in `dim_res` that contains the y-axis values.

- color:

  The name of the column in `dim_res` that contains the variable to be
  used to plot the points color.

## Value

A ggplot object
