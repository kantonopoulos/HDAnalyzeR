# Build an UpSet plot when there is something to intersect

`build_upset_plot()` wraps
[`UpSetR::upset()`](https://rdrr.io/pkg/UpSetR/man/upset.html) so that
degenerate inputs give a clear message instead of a cryptic error from
deep inside `UpSetR`.

## Usage

``` r
build_upset_plot(feature_sets, ordered_names, ordered_colors)
```

## Arguments

- feature_sets:

  A named list of feature vectors, one per group.

- ordered_names:

  The group names in the order they should be drawn.

- ordered_colors:

  The bar colour for each group, named by group.

## Value

An UpSet plot, or `NULL` when fewer than two groups have features.
