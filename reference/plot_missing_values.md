# Create the missing value distribution

`plot_missing_values()` creates a histogram of the missing value
distribution.

## Usage

``` r
plot_missing_values(missing_values, yaxis_name)
```

## Arguments

- missing_values:

  A tibble with the column/row names and the percentage of NAs in each
  column/row.

- yaxis_name:

  The name of the y-axis.

## Value

A histogram of the missing value distribution.
