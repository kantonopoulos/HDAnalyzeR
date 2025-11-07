# Correlate data

`hd_correlate()` calculates the correlation matrix of the input dataset.

## Usage

``` r
hd_correlate(x, y = NULL, use = "pairwise.complete.obs", method = "pearson")
```

## Arguments

- x:

  A numeric vector, matrix or tibble.

- y:

  A numeric vector, matrix or tibble with compatible dimensions with
  `x`. Default is NULL.

- use:

  A character string. The method to use for computing correlations.
  Default is "pairwise.complete.obs". Other options are "everything",
  "all.obs", "complete.obs", or "na.or.complete".

- method:

  A character string. The correlation method to use. Default is
  "pearson". Other options are "kendall" or "spearman".

## Value

A correlation matrix.

## Details

You can read more about the method for computing covariances in the
presence of missing values and the coefficient that is calculated in the
documentation of the [`cor()`](https://rdrr.io/r/stats/cor.html)
function in the `stats` package.

## Examples

``` r
# Correlate features in a dataset (column wise)
dat <- example_data |>
 dplyr::select(DAid, Assay, NPX) |>
 tidyr::pivot_wider(names_from = "Assay", values_from = "NPX") |>
 dplyr::select(-DAid)

hd_correlate(dat)[seq_len(5), seq_len(5)]  # Subset of the correlation matrix
#>        AARSD1  ABL1 ACAA1  ACAN ACE2
#> AARSD1   1.00  0.47  0.19 -0.06 0.04
#> ABL1     0.47  1.00  0.46 -0.01 0.13
#> ACAA1    0.19  0.46  1.00  0.03 0.32
#> ACAN    -0.06 -0.01  0.03  1.00 0.07
#> ACE2     0.04  0.13  0.32  0.07 1.00

# Correlate 2 vectors
vec1 <- c(1, 2, 3, 4, 5)
vec2 <- c(5, 4, 3, 2, 1)
hd_correlate(vec1, vec2)
#> [1] -1
```
