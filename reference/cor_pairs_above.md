# Extract the feature pairs above a correlation threshold

`cor_pairs_above()` pulls the entries of a correlation matrix whose
absolute value exceeds a threshold, without expanding the matrix into a
long table.

## Usage

``` r
cor_pairs_above(cor_matrix, threshold, chunk_size = 1000)
```

## Arguments

- cor_matrix:

  A correlation matrix.

- threshold:

  The reporting correlation threshold.

- chunk_size:

  The number of columns to scan at a time. Default is 1000.

## Value

A tibble with `Protein1`, `Protein2` and `Correlation`, sorted by
correlation in decreasing order. Each pair appears in both directions,
as it does in the correlation matrix itself.

## Details

Reshaping the matrix to long format allocates one row per entry. At
10,000 features that is 100 million rows across three columns, which
exhausts memory before any filtering happens. Scanning blocks of columns
and keeping only the entries above the threshold makes the cost
proportional to the number of reported pairs instead.
