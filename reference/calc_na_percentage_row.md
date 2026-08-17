# Calculate the percentage of NAs in each row of the dataset

`calc_na_percentage_row()` calculates the percentage of NAs in each row
of the input dataset. It filters out the rows with 0% missing data and
returns the rest in descending order.

## Usage

``` r
calc_na_percentage_row(dat, sample_id, chunk_size = 500)
```

## Arguments

- dat:

  The input dataset.

- sample_id:

  The name of the column containing the sample IDs.

- chunk_size:

  The number of columns to scan at a time. Default is 500.

## Value

A tibble with the DAids and the percentage of NAs in each row.

## Details

The counts are accumulated with
[`rowSums()`](https://rdrr.io/r/base/colSums.html) over blocks of
columns instead of `rowwise()`, which evaluates once per row and becomes
the dominant cost on datasets with thousands of samples and features.
Chunking keeps the temporary logical matrix small regardless of how many
features there are.
