# Calculate the percentage of NAs in each row of the dataset

`calc_na_percentage_row()` calculates the percentage of NAs in each row
of the input dataset. It filters out the rows with 0% missing data and
returns the rest in descending order.

## Usage

``` r
calc_na_percentage_row(dat, sample_id)
```

## Arguments

- dat:

  The input dataset.

- sample_id:

  The name of the column containing the sample IDs.

## Value

A tibble with the DAids and the percentage of NAs in each row.
