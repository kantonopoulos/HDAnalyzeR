# Calculate the percentage of NAs in each column of the dataset

`calc_na_percentage_col()` calculates the percentage of NAs in each
column of the input dataset. It filters out the columns with 0% missing
data and returns the rest in descending order.

## Usage

``` r
calc_na_percentage_col(dat)
```

## Arguments

- dat:

  The input dataset.

## Value

A tibble with the column names and the percentage of NAs in each column.
