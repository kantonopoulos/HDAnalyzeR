# Check the column types of the dataset

`check_col_types()` checks the column types of the input dataset and
returns the counts of each class.

## Usage

``` r
check_col_types(dat, unique_threshold = 5)
```

## Arguments

- dat:

  The input dataset.

- unique_threshold:

  The threshold to consider a numeric variable as categorical. Default
  is 5.

## Value

A table with the counts of each class in the dataset.
