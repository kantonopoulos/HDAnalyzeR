# Check for non-numeric columns

`check_numeric_columns()` checks if all columns except the first (Sample
ID) in a dataframe are numeric.

## Usage

``` r
check_numeric_columns(dat)
```

## Arguments

- dat:

  The dataframe to check.

## Value

A warning with the names of non-numeric columns if any.
