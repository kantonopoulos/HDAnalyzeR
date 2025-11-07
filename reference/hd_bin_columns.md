# Bin variables

`hd_bin_columns()` bins continuous variables and labels them with
ranges.

## Usage

``` r
hd_bin_columns(dat, column_types, bins = 5, round_digits = 0)
```

## Arguments

- dat:

  The data to bin.

- column_types:

  A vector containing the type of each variable (column) in the
  dataframe.

- bins:

  The number of bins to create. Default is 5.

- round_digits:

  The number of digits to round the bin ranges to. Default is 0.

## Value

The data with continuous variables binned.

## Details

In case of a dataset with many variables, it is recommended to use the
function
[`hd_detect_vartype()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_detect_vartype.md)
to automatically detect the type of each variable. See examples.

## Examples

``` r
# Example dataframe
test_data <- data.frame(
  age = c(25, 35, 45, 55, 65),
  BMI = c(25, 35, 30, 32, 28),
  sex = c("M", "F", "M", "F", "M")
)

column_types <- c("continuous", "continuous", "categorical")
hd_bin_columns(test_data, column_types, bins = 3)
#>     age   BMI sex
#> 1 25-38 25-28   M
#> 2 25-38 32-35   F
#> 3 38-52 28-32   M
#> 4 52-65 32-35   F
#> 5 52-65 25-28   M

# Automatically detect variable types
test_data <- data.frame(Category = c("A", "B", "A", "C", "B", "A"),
                        Continuous = c(1.1, 2.5, 3.8, 4.0, 5.8, 9))
column_types <- sapply(test_data, hd_detect_vartype)

# The variable to be binned has one significant digit
# So we will also round the bins to one digit
hd_bin_columns(test_data, column_types, bins = 3, round_digits = 1)
#>   Category Continuous
#> 1        A    1.1-3.7
#> 2        B    1.1-3.7
#> 3        A    3.7-6.4
#> 4        C    3.7-6.4
#> 5        B    3.7-6.4
#> 6        A      6.4-9
```
