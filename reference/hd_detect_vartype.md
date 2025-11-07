# Detect variable type

`hd_detect_vartype()` detects the type of a variable based on its
content. If a variable is a factor or character, it is considered
categorical. If a variable is numeric and has less than or equal to
`unique_threshold` unique values, it is considered categorical.
Otherwise, it is considered continuous.

## Usage

``` r
hd_detect_vartype(var, unique_threshold = 5)
```

## Arguments

- var:

  The variable (vector or dataframe column)to detect the type of.

- unique_threshold:

  The threshold to consider a numeric variable as categorical. Default
  is 5.

## Value

The type of the variable as a string: "categorical", "continuous", or
"unknown".

## Details

If you want to apply this function to each column of a dataframe, you
can use the [`sapply()`](https://rdrr.io/r/base/lapply.html) function.
See examples. For more information check `sapply` documentation.

## Examples

``` r
# Check categorical data
category <- c("A", "B", "A", "C")
hd_detect_vartype(category)
#> [1] "categorical"

# Check continuous data
continuous <- c(1, 2, 3, 4, 5, 6)
hd_detect_vartype(continuous)
#> [1] "continuous"

# Apply the function to each column of a dataframe
example <- data.frame(Category = c("A", "B", "A", "C", "B", "A"),
                      Continuous = c(1.1, 2.5, 3.8, 4.0, 5.8, 9),
                      Mixed = c(1, "1", 2, 2, "3", 3))

sapply(example, hd_detect_vartype)
#>      Category    Continuous         Mixed 
#> "categorical"  "continuous" "categorical" 
```
