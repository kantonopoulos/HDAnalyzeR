# Check data structure of object

`check_data()` checks the structure of the object provided. It checks if
the object is an `hd_model` object or a list containing the train and
test data. It also checks if the variable is present in the train and
test data.

## Usage

``` r
check_data(dat, variable = "Disease")
```

## Arguments

- dat:

  An `hd_model` object or a list containing the train and test data.

- variable:

  The name of the column containing the case and control groups. Default
  is "Disease".

## Value

A model object containing the train and test data.
