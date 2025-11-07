# Prepare data for model fitting

`prepare_data()` prepares the data for model fitting. It filters out
rows with NAs in the variable column, converts the variable to a factor
if it is not a continuous variable, and creates cross-validation sets.

## Usage

``` r
prepare_data(
  dat,
  variable = "Disease",
  case,
  control = NULL,
  balance_groups = TRUE,
  cv_sets = 5,
  seed = 123
)
```

## Arguments

- dat:

  An `hd_model` object coming from
  [`check_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/check_data.md).

- variable:

  The variable to predict. Default is "Disease".

- case:

  The case class.

- control:

  The control class. Default is NULL.

- balance_groups:

  Whether to balance the groups. Default is TRUE.

- cv_sets:

  Number of cross-validation sets. Default is 5.

- seed:

  Seed for reproducibility. Default is 123.

## Value

A model object containing the train and test data and cross-validation
sets.
