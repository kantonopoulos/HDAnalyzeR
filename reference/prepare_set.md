# Prepare the data for the `hd_model_test()` function

`prepare_set()` prepares the data for the
[`hd_model_test()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_model_test.md)
function.

## Usage

``` r
prepare_set(dat, variable, metadata_cols = NULL)
```

## Arguments

- dat:

  An HDAnalyzeR object or a dataset in wide format with sample_id as its
  first column and class column as its second column.

- variable:

  The name of the column containing the case and control groups. Default
  is "Disease".

- metadata_cols:

  The metadata columns to include in the analysis. Default is NULL.

## Value

The prepared data.
