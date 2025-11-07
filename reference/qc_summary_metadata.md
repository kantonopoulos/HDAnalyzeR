# Summarize the quality control results of metadata

`qc_summary_metadata()` summarizes the quality control results of the
metadata dataframe. It checks the column types, calculates the
percentage of NAs in each column and row, and creates summary
visualizations for categorical and numeric variables.

## Usage

``` r
qc_summary_metadata(
  metadata,
  sample_id,
  variable,
  palette = NULL,
  unique_threshold = 5,
  verbose = TRUE
)
```

## Arguments

- metadata:

  A dataset containing the metadata information with the sample ID as
  the first column.

- sample_id:

  The name of the column containing the sample IDs.

- variable:

  The name of the column containing the different classes.

- palette:

  A list of color palettes for the plots. The names of the list should
  match the column names in the metadata. Default is NULL.

- unique_threshold:

  The threshold to consider a numeric variable as categorical. Default
  is 5.

- verbose:

  Whether to print the summary. Default is TRUE.

## Value

A list of the qc summary of data
