# Summarize the quality control results of Olink data

`qc_summary_data()` summarizes the quality control results of the input
dataset. It can handles both long and wide dataframes. The function
checks the column types, calculates the percentage of NAs in each column
and row, performs a normality test, calculates the protein-protein
correlations, and creates a heatmap of the correlations. The user can
specify the reporting protein-protein correlation threshold.

## Usage

``` r
qc_summary_data(
  wide_data,
  sample_id,
  unique_threshold = 5,
  cor_threshold = 0.8,
  cor_method = "pearson",
  verbose = TRUE
)
```

## Arguments

- wide_data:

  A dataset in wide format and sample_id as its first column.

- sample_id:

  The name of the column containing the sample IDs.

- unique_threshold:

  The threshold to consider a numeric variable as categorical. Default
  is 5.

- cor_threshold:

  The threshold to consider a protein-protein correlation as high.
  Default is 0.8.

- cor_method:

  The method to calculate the correlation. Default is "pearson".

- verbose:

  Whether to print the summary. Default is TRUE.

## Value

A list containing the qc summery of data
