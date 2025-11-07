# Print the summary of the quality control results

`print_summary()` prints the summary of the quality control results of
the input dataset. It includes the number of samples and variables, the
counts of each class, the percentage of NAs in each column and row, the
normality test results, the protein-protein correlations above a certain
threshold, and the correlation heatmap.

## Usage

``` r
print_summary(
  sample_n,
  var_n,
  class_summary,
  na_percentage_col,
  na_percentage_row = NULL,
  cor_results = NULL,
  cor_threshold = 0.8
)
```

## Arguments

- sample_n:

  The number of samples.

- var_n:

  The number of variables.

- class_summary:

  A table with the counts of each class in the dataframe.

- na_percentage_col:

  A tibble with the column names and the percentage of NAs in each
  column.

- na_percentage_row:

  A tibble with the DAids and the percentage of NAs in each row.

- cor_results:

  A tibble with the filtered protein pairs and their correlation values.

- cor_threshold:

  The reporting protein-protein correlation threshold.

## Value

Prints the summary, returs NULL
