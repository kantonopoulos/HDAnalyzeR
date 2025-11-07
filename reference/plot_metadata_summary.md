# Plot summary visualization for Sex, Age and BMI metadata

`plot_metadata_summary()` creates three plots:

- Two ridge plots for the Age and BMI distributions.

- A bar plot for the number of samples per Sex.

## Usage

``` r
plot_metadata_summary(
  metadata,
  sample_id,
  variable,
  palette = NULL,
  unique_threshold = 5
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

## Value

A list containing plots and sample counts.
