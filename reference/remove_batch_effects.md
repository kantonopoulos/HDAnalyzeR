# Remove batch effects

`remove_batch_effects()` removes batch effects from the data using the
limma package. It converts the dataframe into matrix and transposes it
to get it ready for limma. It removes the batch effects and then
converts the data back to tibble.

## Usage

``` r
remove_batch_effects(wide_data, metadata, sample_id, batch, batch2 = NULL)
```

## Arguments

- wide_data:

  A tibble containing the data to be normalized. The data should be in
  wide format.

- metadata:

  A tibble containing the metadata information.

- sample_id:

  The column containing the sample ID information.

- batch:

  The metadata column containing the batch information.

- batch2:

  The metadata column containing the second batch information. Default
  is NULL.

## Value

A tibble containing the data without batch effects.
