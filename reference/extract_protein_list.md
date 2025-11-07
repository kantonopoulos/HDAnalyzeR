# Extract protein lists from the upset data

`extract_protein_list()` extracts the protein lists from the upset data.
It creates a list with the proteins for each combination of diseases. It
also creates a tibble with the proteins for each combination of
diseases.

## Usage

``` r
extract_protein_list(upset_data, proteins)
```

## Arguments

- upset_data:

  A tibble with the upset data.

- proteins:

  A list with the protein lists for each disease.

## Value

A list with the following elements:

- proteins_list: A list with the proteins for each combination of
  diseases.

- proteins_df: A tibble with the proteins for each combination of
  diseases.
