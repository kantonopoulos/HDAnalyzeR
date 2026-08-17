# Extract protein lists from the upset data

`extract_protein_list()` extracts the protein lists from the upset data.
It creates a list with the proteins for each combination of diseases. It
also creates a tibble with the proteins for each combination of
diseases.

## Usage

``` r
extract_protein_list(proteins, direction = NA_character_)
```

## Arguments

- proteins:

  A named list with the protein vector of each disease.

- direction:

  The regulation direction to record in the `up/down` column, or `NA`
  when the features are not directional. Default is `NA_character_`.

## Value

A list with the following elements:

- proteins_list: A list with the proteins for each combination of
  diseases.

- proteins_df: A tibble attributing each protein to the exact set of
  diseases it was found in.

## Details

The combinations are derived from the protein lists directly rather than
from an `UpSetR` membership matrix, which degenerates into a named
vector as soon as a single disease is summarised.
