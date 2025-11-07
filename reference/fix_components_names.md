# Fix component names

`fix_components_names()` fixes the names of the PCs in the PCA results.
If number of PCs is higher than 10, the function will remove the zeros
in front of the PC names (for example PC01 -\> PC1).

## Usage

``` r
fix_components_names(
  pca_res,
  components,
  by_sample,
  sample_id,
  var_name,
  type = "pca"
)
```

## Arguments

- pca_res:

  A tibble with the PCA results. Created by
  [`hd_pca()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_pca.md).

- components:

  The number of PCs to be calculated.

- by_sample:

  If TRUE, points represent samples. If FALSE, points represent
  features.

- sample_id:

  The name of the column in the data that contains the sample IDs.

- var_name:

  The name of the column in the data that contains the feature IDs.

- type:

  The type of analysis. Default is "pca".

## Value

A tibble with the PCA results with fixed PC names.
