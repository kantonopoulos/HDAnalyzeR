# Run UMAP analysis

`hd_umap()` runs a UMAP analysis on the provided data. If data contain
missing values, the function imputes them using the k-nearest neighbors
algorithm (k = 5). The number of components to be calculated is defined
by the user. The function returns a tibble with the UMAP results.

## Usage

``` r
hd_umap(dat, by_sample = TRUE, impute = TRUE, components = 2, seed = 123)
```

## Arguments

- dat:

  An HDAnalyzeR object or a dataset in wide format and sample ID as its
  first column.

- by_sample:

  If TRUE, points represent samples. If FALSE, points represent
  features. Default is TRUE.

- impute:

  If TRUE, missing values are imputed using the k-nearest neighbors
  algorithm (k = 5). Default is TRUE.

- components:

  The number of components to be calculated. Default is 10.

- seed:

  The seed to be used in the UMAP analysis. Default is 123.

## Value

A list with the UMAP results.

## Examples

``` r
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)

# Run the UMAP analysis
hd_umap(hd_object, components = 2, by_sample = TRUE, seed = 123)
#> $umap_res
#> # A tibble: 586 × 3
#>    DAid     UMAP1  UMAP2
#>    <chr>    <dbl>  <dbl>
#>  1 DA00001 -2.62   0.384
#>  2 DA00002  1.99   1.73 
#>  3 DA00003 -1.69  -1.87 
#>  4 DA00004 -2.17  -1.74 
#>  5 DA00005 -2.83   0.315
#>  6 DA00006  1.54  -0.733
#>  7 DA00007 -3.49  -0.886
#>  8 DA00008  1.54  -1.08 
#>  9 DA00009 -0.655  0.278
#> 10 DA00010  1.93   1.53 
#> # ℹ 576 more rows
#> 
#> $by_sample
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "hd_umap"

# Run the UMAP analysis by feature
hd_umap(hd_object, components = 2, by_sample = FALSE, seed = 123)
#> $umap_res
#> # A tibble: 100 × 3
#>    Assay   UMAP1   UMAP2
#>    <chr>   <dbl>   <dbl>
#>  1 AARSD1  3.50   2.25  
#>  2 ABL1    3.19   0.255 
#>  3 ACAA1   1.90  -0.679 
#>  4 ACAN   -0.488 -1.08  
#>  5 ACE2    0.330 -0.343 
#>  6 ACOX1  -1.36  -1.87  
#>  7 ACP5    0.289 -0.847 
#>  8 ACP6    0.836  0.0397
#>  9 ACTA2   1.45   1.07  
#> 10 ACTN4  -0.954 -1.50  
#> # ℹ 90 more rows
#> 
#> $by_sample
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "hd_umap"
```
