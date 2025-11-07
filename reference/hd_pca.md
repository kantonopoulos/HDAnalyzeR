# Run PCA analysis

`hd_pca()` runs a PCA analysis on the provided data. If data contain
missing values, the function imputes them using the k-nearest neighbors
algorithm (k = 5). The number of PCs to be calculated is defined by the
user. The function returns a tibble with the PCA results.

## Usage

``` r
hd_pca(dat, components = 10, by_sample = TRUE, impute = TRUE, seed = 123)
```

## Arguments

- dat:

  An HDAnalyzeR object or a dataset in wide format and sample ID as its
  first column.

- components:

  The number of PCs to be calculated. Default is 10.

- by_sample:

  If TRUE, points represent samples. If FALSE, points represent
  features. Default is TRUE.

- impute:

  If TRUE, missing values will be imputed using the k-nearest neighbors
  algorithm (k = 5). Default is TRUE.

- seed:

  The seed to be used in the PCA analysis. Default is 123.

## Value

A list with the PCA results, PCA loadings and explained variance.

## Examples

``` r
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)

# Run the PCA analysis
hd_pca(hd_object, components = 5, by_sample = TRUE, seed = 123)
#> $pca_res
#> # A tibble: 586 × 6
#>    DAid        PC1     PC2      PC3    PC4    PC5
#>    <chr>     <dbl>   <dbl>    <dbl>  <dbl>  <dbl>
#>  1 DA00001  -3.67  -4.28   -2.34    -3.10  -2.65 
#>  2 DA00002   4.11  -2.64    2.04    -0.441 -4.43 
#>  3 DA00003  -3.34   4.72    1.41     0.881 -0.561
#>  4 DA00004  -4.78   0.443   1.41     0.107 -1.10 
#>  5 DA00005  -4.98  -3.67    0.711   -5.70  -0.807
#>  6 DA00006   0.395  0.0572 -1.90    -7.75   0.707
#>  7 DA00007 -10.5   -2.91   -0.382   -0.841 -1.61 
#>  8 DA00008   2.64  -2.01    2.75    -0.128 -0.852
#>  9 DA00009  -1.79  -0.461   2.79    -2.61  -2.71 
#> 10 DA00010   3.57   0.821  -0.00756  1.50  -2.66 
#> # ℹ 576 more rows
#> 
#> $pca_loadings
#> # A tibble: 10,000 × 3
#>    terms    value component
#>    <chr>    <dbl> <chr>    
#>  1 AARSD1 -0.133  PC1      
#>  2 ABL1   -0.198  PC1      
#>  3 ACAA1  -0.163  PC1      
#>  4 ACAN    0.0123 PC1      
#>  5 ACE2   -0.0583 PC1      
#>  6 ACOX1  -0.135  PC1      
#>  7 ACP5   -0.0626 PC1      
#>  8 ACP6   -0.0933 PC1      
#>  9 ACTA2  -0.0751 PC1      
#> 10 ACTN4  -0.0424 PC1      
#> # ℹ 9,990 more rows
#> 
#> $pca_variance
#> # A tibble: 5 × 3
#>   component percent_variance cumulative_percent_variance
#>       <int>            <dbl>                       <dbl>
#> 1         1            15.7                         15.7
#> 2         2             6.72                        22.4
#> 3         3             3.73                        26.2
#> 4         4             3.31                        29.5
#> 5         5             2.90                        32.4
#> 
#> $by_sample
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "hd_pca"

# Run the PCA analysis by feature
hd_pca(hd_object, components = 5, by_sample = FALSE, seed = 123)
#> $pca_res
#> # A tibble: 100 × 6
#>    Assay      PC1   PC2     PC3    PC4     PC5
#>    <chr>    <dbl> <dbl>   <dbl>  <dbl>   <dbl>
#>  1 AARSD1 -39.5    6.28  0.0147 -2.84   6.05  
#>  2 ABL1   -14.0   -7.20 -7.91   -5.03  -0.464 
#>  3 ACAA1   -0.858 -7.96 -3.86   11.6   -1.02  
#>  4 ACAN     6.40   4.48  1.87   -0.667 -1.10  
#>  5 ACE2     1.22   3.49 -5.89    9.29  -1.86  
#>  6 ACOX1    8.36  -7.34  1.39    1.88   0.806 
#>  7 ACP5     0.214  2.36  0.0138  2.68  -0.182 
#>  8 ACP6    -3.40   1.38 -0.348  -0.757 -1.98  
#>  9 ACTA2  -11.3    5.26 -4.83   -1.78  -5.80  
#> 10 ACTN4    9.90   1.23  0.842  -0.685 -0.0497
#> # ℹ 90 more rows
#> 
#> $pca_loadings
#> # A tibble: 58,600 × 3
#>    terms     value component
#>    <chr>     <dbl> <chr>    
#>  1 DA00001 -0.0373 PC1      
#>  2 DA00002 -0.0225 PC1      
#>  3 DA00003 -0.0429 PC1      
#>  4 DA00004 -0.0451 PC1      
#>  5 DA00005 -0.0414 PC1      
#>  6 DA00006 -0.0393 PC1      
#>  7 DA00007 -0.0390 PC1      
#>  8 DA00008 -0.0342 PC1      
#>  9 DA00009 -0.0437 PC1      
#> 10 DA00010 -0.0420 PC1      
#> # ℹ 58,590 more rows
#> 
#> $pca_variance
#> # A tibble: 5 × 3
#>   component percent_variance cumulative_percent_variance
#>       <int>            <dbl>                       <dbl>
#> 1         1            46.9                         46.9
#> 2         2             6.18                        53.1
#> 3         3             3.01                        56.1
#> 4         4             2.37                        58.5
#> 5         5             2.29                        60.8
#> 
#> $by_sample
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "hd_pca"
```
