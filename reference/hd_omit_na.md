# Omit missing values

`hd_omit_na()` removes rows with missing values from a dataset. It
allows the user to specify the columns to consider for the removal of
missing values. If no columns are provided, the function removes rows
with missing values in any column.

## Usage

``` r
hd_omit_na(dat, columns = NULL)
```

## Arguments

- dat:

  An HDAnalyzeR object or a dataset in wide format and sample ID as its
  first column.

- columns:

  The columns to consider for the removal of missing values.

## Value

The dataset without the rows containing missing values.

## Examples

``` r
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)
hd_object$data
#> # A tibble: 586 × 101
#>    DAid    AARSD1   ABL1  ACAA1    ACAN    ACE2  ACOX1   ACP5    ACP6  ACTA2
#>    <chr>    <dbl>  <dbl>  <dbl>   <dbl>   <dbl>  <dbl>  <dbl>   <dbl>  <dbl>
#>  1 DA00001   3.39  2.76   1.71   0.0333  1.76   -0.919 1.54    2.15    2.81 
#>  2 DA00002   1.42  1.25  -0.816 -0.459   0.826  -0.902 0.647   1.30    0.798
#>  3 DA00003  NA    NA     NA      0.989  NA       0.330 1.37   NA      NA    
#>  4 DA00004   3.41  3.38   1.69  NA       1.52   NA     0.841   0.582   1.70 
#>  5 DA00005   5.01  5.05   0.128  0.401  -0.933  -0.584 0.0265  1.16    2.73 
#>  6 DA00006   6.83  1.18  -1.74  -0.156   1.53   -0.721 0.620   0.527   0.772
#>  7 DA00007  NA    NA      3.96   0.682   3.14    2.62  1.47    2.25    2.01 
#>  8 DA00008   2.78  0.812 -0.552  0.982  -0.101  -0.304 0.376  -0.826   1.52 
#>  9 DA00009   4.39  3.34  -0.452 -0.868   0.395   1.71  1.49   -0.0285  0.200
#> 10 DA00010   1.83  1.21  -0.912 -1.04   -0.0918 -0.304 1.69    0.0920  2.04 
#> # ℹ 576 more rows
#> # ℹ 91 more variables: ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>,
#> #   ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>,
#> #   ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>,
#> #   ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>,
#> #   AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>,
#> #   AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, …

# Data after removing missing values
res <- hd_omit_na(hd_object)
res$data
#> # A tibble: 442 × 101
#>    DAid    AARSD1  ABL1  ACAA1    ACAN    ACE2  ACOX1    ACP5    ACP6 ACTA2
#>    <chr>    <dbl> <dbl>  <dbl>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl> <dbl>
#>  1 DA00001   3.39 2.76   1.71   0.0333  1.76   -0.919  1.54    2.15   2.81 
#>  2 DA00002   1.42 1.25  -0.816 -0.459   0.826  -0.902  0.647   1.30   0.798
#>  3 DA00005   5.01 5.05   0.128  0.401  -0.933  -0.584  0.0265  1.16   2.73 
#>  4 DA00008   2.78 0.812 -0.552  0.982  -0.101  -0.304  0.376  -0.826  1.52 
#>  5 DA00009   4.39 3.34  -0.452 -0.868   0.395   1.71   1.49   -0.0285 0.200
#>  6 DA00010   1.83 1.21  -0.912 -1.04   -0.0918 -0.304  1.69    0.0920 2.04 
#>  7 DA00011   3.48 4.96   3.50  -0.338   4.48    1.26   2.18    1.62   1.79 
#>  8 DA00012   4.31 0.710 -1.44  -0.218  -0.469  -0.361 -0.0714 -1.30   2.86 
#>  9 DA00013   1.31 2.52   1.11   0.997   4.56   -1.35   0.833   2.33   3.57 
#> 10 DA00014   6.34 7.25   5.12   0.0193  1.29    0.370 -0.382   0.830  3.89 
#> # ℹ 432 more rows
#> # ℹ 91 more variables: ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>,
#> #   ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>,
#> #   ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>,
#> #   ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>,
#> #   AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>,
#> #   AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, …

# Data after removing missing values in specific columns
res <- hd_omit_na(hd_object, columns = "AARSD1")
res$data
#> # A tibble: 552 × 101
#>    DAid    AARSD1  ABL1  ACAA1    ACAN    ACE2  ACOX1    ACP5    ACP6 ACTA2
#>    <chr>    <dbl> <dbl>  <dbl>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl> <dbl>
#>  1 DA00001   3.39 2.76   1.71   0.0333  1.76   -0.919  1.54    2.15   2.81 
#>  2 DA00002   1.42 1.25  -0.816 -0.459   0.826  -0.902  0.647   1.30   0.798
#>  3 DA00004   3.41 3.38   1.69  NA       1.52   NA      0.841   0.582  1.70 
#>  4 DA00005   5.01 5.05   0.128  0.401  -0.933  -0.584  0.0265  1.16   2.73 
#>  5 DA00006   6.83 1.18  -1.74  -0.156   1.53   -0.721  0.620   0.527  0.772
#>  6 DA00008   2.78 0.812 -0.552  0.982  -0.101  -0.304  0.376  -0.826  1.52 
#>  7 DA00009   4.39 3.34  -0.452 -0.868   0.395   1.71   1.49   -0.0285 0.200
#>  8 DA00010   1.83 1.21  -0.912 -1.04   -0.0918 -0.304  1.69    0.0920 2.04 
#>  9 DA00011   3.48 4.96   3.50  -0.338   4.48    1.26   2.18    1.62   1.79 
#> 10 DA00012   4.31 0.710 -1.44  -0.218  -0.469  -0.361 -0.0714 -1.30   2.86 
#> # ℹ 542 more rows
#> # ℹ 91 more variables: ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>,
#> #   ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>,
#> #   ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>,
#> #   ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>,
#> #   AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>,
#> #   AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, …
```
