# Log transform data with base 2

`hd_log_transform()` log transforms the data in a dataset. It replaces
non-positive values (\<= 0) with NA values and informs the user.

## Usage

``` r
hd_log_transform(dat)
```

## Arguments

- dat:

  An HDAnalyzeR object or a dataset in wide format.

## Value

The log-transformed data.

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

# Log transform the data
hd_object_transformed <- hd_log_transform(hd_object)
#> Warning: Data contains non-positive values (<= 0). These will be replaced with NA during log transformation.
# Normally you should not transform Olink data as they are already log-transformed
hd_object_transformed$data
#> # A tibble: 586 × 101
#>    DAid   AARSD1   ABL1  ACAA1    ACAN   ACE2  ACOX1   ACP5   ACP6  ACTA2  ACTN4
#>    <chr>   <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1 DA000…  1.76   1.46   0.771 -4.91    0.812 NA      0.620  1.11   1.49  -0.431
#>  2 DA000…  0.509  0.319 NA     NA      -0.275 NA     -0.628  0.379 -0.325 NA    
#>  3 DA000… NA     NA     NA     -0.0155 NA     -1.60   0.456 NA     NA     NA    
#>  4 DA000…  1.77   1.76   0.759 NA       0.605 NA     -0.250 -0.781  0.766 -3.21 
#>  5 DA000…  2.33   2.34  -2.97  -1.32   NA     NA     -5.24   0.210  1.45  -1.51 
#>  6 DA000…  2.77   0.242 NA     NA       0.611 NA     -0.690 -0.925 -0.374 NA    
#>  7 DA000… NA     NA      1.99  -0.551   1.65   1.39   0.559  1.17   1.01  -2.56 
#>  8 DA000…  1.48  -0.301 NA     -0.0269 NA     NA     -1.41  NA      0.605 NA    
#>  9 DA000…  2.14   1.74  NA     NA      -1.34   0.771  0.573 NA     -2.32  NA    
#> 10 DA000…  0.871  0.280 NA     NA      NA     NA      0.757 -3.44   1.03  -0.997
#> # ℹ 576 more rows
#> # ℹ 90 more variables: ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>, ADAM15 <dbl>,
#> #   ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>, ADAMTS16 <dbl>,
#> #   ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>, ADGRG1 <dbl>,
#> #   ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>, AGR3 <dbl>,
#> #   AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>, AIF1 <dbl>,
#> #   AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, AKT1S1 <dbl>, …
```
