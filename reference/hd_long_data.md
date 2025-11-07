# Convert omics data to long format

`hd_long_data()` transforms omics data from wide to long format.

## Usage

``` r
hd_long_data(dat, exclude = "DAid", names_to = "Assay", values_to = "NPX")
```

## Arguments

- dat:

  A tibble containing data in wide format.

- exclude:

  The name of the columns to exclude from the transformation. Default is
  "DAid".

- names_to:

  The name of the column to create for the variable names. Default is
  "Assay".

- values_to:

  The name of the column to create for the values. Default is "NPX".

## Value

A tibble containing the data in long format.

## Examples

``` r
# Olink data in wide format
example_data_wide <- hd_widen_data(example_data)
example_data_wide
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

# Transform Olink data in long format
hd_long_data(example_data_wide)
#> # A tibble: 58,600 × 3
#>    DAid    Assay      NPX
#>    <chr>   <chr>    <dbl>
#>  1 DA00001 AARSD1  3.39  
#>  2 DA00001 ABL1    2.76  
#>  3 DA00001 ACAA1   1.71  
#>  4 DA00001 ACAN    0.0333
#>  5 DA00001 ACE2    1.76  
#>  6 DA00001 ACOX1  -0.919 
#>  7 DA00001 ACP5    1.54  
#>  8 DA00001 ACP6    2.15  
#>  9 DA00001 ACTA2   2.81  
#> 10 DA00001 ACTN4   0.742 
#> # ℹ 58,590 more rows

# Use Sample name instead of Sample ID and Olink IDs instead of Assay names
example_data_wide <- hd_widen_data(example_data,
                                   exclude = "Sample",
                                   names_from = "OlinkID")
hd_long_data(example_data_wide, exclude = "Sample", names_to = "OlinkID")
#> # A tibble: 58,600 × 3
#>    Sample    OlinkID      NPX
#>    <chr>     <chr>      <dbl>
#>  1 AML_syn_1 OID21311  3.39  
#>  2 AML_syn_1 OID21280  2.76  
#>  3 AML_syn_1 OID21269  1.71  
#>  4 AML_syn_1 OID20159  0.0333
#>  5 AML_syn_1 OID20105  1.76  
#>  6 AML_syn_1 OID20124 -0.919 
#>  7 AML_syn_1 OID20314  1.54  
#>  8 AML_syn_1 OID21432  2.15  
#>  9 AML_syn_1 OID20079  2.81  
#> 10 AML_syn_1 OID20435  0.742 
#> # ℹ 58,590 more rows
```
