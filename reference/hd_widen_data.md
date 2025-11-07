# Convert omics data to wide format

`hd_widen_data()` transforms omics data from long to wide format with
variables like different "Assays" as column names and expression values
like "NPX" as values.

## Usage

``` r
hd_widen_data(dat, exclude = "DAid", names_from = "Assay", values_from = "NPX")
```

## Arguments

- dat:

  A tibble containing data in long format.

- exclude:

  The name of the columns to exclude from the transformation. Default is
  "DAid".

- names_from:

  The name of the column containing the variable names. Default is
  "Assay".

- values_from:

  The name of the column containing the values. Default is "NPX".

## Value

A tibble containing the data in wide format.

## Examples

``` r
# Olink data in long format
example_data
#> # A tibble: 56,142 × 10
#>    DAid    Sample   OlinkID UniProt Assay Panel     NPX Assay_Warning QC_Warning
#>    <chr>   <chr>    <chr>   <chr>   <chr> <chr>   <dbl> <chr>         <chr>     
#>  1 DA00001 AML_syn… OID213… Q9BTE6  AARS… Onco…  3.39   PASS          PASS      
#>  2 DA00001 AML_syn… OID212… P00519  ABL1  Onco…  2.76   PASS          PASS      
#>  3 DA00001 AML_syn… OID212… P09110  ACAA1 Onco…  1.71   PASS          PASS      
#>  4 DA00001 AML_syn… OID201… P16112  ACAN  Card…  0.0333 PASS          PASS      
#>  5 DA00001 AML_syn… OID201… Q9BYF1  ACE2  Card…  1.76   PASS          PASS      
#>  6 DA00001 AML_syn… OID201… Q15067  ACOX1 Card… -0.919  PASS          PASS      
#>  7 DA00001 AML_syn… OID203… P13686  ACP5  Card…  1.54   PASS          PASS      
#>  8 DA00001 AML_syn… OID214… Q9NPH0  ACP6  Onco…  2.15   PASS          PASS      
#>  9 DA00001 AML_syn… OID200… P62736  ACTA2 Card…  2.81   PASS          PASS      
#> 10 DA00001 AML_syn… OID204… O43707  ACTN4 Infl…  0.742  PASS          PASS      
#> # ℹ 56,132 more rows
#> # ℹ 1 more variable: PlateID <chr>

# Transform Olink data in wide format
hd_widen_data(example_data)
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

# Use Sample name instead of Sample ID and Olink IDs instead of Assay names
hd_widen_data(example_data, exclude = "Sample", names_from = "OlinkID")
#> # A tibble: 586 × 101
#>    Sample     OID21311 OID21280 OID21269 OID20159 OID20105 OID20124 OID20314
#>    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1 AML_syn_1      3.39    2.76     1.71    0.0333   1.76     -0.919   1.54  
#>  2 AML_syn_2      1.42    1.25    -0.816  -0.459    0.826    -0.902   0.647 
#>  3 AML_syn_3     NA      NA       NA       0.989   NA         0.330   1.37  
#>  4 AML_syn_4      3.41    3.38     1.69   NA        1.52     NA       0.841 
#>  5 AML_syn_5      5.01    5.05     0.128   0.401   -0.933    -0.584   0.0265
#>  6 AML_syn_6      6.83    1.18    -1.74   -0.156    1.53     -0.721   0.620 
#>  7 AML_syn_7     NA      NA        3.96    0.682    3.14      2.62    1.47  
#>  8 AML_syn_8      2.78    0.812   -0.552   0.982   -0.101    -0.304   0.376 
#>  9 AML_syn_9      4.39    3.34    -0.452  -0.868    0.395     1.71    1.49  
#> 10 AML_syn_10     1.83    1.21    -0.912  -1.04    -0.0918   -0.304   1.69  
#> # ℹ 576 more rows
#> # ℹ 93 more variables: OID21432 <dbl>, OID20079 <dbl>, OID20435 <dbl>,
#> #   OID20137 <dbl>, OID20645 <dbl>, OID20394 <dbl>, OID20109 <dbl>,
#> #   OID20651 <dbl>, OID21039 <dbl>, OID20249 <dbl>, OID21275 <dbl>,
#> #   OID20089 <dbl>, OID21502 <dbl>, OID21188 <dbl>, OID20755 <dbl>,
#> #   OID20260 <dbl>, OID21294 <dbl>, OID20189 <dbl>, OID20117 <dbl>,
#> #   OID21467 <dbl>, OID20756 <dbl>, OID20896 <dbl>, OID21243 <dbl>, …
```
