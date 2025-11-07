# Initialize HDAnalyzeR object

`hd_initialize()` initializes an HDAnalyzeR object with the data,
metadata, and other parameters. This object can be used to run various
analyses in the package.

## Usage

``` r
hd_initialize(
  dat,
  metadata = NULL,
  is_wide = FALSE,
  sample_id = "DAid",
  var_name = "Assay",
  value_name = "NPX"
)
```

## Arguments

- dat:

  A tibble containing the omics data.

- metadata:

  A tibble containing the metadata. Default is NULL.

- is_wide:

  A logical indicating if the data is in wide format. Default is FALSE.

- sample_id:

  The name of the column containing the sample IDs. Default is "DAid".

- var_name:

  The name of the column containing the variable names. Default is
  "Assay".

- value_name:

  The name of the column containing the values. Default is "NPX".

## Value

An HDAnalyzeR object.

## Details

It is strongly recommended to use this function to initialize an
HDAnalyzeR object before starting any analysis. This initialization step
ensures that your input data and metadata are correctly formatted and
compatible with the package. Specifically, the function validates your
data and automatically converts it to wide format if provided in long
format (`is_wide`).

While it is possible to use HDAnalyzeR functions on data outside of an
HDAnalyzeR object, users must ensure that the data adheres to the
required structure: the first column must contain sample IDs, followed
by numeric columns (protein expression) in wide format. Additionally,
the metadata must include the same set of sample IDs. Proper
initialization or careful adherence to these requirements is crucial for
accurate and efficient analysis. Defaults are provided according to the
Human Disease Blood Atlas Olink data format.

## Examples

``` r
# Initialize an HDAnalyzeR object
hd_initialize(example_data, example_metadata)
#> $data
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
#> 
#> $metadata
#> # A tibble: 586 × 9
#>    DAid    Sample     Disease Stage   Grade Sex     Age   BMI Cohort
#>    <chr>   <chr>      <chr>   <chr>   <chr> <chr> <dbl> <dbl> <chr> 
#>  1 DA00001 AML_syn_1  AML     2       NA    F        42  22.7 UCAN  
#>  2 DA00002 AML_syn_2  AML     Unknown NA    M        69  33.1 UCAN  
#>  3 DA00003 AML_syn_3  AML     2       NA    F        61  26.2 UCAN  
#>  4 DA00004 AML_syn_4  AML     Unknown NA    M        54  28.1 UCAN  
#>  5 DA00005 AML_syn_5  AML     2       NA    F        57  21.4 UCAN  
#>  6 DA00006 AML_syn_6  AML     Unknown NA    M        86  33.9 UCAN  
#>  7 DA00007 AML_syn_7  AML     1       NA    F        85  28.7 UCAN  
#>  8 DA00008 AML_syn_8  AML     3       NA    F        88  32.6 UCAN  
#>  9 DA00009 AML_syn_9  AML     Unknown NA    M        80  26.1 UCAN  
#> 10 DA00010 AML_syn_10 AML     3       NA    M        48  33.8 UCAN  
#> # ℹ 576 more rows
#> 
#> $sample_id
#> [1] "DAid"
#> 
#> $var_name
#> [1] "Assay"
#> 
#> $value_name
#> [1] "NPX"
#> 
#> attr(,"class")
#> [1] "HDAnalyzeR"
```
