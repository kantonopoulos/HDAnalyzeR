# Filter an HD object based on a variable

Filter an HD object based on a variable, either in the data or metadata
component.

## Usage

``` r
hd_filter(hd_obj, variable, values, flag, verbose = TRUE)
```

## Arguments

- hd_obj:

  An HD object to be filtered.

- variable:

  The name of the variable to filter on.

- values:

  The values to filter on (for categorical variables) or the value to
  compare to (for continuous variables).

- flag:

  The type of filter to apply (see details).

- verbose:

  A logical indicating whether to print messages during filtering.
  Default is TRUE.

## Value

The filtered HD object

## Details

The `flag` argument can take the following values:

- "k" : Keep only rows where the variable matches the values
  (categorical)

- "r" : Remove rows where the variable matches the values (categorical)

- "=" : Keep only rows where the variable equals the value (continuous)

- "\<" : Keep only rows where the variable is less than the value
  (continuous)

- "\<=" : Keep only rows where the variable is less than or equal to the
  value (continuous)

- "\>" : Keep only rows where the variable is greater than the value
  (continuous)

- "\>=" : Keep only rows where the variable is greater than or equal to
  the value (continuous)

- "!=" : Keep only rows where the variable is not equal to the value
  (continuous)

## Examples

``` r
# Create the HDAnalyzeR object providing the data and metadata
hd_obj <- hd_initialize(example_data, example_metadata)
hd_obj
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

# Filter by categorical variable
hd_filter(hd_obj, variable = "Sex", values = "F", flag = "k")
#> Variable Sex is categorical
#> Filtering complete. Rows remaining:366
#> $data
#> # A tibble: 366 × 101
#>    DAid  AARSD1   ABL1  ACAA1    ACAN   ACE2   ACOX1    ACP5   ACP6 ACTA2  ACTN4
#>    <chr>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl>   <dbl>  <dbl> <dbl>  <dbl>
#>  1 DA00…   3.39  2.76   1.71   0.0333  1.76  -0.919   1.54    2.15   2.81  0.742
#>  2 DA00…  NA    NA     NA      0.989  NA      0.330   1.37   NA     NA    NA    
#>  3 DA00…   5.01  5.05   0.128  0.401  -0.933 -0.584   0.0265  1.16   2.73  0.350
#>  4 DA00…  NA    NA      3.96   0.682   3.14   2.62    1.47    2.25   2.01  0.170
#>  5 DA00…   2.78  0.812 -0.552  0.982  -0.101 -0.304   0.376  -0.826  1.52 -0.597
#>  6 DA00…   3.48  4.96   3.50  -0.338   4.48   1.26    2.18    1.62   1.79  0.233
#>  7 DA00…   4.31  0.710 -1.44  -0.218  -0.469 -0.361  -0.0714 -1.30   2.86  0.129
#>  8 DA00…   1.79  1.36   0.106 -0.372   3.40  -1.19    1.77    1.07   2.00  0.980
#>  9 DA00…   3.59  3.38   1.79  -0.303   1.59   0.604   1.71   -0.837  1.65  1.13 
#> 10 DA00…   1.80  1.70   2.77  -1.04    1.33  -0.0247  1.02    0.112  2.58  1.14 
#> # ℹ 356 more rows
#> # ℹ 90 more variables: ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>, ADAM15 <dbl>,
#> #   ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>, ADAMTS16 <dbl>,
#> #   ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>, ADGRG1 <dbl>,
#> #   ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>, AGR3 <dbl>,
#> #   AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>, AIF1 <dbl>,
#> #   AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, AKT1S1 <dbl>, …
#> 
#> $metadata
#> # A tibble: 366 × 9
#>    DAid    Sample     Disease Stage Grade Sex     Age   BMI Cohort
#>    <chr>   <chr>      <chr>   <chr> <chr> <chr> <dbl> <dbl> <chr> 
#>  1 DA00001 AML_syn_1  AML     2     NA    F        42  22.7 UCAN  
#>  2 DA00003 AML_syn_3  AML     2     NA    F        61  26.2 UCAN  
#>  3 DA00005 AML_syn_5  AML     2     NA    F        57  21.4 UCAN  
#>  4 DA00007 AML_syn_7  AML     1     NA    F        85  28.7 UCAN  
#>  5 DA00008 AML_syn_8  AML     3     NA    F        88  32.6 UCAN  
#>  6 DA00011 AML_syn_11 AML     3     NA    F        54  34.7 UCAN  
#>  7 DA00012 AML_syn_12 AML     3     NA    F        78  21.4 UCAN  
#>  8 DA00016 AML_syn_16 AML     3     NA    F        78  25.4 UCAN  
#>  9 DA00019 AML_syn_19 AML     1     NA    F        81  22.9 UCAN  
#> 10 DA00020 AML_syn_20 AML     3     NA    F        65  24.1 UCAN  
#> # ℹ 356 more rows
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

# Filter by continuous variable
hd_filter(hd_obj, variable = "Age", values = 80, flag = ">")
#> Variable Age is continuous
#> Filtering complete. Rows remaining:142
#> $data
#> # A tibble: 142 × 101
#>    DAid  AARSD1   ABL1  ACAA1    ACAN     ACE2   ACOX1  ACP5   ACP6 ACTA2  ACTN4
#>    <chr>  <dbl>  <dbl>  <dbl>   <dbl>    <dbl>   <dbl> <dbl>  <dbl> <dbl>  <dbl>
#>  1 DA00…   6.83  1.18  -1.74  -0.156   1.53    -0.721  0.620  0.527 0.772 NA    
#>  2 DA00…  NA    NA      3.96   0.682   3.14     2.62   1.47   2.25  2.01   0.170
#>  3 DA00…   2.78  0.812 -0.552  0.982  -0.101   -0.304  0.376 -0.826 1.52  -0.597
#>  4 DA00…   1.31  2.52   1.11   0.997   4.56    -1.35   0.833  2.33  3.57   1.37 
#>  5 DA00…   3.59  3.38   1.79  -0.303   1.59     0.604  1.71  -0.837 1.65   1.13 
#>  6 DA00…   2.40  3.50   2.47  -0.0788  2.25    -0.0102 0.581  1.28  1.45   0.320
#>  7 DA00…   2.53  3.00   0.166  0.707  -0.00699  1.05   0.898  1.53  2.02   0.127
#>  8 DA00…   2.93  1.55  -0.126  0.868   0.963    0.249  1.15   1.16  1.32  -0.753
#>  9 DA00…   4.29  1.66   4.13   1.56    3.66    -1.06   2.09   1.26  2.78   0.786
#> 10 DA00…   1.33  0.406  0.934  0.253   0.133    0.117  0.289  0.878 1.43   1.34 
#> # ℹ 132 more rows
#> # ℹ 90 more variables: ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>, ADAM15 <dbl>,
#> #   ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>, ADAMTS16 <dbl>,
#> #   ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>, ADGRG1 <dbl>,
#> #   ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>, AGR3 <dbl>,
#> #   AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>, AIF1 <dbl>,
#> #   AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, AKT1S1 <dbl>, …
#> 
#> $metadata
#> # A tibble: 142 × 9
#>    DAid    Sample     Disease Stage   Grade Sex     Age   BMI Cohort         
#>    <chr>   <chr>      <chr>   <chr>   <chr> <chr> <dbl> <dbl> <chr>          
#>  1 DA00006 AML_syn_6  AML     Unknown NA    M        86  33.9 UCAN           
#>  2 DA00007 AML_syn_7  AML     1       NA    F        85  28.7 UCAN           
#>  3 DA00008 AML_syn_8  AML     3       NA    F        88  32.6 UCAN           
#>  4 DA00013 AML_syn_13 AML     Unknown NA    M        81  20.9 UCAN           
#>  5 DA00019 AML_syn_19 AML     1       NA    F        81  22.9 UCAN           
#>  6 DA00031 AML_syn_31 AML     3       NA    M        85  22   UCAN           
#>  7 DA00051 BRC_syn_1  BRC     2       NA    F        82  30.1 Gender_specific
#>  8 DA00053 BRC_syn_3  BRC     2       NA    F        82  33   Gender_specific
#>  9 DA00059 BRC_syn_9  BRC     2       NA    F        81  33.3 Gender_specific
#> 10 DA00060 BRC_syn_10 BRC     2       NA    F        90  26.2 Gender_specific
#> # ℹ 132 more rows
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
