# Differential expression analysis with t-test

`hd_de_ttest()` performs differential expression analysis using t-test.
The output tibble includes the logFC, p-values, as well as the FDR
adjusted p-values. The function removes the rows with NAs in the
variables that are used to correct for as well as the variable of
interest.

## Usage

``` r
hd_de_ttest(
  dat,
  metadata = NULL,
  variable = "Disease",
  case,
  control = NULL,
  log_transform = FALSE
)
```

## Arguments

- dat:

  An HDAnalyzeR object or a dataset in wide format and sample ID as its
  first column.

- metadata:

  A dataset containing the metadata information with the sample ID as
  the first column. If a HDAnalyzeR object is provided, this parameter
  is not needed.

- variable:

  The name of the metadata variable containing the case and control
  groups.

- case:

  The case group.

- control:

  The control groups. If NULL, it will be set to all other unique values
  of the variable that are not the case.

- log_transform:

  If the data should be log transformed. Default is FALSE.

## Value

An object with the DE results.

## Details

The variable of interest should be categorical and present in the
metadata. In case your data are not already log transformed, you can set
`log_transform = TRUE` to log transform the data with base 2 before the
analysis start.

When using this function you cannot correct for other variables or run
it against a continuous variable. If you need to correct for other
variables or run the analysis against a continuous variable, use
[`hd_de_limma()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_limma.md).

## Examples

``` r
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Run differential expression analysis for AML vs all others
hd_de_ttest(hd_object, case = "AML")
#> $de_res
#> # A tibble: 100 × 8
#>    Feature  logFC CI.L  CI.R  t          P.Value  adj.P.Val Disease
#>    <chr>    <dbl> <chr> <chr> <chr>        <dbl>      <dbl> <chr>  
#>  1 ANGPT2   0.795 0.55  1.04  6.58  0.0000000147 0.00000147 AML    
#>  2 ADA      1.46  1     1.91  6.42  0.0000000409 0.00000205 AML    
#>  3 ANGPT1  -1.70  -2.28 -1.12 -5.87 0.000000312  0.0000104  AML    
#>  4 APEX1    1.30  0.79  1.82  5.1   0.00000455   0.000114   AML    
#>  5 ADGRG1   1.27  0.76  1.77  5     0.00000736   0.000147   AML    
#>  6 APP     -0.764 -1.09 -0.44 -4.69 0.0000178    0.000255   AML    
#>  7 AZU1     1.64  0.95  2.32  4.78  0.0000154    0.000255   AML    
#>  8 ALPP    -1.15  -1.68 -0.61 -4.31 0.0000688    0.000777   AML    
#>  9 ARTN     1.09  0.58  1.59  4.34  0.0000699    0.000777   AML    
#> 10 APBB1IP  1.16  0.61  1.72  4.24  0.000102     0.00102    AML    
#> # ℹ 90 more rows
#> 
#> attr(,"class")
#> [1] "hd_de"

# Run differential expression analysis for AML vs CLL
hd_de_ttest(hd_object, case = "AML", control = "CLL")
#> $de_res
#> # A tibble: 100 × 8
#>    Feature  logFC CI.L  CI.R  t         P.Value adj.P.Val Disease
#>    <chr>    <dbl> <chr> <chr> <chr>       <dbl>     <dbl> <chr>  
#>  1 ADA      1.41  0.91  1.91  5.57  0.000000386 0.0000193 AML    
#>  2 ADAM8   -1.36  -1.86 -0.87 -5.48 0.000000363 0.0000193 AML    
#>  3 AZU1     1.93  1.2   2.66  5.3   0.00000154  0.0000513 AML    
#>  4 ANGPT1  -1.74  -2.47 -1.01 -4.76 0.00000754  0.000162  AML    
#>  5 ARID4B  -1.53  -2.16 -0.89 -4.79 0.00000809  0.000162  AML    
#>  6 ACAN    -0.679 -0.97 -0.39 -4.64 0.0000116   0.000193  AML    
#>  7 ARTN     1.32  0.76  1.89  4.67  0.0000135   0.000193  AML    
#>  8 ADGRG2  -0.641 -0.93 -0.35 -4.35 0.0000353   0.000441  AML    
#>  9 ACP6    -0.814 -1.19 -0.44 -4.31 0.0000411   0.000457  AML    
#> 10 ADAMTS8 -0.758 -1.12 -0.4  -4.18 0.0000687   0.000687  AML    
#> # ℹ 90 more rows
#> 
#> attr(,"class")
#> [1] "hd_de"
```
