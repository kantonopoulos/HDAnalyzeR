# Differential expression analysis with limma

`hd_de_limma()` performs differential expression analysis using the
`limma` package. It can correct the results for metadata columns like
Sex, Age, BMI, etc. The output tibble includes the logFC, p-values, as
well as the FDR adjusted p-values. The function removes the rows with
NAs in the variables that are used to correct for as well as the
variable of interest.

## Usage

``` r
hd_de_limma(
  dat,
  metadata = NULL,
  variable = "Disease",
  case,
  control = NULL,
  correct = NULL,
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
  groups or a continuous variable.

- case:

  The case group. In case of a continuous variable, it must be NULL.

- control:

  The control groups. If NULL, it will be set to all other unique values
  of the variable that are not the case. In case of a continuous
  variable, it must be NULL.

- correct:

  The variables to correct the results with. Default is NULL.

- log_transform:

  If the data should be log transformed. Default is FALSE.

## Value

An object with the DE results.

## Details

The variable of interest can be either categorical or continuous. This
variable and all variables in the `correct` argument should be present
in the metadata. Also, the variable of interest should not be present in
the `correct` argument.

In case of a continuous variable or if you are correcting based on a
continuous variable, the variable should be numeric and contain at least
6 unique variables. In case your data are not already log transformed,
you can set `log_transform = TRUE` to log transform the data with base 2
before the analysis start.

## Examples

``` r
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Run differential expression analysis for AML vs all others
hd_de_limma(hd_object, case = "AML")
#> $de_res
#> # A tibble: 100 × 10
#>    Feature  logFC   CI.L   CI.R AveExpr     t  P.Value adj.P.Val     B Disease
#>    <chr>    <dbl>  <dbl>  <dbl>   <dbl> <dbl>    <dbl>     <dbl> <dbl> <chr>  
#>  1 ADA      1.46   1.17   1.75   0.924   9.83 3.29e-21  3.29e-19 37.3  AML    
#>  2 AZU1     1.53   1.20   1.86   0.226   9.10 1.32e-18  6.62e-17 31.4  AML    
#>  3 APEX1    1.56   1.17   1.95   0.517   7.90 1.40e-14  4.67e-13 22.3  AML    
#>  4 APBB1IP  1.17   0.855  1.49  -0.237   7.24 1.48e-12  3.69e-11 17.8  AML    
#>  5 ANGPT1  -1.70  -2.18  -1.21   1.72   -6.92 1.24e-11  2.48e-10 15.6  AML    
#>  6 ADGRG1   1.23   0.837  1.63   1.54    6.12 1.75e- 9  2.92e- 8 10.8  AML    
#>  7 ANGPT2   0.773  0.522  1.02   0.920   6.04 2.70e- 9  3.86e- 8 10.3  AML    
#>  8 ARTN     0.829  0.552  1.11   0.429   5.88 6.95e- 9  8.69e- 8  9.47 AML    
#>  9 APP     -0.823 -1.15  -0.493  1.17   -4.90 1.24e- 6  1.35e- 5  4.42 AML    
#> 10 AGRP     0.756  0.452  1.06   0.0229  4.88 1.35e- 6  1.35e- 5  4.35 AML    
#> # ℹ 90 more rows
#> 
#> attr(,"class")
#> [1] "hd_de"

# Run differential expression analysis for AML vs CLL and MYEL and correct for metadata variables
hd_de_limma(hd_object,
                case = "AML",
                control = c("CLL", "MYEL"),
                correct = c("Sex", "Age", "BMI"))
#> $de_res
#> # A tibble: 100 × 10
#>    Feature   logFC   CI.L   CI.R AveExpr     t  P.Value adj.P.Val      B Disease
#>    <chr>     <dbl>  <dbl>  <dbl>   <dbl> <dbl>    <dbl>     <dbl>  <dbl> <chr>  
#>  1 ADA       1.35   0.959  1.74    1.46   6.86 2.23e-10   2.23e-8 13.2   AML    
#>  2 AZU1      1.77   1.23   2.31    0.592  6.49 1.39e- 9   6.95e-8 11.4   AML    
#>  3 ANGPT1   -1.77  -2.37  -1.18    1.26  -5.92 2.59e- 8   8.64e-7  8.60  AML    
#>  4 ACP6     -0.814 -1.15  -0.482   1.36  -4.85 3.43e- 6   8.56e-5  3.87  AML    
#>  5 ARHGEF12 -1.30  -1.89  -0.716   3.38  -4.40 2.21e- 5   4.35e-4  2.08  AML    
#>  6 APP      -0.864 -1.26  -0.472   0.959 -4.36 2.61e- 5   4.35e-4  1.93  AML    
#>  7 ACAN     -0.646 -0.946 -0.346   0.596 -4.26 3.81e- 5   5.44e-4  1.56  AML    
#>  8 AGR2     -1.18  -1.82  -0.545   1.78  -3.67 3.60e- 4   4.29e-3 -0.539 AML    
#>  9 ATOX1    -0.897 -1.38  -0.410   3.23  -3.64 3.86e- 4   4.29e-3 -0.607 AML    
#> 10 ANXA11   -0.632 -0.990 -0.274   0.973 -3.49 6.55e- 4   6.55e-3 -1.11  AML    
#> # ℹ 90 more rows
#> 
#> attr(,"class")
#> [1] "hd_de"

# Run differential expression analysis for continuous variable
hd_de_limma(hd_object, variable = "Age", case = NULL, correct = c("Sex"))
#> $de_res
#> # A tibble: 100 × 8
#>    Feature      logFC as.factor.Sex.F as.factor.Sex.M AveExpr     F   P.Value
#>    <chr>        <dbl>           <dbl>           <dbl>   <dbl> <dbl>     <dbl>
#>  1 ADAMTS15 -0.000719            3.09            2.92    2.99 1874. 1.02e-291
#>  2 AARSD1    0.000327            2.96            3.25    3.13 1609. 6.06e-275
#>  3 AKT1S1    0.00154             3.28            3.46    3.47 1479. 1.56e-265
#>  4 ATG4A    -0.00157             2.56            2.71    2.55 1138. 1.26e-238
#>  5 ATOX1    -0.00166             3.02            3.18    2.97 1062. 5.65e-233
#>  6 ADM       0.00536             1.53            1.47    1.87  953. 5.49e-224
#>  7 AK1      -0.00373             2.51            2.66    2.34  786. 2.33e-202
#>  8 AKR1B1   -0.000171            2.28            2.33    2.29  783. 1.59e-200
#>  9 ATP5IF1  -0.00321             3.66            4.02    3.60  741. 5.59e-197
#> 10 ARHGEF12 -0.00163             3.19            3.56    3.26  684. 1.08e-187
#> # ℹ 90 more rows
#> # ℹ 1 more variable: adj.P.Val <dbl>
#> 
#> attr(,"class")
#> [1] "hd_de"
```
