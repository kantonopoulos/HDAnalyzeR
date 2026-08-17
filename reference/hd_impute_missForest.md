# Impute via missForest

`impute_missForest()` imputes missing values in a dataset using the
`missForest` method. It can also display the percentage of missing
values in each column before imputation.

## Usage

``` r
hd_impute_missForest(
  dat,
  maxiter = 10,
  ntree = 100,
  parallelize = "no",
  seed = 123,
  verbose = TRUE
)
```

## Arguments

- dat:

  An HDAnalyzeR object or a dataset in wide format and sample ID as its
  first column.

- maxiter:

  The maximum number of iterations.

- ntree:

  The number of trees to grow.

- parallelize:

  If "no", the imputation is done in a single core. If "variables", the
  imputation is done in parallel for each variable. If "forest", the
  imputation is done in parallel for each tree. For more information,
  check the `missForest` documentation.

- seed:

  The seed to be used in the imputation. Default is 123.

- verbose:

  If TRUE, the percentage of missing values in each column is displayed.

## Value

The imputed dataset.

## Details

This is the slowest and more complex imputation method. If KNN works
fine, it is recommended to use it instead of `missForest`. In case of
large datasets, it is recommended to parallelize the imputation.
However, the user must have the `doParallel` package installed and
register a cluster before running the function. An example of how to
parallelize the imputation is provided in the examples section.

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

# Data after imputation
res <- hd_impute_missForest(hd_object, maxiter = 1, ntree = 50)
#> Missing values before imputation:
#> # A tibble: 10 × 2
#>    Variable NA_percentage
#>    <chr>            <dbl>
#>  1 AARSD1            5.80
#>  2 ABL1              5.80
#>  3 ACAA1             5.29
#>  4 ACAN              3.92
#>  5 ACE2              6.14
#>  6 ACOX1             3.92
#>  7 ACP6              2.22
#>  8 ACTA2             6.14
#>  9 ACTN4             6.14
#> 10 ACY1              3.92
#> ... and 81 more rows.
#>   missForest iteration 1 in progress...done!
#>     estimated error(s): 0.615834 
#>     difference(s): 0.003528261 
#>     time: 4.524 seconds
#> 
res$data
#> # A tibble: 586 × 101
#>    DAid  AARSD1  ABL1  ACAA1    ACAN    ACE2  ACOX1   ACP5    ACP6 ACTA2   ACTN4
#>    <chr>  <dbl> <dbl>  <dbl>   <dbl>   <dbl>  <dbl>  <dbl>   <dbl> <dbl>   <dbl>
#>  1 DA00…   3.39 2.76   1.71   0.0333  1.76   -0.919 1.54    2.15   2.81   0.742 
#>  2 DA00…   1.42 1.25  -0.816 -0.459   0.826  -0.902 0.647   1.30   0.798 -0.0659
#>  3 DA00…   3.38 2.35   1.12   0.989   0.513   0.330 1.37    1.11   1.43   0.397 
#>  4 DA00…   3.41 3.38   1.69   0.460   1.52    1.18  0.841   0.582  1.70   0.108 
#>  5 DA00…   5.01 5.05   0.128  0.401  -0.933  -0.584 0.0265  1.16   2.73   0.350 
#>  6 DA00…   6.83 1.18  -1.74  -0.156   1.53   -0.721 0.620   0.527  0.772  0.307 
#>  7 DA00…   4.11 3.78   3.96   0.682   3.14    2.62  1.47    2.25   2.01   0.170 
#>  8 DA00…   2.78 0.812 -0.552  0.982  -0.101  -0.304 0.376  -0.826  1.52  -0.597 
#>  9 DA00…   4.39 3.34  -0.452 -0.868   0.395   1.71  1.49   -0.0285 0.200 -0.532 
#> 10 DA00…   1.83 1.21  -0.912 -1.04   -0.0918 -0.304 1.69    0.0920 2.04   0.501 
#> # ℹ 576 more rows
#> # ℹ 90 more variables: ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>, ADAM15 <dbl>,
#> #   ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>, ADAMTS16 <dbl>,
#> #   ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>, ADGRG1 <dbl>,
#> #   ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>, AGR3 <dbl>,
#> #   AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>, AIF1 <dbl>,
#> #   AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, AKT1S1 <dbl>, …

if (FALSE) { # \dontrun{
# Parallelize the imputation
library(doParallel)  # Load the doParallel package
cl <- makeCluster(4)  # Create a cluster with 4 cores
registerDoParallel(cl)  # Register the cluster
res <- hd_impute_missForest(hd_object, maxiter = 1, ntree = 50, parallelize = "forests")
} # }
```
