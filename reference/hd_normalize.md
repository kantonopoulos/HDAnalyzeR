# Normalize data and remove batch effects

`hd_normalize()` normalizes the data by scaling them and removing their
batch effects. It removes the batch effects and scales or centers the
data. To remove batch effects, it uses the
[`remove_batch_effects()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/remove_batch_effects.md),
that utilizes limma package. For scaling, it uses the
[`scale()`](https://rdrr.io/r/base/scale.html) from base R.

## Usage

``` r
hd_normalize(
  dat,
  metadata = NULL,
  center = TRUE,
  scale = TRUE,
  batch = NULL,
  batch2 = NULL
)
```

## Arguments

- dat:

  An HDAnalyzeR object or a dataset in wide format and sample ID as its
  first column.

- metadata:

  A dataset containing the metadata information. If a HDAnalyzeR object
  is provided, this parameter is not needed.

- center:

  A logical value indicating whether to center the data. Default is
  TRUE.

- scale:

  A logical value indicating whether to scale the data. Default is TRUE.

- batch:

  The metadata column containing the batch information. In order to
  correct for batch effects, this parameter should be provided. Default
  is NULL.

- batch2:

  The metadata column containing the second batch information if
  available. Default is NULL.

## Value

The normalized dataset.

## Details

You can read more about the scaling and centering methods in the
documentation of the [`scale()`](https://rdrr.io/r/base/scale.html)
function in the `base` package, as well as about the method for removing
batch effects in the documentation of the `removeBatchEffect()` function
in the `limma` package.

## Examples

``` r
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)

# Center data
scaled_dat <- hd_normalize(hd_object, center = TRUE, scale = FALSE)
scaled_dat$data
#> # A tibble: 586 × 101
#>    DAid    AARSD1   ABL1  ACAA1   ACAN   ACE2  ACOX1    ACP5    ACP6   ACTA2
#>    <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl>
#>  1 DA00001  0.259  0.949  0.697 -0.532  0.827 -1.42   0.612   1.02    1.20  
#>  2 DA00002 -1.71  -0.563 -1.83  -1.02  -0.102 -1.40  -0.278   0.168  -0.813 
#>  3 DA00003 NA     NA     NA      0.424 NA     -0.167  0.447  NA      NA     
#>  4 DA00004  0.278  1.57   0.683 NA      0.593 NA     -0.0839 -0.551   0.0892
#>  5 DA00005  1.88   3.24  -0.882 -0.165 -1.86  -1.08  -0.898   0.0236  1.12  
#>  6 DA00006  3.70  -0.628 -2.75  -0.721  0.600 -1.22  -0.305  -0.606  -0.840 
#>  7 DA00007 NA     NA      2.95   0.117  2.21   2.12   0.548   1.12    0.398 
#>  8 DA00008 -0.351 -0.998 -1.56   0.416 -1.03  -0.800 -0.549  -1.96   -0.0901
#>  9 DA00009  1.26   1.53  -1.46  -1.43  -0.533  1.21   0.562  -1.16   -1.41  
#> 10 DA00010 -1.30  -0.596 -1.92  -1.60  -1.02  -0.801  0.765  -1.04    0.427 
#> # ℹ 576 more rows
#> # ℹ 91 more variables: ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>,
#> #   ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>,
#> #   ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>,
#> #   ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>,
#> #   AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>,
#> #   AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, …
round(mean(scaled_dat$data$ABL1, na.rm = TRUE), 4)  # should be 0
#> [1] 0
round(sd(scaled_dat$data$ABL1, na.rm = TRUE), 4)
#> [1] 1.3846

# Center and scale data (z-score scaling)
scaled_dat <- hd_normalize(hd_object, center = TRUE, scale = TRUE)
scaled_dat$data
#> # A tibble: 586 × 101
#>    DAid    AARSD1   ABL1  ACAA1   ACAN    ACE2  ACOX1   ACP5    ACP6   ACTA2
#>    <chr>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl>   <dbl>   <dbl>
#>  1 DA00001  0.240  0.685  0.498 -0.753  0.722  -1.39   0.800  0.991   1.16  
#>  2 DA00002 -1.58  -0.406 -1.30  -1.45  -0.0885 -1.37  -0.364  0.163  -0.786 
#>  3 DA00003 NA     NA     NA      0.600 NA      -0.163  0.584 NA      NA     
#>  4 DA00004  0.257  1.14   0.488 NA      0.517  NA     -0.110 -0.536   0.0862
#>  5 DA00005  1.74   2.34  -0.629 -0.233 -1.62   -1.06  -1.18   0.0230  1.08  
#>  6 DA00006  3.42  -0.453 -1.96  -1.02   0.523  -1.19  -0.399 -0.590  -0.812 
#>  7 DA00007 NA     NA      2.11   0.165  1.93    2.08   0.717  1.09    0.385 
#>  8 DA00008 -0.325 -0.721 -1.12   0.589 -0.898  -0.783 -0.719 -1.90   -0.0871
#>  9 DA00009  1.17   1.11  -1.04  -2.03  -0.464   1.18   0.735 -1.13   -1.36  
#> 10 DA00010 -1.20  -0.431 -1.37  -2.27  -0.889  -0.784  1.00  -1.01    0.413 
#> # ℹ 576 more rows
#> # ℹ 91 more variables: ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>,
#> #   ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>,
#> #   ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>,
#> #   ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>,
#> #   AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>,
#> #   AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, …
round(mean(scaled_dat$data$ABL1, na.rm = TRUE), 4)  # should be 0
#> [1] 0
round(sd(scaled_dat$data$ABL1, na.rm = TRUE), 4)  # should be 1
#> [1] 1

# Center, scale and remove batch effects
scaled_dat <- hd_normalize(hd_object, batch = "Cohort")
#> design matrix of interest not specified. Assuming a one-group experiment.
scaled_dat$data
#> # A tibble: 586 × 101
#>    DAid   AARSD1   ABL1  ACAA1   ACAN   ACE2  ACOX1   ACP5    ACP6  ACTA2  ACTN4
#>    <chr>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
#>  1 DA000…  0.104  0.476  0.391 -0.779  0.544 -1.46   0.701  0.915   0.985  0.516
#>  2 DA000… -1.74  -0.656 -1.43  -1.48  -0.286 -1.44  -0.471  0.0838 -1.02  -0.761
#>  3 DA000… NA     NA     NA      0.574 NA     -0.232  0.484 NA      NA     NA    
#>  4 DA000…  0.121  0.944  0.380 NA      0.335 NA     -0.215 -0.618  -0.122 -0.486
#>  5 DA000…  1.62   2.20  -0.746 -0.259 -1.86  -1.13  -1.29  -0.0569  0.903 -0.103
#>  6 DA000…  3.32  -0.705 -2.09  -1.05   0.341 -1.26  -0.507 -0.672  -1.05  NA    
#>  7 DA000… NA     NA      2.02   0.140  1.78   2.01   0.618  1.01    0.186 -0.388
#>  8 DA000… -0.468 -0.983 -1.24   0.563 -1.12  -0.854 -0.828 -1.99   -0.300 -1.60 
#>  9 DA000…  1.04   0.915 -1.16  -2.05  -0.671  1.12   0.636 -1.21   -1.61  -1.50 
#> 10 DA000… -1.36  -0.681 -1.49  -2.30  -1.11  -0.855  0.903 -1.10    0.214  0.136
#> # ℹ 576 more rows
#> # ℹ 90 more variables: ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>, ADAM15 <dbl>,
#> #   ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>, ADAMTS16 <dbl>,
#> #   ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>, ADGRG1 <dbl>,
#> #   ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>, AGR3 <dbl>,
#> #   AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>, AIF1 <dbl>,
#> #   AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, AKT1S1 <dbl>, …
```
