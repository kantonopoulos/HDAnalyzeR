# Split data

`hd_split_data()` splits the data into training and test sets based on
the ratio provided. It also stratifies the data based on the variable of
interest if any. At this stage the user can select metadata variable
predictors to be included in the training and test sets.

## Usage

``` r
hd_split_data(
  dat,
  metadata = NULL,
  variable = "Disease",
  metadata_cols = NULL,
  ratio = 0.75,
  seed = 123
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
  groups. Default is "Disease".

- metadata_cols:

  The metadata variables to be selected from the metadata as predictors.
  Default is NULL.

- ratio:

  The ratio of training data to test data. Default is 0.75.

- seed:

  Seed for reproducibility. Default is 123.

## Value

A split object containing train and test data splits.

## Details

It is always recommended to split the data into training and test sets
to avoid overfitting. This function also initializes the model object to
be used in the downstream machine learning pipeline. The user can create
their own model object with the train and test data splits, but it must
be a list with the train set as the first and the test set as the second
element. The function utilizes the `initial_split()` function from the
`rsample` package to split the data. For more information on the
`rsample` package, please check their documentation.

## Examples

``` r
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Split the data into training and test sets
hd_split_data(hd_object, variable = "Disease")
#> Warning: Too little data to stratify.
#> • Resampling will be unstratified.
#> $train_data
#> # A tibble: 439 × 102
#>    DAid    Disease AARSD1   ABL1  ACAA1   ACAN    ACE2  ACOX1    ACP5    ACP6
#>    <chr>   <chr>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl>   <dbl>
#>  1 DA00003 AML      NA    NA     NA      0.989 NA       0.330  1.37   NA     
#>  2 DA00004 AML       3.41  3.38   1.69  NA      1.52   NA      0.841   0.582 
#>  3 DA00005 AML       5.01  5.05   0.128  0.401 -0.933  -0.584  0.0265  1.16  
#>  4 DA00006 AML       6.83  1.18  -1.74  -0.156  1.53   -0.721  0.620   0.527 
#>  5 DA00007 AML      NA    NA      3.96   0.682  3.14    2.62   1.47    2.25  
#>  6 DA00008 AML       2.78  0.812 -0.552  0.982 -0.101  -0.304  0.376  -0.826 
#>  7 DA00010 AML       1.83  1.21  -0.912 -1.04  -0.0918 -0.304  1.69    0.0920
#>  8 DA00011 AML       3.48  4.96   3.50  -0.338  4.48    1.26   2.18    1.62  
#>  9 DA00012 AML       4.31  0.710 -1.44  -0.218 -0.469  -0.361 -0.0714 -1.30  
#> 10 DA00013 AML       1.31  2.52   1.11   0.997  4.56   -1.35   0.833   2.33  
#> # ℹ 429 more rows
#> # ℹ 92 more variables: ACTA2 <dbl>, ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>,
#> #   ADA2 <dbl>, ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>,
#> #   ADAMTS15 <dbl>, ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>,
#> #   ADGRE2 <dbl>, ADGRE5 <dbl>, ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>,
#> #   ADM <dbl>, AGER <dbl>, AGR2 <dbl>, AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>,
#> #   AGXT <dbl>, AHCY <dbl>, AHSP <dbl>, AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, …
#> 
#> $test_data
#> # A tibble: 147 × 102
#>    DAid   Disease AARSD1  ABL1  ACAA1    ACAN  ACE2   ACOX1   ACP5    ACP6 ACTA2
#>    <chr>  <chr>    <dbl> <dbl>  <dbl>   <dbl> <dbl>   <dbl>  <dbl>   <dbl> <dbl>
#>  1 DA000… AML      3.39  2.76   1.71   0.0333 1.76  -0.919   1.54   2.15   2.81 
#>  2 DA000… AML      1.42  1.25  -0.816 -0.459  0.826 -0.902   0.647  1.30   0.798
#>  3 DA000… AML      4.39  3.34  -0.452 -0.868  0.395  1.71    1.49  -0.0285 0.200
#>  4 DA000… AML      3.31  1.90  NA     -0.926  0.408  0.687   1.03   0.612  2.19 
#>  5 DA000… AML      1.46  0.832 -2.73  -0.371  2.27   0.0234  0.144  0.826  1.98 
#>  6 DA000… AML      2.62  2.48   0.537 -0.215  1.82   0.290   1.27   1.11   0.206
#>  7 DA000… AML      2.47  2.16  -0.486 NA      0.386 NA       1.38   0.536  1.86 
#>  8 DA000… AML      3.62  3.06  -1.34   0.965  1.05   1.53    0.152 -0.124  2.81 
#>  9 DA000… AML      4.39  3.31   0.454  0.290  2.68   0.116  -1.32   0.945  2.14 
#> 10 DA000… AML      0.964 2.94   1.55   1.67   2.50   0.164   1.83   1.46   3.03 
#> # ℹ 137 more rows
#> # ℹ 91 more variables: ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>,
#> #   ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>,
#> #   ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>,
#> #   ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>,
#> #   AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>,
#> #   AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, …
#> 
#> attr(,"class")
#> [1] "hd_model"
```
