# Validate model on new data

`hd_model_test()` validates the model on new data. It takes an already
tuned model, evaluates it on the validation (new test) set, calculates
the metrics and plots the probability and ROC curve based on the new
data.

## Usage

``` r
hd_model_test(
  model_object,
  train_set,
  test_set,
  variable = "Disease",
  metadata_cols = NULL,
  case,
  control = NULL,
  balance_groups = TRUE,
  palette = NULL,
  seed = 123
)
```

## Arguments

- model_object:

  An `hd_model` object coming from
  [`hd_model_rreg()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_model_rreg.md)
  and
  [`hd_model_rf()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_model_rf.md)
  binary or multiclass classification.

- train_set:

  The training set as an HDAnalyzeR object or a dataset in wide format
  with sample ID as its first column and class column as its second
  column.

- test_set:

  The validation/test set as an HDAnalyzeR object or a dataset in wide
  format with sample ID as its first column and class column as its
  second column.

- variable:

  The name of the metadata variable containing the case and control
  groups. Default is "Disease".

- metadata_cols:

  The metadata variables to include in the analysis. Default is NULL.

- case:

  The case class.

- control:

  The control groups. If NULL, it will be set to all other unique values
  of the variable that are not the case. Default is NULL.

- balance_groups:

  Whether to balance the groups in the train set. It is only valid in
  binary classification settings. Default is TRUE.

- palette:

  The color palette for the classes. If it is a character, it should be
  one of the palettes from
  [`hd_palettes()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_palettes.md).
  Default is NULL.

- seed:

  Seed for reproducibility. Default is 123.

## Value

The model object containing the validation set, the metrics, the ROC
curve, the probability plot, and the confusion matrix for the new data.

## Details

In order to run this function, the train and test sets should be in
exactly the same format meaning that they must have the same columns in
the same order. Some function arguments like the case/control, variable,
and metadata_cols should be also the same. If the data contain missing
values, KNN (k=5) imputation will be used to impute. If `case` is
provided, the model will be a binary classification model. If `case` is
NULL, the model will be a multiclass classification model.

In multi-class models, the groups in the train set are not balanced and
sensitivity and specificity are calculated via macro-averaging. In case
the model is run against a continuous variable, the palette will be
ignored.

## Examples

``` r
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Split the data for training and validation sets
dat <- hd_object$data
train_indices <- sample(seq_len(nrow(dat)), size = floor(0.8 * nrow(dat)))
train_data <- dat[train_indices, ]
validation_data <- dat[-train_indices, ]

hd_object_train <- hd_initialize(train_data, example_metadata, is_wide = TRUE)
hd_object_val <- hd_initialize(validation_data, example_metadata, is_wide = TRUE)

# Split the training set into training and inner test sets
hd_split <- hd_split_data(hd_object_train, variable = "Disease")
#> Warning: Too little data to stratify.
#> • Resampling will be unstratified.

# Run the regularized regression model pipeline
model_object <- hd_model_rreg(hd_split,
                              variable = "Disease",
                              case = "AML",
                              grid_size = 5,
                              palette = "cancers12",
                              verbose = FALSE)
#> The groups in the train set are balanced. If you do not want to balance the groups, set `balance_groups = FALSE`.

# Run the model evaluation pipeline
hd_model_test(model_object, hd_object_train, hd_object_val, case = "AML", palette = "cancers12")
#> The groups in the train set are balanced. If you do not want to balance the groups, set `balance_groups = FALSE`.
#> $train_data
#> # A tibble: 66 × 102
#>    DAid  Disease AARSD1  ABL1   ACAA1     ACAN  ACE2   ACOX1   ACP5   ACP6 ACTA2
#>    <chr> <fct>    <dbl> <dbl>   <dbl>    <dbl> <dbl>   <dbl>  <dbl>  <dbl> <dbl>
#>  1 DA00… 1         6.83 1.18  -1.74   -0.156   1.53  -0.721  0.620   0.527 0.772
#>  2 DA00… 1         4.48 4.56   4.86    0.230   2.24   2.97   2.60   -1.11  2.20 
#>  3 DA00… 1         2.19 1.66  -0.0167 -0.567   3.77   0.369  1.38    1.09  2.09 
#>  4 DA00… 1         4.04 1.41  -2.09    0.427   0.200  0.537  0.0262  0.105 1.73 
#>  5 DA00… 1         1.80 1.70   2.77   -1.04    1.33  -0.0247 1.02    0.112 2.58 
#>  6 DA00… 1         2.99 2.24  -0.180  -0.00102 0.367  0.604  0.843  -1.96  2.51 
#>  7 DA00… 1         3.03 0.390  1.83    0.983   2.60   0.113  0.504   1.42  1.22 
#>  8 DA00… 1         3.59 3.38   1.79   -0.303   1.59   0.604  1.71   -0.837 1.65 
#>  9 DA00… 1         1.42 1.25  -0.816  -0.459   0.826 -0.902  0.647   1.30  0.798
#> 10 DA00… 1         3.48 4.96   3.50   -0.338   4.48   1.26   2.18    1.62  1.79 
#> # ℹ 56 more rows
#> # ℹ 91 more variables: ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>,
#> #   ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>,
#> #   ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>,
#> #   ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>,
#> #   AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>,
#> #   AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, …
#> 
#> $test_data
#> # A tibble: 117 × 102
#>    DAid    Disease AARSD1   ABL1   ACAA1     ACAN     ACE2  ACOX1   ACP5   ACP6
#>    <chr>   <fct>    <dbl>  <dbl>   <dbl>    <dbl>    <dbl>  <dbl>  <dbl>  <dbl>
#>  1 DA00120 0         3.91  1.22   0.692   1.08     0.254    0.864  1.39   0.936
#>  2 DA00533 0        NA    NA      0.441   0.185   -0.234   -1.26   1.30   0.792
#>  3 DA00315 0         3.66  1.71   0.812   0.690   -0.618    1.50   1.42   2.62 
#>  4 DA00081 0         2.82 -0.297  0.523  -0.657   -0.840    1.34  -0.473 -0.332
#>  5 DA00239 0         2.97  2.73   2.66    1.03     0.471    0.636 -0.600  1.88 
#>  6 DA00340 0         2.85  0.719  0.918   2.64     3.18     0.342  1.72  -2.39 
#>  7 DA00193 0         3.14  3.01   0.926   0.0593   1.01     1.40   2.07   0.593
#>  8 DA00061 0         1.90  2.11   0.953   1.45    -0.00630  0.357  1.60   1.24 
#>  9 DA00203 0         3.51  0.603  1.86    0.00641  0.143    0.587 -0.383  2.00 
#> 10 DA00246 0         3.39 -0.658 -0.0989  0.356   -1.00     0.516  0.576 -0.811
#> # ℹ 107 more rows
#> # ℹ 92 more variables: ACTA2 <dbl>, ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>,
#> #   ADA2 <dbl>, ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>,
#> #   ADAMTS15 <dbl>, ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>,
#> #   ADGRE2 <dbl>, ADGRE5 <dbl>, ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>,
#> #   ADM <dbl>, AGER <dbl>, AGR2 <dbl>, AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>,
#> #   AGXT <dbl>, AHCY <dbl>, AHSP <dbl>, AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, …
#> 
#> $model_type
#> [1] "binary_class"
#> 
#> $final_workflow
#> ══ Workflow ════════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: logistic_reg()
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 5 Recipe Steps
#> 
#> • step_dummy()
#> • step_nzv()
#> • step_normalize()
#> • step_corr()
#> • step_impute_knn()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> Logistic Regression Model Specification (classification)
#> 
#> Main Arguments:
#>   penalty = 0.44087827749012
#>   mixture = 0.23890444329707
#> 
#> Computational engine: glmnet 
#> 
#> 
#> $metrics
#> $metrics$accuracy
#> [1] 0.8205128
#> 
#> $metrics$sensitivity
#> [1] 0.625
#> 
#> $metrics$specificity
#> [1] 0.8348624
#> 
#> $metrics$auc
#> [1] 0.7993119
#> 
#> $metrics$confusion_matrix
#>           Truth
#> Prediction  0  1
#>          0 91  3
#>          1 18  5
#> 
#> 
#> $roc_curve

#> 
#> $probability_plot
#> Warning: `label` cannot be a <ggplot2::element_blank> object.

#> 
#> $mixture
#> [1] 0.2389044
#> 
#> $features
#> # A tibble: 100 × 4
#>    Feature Importance Sign  Scaled_Importance
#>    <fct>        <dbl> <chr>             <dbl>
#>  1 ALPP       0.242   NEG              1     
#>  2 ACAN       0.163   NEG              0.674 
#>  3 ADA        0.127   POS              0.525 
#>  4 ANGPT1     0.0971  NEG              0.402 
#>  5 ARTN       0.0866  POS              0.358 
#>  6 ANGPT2     0.0533  POS              0.221 
#>  7 APOM       0.0508  NEG              0.210 
#>  8 APEX1      0.0439  POS              0.182 
#>  9 AMIGO2     0.0133  NEG              0.0549
#> 10 ACOX1      0.00773 POS              0.0320
#> # ℹ 90 more rows
#> 
#> $feat_imp_plot

#> 
#> $validation_data
#> # A tibble: 118 × 102
#>    DAid    Disease AARSD1   ABL1  ACAA1    ACAN     ACE2   ACOX1    ACP5    ACP6
#>    <chr>   <fct>    <dbl>  <dbl>  <dbl>   <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
#>  1 DA00001 1         3.39  2.76   1.71   0.0333  1.76    -0.919   1.54    2.15  
#>  2 DA00003 1        NA    NA     NA      0.989  NA        0.330   1.37   NA     
#>  3 DA00007 1        NA    NA      3.96   0.682   3.14     2.62    1.47    2.25  
#>  4 DA00012 1         4.31  0.710 -1.44  -0.218  -0.469   -0.361  -0.0714 -1.30  
#>  5 DA00014 1         6.34  7.25   5.12   0.0193  1.29     0.370  -0.382   0.830 
#>  6 DA00016 1         1.79  1.36   0.106 -0.372   3.40    -1.19    1.77    1.07  
#>  7 DA00030 1         3.31  5.38   4.82   0.266   0.606    3.12    1.22    2.13  
#>  8 DA00038 1         2.23  1.42   0.484  1.72    1.46     0.0747  1.82    0.109 
#>  9 DA00043 1         2.48  1.49   0.605  0.339   0.436    0.690   1.11    0.0158
#> 10 DA00051 0         2.53  3.00   0.166  0.707  -0.00699  1.05    0.898   1.53  
#> # ℹ 108 more rows
#> # ℹ 92 more variables: ACTA2 <dbl>, ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>,
#> #   ADA2 <dbl>, ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>,
#> #   ADAMTS15 <dbl>, ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>,
#> #   ADGRE2 <dbl>, ADGRE5 <dbl>, ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>,
#> #   ADM <dbl>, AGER <dbl>, AGR2 <dbl>, AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>,
#> #   AGXT <dbl>, AHCY <dbl>, AHSP <dbl>, AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, …
#> 
#> $test_metrics
#> $test_metrics$accuracy
#> [1] 0.8728814
#> 
#> $test_metrics$sensitivity
#> [1] 0.7777778
#> 
#> $test_metrics$specificity
#> [1] 0.8807339
#> 
#> $test_metrics$auc
#> [1] 0.8929664
#> 
#> $test_metrics$confusion_matrix
#>           Truth
#> Prediction  0  1
#>          0 96  2
#>          1 13  7
#> 
#> 
#> $test_roc_curve

#> 
#> $test_probability_plot
#> Warning: `label` cannot be a <ggplot2::element_blank> object.

#> 
#> attr(,"class")
#> [1] "hd_model"

# Run the pipeline against continuous variable
# Split the training set into training and inner test sets
hd_split <- hd_split_data(hd_object_train, variable = "Age")

# Run the regularized regression model pipeline
model_object <- hd_model_rreg(hd_split,
                              variable = "Age",
                              case = "AML",
                              grid_size = 2,
                              cv_sets = 2,
                              plot_title = NULL,
                              verbose = FALSE)
#> The groups in the train set are balanced. If you do not want to balance the groups, set `balance_groups = FALSE`.

# Run the model evaluation pipeline
hd_model_test(model_object, hd_object_train, hd_object_val, variable = "Age", case = NULL)
#> The groups in the train set are balanced. If you do not want to balance the groups, set `balance_groups = FALSE`.
#> $train_data
#> # A tibble: 349 × 102
#>    DAid      Age AARSD1       ABL1  ACAA1     ACAN   ACE2  ACOX1   ACP5    ACP6
#>    <chr>   <dbl>  <dbl>      <dbl>  <dbl>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
#>  1 DA00315    48   3.66  1.71       0.812  0.690   -0.618  1.50   1.42   2.62  
#>  2 DA00424    45   3.75  1.64      NA      0.414    1.84   1.83   0.829  2.02  
#>  3 DA00049    40   4.48  4.56       4.86   0.230    2.24   2.97   2.60  -1.11  
#>  4 DA00292    48   4.25  5.63       4.43  -0.467   -0.310  2.93   0.907  3.18  
#>  5 DA00203    44   3.51  0.603      1.86   0.00641  0.143  0.587 -0.383  2.00  
#>  6 DA00445    44   2.98  0.680     -0.310 -0.279    1.56  -0.240 -0.368  0.0101
#>  7 DA00221    51  NA    NA         -0.881  0.447    0.458 -0.562  0.442  0.546 
#>  8 DA00229    52   2.86  3.89       3.42   1.26     0.883  2.70   1.13   2.60  
#>  9 DA00023    42   2.92 -0.0000706  0.602  1.59     0.198  1.61   0.283  2.35  
#> 10 DA00079    49   4.49  3.66      NA      1.85    NA      2.03   1.76   2.52  
#> # ℹ 339 more rows
#> # ℹ 92 more variables: ACTA2 <dbl>, ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>,
#> #   ADA2 <dbl>, ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>,
#> #   ADAMTS15 <dbl>, ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>,
#> #   ADGRE2 <dbl>, ADGRE5 <dbl>, ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>,
#> #   ADM <dbl>, AGER <dbl>, AGR2 <dbl>, AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>,
#> #   AGXT <dbl>, AHCY <dbl>, AHSP <dbl>, AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, …
#> 
#> $test_data
#> # A tibble: 119 × 102
#>    DAid     Age AARSD1   ABL1  ACAA1   ACAN   ACE2  ACOX1     ACP5   ACP6  ACTA2
#>    <chr>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>    <dbl>  <dbl>  <dbl>
#>  1 DA004…    51   3.63  2.89   3.73   0.481  0.319  0.712  1.48     1.90   3.60 
#>  2 DA001…    47   3.14  1.77  NA     -0.489 -0.780  1.41   1.40     1.79  -0.298
#>  3 DA001…    61   2.00  0.880 -0.191  0.462  0.883  1.35   1.56     1.60   0.937
#>  4 DA003…    88   2.85  0.719  0.918  2.64   3.18   0.342  1.72    -2.39   0.614
#>  5 DA001…    89   4.15  0.910 -0.900 -0.161  0.922  0.624 -0.298    0.791  2.03 
#>  6 DA004…    49   1.09  0.594  1.60   0.294  1.77   0.737  1.74     0.576  1.00 
#>  7 DA005…    56   2.74  1.01  -1.61   1.12   0.910 -2.00  -0.00928  1.39   1.93 
#>  8 DA000…    45   2.16  0.800 -0.524 -0.350  0.289  0.726  1.31     1.50   0.495
#>  9 DA003…    47   2.99 -0.181 -0.500  0.903  1.05  -1.58  -0.119    1.14   2.06 
#> 10 DA002…    56   1.94 -0.530  0.551  0.645  0.916 -0.429  0.603    0.196 -0.171
#> # ℹ 109 more rows
#> # ℹ 91 more variables: ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>, ADA2 <dbl>,
#> #   ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>, ADAMTS15 <dbl>,
#> #   ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>, ADGRE2 <dbl>, ADGRE5 <dbl>,
#> #   ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>, ADM <dbl>, AGER <dbl>, AGR2 <dbl>,
#> #   AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>, AGXT <dbl>, AHCY <dbl>, AHSP <dbl>,
#> #   AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, AKR1B1 <dbl>, AKR1C4 <dbl>, …
#> 
#> $model_type
#> [1] "regression"
#> 
#> $final_workflow
#> ══ Workflow ════════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 5 Recipe Steps
#> 
#> • step_dummy()
#> • step_nzv()
#> • step_normalize()
#> • step_corr()
#> • step_impute_knn()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   penalty = 0.0118952086562253
#>   mixture = 0.232995977951214
#> 
#> Computational engine: glmnet 
#> 
#> 
#> $metrics
#> $metrics$rmse
#> [1] 17.70643
#> 
#> $metrics$rsq
#> [1] 0.0001858973
#> 
#> 
#> $comparison_plot

#> 
#> $mixture
#> [1] 0.232996
#> 
#> $features
#> # A tibble: 100 × 4
#>    Feature  Importance Sign  Scaled_Importance
#>    <fct>         <dbl> <chr>             <dbl>
#>  1 ARSB           3.62 NEG               1    
#>  2 ACY1           3.28 NEG               0.905
#>  3 AREG           3.03 POS               0.836
#>  4 ATP6AP2        2.64 NEG               0.728
#>  5 ATOX1          2.22 NEG               0.613
#>  6 AGXT           2.20 NEG               0.606
#>  7 ARHGEF12       2.19 NEG               0.604
#>  8 ALDH1A1        2.18 POS               0.602
#>  9 ALCAM          2.10 POS               0.581
#> 10 APP            1.93 POS               0.533
#> # ℹ 90 more rows
#> 
#> $feat_imp_plot

#> 
#> $validation_data
#> # A tibble: 118 × 102
#>    DAid      Age AARSD1   ABL1  ACAA1    ACAN     ACE2   ACOX1    ACP5    ACP6
#>    <chr>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
#>  1 DA00001    42   3.39  2.76   1.71   0.0333  1.76    -0.919   1.54    2.15  
#>  2 DA00003    61  NA    NA     NA      0.989  NA        0.330   1.37   NA     
#>  3 DA00007    85  NA    NA      3.96   0.682   3.14     2.62    1.47    2.25  
#>  4 DA00012    78   4.31  0.710 -1.44  -0.218  -0.469   -0.361  -0.0714 -1.30  
#>  5 DA00014    68   6.34  7.25   5.12   0.0193  1.29     0.370  -0.382   0.830 
#>  6 DA00016    78   1.79  1.36   0.106 -0.372   3.40    -1.19    1.77    1.07  
#>  7 DA00030    67   3.31  5.38   4.82   0.266   0.606    3.12    1.22    2.13  
#>  8 DA00038    69   2.23  1.42   0.484  1.72    1.46     0.0747  1.82    0.109 
#>  9 DA00043    78   2.48  1.49   0.605  0.339   0.436    0.690   1.11    0.0158
#> 10 DA00051    82   2.53  3.00   0.166  0.707  -0.00699  1.05    0.898   1.53  
#> # ℹ 108 more rows
#> # ℹ 92 more variables: ACTA2 <dbl>, ACTN4 <dbl>, ACY1 <dbl>, ADA <dbl>,
#> #   ADA2 <dbl>, ADAM15 <dbl>, ADAM23 <dbl>, ADAM8 <dbl>, ADAMTS13 <dbl>,
#> #   ADAMTS15 <dbl>, ADAMTS16 <dbl>, ADAMTS8 <dbl>, ADCYAP1R1 <dbl>,
#> #   ADGRE2 <dbl>, ADGRE5 <dbl>, ADGRG1 <dbl>, ADGRG2 <dbl>, ADH4 <dbl>,
#> #   ADM <dbl>, AGER <dbl>, AGR2 <dbl>, AGR3 <dbl>, AGRN <dbl>, AGRP <dbl>,
#> #   AGXT <dbl>, AHCY <dbl>, AHSP <dbl>, AIF1 <dbl>, AIFM1 <dbl>, AK1 <dbl>, …
#> 
#> $test_metrics
#> $test_metrics$rmse
#> [1] 17.14835
#> 
#> $test_metrics$rsq
#> [1] 0.01120922
#> 
#> 
#> $test_comparison_plot

#> 
#> attr(,"class")
#> [1] "hd_model"
```
