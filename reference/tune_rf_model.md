# Hyperparameter optimization for random forest models

`tune_rf_model()` performs hyperparameter optimization for random forest
models. It tunes the model using the provided grid size and
cross-validation sets. It returns the best model and hyperparameters.

## Usage

``` r
tune_rf_model(
  dat,
  variable = "Disease",
  cor_threshold = 0.9,
  grid_size = 30,
  verbose = TRUE,
  seed = 123
)
```

## Arguments

- dat:

  An `hd_model` object coming from
  [`prepare_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/prepare_data.md).

- variable:

  The variable to predict. Default is "Disease".

- cor_threshold:

  Threshold of absolute correlation values. This will be used to remove
  the minimum number of features so that all their resulting absolute
  correlations are less than this value.

- grid_size:

  Size of the hyperparameter optimization grid. Default is 10.

- verbose:

  Whether to print progress messages. Default is TRUE.

- seed:

  Seed for reproducibility. Default is 123.

## Value

A model object containing the train and test data, the tuned model, and
the workflow.
