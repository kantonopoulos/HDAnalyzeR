# Finalize and evaluate the model

`evaluate_regression_model()` finalizes the model using the best
hyperparameters and evaluates the model using the test set. It
calculates the RMSE and RSQ metrics and plots the predicted vs observed
values.

## Usage

``` r
evaluate_regression_model(
  dat,
  variable = "Age",
  case,
  mixture = NULL,
  palette = NULL,
  verbose = TRUE,
  seed = 123
)
```

## Arguments

- dat:

  An `hd_model` object coming from a tuning function.

- variable:

  The variable to predict. Default is "Age".

- case:

  The case class.

- mixture:

  The mixture parameter for the elastic net. If NULL it will be tuned.
  Default is NULL.

- palette:

  The color palette for the classes. If it is a character, it should be
  one of the palettes from
  [`hd_palettes()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_palettes.md).
  Default is NULL.

- verbose:

  Whether to print progress messages. Default is TRUE.

- seed:

  Seed for reproducibility. Default is 123.

## Value

A model object containing the train and test data, the final model, the
metrics, the ROC curve, and the mixture parameter.
