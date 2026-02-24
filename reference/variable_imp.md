# Extract model features and plot feature importance

`variable_imp()` calculates the feature importance of the model and
plots the top features. It also generates a title for the plot based on
the model metrics and the mixture parameter.

## Usage

``` r
variable_imp(
  dat,
  variable = "Disease",
  case,
  mixture = NULL,
  palette = NULL,
  y_labels = FALSE,
  title = c("accuracy", "sensitivity", "specificity", "auc", "features", "top-features"),
  verbose = TRUE,
  engine = "glmnet",
  seed = 123
)
```

## Arguments

- dat:

  An `hd_model` object coming from
  [`evaluate_model()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/evaluate_model.md).

- variable:

  The variable to predict. Default is "Disease".

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

- title:

  Vector of title elements to include in the plot.

- engine:

  Either glmnet, rf or lr. If glmnet and the model type is multi-class
  then the variable importance is calculated per class.

- seed:

  Seed for reproducibility. Default is 123.

## Value

A model object containing the features and the feature importance plot.
