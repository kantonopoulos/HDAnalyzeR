# Create title for feature importance plot

`generate_title()` generates a subtitle for the feature importance plot.

## Usage

``` r
generate_title(
  features,
  accuracy = NULL,
  sensitivity = NULL,
  specificity = NULL,
  auc = NULL,
  rmse = NULL,
  rsq = NULL,
  mixture = NULL,
  title = c("accuracy", "sensitivity", "specificity", "auc", "features", "top-features",
    "mixture")
)
```

## Arguments

- features:

  A tibble with features and their model importance.

- accuracy:

  Accuracy of the model.

- sensitivity:

  Sensitivity of the model.

- specificity:

  Specificity of the model.

- auc:

  AUC of the model.

- mixture:

  Mixture of lasso and ridge regularization. In random forest models it
  is NULL.

- title:

  Vector of subtitle elements to include in the plot.

## Value

The plot subtitle as character vector.
