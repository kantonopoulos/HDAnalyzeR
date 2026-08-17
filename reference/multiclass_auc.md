# One-vs-rest ROC AUC for a multiclass model

`multiclass_auc()` computes the one-vs-rest ROC AUC of every class,
together with the macro and micro averages.

## Usage

``` r
multiclass_auc(truth, prob_predictions)
```

## Arguments

- truth:

  A vector with the observed classes.

- prob_predictions:

  A tibble of predicted class probabilities as returned by
  `predict(type = "prob")`, i.e. one `.pred_<class>` column per class.

## Value

A named numeric vector with one AUC per class, plus `macro` and `micro`.

## Details

Each class is scored against all remaining classes pooled together.
`macro` is the unweighted mean of the per-class AUCs and `micro` is the
AUC obtained by pooling every one-vs-rest decision into a single binary
problem.
