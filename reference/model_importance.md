# Extract model-specific variable importance

`model_importance()` extracts the variable importance scores from a
fitted workflow for the engines used by the package.

## Usage

``` r
model_importance(fit)
```

## Arguments

- fit:

  A fitted `workflow` or `model_fit` object.

## Value

A tibble with a `Variable` and an `Importance` column, plus a `Sign`
column for the (generalized) linear models.

## Details

For `ranger` fits the permutation importance recorded at fit time is
returned. For `glm` and `lm` fits the importance is the absolute t- or
z-statistic of each coefficient and the sign is taken from the
coefficient estimate, matching the convention used elsewhere in the
package.
