# Balance groups

`balance_groups()` balances the groups based on the case variable
provided. It downsamples the control group to match the number of
samples in the case group.

## Usage

``` r
balance_groups(dat, variable, case = 1, seed = 123)
```

## Arguments

- dat:

  A dataset containing the case and control groups.

- variable:

  The name of the column containing the case and control groups. Default
  is "Disease".

- case:

  The case class. Default is 1.

- seed:

  Seed for reproducibility. Default is 123.

## Value

A balanced dataset.
