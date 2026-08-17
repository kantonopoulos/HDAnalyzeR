# Ensure an optional package is installed

`check_installed()` errors with an actionable installation hint when a
package that is only listed under `Suggests` is needed but unavailable.

## Usage

``` r
check_installed(package, reason = NULL)
```

## Arguments

- package:

  The name of the package to check.

- reason:

  A short description of what the package is needed for.

## Value

`invisible(TRUE)`, or an error if the package is missing.
