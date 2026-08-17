# Print a table as a message

`message_table()` renders a tibble the way it would be printed at the
console. Passing a data frame straight to
[`message()`](https://rdrr.io/r/base/message.html) deparses it into
source code instead, which is unreadable.

## Usage

``` r
message_table(dat, max_rows = 10)
```

## Arguments

- dat:

  The table to print.

- max_rows:

  The number of rows to show. Default is 10.

## Value

`invisible(NULL)`, called for the side effect.
