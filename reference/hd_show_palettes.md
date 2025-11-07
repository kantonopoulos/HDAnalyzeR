# Display available palettes

`hd_show_palettes()` displays a grid of palettes.

## Usage

``` r
hd_show_palettes(palettes = hd_palettes(), n = NULL)
```

## Arguments

- palettes:

  A list of palettes. Defaults to all palettes from
  [`hd_palettes()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_palettes.md).

- n:

  The number of colors to show for each palette. If `NULL`, all colors
  are shown.

## Value

A ggplot2 plot showing the palettes.

## Examples

``` r
hd_show_palettes()
```
