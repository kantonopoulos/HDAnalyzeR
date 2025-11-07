# Apply palette to plot

`apply_palette` applies the color palette to the plot. It checks if the
palette is a valid palette from the Human Protein Atlas (HPA) or a
custom palette.

## Usage

``` r
apply_palette(plot, palette, type = "color")
```

## Arguments

- plot:

  The plot to apply the palette.

- palette:

  The color palette to apply. It can be either a character with the name
  of the palette from the HPA or a custom palette (for example
  `c("M" = "red", "F" = "blue")`).

- type:

  The type of palette to apply. Default is "color". Other option is
  "fill".

## Value

The plot with the selected palette.
