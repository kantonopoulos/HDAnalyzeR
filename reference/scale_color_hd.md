# HDAnalyzeR color scales

`scale_color_hd()` creates a ggplot2 scale for color aesthetics using
the HDAnalyzeR palettes.

## Usage

``` r
scale_color_hd(palette)
```

## Arguments

- palette:

  The name of the palette to use. It should be one of the palettes from
  [`hd_palettes()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_palettes.md).

## Value

A ggplot2 scale for color aesthetics.

## Examples

``` r
# Create an example dataframe
data <- data.frame(
  var1 = seq_len(10),
  var2 = seq(2, 20, by = 2),
  Sex = rep(c("M", "F"), each = 5)
)

# Create a plot
plot <- ggplot2::ggplot(data, ggplot2::aes(x = var1, y = var2, color = Sex)) +
  ggplot2::geom_point()
plot


# Add a custom palette
plot + scale_color_hd("sex")
```
