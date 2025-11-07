# HDAnalyzeR fill scales

`scale_fill_hd()` creates a ggplot2 scale for fill aesthetics using the
HDAnalyzeR palettes.

## Usage

``` r
scale_fill_hd(palette)
```

## Arguments

- palette:

  The name of the palette to use. It should be one of the palettes from
  [`hd_palettes()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_palettes.md).

## Value

A ggplot2 scale for fill aesthetics.

## Examples

``` r
# Create an example dataframe
data <- data.frame(
  Sex = c("M", "F"),
  Count = c(60, 40)
)

# Create a plot
plot <- ggplot2::ggplot(data, ggplot2::aes(x = Sex, y = Count, fill = Sex)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge")
plot


# Add a custom palette
plot + scale_fill_hd("sex")
```
