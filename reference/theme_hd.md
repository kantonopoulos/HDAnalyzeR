# HDAnalyzeR theme

`theme_hd()` creates a theme for ggplot2 plots that is used by the Human
Disease Blood Atlas resource of the Human Protein Atlas (HPA) project.

## Usage

``` r
theme_hd(angled = 0, axis_x = TRUE, axis_y = TRUE, facet_title = TRUE)
```

## Arguments

- angled:

  The angle of the x-axis text. Default is 0.

- axis_x:

  If FALSE, the x-axis is removed. Default is TRUE.

- axis_y:

  If FALSE, the y-axis is removed. Default is TRUE.

- facet_title:

  If FALSE, the facet title is removed. Default is TRUE.

## Value

A ggplot2 theme object.

## Examples

``` r
# Create a plot
plot <- example_metadata |>
  ggplot2::ggplot(ggplot2::aes(x = Sex)) +
  ggplot2::geom_bar()
plot


# Apply the HPA theme
plot + theme_hd()
```
