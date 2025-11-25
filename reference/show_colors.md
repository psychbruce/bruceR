# Show colors.

Show colors.

## Usage

``` r
show_colors(colors)
```

## Arguments

- colors:

  Color names.

  Examples:

  - `"red"` (R base color names)

  - `"#FF0000"` (hex color names)

  - [`see::social_colors()`](https://easystats.github.io/see/reference/social_colors.html)

  - `viridis::viridis_pal()(10)`

  - `RColorBrewer::brewer.pal(name="Set1", n=9)`

  - `RColorBrewer::brewer.pal(name="Set2", n=8)`

  - `RColorBrewer::brewer.pal(name="Spectral", n=11)`

## Value

A `ggplot` object.

## Examples

``` r
show_colors("blue")

show_colors("#0000FF")  # blue (hex name)

show_colors(RGB(0, 0, 255))  # blue (RGB)

show_colors(see::social_colors())

show_colors(see::pizza_colors())

```
