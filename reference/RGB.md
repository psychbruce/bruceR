# A simple extension of [`rgb()`](https://rdrr.io/r/grDevices/rgb.html).

A simple extension of [`rgb()`](https://rdrr.io/r/grDevices/rgb.html).

## Usage

``` r
RGB(r, g, b, alpha)
```

## Arguments

- r, g, b:

  Red, Green, Blue: 0~255.

- alpha:

  Color transparency (opacity): 0~1. If not specified, an opaque color
  will be generated.

## Value

`"#rrggbb"` or `"#rrggbbaa"`.

## Examples

``` r
RGB(255, 0, 0)  # red: "#FF0000"
#> [1] "#FF0000"
RGB(255, 0, 0, 0.8)  # red with 80% opacity: "#FF0000CC"
#> [1] "#FF0000CC"
```
