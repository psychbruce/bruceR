# Format numeric values.

Format numeric values.

## Usage

``` r
formatF(x, digits = 3)
```

## Arguments

- x:

  A number or numeric vector.

- digits:

  Number of decimal places of output. Defaults to `3`.

## Value

Formatted character string.

## See also

[`format()`](https://rdrr.io/r/base/format.html)

[`formatN()`](https://psychbruce.github.io/bruceR/reference/formatN.md)

## Examples

``` r
formatF(pi, 20)
#> [1] "3.14159265358979311600"
```
