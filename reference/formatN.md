# Format "1234" to "1,234".

Format "1234" to "1,234".

## Usage

``` r
formatN(x, mark = ",")
```

## Arguments

- x:

  A number or numeric vector.

- mark:

  Usually `","`.

## Value

Formatted character string.

## See also

[`format()`](https://rdrr.io/r/base/format.html)

[`formatF()`](https://psychbruce.github.io/bruceR/reference/formatF.md)

## Examples

``` r
formatN(1234)
#> [1] "1,234"
```
