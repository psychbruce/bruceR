# A simple extension of `%in%`.

A simple extension of `%in%`.

## Usage

``` r
x %nonein% vector
```

## Arguments

- x:

  Numeric or character vector.

- vector:

  Numeric or character vector.

## Value

`TRUE` or `FALSE`.

## See also

[`%in%`](https://rdrr.io/r/base/match.html)

[`%allin%`](https://psychbruce.github.io/bruceR/reference/grapes-allin-grapes.md)

[`%anyin%`](https://psychbruce.github.io/bruceR/reference/grapes-anyin-grapes.md)

[`%partin%`](https://psychbruce.github.io/bruceR/reference/grapes-partin-grapes.md)

## Examples

``` r
3:4 %nonein% 1:3  # FALSE
#> [1] FALSE
4:5 %nonein% 1:3  # TRUE
#> [1] TRUE
```
