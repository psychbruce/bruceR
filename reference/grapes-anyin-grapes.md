# A simple extension of `%in%`.

A simple extension of `%in%`.

## Usage

``` r
x %anyin% vector
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

[`%nonein%`](https://psychbruce.github.io/bruceR/reference/grapes-nonein-grapes.md)

[`%partin%`](https://psychbruce.github.io/bruceR/reference/grapes-partin-grapes.md)

## Examples

``` r
3:4 %anyin% 1:3  # TRUE
#> [1] TRUE
4:5 %anyin% 1:3  # FALSE
#> [1] FALSE
```
