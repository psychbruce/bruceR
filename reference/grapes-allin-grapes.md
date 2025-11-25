# A simple extension of `%in%`.

A simple extension of `%in%`.

## Usage

``` r
x %allin% vector
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

[`%anyin%`](https://psychbruce.github.io/bruceR/reference/grapes-anyin-grapes.md)

[`%nonein%`](https://psychbruce.github.io/bruceR/reference/grapes-nonein-grapes.md)

[`%partin%`](https://psychbruce.github.io/bruceR/reference/grapes-partin-grapes.md)

## Examples

``` r
1:2 %allin% 1:3  # TRUE
#> [1] TRUE
3:4 %allin% 1:3  # FALSE
#> [1] FALSE
```
