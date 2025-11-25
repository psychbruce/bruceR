# A simple extension of `%in%`.

A simple extension of `%in%`.

## Usage

``` r
pattern %partin% vector
```

## Arguments

- pattern:

  Character string containing **regular expressions** to be matched.

- vector:

  Character vector.

## Value

`TRUE` or `FALSE`.

## See also

[`%in%`](https://rdrr.io/r/base/match.html)

[`%allin%`](https://psychbruce.github.io/bruceR/reference/grapes-allin-grapes.md)

[`%anyin%`](https://psychbruce.github.io/bruceR/reference/grapes-anyin-grapes.md)

[`%nonein%`](https://psychbruce.github.io/bruceR/reference/grapes-nonein-grapes.md)

## Examples

``` r
"Bei" %partin% c("Beijing", "Shanghai")  # TRUE
#> [1] TRUE
"bei" %partin% c("Beijing", "Shanghai")  # FALSE
#> [1] FALSE
"[aeiou]ng" %partin% c("Beijing", "Shanghai")  # TRUE
#> [1] TRUE
```
