# Paste a formula into a string.

Paste a formula into a string.

## Usage

``` r
formula_paste(formula)
```

## Arguments

- formula:

  R formula.

## Value

A character string indicating the formula.

## Examples

``` r
formula_paste(y ~ x)
#> [1] "y ~ x"
formula_paste(y ~ x + (1 | g))
#> [1] "y ~ x + (1 | g)"
```
