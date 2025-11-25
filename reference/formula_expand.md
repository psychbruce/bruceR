# Expand all interaction terms in a formula.

Expand all interaction terms in a formula.

## Usage

``` r
formula_expand(formula, as.char = FALSE)
```

## Arguments

- formula:

  R formula or a character string indicating the formula.

- as.char:

  Return character? Defaults to `FALSE`.

## Value

A formula/character object including all expanded terms.

## Examples

``` r
formula_expand(y ~ a*b*c)
#> y ~ a + b + c + a:b + a:c + b:c + a:b:c
#> <environment: 0x5639f7357980>
formula_expand("y ~ a*b*c")
#> y ~ a + b + c + a:b + a:c + b:c + a:b:c
#> <environment: 0x5639f0ab3028>
```
