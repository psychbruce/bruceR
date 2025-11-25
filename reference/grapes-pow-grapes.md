# Paste strings together.

Paste strings together. A wrapper of
[`paste0()`](https://rdrr.io/r/base/paste.html). Why `%^%`? Because
typing `%` and `^` is pretty easy by pressing **Shift + 5 + 6 + 5**.

## Usage

``` r
x %^% y
```

## Arguments

- x, y:

  Any objects, usually a numeric or character string or vector.

## Value

A character string/vector of the pasted values.

## Examples

``` r
"He" %^% "llo"
#> [1] "Hello"
"X" %^% 1:10
#>  [1] "X1"  "X2"  "X3"  "X4"  "X5"  "X6"  "X7"  "X8"  "X9"  "X10"
"Q" %^% 1:5 %^% letters[1:5]
#> [1] "Q1a" "Q2b" "Q3c" "Q4d" "Q5e"
```
