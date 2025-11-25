# Run code parsed from text.

Run code parsed from text.

## Usage

``` r
Run(..., silent = FALSE)
```

## Arguments

- ...:

  Character string(s) to run. You can use `"{ }"` to insert any R object
  in the environment.

- silent:

  Suppress error/warning messages. Defaults to `FALSE`.

## Value

Invisibly return the running expression(s).

## Examples

``` r
Run("a=1", "b=2")
Run("print({a+b})")
#> [1] 3
```
