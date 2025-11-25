# Min-max scaling (min-max normalization).

This function resembles
[`RESCALE()`](https://psychbruce.github.io/bruceR/reference/RESCALE.md)
and it is just equivalent to `RESCALE(var, to=0:1)`.

## Usage

``` r
scaler(v, min = 0, max = 1)
```

## Arguments

- v:

  Variable (numeric vector).

- min:

  Minimum value (defaults to 0).

- max:

  Maximum value (defaults to 1).

## Value

A vector of rescaled variable.

## Examples

``` r
scaler(1:5)
#> [1] 0.00 0.25 0.50 0.75 1.00
# the same: RESCALE(1:5, to=0:1)
```
