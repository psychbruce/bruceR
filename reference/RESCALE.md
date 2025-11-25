# Rescale a variable (e.g., from 5-point to 7-point).

Rescale a variable (e.g., from 5-point to 7-point).

## Usage

``` r
RESCALE(var, from = range(var, na.rm = T), to)
```

## Arguments

- var:

  Variable (numeric).

- from:

  Numeric vector, the range of old scale (e.g., `1:5`). If not defined,
  it will compute the range of `var`.

- to:

  Numeric vector, the range of new scale (e.g., `1:7`).

## Value

A vector of rescaled variable.

## Examples

``` r
d = data.table(var=rep(1:5, 2))
added(d, {
  var1 = RESCALE(var, to=1:7)
  var2 = RESCALE(var, from=1:5, to=1:7)
})
d  # var1 is equal to var2
#>       var  var1  var2
#>     <int> <num> <num>
#>  1:     1   1.0   1.0
#>  2:     2   2.5   2.5
#>  3:     3   4.0   4.0
#>  4:     4   5.5   5.5
#>  5:     5   7.0   7.0
#>  6:     1   1.0   1.0
#>  7:     2   2.5   2.5
#>  8:     3   4.0   4.0
#>  9:     4   5.5   5.5
#> 10:     5   7.0   7.0
```
