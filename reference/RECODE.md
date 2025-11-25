# Recode a variable.

A wrapper of [`car::recode()`](https://rdrr.io/pkg/car/man/recode.html).

## Usage

``` r
RECODE(var, recodes)
```

## Arguments

- var:

  Variable (numeric, character, or factor).

- recodes:

  A character string definine the rule of recoding. e.g.,
  `"lo:1=0; c(2,3)=1; 4=2; 5:hi=3; else=999"`

## Value

A vector of recoded variable.

## Examples

``` r
d = data.table(var=c(NA, 0, 1, 2, 3, 4, 5, 6))
added(d, {
  var.new = RECODE(var, "lo:1=0; c(2,3)=1; 4=2; 5:hi=3; else=999")
})
d
#>      var var.new
#>    <num>   <num>
#> 1:    NA     999
#> 2:     0       0
#> 3:     1       0
#> 4:     2       1
#> 5:     3       1
#> 6:     4       2
#> 7:     5       3
#> 8:     6       3
```
