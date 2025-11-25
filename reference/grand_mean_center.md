# Grand-mean centering.

Compute grand-mean centered variables. Usually used for GLM
interaction-term predictors and HLM level-2 predictors.

## Usage

``` r
grand_mean_center(data, vars = names(data), std = FALSE, add.suffix = "")
```

## Arguments

- data:

  Data object.

- vars:

  Variable(s) to be centered.

- std:

  Standardized or not. Defaults to `FALSE`.

- add.suffix:

  The suffix of the centered variable(s). Defaults to `""`. You may set
  it to `"_c"`, `"_center"`, etc.

## Value

A new data object containing the centered variable(s).

## See also

[`group_mean_center()`](https://psychbruce.github.io/bruceR/reference/group_mean_center.md)

## Examples

``` r
d = data.table(a=1:5, b=6:10)

d.c = grand_mean_center(d, "a")
d.c
#>        a     b
#>    <num> <int>
#> 1:    -2     6
#> 2:    -1     7
#> 3:     0     8
#> 4:     1     9
#> 5:     2    10

d.c = grand_mean_center(d, c("a", "b"), add.suffix="_center")
d.c
#>        a     b a_center b_center
#>    <int> <int>    <num>    <num>
#> 1:     1     6       -2       -2
#> 2:     2     7       -1       -1
#> 3:     3     8        0        0
#> 4:     4     9        1        1
#> 5:     5    10        2        2
```
