# Group-mean centering.

Compute group-mean centered variables. Usually used for HLM level-1
predictors.

## Usage

``` r
group_mean_center(
  data,
  vars = setdiff(names(data), by),
  by,
  std = FALSE,
  add.suffix = "",
  add.group.mean = "_mean"
)
```

## Arguments

- data:

  Data object.

- vars:

  Variable(s) to be centered.

- by:

  Grouping variable.

- std:

  Standardized or not. Defaults to `FALSE`.

- add.suffix:

  The suffix of the centered variable(s). Defaults to `""`. You may set
  it to `"_c"`, `"_center"`, etc.

- add.group.mean:

  The suffix of the variable name(s) of group means. Defaults to
  `"_mean"` (see Examples).

## Value

A new data object containing the centered variable(s).

## See also

[`grand_mean_center()`](https://psychbruce.github.io/bruceR/reference/grand_mean_center.md)

## Examples

``` r
d = data.table(x=1:9, g=rep(1:3, each=3))

d.c = group_mean_center(d, "x", by="g")
d.c
#>        x     g x_mean
#>    <num> <int>  <num>
#> 1:    -1     1      2
#> 2:     0     1      2
#> 3:     1     1      2
#> 4:    -1     2      5
#> 5:     0     2      5
#> 6:     1     2      5
#> 7:    -1     3      8
#> 8:     0     3      8
#> 9:     1     3      8

d.c = group_mean_center(d, "x", by="g", add.suffix="_c")
d.c
#>        x     g x_mean   x_c
#>    <int> <int>  <num> <num>
#> 1:     1     1      2    -1
#> 2:     2     1      2     0
#> 3:     3     1      2     1
#> 4:     4     2      5    -1
#> 5:     5     2      5     0
#> 6:     6     2      5     1
#> 7:     7     3      8    -1
#> 8:     8     3      8     0
#> 9:     9     3      8     1
```
