# Multivariate computation.

Easily compute multivariate sum, mean, and other scores. Reverse scoring
can also be easily implemented without saving extra variables.
[`Alpha()`](https://psychbruce.github.io/bruceR/reference/Alpha.md)
function uses a similar method to deal with reverse scoring.

Three options to specify variables:

1.  **`var` + `items`**: common and unique parts of variable names
    (suggested).

2.  **`vars`**: a character vector of variable names (suggested).

3.  **`varrange`**: starting and stopping positions of variables (NOT
    suggested).

## Usage

``` r
COUNT(data, var = NULL, items = NULL, vars = NULL, varrange = NULL, value = NA)

MODE(data, var = NULL, items = NULL, vars = NULL, varrange = NULL)

SUM(
  data,
  var = NULL,
  items = NULL,
  vars = NULL,
  varrange = NULL,
  rev = NULL,
  range = likert,
  likert = NULL,
  na.rm = TRUE
)

.sum(
  var = NULL,
  items = NULL,
  vars = NULL,
  varrange = NULL,
  rev = NULL,
  range = likert,
  likert = NULL,
  na.rm = TRUE
)

MEAN(
  data,
  var = NULL,
  items = NULL,
  vars = NULL,
  varrange = NULL,
  rev = NULL,
  range = likert,
  likert = NULL,
  na.rm = TRUE
)

.mean(
  var = NULL,
  items = NULL,
  vars = NULL,
  varrange = NULL,
  rev = NULL,
  range = likert,
  likert = NULL,
  na.rm = TRUE
)

STD(
  data,
  var = NULL,
  items = NULL,
  vars = NULL,
  varrange = NULL,
  rev = NULL,
  range = likert,
  likert = NULL,
  na.rm = TRUE
)

CONSEC(
  data,
  var = NULL,
  items = NULL,
  vars = NULL,
  varrange = NULL,
  values = 0:9
)
```

## Arguments

- data:

  Data frame.

- var:

  **\[Option 1\]** Common part across variables: e.g., `"RSES"`,
  `"XX.{i}.pre"` (if `var` string has any placeholder in braces `{...}`,
  then `items` will be pasted into the braces, see examples)

- items:

  **\[Option 1\]** Unique part across variables: e.g., `1:10`,
  `c("a", "b", "c")`

- vars:

  **\[Option 2\]** Character vector specifying variables: e.g.,
  `c("X1", "X2", "X3", "X4", "X5")`

- varrange:

  **\[Option 3\]** Character string specifying positions
  (`"start:stop"`) of variables: e.g., `"A1:E5"`

- value:

  \[Only for `COUNT()`\] The value to be counted.

- rev:

  \[Optional\] Variables that need to be reversed. It can be (1) a
  character vector specifying the reverse-scoring variables
  (recommended), or (2) a numeric vector specifying the item number of
  reverse-scoring variables (not recommended).

- range, likert:

  \[Optional\] Range of likert scale: e.g., `1:5`, `c(1, 5)`. If not
  provided, it will be automatically estimated from the given data (BUT
  you should use this carefully).

- na.rm:

  Ignore missing values. Defaults to `TRUE`.

- values:

  \[Only for `CONSEC()`\] Values to be counted as consecutive identical
  values. Defaults to all numbers (`0:9`).

## Value

A vector of computed values.

## Functions

- `COUNT()`: **Count** a certain value across variables.

- `MODE()`: Compute **mode** across variables.

- `SUM()`: Compute **sum** across variables.

- `.sum()`: Tidy version of `SUM()`, can only be used in
  [`add()/added()`](https://psychbruce.github.io/bruceR/reference/add.md).

- `MEAN()`: Compute **mean** across variables.

- `.mean()`: Tidy version of `MEAN()`, can only be used in
  [`add()/added()`](https://psychbruce.github.io/bruceR/reference/add.md).

- `STD()`: Compute **standard deviation** across variables.

- `CONSEC()`: Compute **consecutive identical digits** across variables
  (especially useful in detecting careless responding).

## Examples

``` r
d = data.table(
  x1 = 1:5,
  x4 = c(2,2,5,4,5),
  x3 = c(3,2,NA,NA,5),
  x2 = c(4,4,NA,2,5),
  x5 = c(5,4,1,4,5)
)
d
#>       x1    x4    x3    x2    x5
#>    <int> <num> <num> <num> <num>
#> 1:     1     2     3     4     5
#> 2:     2     2     2     4     4
#> 3:     3     5    NA    NA     1
#> 4:     4     4    NA     2     4
#> 5:     5     5     5     5     5
## I deliberately set this order to show you
## the difference between "vars" and "varrange".

## ====== Usage 1: data.table `:=` ====== ##
d[, `:=`(
  na = COUNT(d, "x", 1:5, value=NA),
  n.2 = COUNT(d, "x", 1:5, value=2),
  sum = SUM(d, "x", 1:5),
  m1 = MEAN(d, "x", 1:5),
  m2 = MEAN(d, vars=c("x1", "x4")),
  m3 = MEAN(d, varrange="x1:x2", rev="x2", range=1:5),
  cons1 = CONSEC(d, "x", 1:5),
  cons2 = CONSEC(d, varrange="x1:x5")
)]
#>       x1    x4    x3    x2    x5    na   n.2   sum    m1    m2    m3 cons1
#>    <int> <num> <num> <num> <num> <int> <int> <num> <num> <num> <num> <num>
#> 1:     1     2     3     4     5     0     1    15   3.0   1.5     2     0
#> 2:     2     2     2     4     4     0     3    14   2.8   2.0     2     2
#> 3:     3     5    NA    NA     1     2     0     9   3.0   4.0     4     0
#> 4:     4     4    NA     2     4     1     1    14   3.5   4.0     4     2
#> 5:     5     5     5     5     5     0     0    25   5.0   5.0     4     5
#>    cons2
#>    <num>
#> 1:     0
#> 2:     3
#> 3:     0
#> 4:     2
#> 5:     5
d
#>       x1    x4    x3    x2    x5    na   n.2   sum    m1    m2    m3 cons1
#>    <int> <num> <num> <num> <num> <int> <int> <num> <num> <num> <num> <num>
#> 1:     1     2     3     4     5     0     1    15   3.0   1.5     2     0
#> 2:     2     2     2     4     4     0     3    14   2.8   2.0     2     2
#> 3:     3     5    NA    NA     1     2     0     9   3.0   4.0     4     0
#> 4:     4     4    NA     2     4     1     1    14   3.5   4.0     4     2
#> 5:     5     5     5     5     5     0     0    25   5.0   5.0     4     5
#>    cons2
#>    <num>
#> 1:     0
#> 2:     3
#> 3:     0
#> 4:     2
#> 5:     5

## ====== Usage 2: `add()` & `added()` ====== ##
data = as.data.table(psych::bfi)
added(data, {
  gender = as.factor(gender)
  education = as.factor(education)
  E = .mean("E", 1:5, rev=c(1,2), range=1:6)
  A = .mean("A", 1:5, rev=1, range=1:6)
  C = .mean("C", 1:5, rev=c(4,5), range=1:6)
  N = .mean("N", 1:5, range=1:6)
  O = .mean("O", 1:5, rev=c(2,5), range=1:6)
}, drop=TRUE)
data
#>       gender education     E     A     C     N     O
#>       <fctr>    <fctr> <num> <num> <num> <num> <num>
#>    1:      1      <NA>   3.8   4.0   2.8  2.80   3.0
#>    2:      2      <NA>   5.0   4.2   4.0  3.80   4.0
#>    3:      2      <NA>   4.2   3.8   4.0  3.60   4.8
#>    4:      2      <NA>   3.6   4.6   3.0  2.80   3.2
#>    5:      1      <NA>   4.8   4.0   4.4  3.20   3.6
#>   ---                                               
#> 2796:      1         3   5.0   2.2   6.0  1.00   6.0
#> 2797:      1         4   4.2   4.2   3.2  2.75   4.8
#> 2798:      2         4   5.0   4.0   5.4  2.80   5.0
#> 2799:      1         4   4.6   2.8   4.2  4.20   5.2
#> 2800:      2         4   2.6   3.0   4.2  1.40   4.6

## ====== New Feature for `var` & `items` ====== ##
d = data.table(
  XX.1.pre = 1:5,
  XX.2.pre = 6:10,
  XX.3.pre = 11:15
)
add(d, { XX.mean = .mean("XX.{i}.pre", 1:3) })
#>    XX.1.pre XX.2.pre XX.3.pre XX.mean
#>       <int>    <int>    <int>   <num>
#> 1:        1        6       11       6
#> 2:        2        7       12       7
#> 3:        3        8       13       8
#> 4:        4        9       14       9
#> 5:        5       10       15      10
add(d, { XX.mean = .mean("XX.{items}.pre", 1:3) })  # the same
#>    XX.1.pre XX.2.pre XX.3.pre XX.mean
#>       <int>    <int>    <int>   <num>
#> 1:        1        6       11       6
#> 2:        2        7       12       7
#> 3:        3        8       13       8
#> 4:        4        9       14       9
#> 5:        5       10       15      10
add(d, { XX.mean = .mean("XX.{#$%^&}.pre", 1:3) })  # the same
#>    XX.1.pre XX.2.pre XX.3.pre XX.mean
#>       <int>    <int>    <int>   <num>
#> 1:        1        6       11       6
#> 2:        2        7       12       7
#> 3:        3        8       13       8
#> 4:        4        9       14       9
#> 5:        5       10       15      10
```
