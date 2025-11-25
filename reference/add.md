# Create, modify, and delete variables.

Enhanced functions to create, modify, and/or delete variables. The
functions **integrate** the advantages of
[`base::within()`](https://rdrr.io/r/base/with.html),
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html),
and
[`data.table::let()`](https://rdatatable.gitlab.io/data.table/reference/assign.html).

## Usage

``` r
add(data, expr, when, by, drop = FALSE)

added(data, expr, when, by, drop = FALSE)
```

## Arguments

- data:

  A
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  (preferred).

- expr:

  Passing to
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html):
  `DT[ , let(expr), ]`

  R expression(s) to compute variables. Execute each line of expression
  *one by one*, such that newly created variables are available
  immediately. This is an advantage of
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  and has been implemented here for
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

- when:

  \[Optional\] Passing to
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html):
  `DT[when, , ]`

  Compute *for* which rows or rows meeting what condition(s)?

- by:

  \[Optional\] Passing to
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html):
  `DT[ , , by]`

  Compute *by* what group(s)?

- drop:

  Drop existing variables and return only new variables? Defaults to
  `FALSE`, which returns all variables.

## Value

- `add()` returns a new
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html),
  with the raw data unchanged.

- `added()` returns nothing and has already changed the raw data.

## Examples

``` r
## ====== Usage 1: add() ====== ##

d = as.data.table(within.1)
d$XYZ = 1:8
d
#>        ID    A1    A2    A3    A4   XYZ
#>    <char> <num> <num> <num> <num> <int>
#> 1:     S1     3     4     8     9     1
#> 2:     S2     6     6     9     8     2
#> 3:     S3     4     4     8     8     3
#> 4:     S4     3     2     7     7     4
#> 5:     S5     5     4     5    12     5
#> 6:     S6     7     5     6    13     6
#> 7:     S7     5     3     7    12     7
#> 8:     S8     2     3     6    11     8

# add() does not change the raw data:
add(d, {B = 1; C = 2})
#>        ID    A1    A2    A3    A4   XYZ     B     C
#>    <char> <num> <num> <num> <num> <int> <num> <num>
#> 1:     S1     3     4     8     9     1     1     2
#> 2:     S2     6     6     9     8     2     1     2
#> 3:     S3     4     4     8     8     3     1     2
#> 4:     S4     3     2     7     7     4     1     2
#> 5:     S5     5     4     5    12     5     1     2
#> 6:     S6     7     5     6    13     6     1     2
#> 7:     S7     5     3     7    12     7     1     2
#> 8:     S8     2     3     6    11     8     1     2
d
#>        ID    A1    A2    A3    A4   XYZ
#>    <char> <num> <num> <num> <num> <int>
#> 1:     S1     3     4     8     9     1
#> 2:     S2     6     6     9     8     2
#> 3:     S3     4     4     8     8     3
#> 4:     S4     3     2     7     7     4
#> 5:     S5     5     4     5    12     5
#> 6:     S6     7     5     6    13     6
#> 7:     S7     5     3     7    12     7
#> 8:     S8     2     3     6    11     8

# new data should be assigned to an object:
d = d %>% add({
  ID = str_extract(ID, "\\d")  # modify a variable
  XYZ = NULL                   # delete a variable
  A = .mean("A", 1:4)          # create a new variable
  B = A * 4    # new variable is immediately available
  C = 1        # never need ,/; at the end of any line
})
d
#>        ID    A1    A2    A3    A4     A     B     C
#>    <char> <num> <num> <num> <num> <num> <num> <num>
#> 1:      1     3     4     8     9  6.00    24     1
#> 2:      2     6     6     9     8  7.25    29     1
#> 3:      3     4     4     8     8  6.00    24     1
#> 4:      4     3     2     7     7  4.75    19     1
#> 5:      5     5     4     5    12  6.50    26     1
#> 6:      6     7     5     6    13  7.75    31     1
#> 7:      7     5     3     7    12  6.75    27     1
#> 8:      8     2     3     6    11  5.50    22     1


## ====== Usage 2: added() ====== ##

d = as.data.table(within.1)
d$XYZ = 1:8
d
#>        ID    A1    A2    A3    A4   XYZ
#>    <char> <num> <num> <num> <num> <int>
#> 1:     S1     3     4     8     9     1
#> 2:     S2     6     6     9     8     2
#> 3:     S3     4     4     8     8     3
#> 4:     S4     3     2     7     7     4
#> 5:     S5     5     4     5    12     5
#> 6:     S6     7     5     6    13     6
#> 7:     S7     5     3     7    12     7
#> 8:     S8     2     3     6    11     8

# added() has already changed the raw data:
added(d, {B = 1; C = 2})
d
#>        ID    A1    A2    A3    A4   XYZ     B     C
#>    <char> <num> <num> <num> <num> <int> <num> <num>
#> 1:     S1     3     4     8     9     1     1     2
#> 2:     S2     6     6     9     8     2     1     2
#> 3:     S3     4     4     8     8     3     1     2
#> 4:     S4     3     2     7     7     4     1     2
#> 5:     S5     5     4     5    12     5     1     2
#> 6:     S6     7     5     6    13     6     1     2
#> 7:     S7     5     3     7    12     7     1     2
#> 8:     S8     2     3     6    11     8     1     2

# raw data has already become the new data:
added(d, {
  ID = str_extract(ID, "\\d")
  XYZ = NULL
  A = .mean("A", 1:4)
  B = A * 4
  C = 1
})
d
#>        ID    A1    A2    A3    A4     B     C     A
#>    <char> <num> <num> <num> <num> <num> <num> <num>
#> 1:      1     3     4     8     9    24     1  6.00
#> 2:      2     6     6     9     8    29     1  7.25
#> 3:      3     4     4     8     8    24     1  6.00
#> 4:      4     3     2     7     7    19     1  4.75
#> 5:      5     5     4     5    12    26     1  6.50
#> 6:      6     7     5     6    13    31     1  7.75
#> 7:      7     5     3     7    12    27     1  6.75
#> 8:      8     2     3     6    11    22     1  5.50


## ====== Using `when` and `by` ====== ##

d = as.data.table(between.2)
d
#>         A     B SCORE
#>     <num> <num> <num>
#>  1:     1     1     3
#>  2:     1     1     6
#>  3:     1     1     4
#>  4:     1     1     3
#>  5:     1     2     4
#>  6:     1     2     6
#>  7:     1     2     4
#>  8:     1     2     2
#>  9:     1     3     5
#> 10:     1     3     7
#> 11:     1     3     5
#> 12:     1     3     2
#> 13:     2     1     4
#> 14:     2     1     5
#> 15:     2     1     3
#> 16:     2     1     3
#> 17:     2     2     8
#> 18:     2     2     9
#> 19:     2     2     8
#> 20:     2     2     7
#> 21:     2     3    12
#> 22:     2     3    13
#> 23:     2     3    12
#> 24:     2     3    11
#>         A     B SCORE

added(d, {SCORE2 = SCORE - mean(SCORE)},
      A == 1 & B %in% 1:2,  # `when`: for what conditions
      by=B)                 # `by`: by what groups
d
#> Index: <B__A>
#>         A     B SCORE SCORE2
#>     <num> <num> <num>  <num>
#>  1:     1     1     3     -1
#>  2:     1     1     6      2
#>  3:     1     1     4      0
#>  4:     1     1     3     -1
#>  5:     1     2     4      0
#>  6:     1     2     6      2
#>  7:     1     2     4      0
#>  8:     1     2     2     -2
#>  9:     1     3     5     NA
#> 10:     1     3     7     NA
#> 11:     1     3     5     NA
#> 12:     1     3     2     NA
#> 13:     2     1     4     NA
#> 14:     2     1     5     NA
#> 15:     2     1     3     NA
#> 16:     2     1     3     NA
#> 17:     2     2     8     NA
#> 18:     2     2     9     NA
#> 19:     2     2     8     NA
#> 20:     2     2     7     NA
#> 21:     2     3    12     NA
#> 22:     2     3    13     NA
#> 23:     2     3    12     NA
#> 24:     2     3    11     NA
#>         A     B SCORE SCORE2
na.omit(d)
#>        A     B SCORE SCORE2
#>    <num> <num> <num>  <num>
#> 1:     1     1     3     -1
#> 2:     1     1     6      2
#> 3:     1     1     4      0
#> 4:     1     1     3     -1
#> 5:     1     2     4      0
#> 6:     1     2     6      2
#> 7:     1     2     4      0
#> 8:     1     2     2     -2


## ====== Return Only New Variables ====== ##

newvars = add(within.1, {
  ID = str_extract(ID, "\\d")
  A = .mean("A", 1:4)
}, drop=TRUE)
newvars
#>        ID     A
#>    <char> <num>
#> 1:      1  6.00
#> 2:      2  7.25
#> 3:      3  6.00
#> 4:      4  4.75
#> 5:      5  6.50
#> 6:      6  7.75
#> 7:      7  6.75
#> 8:      8  5.50


## ====== Better Than `base::within()` ====== ##

d = as.data.table(within.1)

# wrong order: C B A
within(d, {
  A = 4
  B = A + 1
  C = 6
})
#>        ID    A1    A2    A3    A4     C     B     A
#>    <char> <num> <num> <num> <num> <num> <num> <num>
#> 1:     S1     3     4     8     9     6     5     4
#> 2:     S2     6     6     9     8     6     5     4
#> 3:     S3     4     4     8     8     6     5     4
#> 4:     S4     3     2     7     7     6     5     4
#> 5:     S5     5     4     5    12     6     5     4
#> 6:     S6     7     5     6    13     6     5     4
#> 7:     S7     5     3     7    12     6     5     4
#> 8:     S8     2     3     6    11     6     5     4

# correct order: A B C
add(d, {
  A = 4
  B = A + 1
  C = 6
})
#>        ID    A1    A2    A3    A4     A     B     C
#>    <char> <num> <num> <num> <num> <num> <num> <num>
#> 1:     S1     3     4     8     9     4     5     6
#> 2:     S2     6     6     9     8     4     5     6
#> 3:     S3     4     4     8     8     4     5     6
#> 4:     S4     3     2     7     7     4     5     6
#> 5:     S5     5     4     5    12     4     5     6
#> 6:     S6     7     5     6    13     4     5     6
#> 7:     S7     5     3     7    12     4     5     6
#> 8:     S8     2     3     6    11     4     5     6
```
