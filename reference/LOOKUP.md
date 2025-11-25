# Search, match, and look up values (like Excel's functions `INDEX + MATCH`).

In Excel, we can use `VLOOKUP`, `HLOOKUP`, `XLOOKUP` (a new function
released in 2019), or the combination of `INDEX` and `MATCH` to search,
match, and look up values. Here I provide a similar function. If
multiple values were simultaneously matched, a warning message would be
printed.

## Usage

``` r
LOOKUP(
  data,
  vars,
  data.ref,
  vars.ref,
  vars.lookup,
  return = c("new.data", "new.var", "new.value")
)
```

## Arguments

- data:

  Main data.

- vars:

  Character (vector), specifying the variable(s) to be searched in
  `data`.

- data.ref:

  Reference data containing both the reference variable(s) and the
  lookup variable(s).

- vars.ref:

  Character (vector), with the **same length and order** as `vars`,
  specifying the reference variable(s) to be matched in `data.ref`.

- vars.lookup:

  Character (vector), specifying the variable(s) to be looked up and
  returned from `data.ref`.

- return:

  What to return. Default (`"new.data"`) is to return a data frame with
  the lookup values added. You may also set it to `"new.var"` or
  `"new.value"`.

## Value

New data object, new variable, or new value (see the argument `return`).

## See also

[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)

## Examples

``` r
ref = data.table(City=rep(c("A", "B", "C"), each=5),
                 Year=rep(2013:2017, times=3),
                 GDP=sample(1000:2000, 15),
                 PM2.5=sample(10:300, 15))
ref
#>       City  Year   GDP PM2.5
#>     <char> <int> <int> <int>
#>  1:      A  2013  1439    36
#>  2:      A  2014  1275   102
#>  3:      A  2015  1827    52
#>  4:      A  2016  1879   128
#>  5:      A  2017  1564   115
#>  6:      B  2013  1988   286
#>  7:      B  2014  1456   183
#>  8:      B  2015  1450   150
#>  9:      B  2016  1258    85
#> 10:      B  2017  1203    37
#> 11:      C  2013  1286   190
#> 12:      C  2014  1739   243
#> 13:      C  2015  1015   175
#> 14:      C  2016  1689    91
#> 15:      C  2017  1814   203

data = data.table(sub=1:5,
                  city=c("A", "A", "B", "C", "C"),
                  year=c(2013, 2014, 2015, 2016, 2017))
data
#>      sub   city  year
#>    <int> <char> <num>
#> 1:     1      A  2013
#> 2:     2      A  2014
#> 3:     3      B  2015
#> 4:     4      C  2016
#> 5:     5      C  2017

LOOKUP(data, c("city", "year"), ref, c("City", "Year"), "GDP")
#>      sub   city  year   GDP
#>    <int> <char> <num> <int>
#> 1:     1      A  2013  1439
#> 2:     2      A  2014  1275
#> 3:     3      B  2015  1450
#> 4:     4      C  2016  1689
#> 5:     5      C  2017  1814
LOOKUP(data, c("city", "year"), ref, c("City", "Year"), c("GDP", "PM2.5"))
#>      sub   city  year   GDP PM2.5
#>    <int> <char> <num> <int> <int>
#> 1:     1      A  2013  1439    36
#> 2:     2      A  2014  1275   102
#> 3:     3      B  2015  1450   150
#> 4:     4      C  2016  1689    91
#> 5:     5      C  2017  1814   203
```
