# Print strings with rich formats and colors.

Frustrated with [`print()`](https://rdrr.io/r/base/print.html) and
[`cat()`](https://rdrr.io/r/base/cat.html)? Try this! Run examples to
see what it can do.

## Usage

``` r
Print(...)

Glue(...)
```

## Arguments

- ...:

  Character strings enclosed by `"{ }"` will be evaluated as R code.

  Character strings enclosed by `"<< >>"` will be printed as formatted
  and colored text.

  Long strings are broken by line and concatenated together.

  Leading whitespace and blank lines from the first and last lines are
  automatically trimmed.

## Value

Formatted text.

## Details

Possible formats/colors that can be used in `"<< >>"` include:

\(1\) bold, italic, underline, reset, blurred, inverse, hidden,
strikethrough;

\(2\) black, white, silver, red, green, blue, yellow, cyan, magenta;

\(3\) bgBlack, bgWhite, bgRed, bgGreen, bgBlue, bgYellow, bgCyan,
bgMagenta.

See more details in
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) and
[`glue::glue_col()`](https://glue.tidyverse.org/reference/glue_col.html).

## Functions

- `Print()`: Paste and print strings.

- `Glue()`: Paste strings.

## Examples

``` r
name = "Bruce"
Print("My name is <<underline <<bold {name}>>>>.
       <<bold <<blue Pi = {pi:.15}.>>>>
       <<italic <<green 1 + 1 = {1 + 1}.>>>>
       sqrt({x}) = <<red {sqrt(x):.3}>>", x=10)
#> My name is Bruce.
#> Pi = 3.141592653589793.
#> 1 + 1 = 2.
#> sqrt(10) = 3.162
```
