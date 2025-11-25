# Print a three-line table (to R Console and Microsoft Word).

This basic function prints any data frame as a three-line table to
either R Console or Microsoft Word (.doc). It has been used in many
other functions of `bruceR` (see below).

## Usage

``` r
print_table(
  x,
  digits = 3,
  nspaces = 1,
  row.names = TRUE,
  col.names = TRUE,
  title = "",
  note = "",
  append = "",
  line = TRUE,
  file = NULL,
  file.align.head = "auto",
  file.align.text = "auto"
)
```

## Arguments

- x:

  Matrix, data.frame (or data.table), or any model object (e.g., `lm`,
  `glm`, `lmer`, `glmer`, ...).

- digits:

  Numeric vector specifying the number of decimal places of output.
  Defaults to `3`.

- nspaces:

  Number of whitespaces between columns. Defaults to `1`.

- row.names, col.names:

  Print row/column names. Defaults to `TRUE` (column names are always
  printed). To modify the names, you can use a character vector with the
  same length as the raw names.

- title:

  Title text, which will be inserted in `<p></p>` (HTML code).

- note:

  Note text, which will be inserted in `<p></p>` (HTML code).

- append:

  Other contents, which will be appended in the end (HTML code).

- line:

  Lines looks like true line (`TRUE`) or `=== --- ===` (`FALSE`).

- file:

  File name of MS Word (`.doc`).

- file.align.head, file.align.text:

  Alignment of table head or table text: `"left"`, `"right"`,
  `"center"`. Either one value of them OR a character vector of mixed
  values with the same length as the table columns. Default alignment
  (if set as `"auto"`): left, right, right, ..., right.

## Value

Invisibly return a list of data frame and HTML code.

## Examples

``` r
print_table(data.frame(x=1))
#> ────────
#>        x
#> ────────
#> 1  1.000
#> ────────

print_table(airquality, file="airquality.doc")
#> ✔ Table saved to "/home/runner/work/bruceR/bruceR/docs/reference/airquality.doc"
#> 
unlink("airquality.doc")  # delete file for code check

model = lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
print_table(model)
#> ──────────────────────────────────────────────
#>              Estimate    S.E.      t     p    
#> ──────────────────────────────────────────────
#> (Intercept)    68.770 (4.391) 15.662 <.001 ***
#> Month           2.225 (0.441)  5.047 <.001 ***
#> Day            -0.084 (0.070) -1.194  .234    
#> Wind           -1.003 (0.176) -5.695 <.001 ***
#> Solar.R         0.027 (0.007)  3.991 <.001 ***
#> ──────────────────────────────────────────────
print_table(model, file="model.doc")
#> ✔ Table saved to "/home/runner/work/bruceR/bruceR/docs/reference/model.doc"
#> 
unlink("model.doc")  # delete file for code check
```
