# Import data from a file (TXT, CSV, Excel, SPSS, Stata, ...) or clipboard.

Import data from a file, with format automatically judged from file
extension. This function is inspired by
[`rio::import()`](http://gesistsa.github.io/rio/reference/import.md) and
has several modifications. Its purpose is to avoid using lots of
`read_xxx()` functions in your code and to provide one tidy function for
data import. It supports many file formats (local or URL) and uses the
corresponding R functions:

- Plain text (.txt, .csv, .csv2, .tsv, .psv), using
  [`data.table::fread()`](https://rdatatable.gitlab.io/data.table/reference/fread.html)

- Excel (.xls, .xlsx), using
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html)

- SPSS (.sav), using
  [`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
  or
  [`foreign::read.spss()`](https://rdrr.io/pkg/foreign/man/read.spss.html)

- Stata (.dta), using
  [`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html)
  or
  [`foreign::read.dta()`](https://rdrr.io/pkg/foreign/man/read.dta.html)

- R objects (.rda, .rdata, .RData), using
  [`load()`](https://rdrr.io/r/base/load.html)

- R serialized objects (.rds), using
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html)

- Clipboard (on Windows and Mac OS), using
  [`clipr::read_clip_tbl()`](http://matthewlincoln.net/clipr/reference/read_clip_tbl.md)

- Other formats, using
  [`rio::import()`](http://gesistsa.github.io/rio/reference/import.md)

## Usage

``` r
import(
  file,
  encoding = NULL,
  header = "auto",
  sheet = NULL,
  range = NULL,
  pkg = c("haven", "foreign"),
  value.labels = FALSE,
  as = "data.frame",
  verbose = FALSE
)
```

## Arguments

- file:

  File name (with extension). If unspecified, then data will be imported
  from clipboard.

- encoding:

  File encoding. Defaults to `NULL`.

  Options: `"UTF-8"`, `"GBK"`, `"CP936"`, etc.

  If you find messy code for Chinese text in the imported data, it is
  usually effective to set `encoding="UTF-8"`.

- header:

  Does the first row contain column names (`TRUE` or `FALSE`)? Defaults
  to `"auto"`.

- sheet:

  \[Only for Excel\] Excel sheet name (or sheet number). Defaults to the
  first sheet. Ignored if the sheet is specified via `range`.

- range:

  \[Only for Excel\] Excel cell range. Defaults to all cells in a sheet.
  You may specify it as `range="A1:E100"` or `range="Sheet1!A1:E100"`.

- pkg:

  \[Only for SPSS & Stata\] Use which R package to read SPSS (.sav) or
  Stata (.dta) data file? Defaults to `"haven"`. You may also use
  `"foreign"`.

  Notably, `"haven"` may be preferred because it is more robust to
  non-English characters and can also keep variable labels
  (descriptions) from SPSS.

- value.labels:

  \[Only for SPSS & Stata\] Convert variables with value labels into R
  factors with those levels? Defaults to `FALSE`.

- as:

  Class of the imported data. Defaults to `"data.frame"`. Ignored if the
  file is an R data object (.rds, .rda, .rdata, .RData).

  Options:

  - data.frame: `"data.frame"`, `"df"`, `"DF"`

  - data.table: `"data.table"`, `"dt"`, `"DT"`

  - tbl_df: `"tibble"`, `"tbl_df"`, `"tbl"`

- verbose:

  Print data information? Defaults to `FALSE`.

## Value

A data object (default class is `data.frame`).

## See also

[`export()`](https://psychbruce.github.io/bruceR/reference/export.md)

## Examples

``` r
if (FALSE) { # \dontrun{

  # Import data from system clipboard
  data = import()  # read from clipboard (on Windows and Mac OS)

  # If you have an Excel file named "mydata.xlsx"
  export(airquality, file="mydata.xlsx")

  # Import data from a file
  data = import("mydata.xlsx")  # default: data.frame
  data = import("mydata.xlsx", as="data.table")
} # }
```
