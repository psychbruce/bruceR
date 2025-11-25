# Export data to a file (TXT, CSV, Excel, SPSS, Stata, ...) or clipboard.

Export data to a file, with format automatically judged from file
extension. This function is inspired by
[`rio::export()`](http://gesistsa.github.io/rio/reference/export.md) and
has several modifications. Its purpose is to avoid using lots of
`write_xxx()` functions in your code and to provide one tidy function
for data export.

It supports many file formats and uses corresponding R functions:

- Plain text (.txt, .csv, .csv2, .tsv, .psv), using
  [`data.table::fwrite()`](https://rdatatable.gitlab.io/data.table/reference/fwrite.html);
  if the `encoding` argument is specified, using
  [`utils::write.table()`](https://rdrr.io/r/utils/write.table.html)
  instead

- Excel (.xls, .xlsx), using
  [`openxlsx::write.xlsx()`](https://rdrr.io/pkg/openxlsx/man/write.xlsx.html)

- SPSS (.sav), using
  [`haven::write_sav()`](https://haven.tidyverse.org/reference/read_spss.html)

- Stata (.dta), using
  [`haven::write_dta()`](https://haven.tidyverse.org/reference/read_dta.html)

- R objects (.rda, .rdata, .RData), using
  [`save()`](https://rdrr.io/r/base/save.html)

- R serialized objects (.rds), using
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)

- Clipboard (on Windows and Mac OS), using
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md)

- Other formats, using
  [`rio::export()`](http://gesistsa.github.io/rio/reference/export.md)

## Usage

``` r
export(
  x,
  file,
  encoding = NULL,
  header = "auto",
  sheet = NULL,
  overwrite = TRUE,
  verbose = FALSE
)
```

## Arguments

- x:

  Any R object, usually a data frame (`data.frame`, `data.table`,
  `tbl_df`). Multiple R objects should be included in a *named* `list`
  (see examples). To save R objects, specify `file` with extensions
  `.rda`, `.rdata`, or `.RData`.

- file:

  File name (with extension). If unspecified, data will be exported to
  clipboard.

- encoding:

  File encoding. Defaults to `NULL`.

  Options: `"UTF-8"`, `"GBK"`, `"CP936"`, etc.

  If you find messy code for Chinese text in the exported data (often in
  CSV when opened with Excel), it is usually useful to set
  `encoding="GBK"` or `encoding="CP936"`.

- header:

  Does the first row contain column names (`TRUE` or `FALSE`)? Defaults
  to `"auto"`.

- sheet:

  \[Only for Excel\] Excel sheet name(s). Defaults to "Sheet1",
  "Sheet2", ... You may specify multiple sheet names in a character
  vector [`c()`](https://rdrr.io/r/base/c.html) with the *same length*
  as `x` (see examples).

- overwrite:

  Overwrite the existing file (if any)? Defaults to `TRUE`.

- verbose:

  Print output information? Defaults to `FALSE`.

## Value

No return value.

## See also

[`import()`](https://psychbruce.github.io/bruceR/reference/import.md)

[`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md)

## Examples

``` r
if (FALSE) { # \dontrun{

  export(airquality)  # paste to clipboard
  export(airquality, file="mydata.csv")
  export(airquality, file="mydata.sav")

  export(list(airquality, npk), file="mydata.xlsx")  # Sheet1, Sheet2
  export(list(air=airquality, npk=npk), file="mydata.xlsx")  # a named list
  export(list(airquality, npk), sheet=c("air", "npk"), file="mydata.xlsx")

  export(list(a=1, b=npk, c="character"), file="abc.Rdata")  # .rda, .rdata
  d = import("abc.Rdata")  # load only the first object and rename it to `d`
  load("abc.Rdata")  # load all objects with original names to environment

  export(lm(yield ~ N*P*K, data=npk), file="lm_npk.Rdata")
  model = import("lm_npk.Rdata")
  load("lm_npk.Rdata")  # because x is unnamed, the object has a name "List1"

  export(list(m1=lm(yield ~ N*P*K, data=npk)), file="lm_npk.Rdata")
  model = import("lm_npk.Rdata")
  load("lm_npk.Rdata")  # because x is named, the object has a name "m1"
} # }
```
