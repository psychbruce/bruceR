# Set working directory to the path of currently opened file.

Set working directory to the path of currently opened file (usually an R
script). You may use this function in both **.R/.Rmd files and R
Console**. [RStudio](https://posit.co/download/rstudio-desktop/)
(version \>= 1.2) is required for running this function.

## Usage

``` r
set.wd(path = NULL, ask = FALSE)

set_wd(path = NULL, ask = FALSE)
```

## Arguments

- path:

  `NULL` (default) or a specific path. Defaults to extract the path of
  the currently opened file (usually .R or .Rmd) using the
  [`rstudioapi::getSourceEditorContext`](https://rstudio.github.io/rstudioapi/reference/rstudio-editors.html)
  function.

- ask:

  `TRUE` or `FALSE` (default). If `TRUE`, you can select a folder with
  the prompt of a dialog.

## Value

Invisibly return the path.

## Functions

- `set.wd()`: Main function

- `set_wd()`: The alias of `set.wd` (the same)

## See also

[`setwd()`](https://rdrr.io/r/base/getwd.html)

## Examples

``` r
if (FALSE) { # \dontrun{

  # RStudio (version >= 1.2) is required for running this function.
  set.wd()  # set working directory to the path of the currently opened file
  set.wd("~/")  # set working directory to the home path
  set.wd("../")  # set working directory to the parent path
  set.wd(ask=TRUE)  # select a folder with the prompt of a dialog
} # }
```
