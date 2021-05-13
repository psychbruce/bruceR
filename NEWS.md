**If you are viewing this file on CRAN, please check [latest news on GitHub](https://github.com/psychbruce/bruceR/blob/master/NEWS.md) where the formatting is also better.**

# bruceR 0.6.4 (in development)

### New Features

-   Added Word output (.doc) in `print_table()` and other functions using `print_table()` inside: `Describe()`, `Freq()`, `Corr()`, `MANOVA()`, `med_summary()`, `granger_causality()`.

### Minor Changes

-   Added a disclaimer about Cohen's *d* in the output and documentation of `EMMEANS()`: There is considerable disagreement on how to compute Cohen's *d*. Users should not take the default output as the only right results and are completely responsible for setting the "sd.pooled".

### Bug Fixes

-   Fixed bugs in `model_summary()`: (1) Model names with `NULL`; (2) Multicollinearity check results with `NULL` or other problems; (3) UTF-8 encoding problem in WPS software (no such problem in Microsoft Word).

# bruceR 0.6.3 (Apr 2021)

### New Features

-   New function `granger_causality()`: Granger causality test (multivariate) based on vector autoregression (VAR) model. This function is an advanced and more general version of the function `granger_test()` (bivariate).
-   Added logo (designed by [\@Meijia Li](https://github.com/Stellapros))

# bruceR 0.6.2 (Apr 2021)

### Major Changes

-   Improved `set.wd()`: Now it uses `rstudioapi::getSourceEditorContext()` to extract file path (even effective when running in R console), which only requires RStudio version \>= 0.99.1111 and no longer has encoding problems (see release note in 0.6.1).
-   Improved `theme_bruce()`: Now it uses `ggtext::element_markdown()` to render Markdown/HTML rich text format, which can be used in plot text (e.g., titles).
-   Improved `EMMEANS()`: Now its results are always identical to those in SPSS (by setting `model="multivariate"` in `emmeans::joint_tests()` and `emmeans::emmeans()`, which use the `lm` or `mlm` objects rather than the `aov` object to perform tests). For a few cases with singular error matrix (i.e., some variables are linearly dependent), the results of simple-effect *F* tests will not be reported, but estimated marginal means and pairwise comparisons are not affected and so are still reported. Note that the `EMMEANS` results in old versions of `bruceR` (version \< 0.6.0) were identical to SPSS, but version 0.6.0 deprecated the parameter `repair` and no longer set `model$aov=NULL`, which made the results not identical to SPSS (particularly for ANOVAs with repeated measures). In response to a user's feedback, now 0.6.2 has improved this function and makes its results accurate again.

### Minor Changes

-   Improved function links in R documentation: `\code{\link[package:function]{package::function()}}`.

### Bug Fixes

-   Fixed a bug in `CFA()` (for lavaan-style output).

# bruceR 0.6.1 (Mar 2021)

### New Features

-   New function `HLM_ICC_rWG()`: Tidy report of HLM indices "ICC(1)" (non-independence of data), "ICC(2)" (reliability of group means), and "rWG"/"rWG(J)" (within-group agreement for single-item/multi-item measures).
-   New function `Run()`: Run code parsed from text.
-   New function `show_colors()`: Show multiple colors (or a palette) in a plot.
-   New function `%^%`: Paste strings together (a wrapper of `paste0()`).

### Major Changes

-   Improved `set.wd()`: Now it converts the extracted path string from "UTF-8" to "GBK" on Windows system to support paths including Chinese characters (otherwise, the path would become messy code and cause an error). Note that this problem does not exist on Mac OS. In addition, warning messages will be printed into the console if the user's RStudio version is lower than required (RStudio version \>= 1.4.843 is required for a complete implementation of this function).
-   Improved `Alpha()`: Now it adds a parameter `varrange` (to keep the same as `SUM()`, `MEAN()`, ...) and reports both Cronbach's α and McDonald's ω, with more detailed documentation.

> Three ways to specify the variable list (implemented in the functions such as `SUM()`, `MEAN()`, `Alpha()`):
>
> 1\. **`var + items`**: use the common and unique parts of variable names. (e.g., `var="RSES", items=1:10, rev=c(3, 5, 8, 9, 10)`)
>
> 2\. **`vars`**: directly define the variable list. (e.g., `vars=c("E1", "E2", "E3", "E4", "E5"), rev=c("E1", "E2")`)
>
> 3\. **`varrange`**: use the start and end positions of the variable list. (e.g., `varrange="E1:E5", rev=c("E1", "E2")`)

### Minor Changes

-   Added details about the package's contents in the Description field.

### Bug Fixes

-   Fixed a potential bug in `Corr()` (relevant to the changes in `psych::corr.test()` in a forthcoming release of the `psych` package).

# bruceR 0.6.0 (Mar 2021)

### Breaking News

-   Formally published on [CRAN](https://CRAN.R-project.org/package=bruceR)!!!
-   Passed R CMD check and Travis CI test: `0 errors √ | 0 warnings √ | 0 notes √`

### New Features

-   New function `model_summary()`: Tidy report of (single/multiple) regression models (into console or to a Word/HTML file; supporting most types of models; based on the `texreg` package).
-   New function `med_summary()`: Tidy report of (simple/moderated) mediation analyses (based on the `mediation` package).
-   New function `ccf_plot`: Cross-correlation analysis (plotting with `ggplot2`).
-   New function `granger_test`: Granger test of predictive causality (based on the `lmtest::grangertest()` function).

### Major Changes

-   Improved many major functions, especially `set.wd()`, `Describe()`, `Corr()`, `MANOVA()`, and `EMMEANS()`.
-   Tidy welcome messages when you `library(bruceR)`.
-   More packages for default loading (see [details](https://github.com/psychbruce/bruceR#package-dependency)).
-   Less packages for default installation (you can install all suggested packages by using the `pkg_install_suggested()` function).

### Minor Changes

-   Changed package title and description of `bruceR`: **BR**oadly **U**seful **C**onvenient and **E**fficient **R** functions that **BR**ing **U**sers **C**oncise and **E**legant **R** data analyses.
-   Reorganized [raw code files](https://github.com/psychbruce/bruceR/tree/master/R).

### Bug Fixes

-   Fixed all bugs (errors, warnings, and notes) when conducting R CMD check.
-   Fixed all problems in the manual inspection by CRAN team members.

### Notes

-   Deprecated some useless/defective functions (see [details](https://github.com/psychbruce/bruceR/blob/master/R/deprecated.R)).

# bruceR 0.5.0 (Aug 2020)

-   Automatically checking updates when `library(bruceR)`.
-   Requiring R version 4.0+.
-   Improved many functions.
-   Fixed many bugs.

# bruceR 0.4.0 (Dec 2019)

-   Added citation information.
-   General bug-fixes and improvements.

# bruceR 0.3.0 (Oct 2019)

-   New functions `MANOVA` and `EMMEANS`: ANOVA, simple-effect analyses, and multiple comparisons (based on the `afex` and `emmeans` packages).
-   General bug-fixes and improvements.

# bruceR 0.2.0 (Aug 2019)

-   Added all help pages.
-   General bug-fixes and improvements.

# bruceR 0.1.0 (Jun 2019)

-   Initial release on [GitHub](https://github.com/psychbruce/bruceR).
