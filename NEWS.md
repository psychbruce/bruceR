**If you are viewing this file on CRAN, please check [latest news on GitHub](https://github.com/psychbruce/bruceR/blob/master/NEWS.md) where the formatting is also better.**

# bruceR 0.6.0 (Mar 2021)

### BREAKING NEWS

-   Ready for published on CRAN.
-   Passed R CMD check: `0 errors √ | 0 warnings √ | 0 notes √`

### NEW FEATURES

-   New function `model_summary()`: Tidy report of (single/multiple) regression models (into console or to a Word/HTML file; supporting most types of models; based on the `texreg` package).
-   New function `med_summary()`: Tidy report of (simple/moderated) mediation analyses (based on the `mediation` package).
-   New function `ccf_plot`: Cross-correlation analysis (plotting with `ggplot2`).
-   New function `granger_test`: Granger test of predictive causality (based on the `lmtest::grangertest()` function).

### MAJOR CHANGES

-   Improved many major functions, especially `Describe()`, `Corr()`.
-   Tidy welcome messages when you `library(bruceR)`.
-   More packages for default loading (see [details](https://github.com/psychbruce/bruceR#package-dependency)).
-   Less packages for default installation (you can install all suggested packages by using the `pkg_install_suggested()` function).

### MINOR CHANGES

-   Changed package title and description of `bruceR`: **BR**oadly **U**seful **C**onvenient and **E**fficient **R** functions that **BR**ing **U**sers **C**oncise and **E**legent **R** data analyses.
-   Reorganized [raw code files](https://github.com/psychbruce/bruceR/tree/master/R).

### BUG FIXES

-   Fixed all bugs (errors, warnings, and notes) when conducting R CMD check.

### NOTES

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
