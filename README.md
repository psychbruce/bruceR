# bruceR <img src="man/figures/logo.png" align="right" width="160"/>

**BR**oadly **U**seful **C**onvenient and **E**fficient **R** functions that **BR**ing **U**sers **C**oncise and **E**legant **R** data analyses.

This package includes easy-to-use functions for:

1.  Basic R programming (e.g., set working directory to the path of currently opened file; import/export data from/to files in any format; print tables to Microsoft Word);
2.  Multivariate computation (e.g., compute scale sums/means/... with reverse scoring);
3.  Reliability analyses and factor analyses (PCA, EFA, CFA);
4.  Descriptive statistics and correlation analyses;
5.  *t*-test, multi-factor analysis of variance (ANOVA), simple-effect analysis, and post-hoc multiple comparison;
6.  Tidy report of statistical models (to R Console and Microsoft Word);
7.  Mediation and moderation analyses (PROCESS);
8.  Additional toolbox for statistics and graphics.

<!-- badges: start -->

[![CRAN-Version](https://www.r-pkg.org/badges/version/bruceR?color=red)](https://CRAN.R-project.org/package=bruceR) [![GitHub-Version](https://img.shields.io/github/r-package/v/psychbruce/bruceR?label=GitHub&color=orange)](https://github.com/psychbruce/bruceR) [![R-CMD-check](https://github.com/psychbruce/bruceR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/psychbruce/bruceR/actions/workflows/R-CMD-check.yaml) [![CRAN-Downloads](https://cranlogs.r-pkg.org/badges/grand-total/bruceR)](https://CRAN.R-project.org/package=bruceR) [![GitHub-Stars](https://img.shields.io/github/stars/psychbruce/bruceR?style=social)](https://github.com/psychbruce/bruceR/stargazers)

<!-- badges: end -->

<img src="https://s1.ax1x.com/2020/07/28/aAjUJg.jpg" width="120px" height="42px"/>

## Author

Han-Wu-Shuang (Bruce) Bao 包寒吴霜

📬 [baohws\@foxmail.com](mailto:baohws@foxmail.com)

📋 [psychbruce.github.io](https://psychbruce.github.io)

## Citation

-   Bao, H.-W.-S. (2021). *bruceR: Broadly useful convenient and efficient R functions*. <https://CRAN.R-project.org/package=bruceR>
    -   *Note*: This is the original citation format. Please refer to the information when you `library(bruceR)` for the APA-7 format of your installed version.

## User Guide

[NEWS (Changelog)](https://psychbruce.github.io/bruceR/news/index.html)

[Chinese Documentation for bruceR: I. Overview](https://zhuanlan.zhihu.com/p/281150493)

[Chinese Documentation for bruceR: II. FAQ](https://zhuanlan.zhihu.com/p/432931518)

### Installation

**Please always set `dep=TRUE` to install ALL package dependencies for FULL features!**

``` r
## Method 1: Install from CRAN
install.packages("bruceR", dep=TRUE)  # dependencies=TRUE

## Method 2: Install from GitHub
install.packages("devtools")
devtools::install_github("psychbruce/bruceR", dep=TRUE, force=TRUE)
```

**Tips:**

-   Good practices:
    -   **Restart RStudio** before installation.
    -   **Update R** to the [latest version](https://www.r-project.org/) (v4.0+).
    -   **Install** [Rtools.exe](https://CRAN.R-project.org/bin/windows/Rtools/) (it is *not* an R package) on Windows system.
-   If you see *"Do you want to restart R prior to install?"*, **choose "Yes"** for the first time and then **choose "No"**.
-   If you fail to install, please carefully read the warning messages and find out the R package(s) causing the failure, **manually uninstall and reinstall these R package(s)**, and then retry the main installation.

### Package Dependency

`bruceR` depends on many important R packages.

Loading `bruceR` with `library(bruceR)` will also load these R packages for you:

-   **[Data]:**

    -   [`data.table`](https://cran.r-project.org/package=data.table): Advanced `data.frame` with higher efficiency.
    -   [`dplyr`](https://cran.r-project.org/package=dplyr): Data manipulation and processing.
    -   [`tidyr`](https://cran.r-project.org/package=tidyr): Data cleaning and reshaping.
    -   [`stringr`](https://cran.r-project.org/package=stringr): Toolbox for string operation (with regular expressions).
    -   [`ggplot2`](https://cran.r-project.org/package=ggplot2): Data visualization.

-   **[Stat]:**

    -   [`emmeans`](https://cran.r-project.org/package=emmeans): Estimates of marginal means and multiple contrasts.
    -   [`lmerTest`](https://cran.r-project.org/package=lmerTest): Linear mixed effects modeling (multilevel modeling).
    -   [`effectsize`](https://cran.r-project.org/package=effectsize): Effect sizes and standardized parameters.
    -   [`performance`](https://cran.r-project.org/package=performance): Performance of regression models.
    -   [`interactions`](https://cran.r-project.org/package=interactions): Interaction and simple effect analyses.

### Main Functions in `bruceR`

1.  **Basic R Programming**

    -   `cc()` (suggested)
    -   `set.wd()` (alias: `set_wd()`) (suggested)
    -   `import()`, `export()` (suggested)
    -   `pkg_depend()`, `pkg_install_suggested()`
    -   `formatF()`, `formatN()`
    -   `print_table()`
    -   `Print()`, `Glue()`, `Run()`
    -   `%^%`
    -   `%notin%`
    -   `%allin%`, `%anyin%`, `%nonein%`, `%partin%`

2.  **Multivariate Computation**

    -   `add()`, `added()` (suggested)
    -   `.sum()`, `.mean()` (suggested)
    -   `SUM()`, `MEAN()`, `STD()`, `MODE()`, `COUNT()`, `CONSEC()`
    -   `RECODE()`, `RESCALE()`
    -   `LOOKUP()`

3.  **Reliability and Factor Analyses**

    -   `Alpha()`
    -   `EFA()` / `PCA()`
    -   `CFA()`

4.  **Descriptive Statistics and Correlation Analyses**

    -   `Describe()`
    -   `Freq()`
    -   `Corr()`
    -   `cor_diff()`
    -   `cor_multilevel()`

5.  **T-Test, Multi-Factor ANOVA, Simple-Effect Analysis, and Post-Hoc Multiple Comparison**

    -   `TTEST()`
    -   `MANOVA()`
    -   `EMMEANS()`

6.  **Tidy Report of Regression Models**

    -   `model_summary()` (suggested)
    -   `lavaan_summary()`
    -   `GLM_summary()`
    -   `HLM_summary()`
    -   `HLM_ICC_rWG()`
    -   `regress()`

7.  **Mediation and Moderation Analyses**

    -   `PROCESS()` (suggested)
    -   `med_summary()`

8.  **Additional Toolbox for Statistics and Graphics**

    -   `grand_mean_center()`
    -   `group_mean_center()`
    -   `ccf_plot()`
    -   `granger_test()`
    -   `granger_causality()`
    -   `theme_bruce()`
    -   `show_colors()`

### Function Output

For some functions, the results can be saved to Microsoft Word using the `file` argument.

| bruceR Function       | Output: R Console |  Output: MS Word  |
|:----------------------|:-----------------:|:-----------------:|
| `print_table()`       |         √         |  √ (basic usage)  |
| `Describe()`          |         √         |         √         |
| `Freq()`              |         √         |         √         |
| `Corr()`              |         √         | **√ (suggested)** |
| `Alpha()`             |         √         |   (unnecessary)   |
| `EFA()` / `PCA()`     |         √         |         √         |
| `CFA()`               |         √         |         √         |
| `TTEST()`             |         √         |         √         |
| `MANOVA()`            |         √         |         √         |
| `EMMEANS()`           |         √         |                   |
| `PROCESS()`           |         √         |    √ (partial)    |
| `model_summary()`     |         √         | **√ (suggested)** |
| `med_summary()`       |         √         |         √         |
| `lavaan_summary()`    |         √         |         √         |
| `GLM_summary()`       |         √         |                   |
| `HLM_summary()`       |         √         |                   |
| `HLM_ICC_rWG()`       |         √         |   (unnecessary)   |
| `granger_test()`      |         √         |         √         |
| `granger_causality()` |         √         |         √         |

Examples:

``` r
## Correlation analysis (and descriptive statistics)
Corr(airquality, file="cor.doc")

## Regression analysis
lm1 = lm(Temp ~ Month + Day, data=airquality)
lm2 = lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
model_summary(list(lm1, lm2), file="reg.doc")
model_summary(list(lm1, lm2), std=TRUE, file="reg_std.doc")
```

### Learn More From Help Pages

``` r
library(bruceR)

## Overview
help("bruceR")
help(bruceR)
?bruceR

## See help pages of functions
## (use `?function` or `help(function)`)
?cc
?add
?.mean
?set.wd
?import
?export
?Describe
?Freq
?Corr
?Alpha
?MEAN
?RECODE
?TTEST
?MANOVA
?EMMEANS
?PROCESS
?model_summary
?lavaan_summary
?GLM_summary
?HLM_summary
...
```
