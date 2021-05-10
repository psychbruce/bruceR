# bruceR <a href="https://github.com/psychbruce/bruceR/tree/master/logo"><img src="https://raw.githubusercontent.com/psychbruce/bruceR/master/logo/bruceR-logo-gifs.gif" align="right" height="360px"/></a>

**BR**oadly **U**seful **C**onvenient and **E**fficient **R** functions that **BR**ing **U**sers **C**oncise and **E**legant **R** data analyses.

This package includes easy-to-use functions for **(1)** basic R programming (e.g., set working directory to where the current file is, print strings with rich formats and colors); **(2)** multivariate computation (e.g., compute scale sums/means/... with reverse scoring); **(3)** reliability and factor analyses; **(4)** descriptive statistics and correlation analyses; **(5)** multi-factor analysis of variance (ANOVA), simple-effect analysis, and post-hoc multiple comparison; **(6)** tidy report of regression models (to Console and to Word); **(7)** mediation and moderation analyses; and **(8)** additional toolbox for statistics and graphics.

[![CRAN-Version](https://www.r-pkg.org/badges/version/bruceR?color=red)](https://CRAN.R-project.org/package=bruceR) [![CRAN-Downloads](https://cranlogs.r-pkg.org/badges/grand-total/bruceR)](https://cranlogs.r-pkg.org/downloads/daily/last-month/bruceR) [![GitHub-Version](https://img.shields.io/github/r-package/v/psychbruce/bruceR?label=GitHub&color=orange)](https://github.com/psychbruce/bruceR) [![Travis-Build-Status](https://travis-ci.com/psychbruce/bruceR.svg?branch=master)](https://travis-ci.com/psychbruce/bruceR) [![Logo-Designer](https://img.shields.io/badge/Logo%20Designer-Meijia%20Li-blueviolet?logo=github-sponsors)](https://github.com/Stellapros) [![GitHub-Commits](https://img.shields.io/github/commit-activity/y/psychbruce/bruceR?logo=github&label=commits&style=social)](https://github.com/psychbruce/bruceR/commits) [![GitHub-Stars](https://img.shields.io/github/stars/psychbruce/bruceR?style=social)](https://github.com/psychbruce/bruceR/stargazers)

<img src="https://s1.ax1x.com/2020/07/28/aAjUJg.jpg" width="120px" height="42px"/>

## Author

[包寒吴霜 \| Bao H.-W.-S.](https://psychbruce.github.io)

E-mail: [baohws\@foxmail.com](mailto:baohws@foxmail.com)

Website: [psychbruce.github.io](https://psychbruce.github.io)

[ResearchGate](https://www.researchgate.net/profile/Han-Wu-Shuang-Bao) \| [GitHub](https://github.com/psychbruce) \| [知乎](https://www.zhihu.com/people/psychbruce)

## Citation

-   Bao, H.-W.-S. (2021). bruceR: Broadly useful convenient and efficient R functions. R package version 0.x.x. <https://CRAN.R-project.org/package=bruceR> or <https://github.com/psychbruce/bruceR>

## User Guide

[Release Notes](https://github.com/psychbruce/bruceR/blob/master/NEWS.md)

### Installation

``` r
## Method 1: Install from CRAN
install.packages("bruceR")

## Method 2: Install from GitHub
install.packages("devtools")
devtools::install_github("psychbruce/bruceR", force=TRUE, upgrade=FALSE)
```

**Tips:**

1.  Please **restart (close and reopen) RStudio** before installation.
2.  If you see a dialog asking *"Do you want to install from sources the package which needs compilation?"*, it would be better to **choose "No"** (to save your time).
3.  If you fail to install, please read carefully the warning messages and find out the key R package(s) causing the failure, **manually uninstall and reinstall these R package(s)**, and then retry the main installation.
4.  It would be better to update R to its [latest version](https://www.r-project.org/) (v4.0+).
5.  It would be better to download and install [Rtools.exe](https://CRAN.R-project.org/bin/windows/Rtools/) on Windows system.

### Package Dependency

`bruceR` depends on many important R packages.

Loading `bruceR` by `library(bruceR)` will also load these R packages for you:

-   **[Data]:**

    -   [`rio`](https://cran.r-project.org/package=rio): Data import and export (for all file formats).
    -   [`dplyr`](https://cran.r-project.org/package=dplyr): Data manipulation and processing.
    -   [`tidyr`](https://cran.r-project.org/package=tidyr): Data cleaning and reshaping.
    -   [`stringr`](https://cran.r-project.org/package=stringr): Toolbox for string operation (with regular expressions).
    -   [`forcats`](https://cran.r-project.org/package=forcats): Toolbox for factor manipulation (for categorical variables).
    -   [`data.table`](https://cran.r-project.org/package=data.table): Advanced `data.frame` with higher efficiency.

-   **[Stat]:**

    -   [`psych`](https://cran.r-project.org/package=psych): Toolbox for psychological and psychometric research.
    -   [`emmeans`](https://cran.r-project.org/package=emmeans): Toolbox for estimated marginal means and contrasts.
    -   [`effectsize`](https://cran.r-project.org/package=effectsize): Indices of effect size and standardized parameters.
    -   [`performance`](https://cran.r-project.org/package=performance): Assessment of regression models performance.

-   **[Plot]:**

    -   [`ggplot2`](https://cran.r-project.org/package=ggplot2): Data visualization.
    -   [`ggtext`](https://cran.r-project.org/package=ggtext): Markdown/HTML rich text format for `ggplot2` (geoms and themes).
    -   [`cowplot`](https://cran.r-project.org/package=cowplot): Advanced toolbox for `ggplot2` (arrange multiple plots and add labels).
    -   [`see`](https://cran.r-project.org/package=see): Advanced toolbox for `ggplot2` (geoms, scales, themes, and color palettes).

### Main Functions in `bruceR`

1.  **Basic R Programming**

    -   `set.wd()`
    -   `pkg_depend()`, `pkg_install_suggested()`
    -   `formatF()`, `formatN()`
    -   `Print()`, `Glue()`, `Run()`
    -   `%^%`
    -   `%notin%`
    -   `%allin%`, `%anyin%`, `%nonein%`, `%partin%`

2.  **Multivariate Computation**

    -   `SUM()`, `MEAN()`, `STD()`, `MODE()`, `COUNT()`, `CONSEC()`
    -   `RECODE()`, `RESCALE()`, `RANDBETWEEN()`
    -   `LOOKUP()`

3.  **Reliability and Factor analyses**

    -   `Alpha()`
    -   `EFA()`
    -   `CFA()`

4.  **Descriptive Statistics and Correlation Analyses**

    -   `Describe()`
    -   `Freq()`
    -   `Corr()`
    -   `cor_diff()`

5.  **Multi-Factor ANOVA, Simple-Effect Analysis, and Post-Hoc Multiple Comparison**

    -   `MANOVA()`
    -   `EMMEANS()`

6.  **Tidy Report of Regression Models**

    -   `model_summary()`
    -   `GLM_summary()`
    -   `HLM_summary()`
    -   `HLM_ICC_rWG()`
    -   `regress()`

7.  **Mediation and Moderation Analyses**

    -   `med_summary()`
    -   `PROCESS()` (*coming soon...*)

8.  **Additional Toolbox for Statistics and Graphics**

    -   `grand_mean_center()`
    -   `group_mean_center()`
    -   `ccf_plot()`
    -   `granger_test()`
    -   `granger_causality()`
    -   `theme_bruce()`
    -   `show_colors()`

### Function Output

Some functions in `bruceR` allow table output to Microsoft Word.

| bruceR Function       | Output: R Console |   Output: MS Word   |
|:----------------------|:-----------------:|:-------------------:|
| `print_table()`       |         √         |          √          |
| `Describe()`          |         √         |          √          |
| `Freq()`              |         √         |          √          |
| `Corr()`              |         √         | **√ (recommended)** |
| `Alpha()`             |         √         |                     |
| `EFA()`               |         √         |                     |
| `CFA()`               |         √         |                     |
| `MANOVA()`            |         √         |                     |
| `EMMEANS()`           |         √         |                     |
| `model_summary()`     |         √         | **√ (recommended)** |
| `med_summary()`       |         √         |                     |
| `GLM_summary()`       |         √         |                     |
| `HLM_summary()`       |         √         |                     |
| `HLM_ICC_rWG()`       |         √         |                     |
| `granger_test()`      |         √         |                     |
| `granger_causality()` |         √         |                     |

Examples:

``` r
## Correlation analysis (and descriptive statistics)
Corr(airquality, file="cor.doc")

## Regression analysis
lm1=lm(Temp ~ Month + Day, data=airquality)
lm2=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
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
?set.wd
?Describe
?Freq
?Corr
?Alpha
?MEAN
?MANOVA
?EMMEANS
?model_summary
?med_summary
?GLM_summary
?HLM_summary
...
```
