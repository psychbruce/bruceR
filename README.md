# bruceR

**BR**oadly **U**seful **C**onvenient and **E**fficient **R** functions that **BR**ing **U**sers **C**oncise and **E**legant **R** data analyses.

![](https://img.shields.io/badge/R-Package-blue) ![](https://img.shields.io/badge/Version-0.6.0-red) ![](https://img.shields.io/github/license/psychbruce/bruceR?label=License&color=success) ![](https://img.shields.io/github/commit-activity/y/psychbruce/bruceR?label=Commits&color=orange) [![](https://img.shields.io/github/stars/psychbruce/bruceR?style=social)](https://github.com/psychbruce/bruceR/stargazers)

<img src="https://s1.ax1x.com/2020/07/28/aAjUJg.jpg" width="120px" height="42px"/>

-   复制、修改、使用、分享本代码库，必须遵守「创作共用许可协议 [CC BY-NC-SA](https://en.wikipedia.org/wiki/Creative_Commons_license)」（原作者署名-非商业用途使用-相同方式共享）

## Author

[包寒吴霜 \| Bao H.-W.-S.](https://psychbruce.github.io)

E-mail: [baohws\@foxmail.com](mailto:baohws@foxmail.com)

Website: [psychbruce.github.io](https://psychbruce.github.io)

[ResearchGate](https://www.researchgate.net/profile/Han-Wu-Shuang-Bao) \| [GitHub](https://github.com/psychbruce) \| [知乎](https://www.zhihu.com/people/psychbruce)

## User Guide

### Installation

``` r
## Method 1: Install from CRAN (coming soon...)
install.packages("bruceR")

## Method 2: Install from GitHub (using "pacman" package)
install.packages("pacman")
pacman::p_install_gh("psychbruce/bruceR")

## Method 3: Install from GitHub (using "devtools" package)
install.packages("devtools")
devtools::install_github("psychbruce/bruceR")
```

**Tips:**

1.  Please **restart (close and reopen) RStudio** before installation!
2.  If you see a dialog asking *"Do you want to install from sources the package which needs compilation"*, it would be better to **choose "No"** (to save your time).
3.  If you fail to install, please read carefully the warning messages and find out the key R package(s) causing the failure, **manually uninstall and reinstall these R package(s)**, and then retry the main installation.
4.  It would be better to update R to its [latest version](https://www.r-project.org/) (v4.0+).
5.  It would be better to download and install [Rtools.exe](https://CRAN.R-project.org/bin/windows/Rtools/) on Windows system.

### Citation

-   Bao, H.-W.-S. (2021). bruceR: Broadly useful convenient and efficient R functions. R package. <https://github.com/psychbruce/bruceR>

### Help Page

``` r
## Overview
help("bruceR")
help(bruceR)
?bruceR

## Function (take `Describe()` for example)
help("Describe")
help(Describe)
?Describe
```

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
    -   [`cowplot`](https://cran.r-project.org/package=cowplot): Advanced toolbox for `ggplot2` (arrange multiple plots and add labels).
    -   [`see`](https://cran.r-project.org/package=see): Advanced toolbox for `ggplot2` (extra geoms, scales, themes, and color palettes).

### Main Functions in `bruceR`

-   [x] Basic Use and Analysis

    -   `set.wd()`
    -   `pkg_depend()`, `pkg_install_suggested()`
    -   `formatF()`, `formatN()`
    -   `Print()`, `Glue()`
    -   `Describe()`, `Freq()`, `Corr()`, `cor_diff()`
    -   `LOOKUP()`, `RANDBETWEEN()`
    -   `%notin%`, `%partin%`, `%allin%`, `%nonein%`, `%anyin%`

-   [x] Multivariate Computation

    -   `RECODE()`, `RESCALE()`
    -   `SUM()`, `MEAN()`, `STD()`, `MODE()`, `COUNT()`, `CONSEC()`

-   [x] Reliability and Validity Analysis

    -   `Alpha()`
    -   `EFA()`
    -   `CFA()`

-   [x] Multi-Factor Analysis of Variance, Simple-Effect Analysis, and Multiple Comparison

    -   `MANOVA()`
    -   `EMMEANS()`

-   [x] Advanced Statistical Toolbox and Tidy Report

    -   `grand_mean_center()`, `group_mean_center()`
    -   `regress()`, `GLM_summary()`, `HLM_summary()`
    -   `model_summary()`
    -   `med_summary()`
    -   `ccf_plot()`
    -   `granger_test()`

-   [x] Theme for `ggplot2`

    -   `theme_bruce()`

### Learn More From Help Pages

``` r
library(bruceR)

## Overview
?bruceR

## See help pages of R functions
## (use `?function` or `help(function)`)
?Describe
?Corr
?SUM
?MEAN
?MANOVA
?EMMEANS
?model_summary
?med_summary
?GLM_summary
?HLM_summary
...
```

## Release Notes

See [NEWS.md](https://github.com/psychbruce/bruceR/blob/master/NEWS.md)

[![Travis build status](https://travis-ci.com/psychbruce/bruceR.svg?branch=master)](https://travis-ci.com/psychbruce/bruceR)
