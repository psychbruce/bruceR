# bruceR

**BR**oadly **U**seful **C**onvenient and **E**fficient **R** functions
that **BR**ing **U**sers **C**oncise and **E**legant **R** data
analyses.

This package includes easy-to-use functions for:

1.  Basic R programming (e.g., set working directory to the path of
    currently opened file; import/export data from/to files in any
    format; print tables to Microsoft Word);
2.  Multivariate computation (e.g., compute scale sums/means/… with
    reverse scoring);
3.  Reliability analyses and factor analyses (PCA, EFA, CFA);
4.  Descriptive statistics and correlation analyses;
5.  *t*-test, multi-factor analysis of variance (ANOVA), simple-effect
    analysis, and post-hoc multiple comparison;
6.  Tidy report of statistical models (to R Console and Microsoft Word);
7.  Mediation and moderation analyses (PROCESS);
8.  Additional toolbox for statistics and graphics.

![](https://psychbruce.github.io/img/CC-BY-NC-SA.jpg)

## Author

Bruce H. W. S. Bao 包寒吴霜

📬 <baohws@foxmail.com>

📋 [psychbruce.github.io](https://psychbruce.github.io)

## Citation

- Bao, H. W. S. (2021). *bruceR: Broadly useful convenient and efficient
  R functions*. <https://doi.org/10.32614/CRAN.package.bruceR>

## User Guide

[NEWS (Changelog)](https://psychbruce.github.io/bruceR/news/index.html)

[Chinese Documentation for bruceR: I.
Overview](https://zhuanlan.zhihu.com/p/281150493)

[Chinese Documentation for bruceR: II.
FAQ](https://zhuanlan.zhihu.com/p/432931518)

### Installation

**Please always set `dep=TRUE` to install ALL package dependencies for
FULL features!**

``` r
## Method 1: Install from CRAN
install.packages("bruceR", dep=TRUE)  # dependencies=TRUE

## Method 2: Install from GitHub
install.packages("devtools")
devtools::install_github("psychbruce/bruceR", dep=TRUE, force=TRUE)
```

**Tips:**

- Good practices:
  - **Restart RStudio** before installation.
  - **Update R** to the [latest version](https://www.r-project.org/)
    (v4.0+).
  - **Install**
    [Rtools.exe](https://CRAN.R-project.org/bin/windows/Rtools/) (it is
    *not* an R package) on Windows system.
- If you see *“Do you want to restart R prior to install?”*, **choose
  “Yes”** for the first time and then **choose “No”**.
- If you fail to install, please carefully read the warning messages and
  find out the R package(s) causing the failure, **manually uninstall
  and reinstall these R package(s)**, and then retry the main
  installation.

### Package Dependency

`bruceR` depends on many important R packages.

Loading `bruceR` with
[`library(bruceR)`](https://psychbruce.github.io/bruceR/) will also load
these R packages for you:

- **\[Data\]:**

  - [`data.table`](https://cran.r-project.org/package=data.table):
    Advanced `data.frame` with higher efficiency.
  - [`dplyr`](https://cran.r-project.org/package=dplyr): Data
    manipulation and processing.
  - [`tidyr`](https://cran.r-project.org/package=tidyr): Data cleaning
    and reshaping.
  - [`stringr`](https://cran.r-project.org/package=stringr): Toolbox for
    string operation (with regular expressions).
  - [`ggplot2`](https://cran.r-project.org/package=ggplot2): Data
    visualization.

- **\[Stat\]:**

  - [`emmeans`](https://cran.r-project.org/package=emmeans): Estimates
    of marginal means and multiple contrasts.
  - [`lmerTest`](https://cran.r-project.org/package=lmerTest): Linear
    mixed effects modeling (multilevel modeling).
  - [`effectsize`](https://cran.r-project.org/package=effectsize):
    Effect sizes and standardized parameters.
  - [`performance`](https://cran.r-project.org/package=performance):
    Performance of regression models.
  - [`interactions`](https://cran.r-project.org/package=interactions):
    Interaction and simple effect analyses.

### Main Functions in `bruceR`

1.  **Basic R Programming**

    - [`cc()`](https://psychbruce.github.io/bruceR/reference/cc.md)
      (suggested)
    - [`set.wd()`](https://psychbruce.github.io/bruceR/reference/set.wd.md)
      (alias:
      [`set_wd()`](https://psychbruce.github.io/bruceR/reference/set.wd.md))
      (suggested)
    - [`import()`](https://psychbruce.github.io/bruceR/reference/import.md),
      [`export()`](https://psychbruce.github.io/bruceR/reference/export.md)
      (suggested)
    - [`pkg_depend()`](https://psychbruce.github.io/bruceR/reference/pkg_depend.md)
    - [`formatF()`](https://psychbruce.github.io/bruceR/reference/formatF.md),
      [`formatN()`](https://psychbruce.github.io/bruceR/reference/formatN.md)
    - [`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md)
    - [`Print()`](https://psychbruce.github.io/bruceR/reference/Print.md),
      [`Glue()`](https://psychbruce.github.io/bruceR/reference/Print.md),
      [`Run()`](https://psychbruce.github.io/bruceR/reference/Run.md)
    - `%^%`
    - `%notin%`
    - `%allin%`, `%anyin%`, `%nonein%`, `%partin%`

2.  **Multivariate Computation**

    - [`add()`](https://psychbruce.github.io/bruceR/reference/add.md),
      [`added()`](https://psychbruce.github.io/bruceR/reference/add.md)
      (suggested)
    - [`.sum()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
      [`.mean()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)
      (suggested)
    - [`SUM()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
      [`MEAN()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
      [`STD()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
      [`MODE()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
      [`COUNT()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
      [`CONSEC()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)
    - [`RECODE()`](https://psychbruce.github.io/bruceR/reference/RECODE.md),
      [`RESCALE()`](https://psychbruce.github.io/bruceR/reference/RESCALE.md)
    - [`LOOKUP()`](https://psychbruce.github.io/bruceR/reference/LOOKUP.md)

3.  **Reliability and Factor Analyses**

    - [`Alpha()`](https://psychbruce.github.io/bruceR/reference/Alpha.md)
    - [`EFA()`](https://psychbruce.github.io/bruceR/reference/EFA.md) /
      [`PCA()`](https://psychbruce.github.io/bruceR/reference/EFA.md)
    - [`CFA()`](https://psychbruce.github.io/bruceR/reference/CFA.md)

4.  **Descriptive Statistics and Correlation Analyses**

    - [`Describe()`](https://psychbruce.github.io/bruceR/reference/Describe.md)
    - [`Freq()`](https://psychbruce.github.io/bruceR/reference/Freq.md)
    - [`Corr()`](https://psychbruce.github.io/bruceR/reference/Corr.md)
    - [`cor_diff()`](https://psychbruce.github.io/bruceR/reference/cor_diff.md)
    - [`cor_multilevel()`](https://psychbruce.github.io/bruceR/reference/cor_multilevel.md)

5.  **T-Test, Multi-Factor ANOVA, Simple-Effect Analysis, and Post-Hoc
    Multiple Comparison**

    - [`TTEST()`](https://psychbruce.github.io/bruceR/reference/TTEST.md)
    - [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)
    - [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md)

6.  **Tidy Report of Regression Models**

    - [`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md)
      (suggested)
    - [`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md)
    - [`GLM_summary()`](https://psychbruce.github.io/bruceR/reference/GLM_summary.md)
    - [`HLM_summary()`](https://psychbruce.github.io/bruceR/reference/HLM_summary.md)
    - [`HLM_ICC_rWG()`](https://psychbruce.github.io/bruceR/reference/HLM_ICC_rWG.md)
    - [`regress()`](https://psychbruce.github.io/bruceR/reference/regress.md)

7.  **Mediation and Moderation Analyses**

    - [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)
      (suggested)
    - [`med_summary()`](https://psychbruce.github.io/bruceR/reference/med_summary.md)
    - [`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md)

8.  **Additional Toolbox for Statistics and Graphics**

    - [`grand_mean_center()`](https://psychbruce.github.io/bruceR/reference/grand_mean_center.md)
    - [`group_mean_center()`](https://psychbruce.github.io/bruceR/reference/group_mean_center.md)
    - [`ccf_plot()`](https://psychbruce.github.io/bruceR/reference/ccf_plot.md)
    - [`granger_test()`](https://psychbruce.github.io/bruceR/reference/granger_test.md)
    - [`granger_causality()`](https://psychbruce.github.io/bruceR/reference/granger_causality.md)
    - [`theme_bruce()`](https://psychbruce.github.io/bruceR/reference/theme_bruce.md)
    - [`show_colors()`](https://psychbruce.github.io/bruceR/reference/show_colors.md)

### Function Output

For some functions, the results can be saved to Microsoft Word using the
`file` argument.

| bruceR Function                                                                                                                   | Output: R Console |  Output: MS Word  |
|:----------------------------------------------------------------------------------------------------------------------------------|:-----------------:|:-----------------:|
| [`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md)                                                   |         √         |  √ (basic usage)  |
| [`Describe()`](https://psychbruce.github.io/bruceR/reference/Describe.md)                                                         |         √         |         √         |
| [`Freq()`](https://psychbruce.github.io/bruceR/reference/Freq.md)                                                                 |         √         |         √         |
| [`Corr()`](https://psychbruce.github.io/bruceR/reference/Corr.md)                                                                 |         √         | **√ (suggested)** |
| [`Alpha()`](https://psychbruce.github.io/bruceR/reference/Alpha.md)                                                               |         √         |   (unnecessary)   |
| [`EFA()`](https://psychbruce.github.io/bruceR/reference/EFA.md) / [`PCA()`](https://psychbruce.github.io/bruceR/reference/EFA.md) |         √         |         √         |
| [`CFA()`](https://psychbruce.github.io/bruceR/reference/CFA.md)                                                                   |         √         |         √         |
| [`TTEST()`](https://psychbruce.github.io/bruceR/reference/TTEST.md)                                                               |         √         |         √         |
| [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)                                                             |         √         |         √         |
| [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md)                                                           |         √         |         √         |
| [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)                                                           |         √         |    √ (partial)    |
| [`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md)                                               |         √         | **√ (suggested)** |
| [`med_summary()`](https://psychbruce.github.io/bruceR/reference/med_summary.md)                                                   |         √         |         √         |
| [`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md)                                             |         √         |         √         |
| [`GLM_summary()`](https://psychbruce.github.io/bruceR/reference/GLM_summary.md)                                                   |         √         |                   |
| [`HLM_summary()`](https://psychbruce.github.io/bruceR/reference/HLM_summary.md)                                                   |         √         |                   |
| [`HLM_ICC_rWG()`](https://psychbruce.github.io/bruceR/reference/HLM_ICC_rWG.md)                                                   |         √         |   (unnecessary)   |
| [`granger_test()`](https://psychbruce.github.io/bruceR/reference/granger_test.md)                                                 |         √         |         √         |
| [`granger_causality()`](https://psychbruce.github.io/bruceR/reference/granger_causality.md)                                       |         √         |         √         |

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
