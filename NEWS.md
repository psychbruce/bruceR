**Please check the [latest news (change log)](https://psychbruce.github.io/bruceR/news/index.html) and keep this package updated.**

# bruceR 2023.8

## New Features

-   New function `cor_multilevel()`: Multilevel correlations (within-level and between-level).

## Major Changes

-   Now use "YYYY.M" as package version number.
-   Improved `Corr()`: Now it uses `ggplot2` to produce correlation plot.
-   Deprecated the argument `nsmall` for all functions. Now always use `digits` instead. (Both were acceptable in former versions.)

## Bug Fixes

-   Fixed `EMMEANS()` output when `"d"` is used as a variable name.

# bruceR 0.8.10 (Mar 2023)

## Minor Changes

-   Changed the default packages to be loaded when `library(bruceR)`.

## Bug Fixes

-   Fixed new problems on CRAN: Restricted the number of "Imports" dependencies to 20. Changed welcome messages by using `packageStartupMessage()` so that the messages can be suppressed.

# bruceR 0.8.9 (Aug 2022)

## New Features

-   Added `import()` support for importing URL-source data files and no-extension files.

## Bug Fixes

-   Fixed a trivial bug ("NOTE") about HTML5 for documentation pages.

# bruceR 0.8.8 (Jun 2022)

## New Features

-   New `paired.d.type` argument for `TTEST()`: Allow for specifying 3 types of Cohen's *d* for paired-samples *t*-test (`"dz"`, `"dav"`, and `"drm"`). See Lakens (2013) for details.
    -   `"dz"` (*d* for standardized difference)

        $$\text{Cohen's } d_{z} = \frac{M_{diff}}{SD_{diff}}$$

    -   `"dav"` (*d* for average standard deviation)

        $$\text{Cohen's } d_{av} = \frac{M_{diff}}{\frac{SD_{1} + SD_{2}}{2}}$$

    -   `"drm"` (*d* for repeated measures, corrected for correlation)

        $$\text{Cohen's } d_{rm} = \frac{M_{diff} \times \sqrt{2(1 - r_{1,2})}}{\sqrt{SD_{1}^2 + SD_{2}^2 - 2 \times r_{1,2} \times SD_{1} \times SD_{2}}}$$

## Bug Fixes

-   Moved necessary R packages (dependencies) from "Suggests" to "Imports", such that all dependencies will be automatically installed. Also added a check of dependencies when `library(bruceR)`.

# bruceR 0.8.7 (May 2022)

## New Features

-   New functions `add()` and `added()`: Enhanced functions designed to create, modify, and/or delete variables. The functions **combine** the advantages of `:=` (data.table), `mutate()` (dplyr), and `transmute()` (dplyr). See [help page](https://psychbruce.github.io/bruceR/reference/add.html) for the usage and convenience.
-   New functions `.sum()` and `.mean()`: Tidy version of `SUM()` and `MEAN()` designed only for `add()` and `added()`. See [help page](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.html) for the usage and convenience.

## Minor Changes

-   Improved warning information for `SUM()`, `MEAN()`, `EMMEANS()`.

# bruceR 0.8.6 (Apr 2022)

## New Features

-   Rebuilt the package homepage (<https://psychbruce.github.io/bruceR/>) with `pkgdown`. Configuration is specified in `_pkgdown.yml`.
-   Added an `R CMD check` workflow on GitHub, which checks the code for each push.

## Minor Changes

-   Improved `cc()`. Now it becomes much more convenient!
-   Added a prompt message for the use of long data in `MANOVA()`: "*Data are aggregated to mean (across items/trials) if there are \>=2 observations per subject and cell. You may use Linear Mixed Model to analyze the data, e.g., with subjects and items as level-2 clusters.*"
-   Added the `center` argument of `PROCESS()` (default is `TRUE`) for users who want to turn off the automatic grand-mean centering. However, mean centering is still highly suggested if one aim to obtain "main effect" rather than "fixed effect" (note: a fixed effect is not necessarily a main effect).
-   Added the `estimator` argument of `CFA()` (default is `"ML"`) for users who want to use any other estimator (fixed issue [#17](https://github.com/psychbruce/bruceR/issues/17)).

# bruceR 0.8.5 (Mar 2022)

## New Features

-   New function `cc()`: Split up a string (with separators) into a character vector (whitespace around separator is trimmed). For example, `cc("A 1 , B 2 ; C 3 | D 4 \t E 5")` produces a vector of `c("A 1", "B 2", "C 3", "D 4", "E 5")`. The default separators include `, ; | \n \t`. Users may also specify a separator.

## Minor Changes

-   Added a guideline and examples for creating interaction plots using the returned object of `MANOVA()` and `EMMEANS()`. You can save the returned object and use the `emmeans::emmip()` function to create an interaction plot (based on the fitted model and a formula specification). For usage, please see the help page of `emmeans::emmip()`. It returns an object of class `ggplot`, which can be easily modified and saved using `ggplot2` syntax.

-   Added an explanation of the automatic grand-mean centering in `PROCESS()`.

## Bug Fixes

-   Users who have not installed the `afex` package would see an unusual error when using the `EMMEANS()` function (`Error: $ operator is invalid for atomic vectors`). So now `afex` is again a *strong dependency* of `bruceR`, such that it is automatically installed when installing `bruceR`.
-   Improved debugging information for `EMMEANS()` (when `model` is null).
-   Fixed a bug of interaction tests in `PROCESS()` when setting `mod.type="3-way"` for multilevel models.

# bruceR 0.8.3 (Jan 2022)

## Minor Changes

-   Improved `print_table()`.
-   Changed symbol (for better output of R Markdown): ✔ (\\u2714) → √ (\\u221a).

## Bug Fixes

-   Fixed a small bug of direct effect output in `PROCESS()` for models without `"x-y"` in `mod.path` (e.g., Model 7).
-   Fixed a small bug when using `set.wd()` in an R Markdown file.

# bruceR 0.8.2 (Dec 2021)

## Minor Changes

-   Requiring R version 4.0+ again.
-   Added automatic check for new version of `bruceR` when `library(bruceR)`.
-   Added univariate tests (*F*) and multivariate tests (Pillai's trace and *F*) using `phia::testInteractions()` in the output of `EMMEANS()`. These tests produce identical results to those obtained with the SPSS GLM (/EMMEANS) syntax.
-   Improved the flexibility of `Freq()`: Now both vector and data frame can be used. For example, users may specify either `Freq(data$variable)` or `Freq(data, "variable")`.
-   Improved the output format of `GLM_summary()` and `HLM_summary()`.
-   Deprecated two useless arguments of `HLM_summary()`: `level2.predictors` and `vartypes`.
-   Packages `lmerTest`, `mediation`, `interactions`, and `lavaan` are now strong dependencies such that they would be installed when installing `bruceR`. This also fixes an error when using `PROCESS()` without these packages installed.

## Bug Fixes

-   Fixed potential bugs in `print_table()`.
-   Fixed a bug of VIF results in `GLM_summary()` and `HLM_summary()` when only one factor-type predictor with \>= 3 levels is in a regression model. Other bugs in these two functions have also been fixed.
-   Fixed a bug of interaction tests in `PROCESS()` when setting `mod.path="all"` in testing multilevel moderated mediation effects. Fixed another bug of CI output for direct effects when testing multilevel models.

# bruceR 0.8.0 (Nov 2021)

## New Features

-   New function `TTEST()`: One-sample, independent-samples, and paired-samples *t*-test. Multiple dependent/independent variables can be tested simultaneously. It also tests the assumption of homogeneity of variance and allows users to determine whether variances are equal or not. Cohen's *d* and 95% CI are reported by default (see Details and Examples in its help page for an issue about the *inconsistency* in the results of 95% CI of Cohen's *d* between R packages). Bayes factor BF<sub>10</sub> is also supported. Key results can be saved in APA format to MS Word.
-   New functions `import()` / `export()`: Import/export data from/to a file with the two tidy functions, relieving users of the burden of remembering lots of `read_xxx()` / `write_xxx()` functions. Many file formats are supported (especially .txt, .csv, .tsv, .psv, .xls, .xlsx, .sav, .dta, .rda, .rdata, and clipboard). Note that the two functions are inspired by `rio::import()` / `rio::export()` and have several modifications for more convenient use. Since this version, the package `rio` is no longer a strong dependency of `bruceR` and would not be loaded when loading `bruceR`.

## Major Changes

-   Improved `MANOVA()` and `EMMEANS()`:

    -   Fixed several bugs.
    -   Modified the help pages.
    -   Improved output tables. Now all results are printed using `print_table()`.
    -   Improved algorithm for estimating Cohen's *d*: Now it uses ***Root Mean Square Error*** **(RMSE) as the pooled *SD*** to estimate Cohen's *d*. Although there is disagreement on the estimation of pooled *SD*, `EMMEANS()` adopts this reasonable approach. If one uses `MANOVA()` and `EMMEANS()` to conduct the same *t*-test as using the `TTEST()` function, the results will be identical. Indeed, the estimation methods of Cohen's *d* in *t*-tests are acknowledged. In computing pooled *SD* in ANOVAs, it uses **(1)** the square root of *mean square error* (MSE) for between-subjects designs and **(2)** the square root of *mean variance of all paired differences of the residuals of repeated measures* for within-subjects and mixed designs. In both situations, it extracts the `lm` object from the returned value of `MANOVA()`. Then, it mainly uses the `sigma()` and `residuals()` functions, respectively, to do these estimates. For source code, see [R file on GitHub](https://github.com/psychbruce/bruceR/tree/main/R). Thus, the results of Cohen's *d* for designs with repeated measures are now different from those in `bruceR` old versions (\< 0.8.0), which indeed used an inappropriate method to compute pooled *SD* in such designs.
    -   Added arguments (1) `ss.type` for `MANOVA()` to specify either Type-II or Type-III Sum of Square; (2) `aov.include` for `MANOVA()` and `model.type` for `EMMEANS()`, for details, see the help pages.
    -   Added warning messages for wrong usage of these functions. If observations are not uniquely identified in user-defined long-format data, the function takes averages across those multiple observations for each case (thanks to Xiangying Zou for reporting an infrequent bug related to this issue).

-   Improved `Alpha()`: Now it directly uses `psych::alpha()` and `psych::omega()`, rather than `jmv::reliability()`, to perform reliability analysis. The format of result output has been changed and improved.

-   Improved `EFA()` (almost completely rewritten): Now it directly uses `psych::principal()` and `psych::fa()`, rather than `jmv::efa()`, to perform factor analysis (PCA or EFA). The format of result output has been changed and improved. MS Word output has been supported. A wrapper function `PCA()` has been added: `EFA(..., method="pca")`.

-   Improved `CFA()` and `lavaan_summary()`: Now `CFA()` only uses the `lavaan::cfa()`, rather than `jmv:cfa()`, to build model, and then uses `lavaan_summary()` to present results. For `lavaan_summary()`, many bugs have been fixed, and the format of result table has been changed and improved. Both functions now support saving table to MS Word.

-   Package dependencies: Much fewer strong dependencies, for faster and more robust installation. Removed `rio` and `jmv` from dependencies. No longer load `rio` and `psych` when `library(bruceR)`.

## Minor Changes

-   Added an alias `set_wd()` for `set.wd()`.
-   Improved `print_table()`: Fixed an issue of incorrect length of Chinese character output in `print_table()`. Between-column blanks are now 2 spaces (rather than 1 space) for a clearer presentation of table columns.
-   Modified onloading welcome messages.
-   General bug-fixes and improvements.

# bruceR 0.7.3 (Nov 2021)

## Minor Changes

-   Added Word output for `lavaan_summary()` and `granger_test()`.

# bruceR 0.7.2 (Jun 2021)

## Minor Changes

-   Added the `digits` parameter as the equivalent to the `nsmall` parameter for all relevant functions.
-   Packages `mediation`, `interactions`, `MuMIn`, and `texreg` are now SUGGESTS rather than IMPORTS.

## Bug Fixes

-   Fixed a bug of value ordering for 3-way interaction (moderated moderation) in `PROCESS()`.
-   Fixed a bug for Word output in `Corr()`.

# bruceR 0.7.0 (May 2021)

## New Features

-   New function `PROCESS()`: PROCESS for mediation, moderation, and conditional process (moderated mediation) analyses! This function supports a total of 24 kinds of SPSS PROCESS models (Hayes, 2018) and also supports multilevel mediation/moderation analyses. Overall, it supports the most frequently used types of mediation, moderation, moderated moderation (3-way interaction), and moderated mediation (conditional indirect effect) analyses for (generalized) linear or linear mixed models. Regression model summary and effect estimates (simple slopes and/or indirect effects) are printed in an elegant way.
-   New function `lavaan_summary()`: Tidy report of lavaan model.

## Minor Changes

-   Improved many functions.
-   Deprecated the `RANDBETWEEN()` function.

## Bug Fixes

-   Fixed a bug in the CRAN version 0.6.4 (a problem newly emerging on 2021-05-25).

# bruceR 0.6.4 (May 2021)

## New Features

-   Added Word output (.doc) in `print_table()` and other functions using `print_table()` inside: `Describe()`, `Freq()`, `Corr()`, `MANOVA()`, `med_summary()`, `granger_causality()`.

## Minor Changes

-   Added a disclaimer about Cohen's *d* in the output and documentation of `EMMEANS()`: There is considerable disagreement on how to compute Cohen's *d*. Users should not take the default output as the only right results and are completely responsible for setting the "sd.pooled".

## Bug Fixes

-   Fixed bugs in `model_summary()`: (1) Model names with `NULL`; (2) Multicollinearity check results with `NULL` or other problems; (3) UTF-8 encoding problem in WPS software (no such problem in Microsoft Word).

# bruceR 0.6.3 (Apr 2021)

## New Features

-   New function `granger_causality()`: Granger causality test (multivariate) based on vector autoregression (VAR) model. This function is an advanced and more general version of the function `granger_test()` (bivariate).
-   Added logo (designed by [\@Meijia Li](https://github.com/Stellapros))

# bruceR 0.6.2 (Apr 2021)

## Major Changes

-   Improved `set.wd()`: Now it uses `rstudioapi::getSourceEditorContext()` to extract file path (even effective when running in R console), which only requires RStudio version \>= 0.99.1111 and no longer has encoding problems (see release note in 0.6.1).
-   Improved `theme_bruce()`: Now it uses `ggtext::element_markdown()` to render Markdown/HTML rich text format, which can be used in plot text (e.g., titles).
-   Improved `EMMEANS()`: Now its results are always identical to those in SPSS (by setting `model="multivariate"` in `emmeans::joint_tests()` and `emmeans::emmeans()`, which use the `lm` or `mlm` objects rather than the `aov` object to perform tests). For a few cases with singular error matrix (i.e., some variables are linearly dependent), the results of simple-effect *F* tests will not be reported, but estimated marginal means and pairwise comparisons are not affected and so are still reported. Note that the `EMMEANS` results in old versions of `bruceR` (version \< 0.6.0) were identical to SPSS, but version 0.6.0 deprecated the parameter `repair` and no longer set `model$aov=NULL`, which made the results not identical to SPSS (particularly for ANOVAs with repeated measures). In response to a user's feedback, now 0.6.2 has improved this function and makes its results accurate again.

## Minor Changes

-   Improved function links in R documentation: `\code{\link[package:function]{package::function()}}`.

## Bug Fixes

-   Fixed a bug in `CFA()` (for lavaan-style output).

# bruceR 0.6.1 (Mar 2021)

## New Features

-   New function `HLM_ICC_rWG()`: Tidy report of HLM indices "ICC(1)" (non-independence of data), "ICC(2)" (reliability of group means), and "rWG"/"rWG(J)" (within-group agreement for single-item/multi-item measures).
-   New function `Run()`: Run code parsed from text.
-   New function `show_colors()`: Show multiple colors (or a palette) in a plot.
-   New function `%^%`: Paste strings together (a wrapper of `paste0()`).

## Major Changes

-   Improved `set.wd()`: Now it converts the extracted path string from "UTF-8" to "GBK" on Windows system to support paths including Chinese characters (otherwise, the path would become messy code and cause an error). Note that this problem does not exist on Mac OS. In addition, warning messages will be printed into the console if the user's RStudio version is lower than required (RStudio version \>= 1.4.843 is required for a complete implementation of this function).
-   Improved `Alpha()`: Now it adds a parameter `varrange` (to keep the same as `SUM()`, `MEAN()`, ...) and reports both Cronbach's α and McDonald's ω, with more detailed documentation.

> Three ways to specify the variable list (implemented in the functions such as `SUM()`, `MEAN()`, `Alpha()`):
>
> 1\. **`var + items`**: use the common and unique parts of variable names. (e.g., `var="RSES", items=1:10, rev=c(3, 5, 8, 9, 10)`)
>
> 2\. **`vars`**: directly define the variable list. (e.g., `vars=c("E1", "E2", "E3", "E4", "E5"), rev=c("E1", "E2")`)
>
> 3\. **`varrange`**: use the start and end positions of the variable list. (e.g., `varrange="E1:E5", rev=c("E1", "E2")`)

## Minor Changes

-   Added details about the package's contents in the Description field.

## Bug Fixes

-   Fixed a potential bug in `Corr()` (relevant to the changes in `psych::corr.test()` in a forthcoming release of the `psych` package).

# bruceR 0.6.0 (Mar 2021)

## Breaking News

-   Formally published on [CRAN](https://CRAN.R-project.org/package=bruceR)!!!
-   Passed R CMD check and Travis CI test: `0 errors √ | 0 warnings √ | 0 notes √`

## New Features

-   New function `model_summary()`: Tidy report of (single/multiple) regression models (into console or to a Word/HTML file; supporting most types of models; based on the `texreg` package).
-   New function `med_summary()`: Tidy report of (simple/moderated) mediation analyses (based on the `mediation` package).
-   New function `ccf_plot()`: Cross-correlation analysis (plotting with `ggplot2`).
-   New function `granger_test()`: Granger test of predictive causality (based on the `lmtest::grangertest()` function).

## Major Changes

-   Improved many major functions, especially `set.wd()`, `Describe()`, `Corr()`, `MANOVA()`, and `EMMEANS()`.
-   Tidy welcome messages when you `library(bruceR)`.
-   More packages for default loading (see [details](https://github.com/psychbruce/bruceR#package-dependency)).
-   Less packages for default installation (you can install all suggested packages by using the `pkg_install_suggested()` function).

## Minor Changes

-   Changed package title and description of `bruceR`: **BR**oadly **U**seful **C**onvenient and **E**fficient **R** functions that **BR**ing **U**sers **C**oncise and **E**legant **R** data analyses.
-   Reorganized [raw code files](https://github.com/psychbruce/bruceR/tree/master/R).

## Bug Fixes

-   Fixed all bugs (errors, warnings, and notes) when conducting R CMD check.
-   Fixed all problems in the manual inspection by CRAN team members.

## Notes

-   Deprecated some useless/defective functions (see [details](https://github.com/psychbruce/bruceR/blob/master/R/deprecated.R)).

# bruceR 0.5.0 (Aug 2020)

-   Requiring R version 4.0+.
-   Improved many functions.
-   Fixed many bugs.

# bruceR 0.4.0 (Dec 2019)

-   Added citation information.
-   General bug-fixes and improvements.

# bruceR 0.3.0 (Oct 2019)

-   New functions `MANOVA()` and `EMMEANS()`: ANOVA, simple-effect analyses, and multiple comparisons (based on the `afex` and `emmeans` packages).
-   General bug-fixes and improvements.

# bruceR 0.2.0 (Aug 2019)

-   Added all help pages.
-   General bug-fixes and improvements.

# bruceR 0.1.0 (Jun 2019)

-   Initial release on [GitHub](https://github.com/psychbruce/bruceR).
