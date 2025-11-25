# Changelog

## bruceR 2025.11

- Fixed
  [`theme_bruce()`](https://psychbruce.github.io/bruceR/reference/theme_bruce.md)
  for `ggplot2` (\>= 3.4.0) warnings about its deprecated features.

## bruceR 2025.8

CRAN release: 2025-08-20

- Rewrote all help pages in the style of Roxygen markdown, and refined
  most of them.
- Provided solutions when
  [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)
  gets unknown errors: Simply updating R and all R packages. This issue
  has been reported by not a few users and can be solved simply by
  updating all packages.
- Highlight in
  [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)
  that it ***DOES NOT*** use any code or macro from the SPSS PROCESS
  macro developed by Hayes, though the output links the model setting to
  the PROCESS Model Number in Hayes’s numbering system. So please ***DO
  NOT*** state that “~~the bruceR package runs the PROCESS Model Code
  developed by Hayes (2018)~~” — it was not the truth.
  - Remember that
    [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)
    uses
    [`mediation::mediate()`](https://rdrr.io/pkg/mediation/man/mediate.html),
    `interactions::sim_slopes()`, and
    [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) as main
    functions inside, completely different from and having no relation
    with the SPSS PROCESS macro.
- Fixed
  [`Corr()`](https://psychbruce.github.io/bruceR/reference/Corr.md) when
  `plot=FALSE` does not return the `ggplot` object (thanks to a student
  in my R course at ECNU for reporting this issue).
- No longer check online updates when
  [`library(bruceR)`](https://psychbruce.github.io/bruceR/) since it is
  getting stable.
- Reexported `%notin%` from `data.table` to avoid function name
  conflict.
- Deprecated `pkg_install_suggested()` from exported functions, but it
  can be accessed via `bruceR:::pkg_install_suggested()`.

## bruceR 2024.6

CRAN release: 2024-06-13

- Had to move `MuMIn` from “Imports” to “Suggests” due to its unresolved
  CRAN issues which need fixing before 2024-06-14.
- Added `file` argument for
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md)
  to allow for saving the contrast table to MS Word.

## bruceR 2023.9

CRAN release: 2023-09-27

### Minor Changes

- Now `var` and `items` support more flexible input, e.g.:
  - `.mean(var="X", items=1:3)` matches `X1`, `X2`, `X3`
  - `.mean(var="X.{any_placeholder}.pre", items=1:3)` matches `X.1.pre`,
    `X.2.pre`, `X.3.pre`
- Use `\donttest{}` in more examples to avoid unnecessary errors.

## bruceR 2023.8

CRAN release: 2023-08-09

### New Features

- New function
  [`cor_multilevel()`](https://psychbruce.github.io/bruceR/reference/cor_multilevel.md):
  Multilevel correlations (within-level and between-level).
- Added arguments `pkg` and `value.labels` for
  [`import()`](https://psychbruce.github.io/bruceR/reference/import.md),
  providing more flexible settings and allowing for converting variables
  with value labels into R factors.

### Major Changes

- Now use “YYYY.M” as package version number.
- Improved
  [`Corr()`](https://psychbruce.github.io/bruceR/reference/Corr.md): Now
  it uses `ggplot2` to produce correlation plot.
- Deprecated the argument `nsmall` for all functions. Now always use
  `digits` instead. (Both were acceptable in former versions.)

### Bug Fixes

- Fixed
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md)
  output when `"d"` is used as a variable name.

## bruceR 0.8.10 (Mar 2023)

CRAN release: 2023-03-03

### Minor Changes

- Changed the default packages to be loaded when
  [`library(bruceR)`](https://psychbruce.github.io/bruceR/).

### Bug Fixes

- Fixed new problems on CRAN: Restricted the number of “Imports”
  dependencies to 20. Changed welcome messages by using
  [`packageStartupMessage()`](https://rdrr.io/r/base/message.html) so
  that the messages can be suppressed.

## bruceR 0.8.9 (Aug 2022)

CRAN release: 2022-08-11

### New Features

- Added
  [`import()`](https://psychbruce.github.io/bruceR/reference/import.md)
  support for importing URL-source data files and no-extension files.

### Bug Fixes

- Fixed a trivial bug (“NOTE”) about HTML5 for documentation pages.

## bruceR 0.8.8 (Jun 2022)

CRAN release: 2022-06-27

### New Features

- New `paired.d.type` argument for
  [`TTEST()`](https://psychbruce.github.io/bruceR/reference/TTEST.md):
  Allow for specifying 3 types of Cohen’s *d* for paired-samples
  *t*-test (`"dz"`, `"dav"`, and `"drm"`). See Lakens (2013) for
  details.
  - `"dz"` (*d* for standardized difference)

    $${\text{Cohen’s}\mspace{6mu}}d_{z} = \frac{M_{diff}}{SD_{diff}}$$

  - `"dav"` (*d* for average standard deviation)

    $${\text{Cohen’s}\mspace{6mu}}d_{av} = \frac{M_{diff}}{\frac{SD_{1} + SD_{2}}{2}}$$

  - `"drm"` (*d* for repeated measures, corrected for correlation)

    $${\text{Cohen’s}\mspace{6mu}}d_{rm} = \frac{M_{diff} \times \sqrt{2\left( 1 - r_{1,2} \right)}}{\sqrt{SD_{1}^{2} + SD_{2}^{2} - 2 \times r_{1,2} \times SD_{1} \times SD_{2}}}$$

### Bug Fixes

- Moved necessary R packages (dependencies) from “Suggests” to
  “Imports”, such that all dependencies will be automatically installed.
  Also added a check of dependencies when
  [`library(bruceR)`](https://psychbruce.github.io/bruceR/).

## bruceR 0.8.7 (May 2022)

CRAN release: 2022-05-23

### New Features

- New functions
  [`add()`](https://psychbruce.github.io/bruceR/reference/add.md) and
  [`added()`](https://psychbruce.github.io/bruceR/reference/add.md):
  Enhanced functions designed to create, modify, and/or delete
  variables. The functions **combine** the advantages of `:=`
  (data.table), `mutate()` (dplyr), and `transmute()` (dplyr). See [help
  page](https://psychbruce.github.io/bruceR/reference/add.html) for the
  usage and convenience.
- New functions
  [`.sum()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)
  and
  [`.mean()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md):
  Tidy version of
  [`SUM()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)
  and
  [`MEAN()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)
  designed only for
  [`add()`](https://psychbruce.github.io/bruceR/reference/add.md) and
  [`added()`](https://psychbruce.github.io/bruceR/reference/add.md). See
  [help
  page](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.html)
  for the usage and convenience.

### Minor Changes

- Improved warning information for
  [`SUM()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
  [`MEAN()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md).

## bruceR 0.8.6 (Apr 2022)

CRAN release: 2022-04-13

### New Features

- Rebuilt the package homepage (<https://psychbruce.github.io/bruceR/>)
  with `pkgdown`. Configuration is specified in `_pkgdown.yml`.
- Added an `R CMD check` workflow on GitHub, which checks the code for
  each push.

### Minor Changes

- Improved
  [`cc()`](https://psychbruce.github.io/bruceR/reference/cc.md). Now it
  becomes much more convenient!
- Added a prompt message for the use of long data in
  [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md):
  “*Data are aggregated to mean (across items/trials) if there are \>=2
  observations per subject and cell. You may use Linear Mixed Model to
  analyze the data, e.g., with subjects and items as level-2 clusters.*”
- Added `center` argument for
  [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)
  (default is `TRUE`) for users who want to turn off the automatic
  grand-mean centering. However, mean centering is still highly
  suggested if one aim to obtain “main effect” rather than “fixed
  effect” (note: a fixed effect is not necessarily a main effect).
- Added `estimator` argument for
  [`CFA()`](https://psychbruce.github.io/bruceR/reference/CFA.md)
  (default is `"ML"`) for users who want to use any other estimator
  (fixed issue [\#17](https://github.com/psychbruce/bruceR/issues/17)).

## bruceR 0.8.5 (Mar 2022)

CRAN release: 2022-03-02

### New Features

- New function
  [`cc()`](https://psychbruce.github.io/bruceR/reference/cc.md): Split
  up a string (with separators) into a character vector (whitespace
  around separator is trimmed). For example,
  `cc("A 1 , B 2 ; C 3 | D 4 \t E 5")` produces a vector of
  `c("A 1", "B 2", "C 3", "D 4", "E 5")`. The default separators include
  `, ; | \n \t`. Users may also specify a separator.

### Minor Changes

- Added a guideline and examples for creating interaction plots using
  the returned object of
  [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)
  and
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md).
  You can save the returned object and use the
  [`emmeans::emmip()`](https://rvlenth.github.io/emmeans/reference/emmip.html)
  function to create an interaction plot (based on the fitted model and
  a formula specification). For usage, please see the help page of
  [`emmeans::emmip()`](https://rvlenth.github.io/emmeans/reference/emmip.html).
  It returns an object of class `ggplot`, which can be easily modified
  and saved using `ggplot2` syntax.

- Added an explanation of the automatic grand-mean centering in
  [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md).

### Bug Fixes

- Users who have not installed the `afex` package would see an unusual
  error when using the
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md)
  function (`Error: $ operator is invalid for atomic vectors`). So now
  `afex` is again a *strong dependency* of `bruceR`, such that it is
  automatically installed when installing `bruceR`.
- Improved debugging information for
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md)
  (when `model` is null).
- Fixed a bug of interaction tests in
  [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)
  when setting `mod.type="3-way"` for multilevel models.

## bruceR 0.8.3 (Jan 2022)

CRAN release: 2022-01-18

### Minor Changes

- Improved
  [`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md).
- Changed symbol (for better output of R Markdown): ✔ (\u2714) → √
  (\u221a).

### Bug Fixes

- Fixed a small bug of direct effect output in
  [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)
  for models without `"x-y"` in `mod.path` (e.g., Model 7).
- Fixed a small bug when using
  [`set.wd()`](https://psychbruce.github.io/bruceR/reference/set.wd.md)
  in an R Markdown file.

## bruceR 0.8.2 (Dec 2021)

CRAN release: 2021-12-12

### Minor Changes

- Requiring R version 4.0+ again.
- Added automatic check for new version of `bruceR` when
  [`library(bruceR)`](https://psychbruce.github.io/bruceR/).
- Added univariate tests (*F*) and multivariate tests (Pillai’s trace
  and *F*) using
  [`phia::testInteractions()`](https://rdrr.io/pkg/phia/man/testInteractions.html)
  in the output of
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md).
  These tests produce identical results to those obtained with the SPSS
  GLM (/EMMEANS) syntax.
- Improved the flexibility of
  [`Freq()`](https://psychbruce.github.io/bruceR/reference/Freq.md): Now
  both vector and data frame can be used. For example, users may specify
  either `Freq(data$variable)` or `Freq(data, "variable")`.
- Improved the output format of
  [`GLM_summary()`](https://psychbruce.github.io/bruceR/reference/GLM_summary.md)
  and
  [`HLM_summary()`](https://psychbruce.github.io/bruceR/reference/HLM_summary.md).
- Deprecated two useless arguments of
  [`HLM_summary()`](https://psychbruce.github.io/bruceR/reference/HLM_summary.md):
  `level2.predictors` and `vartypes`.
- Packages `lmerTest`, `mediation`, `interactions`, and `lavaan` are now
  strong dependencies such that they would be installed when installing
  `bruceR`. This also fixes an error when using
  [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)
  without these packages installed.

### Bug Fixes

- Fixed potential bugs in
  [`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md).
- Fixed a bug of VIF results in
  [`GLM_summary()`](https://psychbruce.github.io/bruceR/reference/GLM_summary.md)
  and
  [`HLM_summary()`](https://psychbruce.github.io/bruceR/reference/HLM_summary.md)
  when only one factor-type predictor with \>= 3 levels is in a
  regression model. Other bugs in these two functions have also been
  fixed.
- Fixed a bug of interaction tests in
  [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)
  when setting `mod.path="all"` in testing multilevel moderated
  mediation effects. Fixed another bug of CI output for direct effects
  when testing multilevel models.

## bruceR 0.8.0 (Nov 2021)

CRAN release: 2021-11-28

### New Features

- New function
  [`TTEST()`](https://psychbruce.github.io/bruceR/reference/TTEST.md):
  One-sample, independent-samples, and paired-samples *t*-test. Multiple
  dependent/independent variables can be tested simultaneously. It also
  tests the assumption of homogeneity of variance and allows users to
  determine whether variances are equal or not. Cohen’s *d* and 95% CI
  are reported by default (see Details and Examples in its help page for
  an issue about the *inconsistency* in the results of 95% CI of Cohen’s
  *d* between R packages). Bayes factor BF₁₀ is also supported. Key
  results can be saved in APA format to MS Word.
- New functions
  [`import()`](https://psychbruce.github.io/bruceR/reference/import.md)
  /
  [`export()`](https://psychbruce.github.io/bruceR/reference/export.md):
  Import/export data from/to a file with the two tidy functions,
  relieving users of the burden of remembering lots of `read_xxx()` /
  `write_xxx()` functions. Many file formats are supported (especially
  .txt, .csv, .tsv, .psv, .xls, .xlsx, .sav, .dta, .rda, .rdata, and
  clipboard). Note that the two functions are inspired by
  [`rio::import()`](http://gesistsa.github.io/rio/reference/import.md) /
  [`rio::export()`](http://gesistsa.github.io/rio/reference/export.md)
  and have several modifications for more convenient use. Since this
  version, the package `rio` is no longer a strong dependency of
  `bruceR` and would not be loaded when loading `bruceR`.

### Major Changes

- Improved
  [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)
  and
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md):

  - Fixed several bugs.
  - Modified the help pages.
  - Improved output tables. Now all results are printed using
    [`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md).
  - Improved algorithm for estimating Cohen’s *d*: Now it uses ***Root
    Mean Square Error*** **(RMSE) as the pooled *SD*** to estimate
    Cohen’s *d*. Although there is disagreement on the estimation of
    pooled *SD*,
    [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md)
    adopts this reasonable approach. If one uses
    [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)
    and
    [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md)
    to conduct the same *t*-test as using the
    [`TTEST()`](https://psychbruce.github.io/bruceR/reference/TTEST.md)
    function, the results will be identical. Indeed, the estimation
    methods of Cohen’s *d* in *t*-tests are acknowledged. In computing
    pooled *SD* in ANOVAs, it uses **(1)** the square root of *mean
    square error* (MSE) for between-subjects designs and **(2)** the
    square root of *mean variance of all paired differences of the
    residuals of repeated measures* for within-subjects and mixed
    designs. In both situations, it extracts the `lm` object from the
    returned value of
    [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md).
    Then, it mainly uses the
    [`sigma()`](https://rdrr.io/r/stats/sigma.html) and
    [`residuals()`](https://rdrr.io/r/stats/residuals.html) functions,
    respectively, to do these estimates. For source code, see [R file on
    GitHub](https://github.com/psychbruce/bruceR/tree/main/R). Thus, the
    results of Cohen’s *d* for designs with repeated measures are now
    different from those in `bruceR` old versions (\< 0.8.0), which
    indeed used an inappropriate method to compute pooled *SD* in such
    designs.
  - Added arguments (1) `ss.type` for
    [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)
    to specify either Type-II or Type-III Sum of Square; (2)
    `aov.include` for
    [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)
    and `model.type` for
    [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md),
    for details, see the help pages.
  - Added warning messages for wrong usage of these functions. If
    observations are not uniquely identified in user-defined long-format
    data, the function takes averages across those multiple observations
    for each case (thanks to Xiangying Zou for reporting an infrequent
    bug related to this issue).

- Improved
  [`Alpha()`](https://psychbruce.github.io/bruceR/reference/Alpha.md):
  Now it directly uses
  [`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html) and
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html), rather
  than `jmv::reliability()`, to perform reliability analysis. The format
  of result output has been changed and improved.

- Improved
  [`EFA()`](https://psychbruce.github.io/bruceR/reference/EFA.md)
  (almost completely rewritten): Now it directly uses
  [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html)
  and [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html), rather
  than `jmv::efa()`, to perform factor analysis (PCA or EFA). The format
  of result output has been changed and improved. MS Word output has
  been supported. A wrapper function
  [`PCA()`](https://psychbruce.github.io/bruceR/reference/EFA.md) has
  been added: `EFA(..., method="pca")`.

- Improved
  [`CFA()`](https://psychbruce.github.io/bruceR/reference/CFA.md) and
  [`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md):
  Now [`CFA()`](https://psychbruce.github.io/bruceR/reference/CFA.md)
  only uses the
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html), rather
  than `jmv:cfa()`, to build model, and then uses
  [`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md)
  to present results. For
  [`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md),
  many bugs have been fixed, and the format of result table has been
  changed and improved. Both functions now support saving table to MS
  Word.

- Package dependencies: Much fewer strong dependencies, for faster and
  more robust installation. Removed `rio` and `jmv` from dependencies.
  No longer load `rio` and `psych` when
  [`library(bruceR)`](https://psychbruce.github.io/bruceR/).

### Minor Changes

- Added an alias
  [`set_wd()`](https://psychbruce.github.io/bruceR/reference/set.wd.md)
  for
  [`set.wd()`](https://psychbruce.github.io/bruceR/reference/set.wd.md).
- Improved
  [`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md):
  Fixed an issue of incorrect length of Chinese character output in
  [`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md).
  Between-column blanks are now 2 spaces (rather than 1 space) for a
  clearer presentation of table columns.
- Modified onloading welcome messages.
- General bug-fixes and improvements.

## bruceR 0.7.3 (Nov 2021)

CRAN release: 2021-11-05

### Minor Changes

- Added Word output for
  [`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md)
  and
  [`granger_test()`](https://psychbruce.github.io/bruceR/reference/granger_test.md).

## bruceR 0.7.2 (Jun 2021)

CRAN release: 2021-06-21

### Minor Changes

- Added the `digits` parameter as the equivalent to the `nsmall`
  parameter for all relevant functions.
- Packages `mediation`, `interactions`, `MuMIn`, and `texreg` are now
  SUGGESTS rather than IMPORTS.

### Bug Fixes

- Fixed a bug of value ordering for 3-way interaction (moderated
  moderation) in
  [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md).
- Fixed a bug for Word output in
  [`Corr()`](https://psychbruce.github.io/bruceR/reference/Corr.md).

## bruceR 0.7.0 (May 2021)

CRAN release: 2021-05-28

### New Features

- New function
  [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md):
  PROCESS for mediation, moderation, and conditional process (moderated
  mediation) analyses! This function supports a total of 24 kinds of
  SPSS PROCESS models (Hayes, 2018) and also supports multilevel
  mediation/moderation analyses. Overall, it supports the most
  frequently used types of mediation, moderation, moderated moderation
  (3-way interaction), and moderated mediation (conditional indirect
  effect) analyses for (generalized) linear or linear mixed models.
  Regression model summary and effect estimates (simple slopes and/or
  indirect effects) are printed in an elegant way.
- New function
  [`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md):
  Tidy report of lavaan model.

### Minor Changes

- Improved many functions.
- Deprecated the `RANDBETWEEN()` function.

### Bug Fixes

- Fixed a bug in the CRAN version 0.6.4 (a problem newly emerging on
  2021-05-25).

## bruceR 0.6.4 (May 2021)

CRAN release: 2021-05-13

### New Features

- Added Word output (.doc) in
  [`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md)
  and other functions using
  [`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md)
  inside:
  [`Describe()`](https://psychbruce.github.io/bruceR/reference/Describe.md),
  [`Freq()`](https://psychbruce.github.io/bruceR/reference/Freq.md),
  [`Corr()`](https://psychbruce.github.io/bruceR/reference/Corr.md),
  [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md),
  [`med_summary()`](https://psychbruce.github.io/bruceR/reference/med_summary.md),
  [`granger_causality()`](https://psychbruce.github.io/bruceR/reference/granger_causality.md).

### Minor Changes

- Added a disclaimer about Cohen’s *d* in the output and documentation
  of
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md):
  There is considerable disagreement on how to compute Cohen’s *d*.
  Users should not take the default output as the only right results and
  are completely responsible for setting the “sd.pooled”.

### Bug Fixes

- Fixed bugs in
  [`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md): (1)
  Model names with `NULL`; (2) Multicollinearity check results with
  `NULL` or other problems; (3) UTF-8 encoding problem in WPS software
  (no such problem in Microsoft Word).

## bruceR 0.6.3 (Apr 2021)

### New Features

- New function
  [`granger_causality()`](https://psychbruce.github.io/bruceR/reference/granger_causality.md):
  Granger causality test (multivariate) based on vector autoregression
  (VAR) model. This function is an advanced and more general version of
  the function
  [`granger_test()`](https://psychbruce.github.io/bruceR/reference/granger_test.md)
  (bivariate).
- Added logo (designed by [@Meijia Li](https://github.com/Stellapros))

## bruceR 0.6.2 (Apr 2021)

CRAN release: 2021-04-11

### Major Changes

- Improved
  [`set.wd()`](https://psychbruce.github.io/bruceR/reference/set.wd.md):
  Now it uses
  [`rstudioapi::getSourceEditorContext()`](https://rstudio.github.io/rstudioapi/reference/rstudio-editors.html)
  to extract file path (even effective when running in R console), which
  only requires RStudio version \>= 0.99.1111 and no longer has encoding
  problems (see release note in 0.6.1).
- Improved
  [`theme_bruce()`](https://psychbruce.github.io/bruceR/reference/theme_bruce.md):
  Now it uses
  [`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html)
  to render Markdown/HTML rich text format, which can be used in plot
  text (e.g., titles).
- Improved
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md):
  Now its results are always identical to those in SPSS (by setting
  `model="multivariate"` in
  [`emmeans::joint_tests()`](https://rvlenth.github.io/emmeans/reference/joint_tests.html)
  and
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
  which use the `lm` or `mlm` objects rather than the `aov` object to
  perform tests). For a few cases with singular error matrix (i.e., some
  variables are linearly dependent), the results of simple-effect *F*
  tests will not be reported, but estimated marginal means and pairwise
  comparisons are not affected and so are still reported. Note that the
  `EMMEANS` results in old versions of `bruceR` (version \< 0.6.0) were
  identical to SPSS, but version 0.6.0 deprecated the parameter `repair`
  and no longer set `model$aov=NULL`, which made the results not
  identical to SPSS (particularly for ANOVAs with repeated measures). In
  response to a user’s feedback, now 0.6.2 has improved this function
  and makes its results accurate again.

### Minor Changes

- Improved function links in R documentation:
  `\code{\link[package:function]{package::function()}}`.

### Bug Fixes

- Fixed a bug in
  [`CFA()`](https://psychbruce.github.io/bruceR/reference/CFA.md) (for
  lavaan-style output).

## bruceR 0.6.1 (Mar 2021)

### New Features

- New function
  [`HLM_ICC_rWG()`](https://psychbruce.github.io/bruceR/reference/HLM_ICC_rWG.md):
  Tidy report of HLM indices “ICC(1)” (non-independence of data),
  “ICC(2)” (reliability of group means), and “rWG”/“rWG(J)”
  (within-group agreement for single-item/multi-item measures).
- New function
  [`Run()`](https://psychbruce.github.io/bruceR/reference/Run.md): Run
  code parsed from text.
- New function
  [`show_colors()`](https://psychbruce.github.io/bruceR/reference/show_colors.md):
  Show multiple colors (or a palette) in a plot.
- New function `%^%`: Paste strings together (a wrapper of
  [`paste0()`](https://rdrr.io/r/base/paste.html)).

### Major Changes

- Improved
  [`set.wd()`](https://psychbruce.github.io/bruceR/reference/set.wd.md):
  Now it converts the extracted path string from “UTF-8” to “GBK” on
  Windows system to support paths including Chinese characters
  (otherwise, the path would become messy code and cause an error). Note
  that this problem does not exist on Mac OS. In addition, warning
  messages will be printed into the console if the user’s RStudio
  version is lower than required (RStudio version \>= 1.4.843 is
  required for a complete implementation of this function).
- Improved
  [`Alpha()`](https://psychbruce.github.io/bruceR/reference/Alpha.md):
  Now it adds a parameter `varrange` (to keep the same as
  [`SUM()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
  [`MEAN()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
  …) and reports both Cronbach’s α and McDonald’s ω, with more detailed
  documentation.

> Three ways to specify the variable list (implemented in the functions
> such as
> [`SUM()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
> [`MEAN()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md),
> [`Alpha()`](https://psychbruce.github.io/bruceR/reference/Alpha.md)):
>
> 1\. **`var + items`**: use the common and unique parts of variable
> names. (e.g., `var="RSES", items=1:10, rev=c(3, 5, 8, 9, 10)`)
>
> 2\. **`vars`**: directly define the variable list. (e.g.,
> `vars=c("E1", "E2", "E3", "E4", "E5"), rev=c("E1", "E2")`)
>
> 3\. **`varrange`**: use the start and end positions of the variable
> list. (e.g., `varrange="E1:E5", rev=c("E1", "E2")`)

### Minor Changes

- Added details about the package’s contents in the Description field.

### Bug Fixes

- Fixed a potential bug in
  [`Corr()`](https://psychbruce.github.io/bruceR/reference/Corr.md)
  (relevant to the changes in
  [`psych::corr.test()`](https://rdrr.io/pkg/psych/man/corr.test.html)
  in a forthcoming release of the `psych` package).

## bruceR 0.6.0 (Mar 2021)

CRAN release: 2021-03-19

### Breaking News

- Formally published on
  [CRAN](https://CRAN.R-project.org/package=bruceR)!!!
- Passed R CMD check and Travis CI test:
  `0 errors √ | 0 warnings √ | 0 notes √`

### New Features

- New function
  [`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md):
  Tidy report of (single/multiple) regression models (into console or to
  a Word/HTML file; supporting most types of models; based on the
  `texreg` package).
- New function
  [`med_summary()`](https://psychbruce.github.io/bruceR/reference/med_summary.md):
  Tidy report of (simple/moderated) mediation analyses (based on the
  `mediation` package).
- New function
  [`ccf_plot()`](https://psychbruce.github.io/bruceR/reference/ccf_plot.md):
  Cross-correlation analysis (plotting with `ggplot2`).
- New function
  [`granger_test()`](https://psychbruce.github.io/bruceR/reference/granger_test.md):
  Granger test of predictive causality (based on the
  [`lmtest::grangertest()`](https://rdrr.io/pkg/lmtest/man/grangertest.html)
  function).

### Major Changes

- Improved many major functions, especially
  [`set.wd()`](https://psychbruce.github.io/bruceR/reference/set.wd.md),
  [`Describe()`](https://psychbruce.github.io/bruceR/reference/Describe.md),
  [`Corr()`](https://psychbruce.github.io/bruceR/reference/Corr.md),
  [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md),
  and
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md).
- Tidy welcome messages when you
  [`library(bruceR)`](https://psychbruce.github.io/bruceR/).
- More packages for default loading (see
  [details](https://github.com/psychbruce/bruceR#package-dependency)).
- Less packages for default installation (you can install all suggested
  packages by using the `pkg_install_suggested()` function).

### Minor Changes

- Changed package title and description of `bruceR`: **BR**oadly
  **U**seful **C**onvenient and **E**fficient **R** functions that
  **BR**ing **U**sers **C**oncise and **E**legant **R** data analyses.
- Reorganized [raw code
  files](https://github.com/psychbruce/bruceR/tree/main/R).

### Bug Fixes

- Fixed all bugs (errors, warnings, and notes) when conducting R CMD
  check.
- Fixed all problems in the manual inspection by CRAN team members.

### Notes

- Deprecated some useless/defective functions (see
  [details](https://github.com/psychbruce/bruceR/blob/main/R/deprecated.R)).

## bruceR 0.5.0 (Aug 2020)

- Requiring R version 4.0+.
- Improved many functions.
- Fixed many bugs.

## bruceR 0.4.0 (Dec 2019)

- Added citation information.
- General bug-fixes and improvements.

## bruceR 0.3.0 (Oct 2019)

- New functions
  [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)
  and
  [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md):
  ANOVA, simple-effect analyses, and multiple comparisons (based on the
  `afex` and `emmeans` packages).
- General bug-fixes and improvements.

## bruceR 0.2.0 (Aug 2019)

- Added all help pages.
- General bug-fixes and improvements.

## bruceR 0.1.0 (Jun 2019)

- Initial release on [GitHub](https://github.com/psychbruce/bruceR).
