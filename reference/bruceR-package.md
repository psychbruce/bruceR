# bruceR: **BR**oadly **U**seful **C**onvenient and **E**fficient **R** functions

Broadly useful convenient and efficient R functions that bring users
concise and elegant R data analyses. This package includes easy-to-use
functions for (1) basic R programming (e.g., set working directory to
the path of currently opened file; import/export data from/to files in
any format; print tables to Microsoft Word); (2) multivariate
computation (e.g., compute scale sums/means/... with reverse scoring);
(3) reliability analyses and factor analyses; (4) descriptive statistics
and correlation analyses; (5) t-test, multi-factor analysis of variance
(ANOVA), simple-effect analysis, and post-hoc multiple comparison; (6)
tidy report of statistical models (to R Console and Microsoft Word); (7)
mediation and moderation analyses (PROCESS); and (8) additional toolbox
for statistics and graphics.

## Main Functions in `bruceR`

### 1. Basic R Programming

- [`set.wd()`](https://psychbruce.github.io/bruceR/reference/set.wd.md)
  (alias:
  [`set_wd()`](https://psychbruce.github.io/bruceR/reference/set.wd.md))

- [`import()`](https://psychbruce.github.io/bruceR/reference/import.md)

- [`export()`](https://psychbruce.github.io/bruceR/reference/export.md)

- [`cc()`](https://psychbruce.github.io/bruceR/reference/cc.md)

- [`pkg_depend()`](https://psychbruce.github.io/bruceR/reference/pkg_depend.md)

- [`formatF()`](https://psychbruce.github.io/bruceR/reference/formatF.md)

- [`formatN()`](https://psychbruce.github.io/bruceR/reference/formatN.md)

- [`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md)

- [`Print()`](https://psychbruce.github.io/bruceR/reference/Print.md)

- [`Glue()`](https://psychbruce.github.io/bruceR/reference/Print.md)

- [`Run()`](https://psychbruce.github.io/bruceR/reference/Run.md)

- [`%^%`](https://psychbruce.github.io/bruceR/reference/grapes-pow-grapes.md)

- [`%notin%`](https://rdatatable.gitlab.io/data.table/reference/notin.html)

- [`%allin%`](https://psychbruce.github.io/bruceR/reference/grapes-allin-grapes.md)

- [`%anyin%`](https://psychbruce.github.io/bruceR/reference/grapes-anyin-grapes.md)

- [`%nonein%`](https://psychbruce.github.io/bruceR/reference/grapes-nonein-grapes.md)

- [`%partin%`](https://psychbruce.github.io/bruceR/reference/grapes-partin-grapes.md)

### 2. Multivariate Computation

- [`add()`](https://psychbruce.github.io/bruceR/reference/add.md)

- [`added()`](https://psychbruce.github.io/bruceR/reference/add.md)

- [`.sum()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)

- [`.mean()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)

- [`SUM()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)

- [`MEAN()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)

- [`STD()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)

- [`MODE()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)

- [`COUNT()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)

- [`CONSEC()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)

- [`RECODE()`](https://psychbruce.github.io/bruceR/reference/RECODE.md)

- [`RESCALE()`](https://psychbruce.github.io/bruceR/reference/RESCALE.md)

- [`LOOKUP()`](https://psychbruce.github.io/bruceR/reference/LOOKUP.md)

### 3. Reliability and Factor Analyses

- [`Alpha()`](https://psychbruce.github.io/bruceR/reference/Alpha.md)

- [`EFA()`](https://psychbruce.github.io/bruceR/reference/EFA.md)

- [`PCA()`](https://psychbruce.github.io/bruceR/reference/EFA.md)

- [`CFA()`](https://psychbruce.github.io/bruceR/reference/CFA.md)

### 4. Descriptive Statistics and Correlation Analyses

- [`Describe()`](https://psychbruce.github.io/bruceR/reference/Describe.md)

- [`Freq()`](https://psychbruce.github.io/bruceR/reference/Freq.md)

- [`Corr()`](https://psychbruce.github.io/bruceR/reference/Corr.md)

- [`cor_diff()`](https://psychbruce.github.io/bruceR/reference/cor_diff.md)

- [`cor_multilevel()`](https://psychbruce.github.io/bruceR/reference/cor_multilevel.md)

### 5. T-Test, Multi-Factor ANOVA, Simple-Effect Analysis, and Post-Hoc Multiple Comparison

- [`TTEST()`](https://psychbruce.github.io/bruceR/reference/TTEST.md)

- [`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)

- [`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md)

### 6. Tidy Report of Regression Models

- [`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md)

- [`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md)

- [`GLM_summary()`](https://psychbruce.github.io/bruceR/reference/GLM_summary.md)

- [`HLM_summary()`](https://psychbruce.github.io/bruceR/reference/HLM_summary.md)

- [`HLM_ICC_rWG()`](https://psychbruce.github.io/bruceR/reference/HLM_ICC_rWG.md)

- [`regress()`](https://psychbruce.github.io/bruceR/reference/regress.md)

### 7. Mediation and Moderation Analyses

- [`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)

- [`med_summary()`](https://psychbruce.github.io/bruceR/reference/med_summary.md)

- [`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md)

### 8. Additional Toolbox for Statistics and Graphics

- [`grand_mean_center()`](https://psychbruce.github.io/bruceR/reference/grand_mean_center.md)

- [`group_mean_center()`](https://psychbruce.github.io/bruceR/reference/group_mean_center.md)

- [`ccf_plot()`](https://psychbruce.github.io/bruceR/reference/ccf_plot.md)

- [`granger_test()`](https://psychbruce.github.io/bruceR/reference/granger_test.md)

- [`granger_causality()`](https://psychbruce.github.io/bruceR/reference/granger_causality.md)

- [`theme_bruce()`](https://psychbruce.github.io/bruceR/reference/theme_bruce.md)

- [`show_colors()`](https://psychbruce.github.io/bruceR/reference/show_colors.md)

## See also

Useful links:

- <https://psychbruce.github.io/bruceR/>

- Report bugs at <https://github.com/psychbruce/bruceR/issues>

## Author

**Maintainer**: Han Wu Shuang Bao <baohws@foxmail.com>
([ORCID](https://orcid.org/0000-0003-3043-710X))
