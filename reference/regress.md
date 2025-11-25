# Regression analysis.

NOTE:
[`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md)
is preferred.

## Usage

``` r
regress(
  formula,
  data,
  family = NULL,
  digits = 3,
  robust = FALSE,
  cluster = NULL,
  test.rand = FALSE
)
```

## Arguments

- formula:

  Model formula.

- data:

  Data frame.

- family:

  \[Optional\] The same as in `glm` and `glmer` (e.g., `family=binomial`
  fits a logistic regression model).

- digits:

  Number of decimal places of output. Defaults to `3`.

- robust:

  \[Only for `lm` and `glm`\] Robust standard errors. Add a table with
  heteroskedasticity-robust standard errors (aka. Huber-White standard
  errors).

  Options: `FALSE` (default), `TRUE` (`"HC1"`), `"HC0"`, `"HC1"`,
  `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. For details, see
  [`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)
  and
  [`jtools::summ.lm()`](https://jtools.jacob-long.com/reference/summ.lm.html).

  Note: `"HC1"` is the default of Stata, while `"HC3"` is the default
  suggested by the `sandwich` package.

- cluster:

  \[Only for `lm` and `glm`\] Cluster-robust standard errors are
  computed if cluster is set to the name of the input data's cluster
  variable or is a vector of clusters.

- test.rand:

  \[Only for `lmer` and `glmer`\] `TRUE` or `FALSE` (default). Test
  random effects (i.e., variance components) by using the
  likelihood-ratio test (LRT), which is asymptotically chi-square
  distributed. For large datasets, it is much time-consuming.

## Value

No return value.

## See also

[`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md)
(print simple table)

[`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md)
(strongly suggested)

[`GLM_summary()`](https://psychbruce.github.io/bruceR/reference/GLM_summary.md)

[`HLM_summary()`](https://psychbruce.github.io/bruceR/reference/HLM_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{

  ## lm
  regress(Temp ~ Month + Day + Wind + Solar.R, data=airquality, robust=TRUE)

  ## glm
  regress(case ~ age + parity + education + spontaneous + induced,
          data=infert, family=binomial, robust="HC1", cluster="stratum")

  ## lmer
  library(lmerTest)
  regress(Reaction ~ Days + (Days | Subject), data=sleepstudy)
  regress(Preference ~ Sweetness + Gender + Age + Frequency +
            (1 | Consumer), data=carrots)

  ## glmer
  library(lmerTest)
  data.glmm = MASS::bacteria
  regress(y ~ trt + week + (1 | ID), data=data.glmm, family=binomial)
  regress(y ~ trt + week + hilo + (1 | ID), data=data.glmm, family=binomial)
} # }
```
