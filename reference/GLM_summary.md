# Tidy report of GLM (`lm` and `glm` models).

NOTE:
[`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md)
is preferred.

## Usage

``` r
GLM_summary(model, robust = FALSE, cluster = NULL, digits = 3, ...)
```

## Arguments

- model:

  A model fitted with `lm` or `glm` function.

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

- digits:

  Number of decimal places of output. Defaults to `3`.

- ...:

  Other arguments. You may re-define `formula`, `data`, or `family`.

## Value

No return value.

## See also

[`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md)
(print simple table)

[`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md)
(strongly suggested)

[`HLM_summary()`](https://psychbruce.github.io/bruceR/reference/HLM_summary.md)

[`regress()`](https://psychbruce.github.io/bruceR/reference/regress.md)

## Examples

``` r
## Example 1: OLS regression
lm = lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
GLM_summary(lm)
#> 
#> General Linear Model (OLS Regression)
#> 
#> Model Fit:
#> F(4, 141) = 22.22, p = 3e-14 ***
#> R² = 0.38659 (Adjusted R² = 0.36919)
#> 
#> Unstandardized Coefficients:
#> Outcome Variable: Temp
#> N = 146 (7 missing cases deleted)
#> ───────────────────────────────────────────────────────────────────
#>                   b    S.E.      t     p        [95% CI of b]   VIF
#> ───────────────────────────────────────────────────────────────────
#> (Intercept)  68.770 (4.391) 15.662 <.001 *** [60.089, 77.450]      
#> Month         2.225 (0.441)  5.047 <.001 *** [ 1.353,  3.096] 1.035
#> Day          -0.084 (0.070) -1.194  .234     [-0.222,  0.055] 1.024
#> Wind         -1.003 (0.176) -5.695 <.001 *** [-1.352, -0.655] 1.032
#> Solar.R       0.027 (0.007)  3.991 <.001 *** [ 0.014,  0.041] 1.034
#> ───────────────────────────────────────────────────────────────────
#> 
#> Registered S3 methods overwritten by 'MuMIn':
#>   method        from 
#>   nobs.multinom broom
#>   nobs.fitdistr broom
#> Standardized Coefficients (β):
#> Outcome Variable: Temp
#> N = 146 (7 missing cases deleted)
#> ────────────────────────────────────────────────────────────────────────────
#>               β    S.E.      t     p        [95% CI of β] r(partial) r(part)
#> ────────────────────────────────────────────────────────────────────────────
#> Month     0.339 (0.067)  5.047 <.001 *** [ 0.206,  0.471]      0.391   0.333
#> Day      -0.080 (0.067) -1.194  .234     [-0.212,  0.052]     -0.100  -0.079
#> Wind     -0.382 (0.067) -5.695 <.001 *** [-0.514, -0.249]     -0.432  -0.376
#> Solar.R   0.268 (0.067)  3.991 <.001 *** [ 0.135,  0.400]      0.319   0.263
#> ────────────────────────────────────────────────────────────────────────────
#> 
GLM_summary(lm, robust="HC1")
#> 
#> General Linear Model (OLS Regression)
#> 
#> Model Fit:
#> F(4, 141) = 22.22, p = 3e-14 ***
#> R² = 0.38659 (Adjusted R² = 0.36919)
#> 
#> Unstandardized Coefficients:
#> Outcome Variable: Temp
#> N = 146 (7 missing cases deleted)
#> ───────────────────────────────────────────────────────────────────
#>                   b    S.E.      t     p        [95% CI of b]   VIF
#> ───────────────────────────────────────────────────────────────────
#> (Intercept)  68.770 (4.391) 15.662 <.001 *** [60.089, 77.450]      
#> Month         2.225 (0.441)  5.047 <.001 *** [ 1.353,  3.096] 1.035
#> Day          -0.084 (0.070) -1.194  .234     [-0.222,  0.055] 1.024
#> Wind         -1.003 (0.176) -5.695 <.001 *** [-1.352, -0.655] 1.032
#> Solar.R       0.027 (0.007)  3.991 <.001 *** [ 0.014,  0.041] 1.034
#> ───────────────────────────────────────────────────────────────────
#> 
#> Heteroskedasticity-Robust Standard Errors:
#> ─────────────────────────────────────────────────────────────
#>                   b    S.E.      t     p        [95% CI of b]
#> ─────────────────────────────────────────────────────────────
#> (Intercept)  68.770 (4.550) 15.113 <.001 *** [59.774, 77.766]
#> Month         2.225 (0.488)  4.557 <.001 *** [ 1.260,  3.190]
#> Day          -0.084 (0.068) -1.231  .220     [-0.218,  0.051]
#> Wind         -1.003 (0.152) -6.604 <.001 *** [-1.304, -0.703]
#> Solar.R       0.027 (0.007)  4.089 <.001 *** [ 0.014,  0.041]
#> ─────────────────────────────────────────────────────────────
#> Robust S.E.: type = HC1.
#> 
#> Standardized Coefficients (β):
#> Outcome Variable: Temp
#> N = 146 (7 missing cases deleted)
#> ────────────────────────────────────────────────────────────────────────────
#>               β    S.E.      t     p        [95% CI of β] r(partial) r(part)
#> ────────────────────────────────────────────────────────────────────────────
#> Month     0.339 (0.067)  5.047 <.001 *** [ 0.206,  0.471]      0.391   0.333
#> Day      -0.080 (0.067) -1.194  .234     [-0.212,  0.052]     -0.100  -0.079
#> Wind     -0.382 (0.067) -5.695 <.001 *** [-0.514, -0.249]     -0.432  -0.376
#> Solar.R   0.268 (0.067)  3.991 <.001 *** [ 0.135,  0.400]      0.319   0.263
#> ────────────────────────────────────────────────────────────────────────────
#> 
# Stata's default is "HC1"
# R package <sandwich>'s default is "HC3"

## Example 2: Logistic regression
glm = glm(case ~ age + parity + education + spontaneous + induced,
          data=infert, family=binomial)
GLM_summary(glm)
#> 
#> Generalized Linear Model (GLM)
#> 
#> Model Fit:
#> AIC = 271.798
#> BIC = 296.392
#> χ²(6) = 58.37, p = 1e-10 ***
#> ─────── Pseudo-R² ───────
#> McFadden’s R²   = 0.18463 (= 1 - logLik(model)/logLik(null.model))
#> Nagelkerke’s R² = 0.29107 (= Cragg-Uhler’s R², adjusts Cox & Snell’s)
#> 
#> Unstandardized Coefficients:
#> Outcome Variable: case (family: binomial; link function: logit)
#> N = 248
#> ──────────────────────────────────────────────────────────────────────────────
#>                        b    S.E.      z     p        [95% CI of b]    OR   VIF
#> ──────────────────────────────────────────────────────────────────────────────
#> (Intercept)       -1.149 (1.412) -0.814  .416     [-3.917,  1.619] 0.317      
#> age                0.040 (0.031)  1.269  .205     [-0.022,  0.101] 1.040 1.174
#> parity            -0.828 (0.196) -4.215 <.001 *** [-1.213, -0.443] 0.437 2.480
#> education6-11yrs  -1.044 (0.793) -1.318  .188     [-2.598,  0.509] 0.352 1.361
#> education12+ yrs  -1.403 (0.834) -1.682  .093 .   [-3.038,  0.232] 0.246 1.361
#> spontaneous        2.046 (0.310)  6.596 <.001 *** [ 1.438,  2.654] 7.736 2.191
#> induced            1.289 (0.301)  4.275 <.001 *** [ 0.698,  1.880] 3.628 2.222
#> ──────────────────────────────────────────────────────────────────────────────
#> OR = odds ratio.
#> 
GLM_summary(glm, robust="HC1", cluster="stratum")
#> 
#> Generalized Linear Model (GLM)
#> 
#> Model Fit:
#> AIC = 271.798
#> BIC = 296.392
#> χ²(6) = 58.37, p = 1e-10 ***
#> ─────── Pseudo-R² ───────
#> McFadden’s R²   = 0.18463 (= 1 - logLik(model)/logLik(null.model))
#> Nagelkerke’s R² = 0.29107 (= Cragg-Uhler’s R², adjusts Cox & Snell’s)
#> 
#> Unstandardized Coefficients:
#> Outcome Variable: case (family: binomial; link function: logit)
#> N = 248
#> ──────────────────────────────────────────────────────────────────────────────
#>                        b    S.E.      z     p        [95% CI of b]    OR   VIF
#> ──────────────────────────────────────────────────────────────────────────────
#> (Intercept)       -1.149 (1.412) -0.814  .416     [-3.917,  1.619] 0.317      
#> age                0.040 (0.031)  1.269  .205     [-0.022,  0.101] 1.040 1.174
#> parity            -0.828 (0.196) -4.215 <.001 *** [-1.213, -0.443] 0.437 2.480
#> education6-11yrs  -1.044 (0.793) -1.318  .188     [-2.598,  0.509] 0.352 1.361
#> education12+ yrs  -1.403 (0.834) -1.682  .093 .   [-3.038,  0.232] 0.246 1.361
#> spontaneous        2.046 (0.310)  6.596 <.001 *** [ 1.438,  2.654] 7.736 2.191
#> induced            1.289 (0.301)  4.275 <.001 *** [ 0.698,  1.880] 3.628 2.222
#> ──────────────────────────────────────────────────────────────────────────────
#> OR = odds ratio.
#> 
#> Cluster-Robust Standard Errors:
#> ──────────────────────────────────────────────────────────────────
#>                        b    S.E.      z     p        [95% CI of b]
#> ──────────────────────────────────────────────────────────────────
#> (Intercept)       -1.149 (0.810) -1.418  .156     [-2.737,  0.439]
#> age                0.040 (0.017)  2.373  .018 *   [ 0.007,  0.072]
#> parity            -0.828 (0.194) -4.278 <.001 *** [-1.208, -0.449]
#> education6-11yrs  -1.044 (0.542) -1.926  .054 .   [-2.107,  0.018]
#> education12+ yrs  -1.403 (0.587) -2.392  .017 *   [-2.553, -0.253]
#> spontaneous        2.046 (0.351)  5.832 <.001 *** [ 1.358,  2.733]
#> induced            1.289 (0.319)  4.044 <.001 *** [ 0.664,  1.913]
#> ──────────────────────────────────────────────────────────────────
#> Robust S.E.: type = HC1; clustering variable = stratum.
#> 
```
