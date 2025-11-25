# One-sample, independent-samples, and paired-samples t-test.

One-sample, independent-samples, and paired-samples *t*-test, with both
Frequentist and Bayesian approaches. The output includes descriptives,
*t* statistics, mean difference with 95% CI, Cohen's *d* with 95% CI,
and Bayes factor (\\BF\_{10}\\; `BayesFactor` package needs to be
installed). It also tests the assumption of homogeneity of variance and
allows users to determine whether variances are equal or not.

Users can simultaneously test multiple dependent and/or independent
variables. The results of one pair of Y-X would be summarized in one row
in the output. Key results can be saved in APA format to MS Word.

## Usage

``` r
TTEST(
  data,
  y,
  x = NULL,
  paired = FALSE,
  paired.d.type = "dz",
  var.equal = TRUE,
  mean.diff = TRUE,
  test.value = 0,
  test.sided = c("=", "<", ">"),
  factor.rev = TRUE,
  bayes.prior = "medium",
  digits = 2,
  file = NULL
)
```

## Arguments

- data:

  Data frame (wide-format only, i.e., one case in one row).

- y:

  Dependent variable(s). Multiple variables should be included in a
  character vector [`c()`](https://rdrr.io/r/base/c.html).

  For paired-samples *t*-test, the number of variables should be 2, 4,
  6, etc.

- x:

  Independent variable(s). Multiple variables should be included in a
  character vector [`c()`](https://rdrr.io/r/base/c.html).

  Only necessary for independent-samples *t*-test.

- paired:

  For paired-samples *t*-test, set it as `TRUE`. Defaults to `FALSE`.

- paired.d.type:

  Type of Cohen's *d* for paired-samples *t*-test (see Lakens, 2013).
  Defaults to `"dz"`.

  Options:

  `"dz"` (*d* for standardized difference)

  :   Cohen's \\d\_{z} = \frac{M\_{diff}}{SD\_{diff}}\\

  `"dav"` (*d* for average standard deviation)

  :   Cohen's \\d\_{av} = \frac{M\_{diff}}{ \frac{SD\_{1} + SD\_{2}}{2}
      }\\

  `"drm"` (*d* for repeated measures, corrected for correlation)

  :   Cohen's \\d\_{rm} = \frac{M\_{diff} \times \sqrt{2(1 -
      r\_{1,2})}}{ \sqrt{SD\_{1}^2 + SD\_{2}^2 - 2 \times r\_{1,2}
      \times SD\_{1} \times SD\_{2}} }\\

- var.equal:

  If Levene's test indicates a violation of the homogeneity of variance,
  then you should better set this argument as `FALSE`. Defaults to
  `TRUE`.

- mean.diff:

  Whether to display results of mean difference and its 95% CI. Defaults
  to `TRUE`.

- test.value:

  The true value of the mean (or difference in means for a two-samples
  test). Defaults to `0`.

- test.sided:

  Any of `"="` (two-sided, the default), `"<"` (one-sided), or `">"`
  (one-sided).

- factor.rev:

  Whether to reverse the levels of factor (X) such that the test
  compares higher vs. lower level. Defaults to `TRUE`.

- bayes.prior:

  Prior scale in Bayesian *t*-test. Defaults to 0.707. See details in
  [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html).

- digits:

  Number of decimal places of output. Defaults to `2`.

- file:

  File name of MS Word (`".doc"`).

## Value

Invisibly return the results.

## Details

Note that the point estimate of Cohen's *d* is computed using the common
method "Cohen's *d* = mean difference / (pooled) standard deviation",
which is consistent with results from other R packages (e.g.,
`effectsize`) and software (e.g., `jamovi`). The 95% CI of Cohen's *d*
is estimated based on the 95% CI of mean difference (i.e., also divided
by the pooled standard deviation).

However, different packages and software diverge greatly on the estimate
of the 95% CI of Cohen's *d*. R packages such as `psych` and
`effectsize`, R software `jamovi`, and several online statistical tools
for estimating effect sizes indeed produce surprisingly inconsistent
results on the 95% CI of Cohen's *d*.

See an illustration of this issue in the section "Examples".

## References

Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
cumulative science: A practical primer for *t*-tests and ANOVAs.
*Frontiers in Psychology, 4*, Article 863.

## See also

[`MANOVA()`](https://psychbruce.github.io/bruceR/reference/MANOVA.md)

[`EMMEANS()`](https://psychbruce.github.io/bruceR/reference/EMMEANS.md)

## Examples

``` r
## Demo data ##
d1 = between.3
d1$Y1 = d1$SCORE  # shorter name for convenience
d1$Y2 = rnorm(32)  # random variable
d1$B = factor(d1$B, levels=1:2, labels=c("Low", "High"))
d1$C = factor(d1$C, levels=1:2, labels=c("M", "F"))
d2 = within.1

## One-sample t-test ##
TTEST(d1, "SCORE")
#> 
#> One-Sample t-test
#> 
#> Hypothesis: two-sided (μ ≠ 0)
#> 
#> Descriptives:
#> ────────────────────────
#>  Variable  N Mean (S.D.)
#> ────────────────────────
#>     SCORE 32 6.31 (2.95)
#> ────────────────────────
#> 
#> Results of t-test:
#> ──────────────────────────────────────────────────────────────────────────────────────
#>                         t df     p     Difference [95% CI] Cohen’s d [95% CI]     BF10
#> ──────────────────────────────────────────────────────────────────────────────────────
#> SCORE: (SCORE - 0)  12.13 31 <.001 ***   6.31 [5.25, 7.37]  2.14 [1.78, 2.50] 2.85e+10
#> ──────────────────────────────────────────────────────────────────────────────────────
#> 
TTEST(d1, "SCORE", test.value=5)
#> 
#> One-Sample t-test
#> 
#> Hypothesis: two-sided (μ ≠ 5)
#> 
#> Descriptives:
#> ────────────────────────
#>  Variable  N Mean (S.D.)
#> ────────────────────────
#>     SCORE 32 6.31 (2.95)
#> ────────────────────────
#> 
#> Results of t-test:
#> ─────────────────────────────────────────────────────────────────────────────────────
#>                        t df     p     Difference [95% CI] Cohen’s d [95% CI]     BF10
#> ─────────────────────────────────────────────────────────────────────────────────────
#> SCORE: (SCORE - 5)  2.52 31  .017 *     1.31 [0.25, 2.37]  0.45 [0.09, 0.81] 2.81e+00
#> ─────────────────────────────────────────────────────────────────────────────────────
#> 

## Independent-samples t-test ##
TTEST(d1, "SCORE", x="A")
#> 
#> Independent-Samples t-test
#> 
#> Hypothesis: two-sided (μ2 - μ1 ≠ 0)
#> 
#> Descriptives:
#> ─────────────────────────────────────
#>  Variable Factor Level  N Mean (S.D.)
#> ─────────────────────────────────────
#>     SCORE      A     1 16 4.12 (1.45)
#>     SCORE      A     2 16 8.50 (2.37)
#> ─────────────────────────────────────
#> 
#> Levene’s test for homogeneity of variance:
#> ──────────────────────────────────────────────
#>                   Levene’s F df1 df2     p    
#> ──────────────────────────────────────────────
#> SCORE: A (2 - 1)        3.25   1  30  .081 .  
#> ──────────────────────────────────────────────
#> Note: H0 = equal variance (homoscedasticity).
#> If significant (violation of the assumption),
#> then you should better set `var.equal=FALSE`.
#> 
#> Results of t-test:
#> ───────────────────────────────────────────────────────────────────────────────────
#>                      t df     p     Difference [95% CI] Cohen’s d [95% CI]     BF10
#> ───────────────────────────────────────────────────────────────────────────────────
#> SCORE: A (2 - 1)  6.30 30 <.001 ***   4.38 [2.96, 5.79]  2.23 [1.51, 2.95] 1.89e+04
#> ───────────────────────────────────────────────────────────────────────────────────
#> 
TTEST(d1, "SCORE", x="A", var.equal=FALSE)
#> 
#> Independent-Samples t-test
#> 
#> Hypothesis: two-sided (μ2 - μ1 ≠ 0)
#> 
#> Descriptives:
#> ─────────────────────────────────────
#>  Variable Factor Level  N Mean (S.D.)
#> ─────────────────────────────────────
#>     SCORE      A     1 16 4.12 (1.45)
#>     SCORE      A     2 16 8.50 (2.37)
#> ─────────────────────────────────────
#> 
#> Levene’s test for homogeneity of variance:
#> ──────────────────────────────────────────────
#>                   Levene’s F df1 df2     p    
#> ──────────────────────────────────────────────
#> SCORE: A (2 - 1)        3.25   1  30  .081 .  
#> ──────────────────────────────────────────────
#> Note: H0 = equal variance (homoscedasticity).
#> If significant (violation of the assumption),
#> then you should better set `var.equal=FALSE`.
#> 
#> Results of t-test (adjusted df):
#> ──────────────────────────────────────────────────────────────────────────────────────
#>                      t    df     p     Difference [95% CI] Cohen’s d [95% CI]     BF10
#> ──────────────────────────────────────────────────────────────────────────────────────
#> SCORE: A (2 - 1)  6.30 24.92 <.001 ***   4.38 [2.94, 5.81]  2.23 [1.50, 2.96] 1.89e+04
#> ──────────────────────────────────────────────────────────────────────────────────────
#> 
TTEST(d1, y="Y1", x=c("A", "B", "C"))
#> 
#> Independent-Samples t-test
#> 
#> Hypothesis: two-sided (μ2 - μ1 ≠ 0)
#> 
#> Descriptives:
#> ─────────────────────────────────────
#>  Variable Factor Level  N Mean (S.D.)
#> ─────────────────────────────────────
#>        Y1      A  1    16 4.12 (1.45)
#>        Y1      A  2    16 8.50 (2.37)
#>        Y1      B  Low  16 5.69 (1.99)
#>        Y1      B  High 16 6.94 (3.62)
#>        Y1      C  M    16 6.00 (2.34)
#>        Y1      C  F    16 6.62 (3.50)
#> ─────────────────────────────────────
#> 
#> Levene’s test for homogeneity of variance:
#> ────────────────────────────────────────────────
#>                     Levene’s F df1 df2     p    
#> ────────────────────────────────────────────────
#> Y1: A (2 - 1)             3.25   1  30  .081 .  
#> Y1: B (High - Low)        7.85   1  30  .009 ** 
#> Y1: C (F - M)             1.88   1  30  .181    
#> ────────────────────────────────────────────────
#> Note: H0 = equal variance (homoscedasticity).
#> If significant (violation of the assumption),
#> then you should better set `var.equal=FALSE`.
#> 
#> Results of t-test:
#> ─────────────────────────────────────────────────────────────────────────────────────
#>                        t df     p     Difference [95% CI] Cohen’s d [95% CI]     BF10
#> ─────────────────────────────────────────────────────────────────────────────────────
#> Y1: A (2 - 1)       6.30 30 <.001 ***  4.38 [ 2.96, 5.79] 2.23 [ 1.51, 2.95] 1.89e+04
#> Y1: B (High - Low)  1.21 30  .236      1.25 [-0.86, 3.36] 0.43 [-0.29, 1.15] 5.86e-01
#> Y1: C (F - M)       0.59 30  .557      0.62 [-1.52, 2.77] 0.21 [-0.51, 0.93] 3.85e-01
#> ─────────────────────────────────────────────────────────────────────────────────────
#> 
TTEST(d1, y=c("Y1", "Y2"), x=c("A", "B", "C"),
      mean.diff=FALSE,  # remove to save space
      file="t-result.doc")
#> 
#> Independent-Samples t-test
#> 
#> Hypothesis: two-sided (μ2 - μ1 ≠ 0)
#> 
#> Descriptives:
#> ──────────────────────────────────────
#>  Variable Factor Level  N  Mean (S.D.)
#> ──────────────────────────────────────
#>        Y1      A  1    16  4.12 (1.45)
#>        Y1      A  2    16  8.50 (2.37)
#>        Y1      B  Low  16  5.69 (1.99)
#>        Y1      B  High 16  6.94 (3.62)
#>        Y1      C  M    16  6.00 (2.34)
#>        Y1      C  F    16  6.62 (3.50)
#>        Y2      A  1    16 -0.65 (0.85)
#>        Y2      A  2    16 -0.11 (0.72)
#>        Y2      B  Low  16 -0.34 (0.79)
#>        Y2      B  High 16 -0.42 (0.88)
#>        Y2      C  M    16 -0.60 (0.83)
#>        Y2      C  F    16 -0.16 (0.77)
#> ──────────────────────────────────────
#> 
#> Levene’s test for homogeneity of variance:
#> ────────────────────────────────────────────────
#>                     Levene’s F df1 df2     p    
#> ────────────────────────────────────────────────
#> Y1: A (2 - 1)             3.25   1  30  .081 .  
#> Y1: B (High - Low)        7.85   1  30  .009 ** 
#> Y1: C (F - M)             1.88   1  30  .181    
#> Y2: A (2 - 1)             0.25   1  30  .619    
#> Y2: B (High - Low)        0.28   1  30  .598    
#> Y2: C (F - M)             0.02   1  30  .890    
#> ────────────────────────────────────────────────
#> Note: H0 = equal variance (homoscedasticity).
#> If significant (violation of the assumption),
#> then you should better set `var.equal=FALSE`.
#> 
#> ✔ Table saved to "/home/runner/work/bruceR/bruceR/docs/reference/t-result.doc"
#> 
unlink("t-result.doc")  # delete file for code check

## Paired-samples t-test ##
TTEST(d2, y=c("A1", "A2"), paired=TRUE)
#> 
#> Paired-Samples t-test
#> 
#> Hypothesis: two-sided (μ2 - μ1 ≠ 0)
#> 
#> Descriptives:
#> ───────────────────────
#>  Variable N Mean (S.D.)
#> ───────────────────────
#>        A1 8 4.38 (1.69)
#>        A2 8 3.88 (1.25)
#> ───────────────────────
#> 
#> Results of t-test:
#> ──────────────────────────────────────────────────────────────────────────────────────
#>                        t df     p     Difference [95% CI]  Cohen’s d [95% CI]     BF10
#> ──────────────────────────────────────────────────────────────────────────────────────
#> Paired: (A2 - A1)  -1.18  7  .275     -0.50 [-1.50, 0.50] -0.42 [-1.25, 0.42] 5.78e-01
#> ──────────────────────────────────────────────────────────────────────────────────────
#> 
TTEST(d2, y=c("A1", "A2", "A3", "A4"), paired=TRUE)
#> 
#> Paired-Samples t-test
#> 
#> Hypothesis: two-sided (μ2 - μ1 ≠ 0)
#> 
#> Descriptives:
#> ────────────────────────
#>  Variable N  Mean (S.D.)
#> ────────────────────────
#>        A1 8  4.38 (1.69)
#>        A2 8  3.88 (1.25)
#>        A3 8  7.00 (1.31)
#>        A4 8 10.00 (2.27)
#> ────────────────────────
#> 
#> Results of t-test:
#> ──────────────────────────────────────────────────────────────────────────────────────
#>                        t df     p     Difference [95% CI]  Cohen’s d [95% CI]     BF10
#> ──────────────────────────────────────────────────────────────────────────────────────
#> Paired: (A2 - A1)  -1.18  7  .275     -0.50 [-1.50, 0.50] -0.42 [-1.25, 0.42] 5.78e-01
#> Paired: (A4 - A3)   2.54  7  .039 *    3.00 [ 0.21, 5.79]  0.90 [ 0.06, 1.73] 2.31e+00
#> ──────────────────────────────────────────────────────────────────────────────────────
#> 


if (FALSE) { # \dontrun{

  ## Illustration for the issue stated in "Details"

  # Inconsistency in the 95% CI of Cohen's d between R packages:
  # In this example, the true point estimate of Cohen's d = 3.00
  # and its 95% CI should be equal to 95% CI of mean difference.

  data = data.frame(X=rep(1:2, each=3), Y=1:6)
  data  # simple demo data

  TTEST(data, y="Y", x="X")
  # d = 3.00 [0.73, 5.27] (estimated based on 95% CI of mean difference)

  MANOVA(data, dv="Y", between="X") %>%
    EMMEANS("X")
  # d = 3.00 [0.73, 5.27] (the same as TTEST)

  psych::cohen.d(x=data, group="X")
  # d = 3.67 [0.04, 7.35] (strange)

  psych::d.ci(d=3.00, n1=3, n2=3)
  # d = 3.00 [-0.15, 6.12] (significance inconsistent with t-test)

  # jamovi uses psych::d.ci() to compute 95% CI
  # so its results are also: 3.00 [-0.15, 6.12]

  effectsize::cohens_d(Y ~ rev(X), data=data)
  # d = 3.00 [0.38, 5.50] (using the noncentrality parameter method)

  effectsize::t_to_d(t=t.test(Y ~ rev(X), data=data, var.equal=TRUE)$statistic,
                     df_error=4)
  # d = 3.67 [0.47, 6.74] (merely an approximate estimate, often overestimated)
  # see ?effectsize::t_to_d

  # https://www.psychometrica.de/effect_size.html
  # d = 3.00 [0.67, 5.33] (slightly different from TTEST)

  # https://www.campbellcollaboration.org/escalc/
  # d = 3.00 [0.67, 5.33] (slightly different from TTEST)

  # Conclusion:
  # TTEST() provides a reasonable estimate of Cohen's d and its 95% CI,
  # and effectsize::cohens_d() offers another method to compute the CI.
} # }
```
