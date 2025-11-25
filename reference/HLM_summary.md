# Tidy report of HLM (`lmer` and `glmer` models).

NOTE:
[`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md)
is preferred.

## Usage

``` r
HLM_summary(model = NULL, test.rand = FALSE, digits = 3, ...)
```

## Arguments

- model:

  A model fitted with `lmer` or `glmer` function using the `lmerTest`
  package.

- test.rand:

  \[Only for `lmer` and `glmer`\] `TRUE` or `FALSE` (default). Test
  random effects (i.e., variance components) by using the
  likelihood-ratio test (LRT), which is asymptotically chi-square
  distributed. For large datasets, it is much time-consuming.

- digits:

  Number of decimal places of output. Defaults to `3`.

- ...:

  Other arguments. You may re-define `formula`, `data`, or `family`.

## Value

No return value.

## References

Hox, J. J. (2010). *Multilevel analysis: Techniques and applications*
(2nd ed.). New York, NY: Routledge.

Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
obtaining *R*^2 from generalized linear mixed-effects models. *Methods
in Ecology and Evolution, 4,* 133–142.

Xu, R. (2003). Measuring explained variation in linear mixed effects
models. *Statistics in Medicine, 22,* 3527–3541.

## See also

[`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md)
(print simple table)

[`model_summary()`](https://psychbruce.github.io/bruceR/reference/model_summary.md)
(strongly suggested)

[`GLM_summary()`](https://psychbruce.github.io/bruceR/reference/GLM_summary.md)

[`regress()`](https://psychbruce.github.io/bruceR/reference/regress.md)

## Examples

``` r
library(lmerTest)

## Example 1: data from lme4::sleepstudy
# (1) 'Subject' is a grouping/clustering variable
# (2) 'Days' is a level-1 predictor nested within 'Subject'
# (3) No level-2 predictors
m1 = lmer(Reaction ~ (1 | Subject), data=sleepstudy)
m2 = lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
m3 = lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
HLM_summary(m1)
#> 
#> Hierarchical Linear Model (HLM)
#> (also known as) Linear Mixed Model (LMM)
#> (also known as) Multilevel Linear Model (MLM)
#> 
#> Model Information:
#> Formula: Reaction ~ (1 | Subject)
#> Level-1 Observations: N = 180
#> Level-2 Groups/Clusters: Subject, 18
#> 
#> Model Fit:
#> AIC = 1910.327
#> BIC = 1919.905
#> R_(m)² = 0.00000  (Marginal R²: fixed effects)
#> R_(c)² = 0.39489  (Conditional R²: fixed + random effects)
#> Omega² = 0.43347  (= 1 - proportion of unexplained variance)
#> 
#> Fixed Effects:
#> Unstandardized Coefficients (b or γ):
#> Outcome Variable: Reaction
#> ────────────────────────────────────────────────────────────────────
#>                  b/γ    S.E.     t   df     p        [95% CI of b/γ]
#> ────────────────────────────────────────────────────────────────────
#> (Intercept)  298.508 (9.050) 32.98 17.0 <.001 *** [279.414, 317.602]
#> ────────────────────────────────────────────────────────────────────
#> 'df' is estimated by Satterthwaite approximation.
#> 
#> Random Effects:
#> ────────────────────────────────────────────
#>  Cluster  K   Parameter     Variance     ICC
#> ────────────────────────────────────────────
#>  Subject   18 (Intercept) 1278.33776 0.39489
#>  Residual                 1958.86518        
#> ────────────────────────────────────────────
#> 
HLM_summary(m2)
#> 
#> Hierarchical Linear Model (HLM)
#> (also known as) Linear Mixed Model (LMM)
#> (also known as) Multilevel Linear Model (MLM)
#> 
#> Model Information:
#> Formula: Reaction ~ Days + (1 | Subject)
#> Level-1 Observations: N = 180
#> Level-2 Groups/Clusters: Subject, 18
#> 
#> Model Fit:
#> AIC = 1794.465
#> BIC = 1807.237
#> R_(m)² = 0.27989  (Marginal R²: fixed effects)
#> R_(c)² = 0.70426  (Conditional R²: fixed + random effects)
#> Omega² = 0.72586  (= 1 - proportion of unexplained variance)
#> 
#> ANOVA Table:
#> ───────────────────────────────────────────────────────
#>          Sum Sq   Mean Sq NumDF  DenDF      F     p    
#> ───────────────────────────────────────────────────────
#> Days  162702.65 162702.65  1.00 161.00 169.40 <.001 ***
#> ───────────────────────────────────────────────────────
#> 
#> Fixed Effects:
#> Unstandardized Coefficients (b or γ):
#> Outcome Variable: Reaction
#> ─────────────────────────────────────────────────────────────────────
#>                  b/γ    S.E.     t    df     p        [95% CI of b/γ]
#> ─────────────────────────────────────────────────────────────────────
#> (Intercept)  251.405 (9.747) 25.79  22.8 <.001 *** [231.233, 271.577]
#> Days          10.467 (0.804) 13.02 161.0 <.001 *** [  8.879,  12.055]
#> ─────────────────────────────────────────────────────────────────────
#> 'df' is estimated by Satterthwaite approximation.
#> 
#> Standardized Coefficients (β):
#> Outcome Variable: Reaction
#> ────────────────────────────────────────────────────────
#>           β    S.E.     t    df     p      [95% CI of β]
#> ────────────────────────────────────────────────────────
#> Days  0.535 (0.041) 13.02 161.0 <.001 *** [0.454, 0.616]
#> ────────────────────────────────────────────────────────
#> 
#> Random Effects:
#> ────────────────────────────────────────────
#>  Cluster  K   Parameter     Variance     ICC
#> ────────────────────────────────────────────
#>  Subject   18 (Intercept) 1378.17851 0.58931
#>  Residual                  960.45658        
#> ────────────────────────────────────────────
#> 
HLM_summary(m3)
#> 
#> Hierarchical Linear Model (HLM)
#> (also known as) Linear Mixed Model (LMM)
#> (also known as) Multilevel Linear Model (MLM)
#> 
#> Model Information:
#> Formula: Reaction ~ Days + (Days | Subject)
#> Level-1 Observations: N = 180
#> Level-2 Groups/Clusters: Subject, 18
#> 
#> Model Fit:
#> AIC = 1755.628
#> BIC = 1774.786
#> R_(m)² = 0.27865  (Marginal R²: fixed effects)
#> R_(c)² = 0.79922  (Conditional R²: fixed + random effects)
#> Omega² = 0.82590  (= 1 - proportion of unexplained variance)
#> 
#> ANOVA Table:
#> ───────────────────────────────────────────────────
#>         Sum Sq  Mean Sq NumDF DenDF     F     p    
#> ───────────────────────────────────────────────────
#> Days  30030.94 30030.94  1.00 17.00 45.85 <.001 ***
#> ───────────────────────────────────────────────────
#> 
#> Fixed Effects:
#> Unstandardized Coefficients (b or γ):
#> Outcome Variable: Reaction
#> ────────────────────────────────────────────────────────────────────
#>                  b/γ    S.E.     t   df     p        [95% CI of b/γ]
#> ────────────────────────────────────────────────────────────────────
#> (Intercept)  251.405 (6.825) 36.84 17.0 <.001 *** [237.006, 265.804]
#> Days          10.467 (1.546)  6.77 17.0 <.001 *** [  7.206,  13.729]
#> ────────────────────────────────────────────────────────────────────
#> 'df' is estimated by Satterthwaite approximation.
#> 
#> Standardized Coefficients (β):
#> Outcome Variable: Reaction
#> ──────────────────────────────────────────────────────
#>           β    S.E.    t   df     p      [95% CI of β]
#> ──────────────────────────────────────────────────────
#> Days  0.535 (0.079) 6.77 17.0 <.001 *** [0.368, 0.702]
#> ──────────────────────────────────────────────────────
#> 
#> Random Effects:
#> ───────────────────────────────────────────
#>  Cluster  K   Parameter    Variance     ICC
#> ───────────────────────────────────────────
#>  Subject   18 (Intercept) 612.10016 0.48309
#>               Days         35.07171        
#>  Residual                 654.94001        
#> ───────────────────────────────────────────
#> 

## Example 2: data from lmerTest::carrots
# (1) 'Consumer' is a grouping/clustering variable
# (2) 'Sweetness' is a level-1 predictor
# (3) 'Age' and 'Frequency' are level-2 predictors
hlm.1 = lmer(Preference ~ Sweetness + Age + Frequency +
               (1 | Consumer), data=carrots)
hlm.2 = lmer(Preference ~ Sweetness + Age + Frequency +
               (Sweetness | Consumer) + (1 | Product), data=carrots)
HLM_summary(hlm.1)
#> 
#> Hierarchical Linear Model (HLM)
#> (also known as) Linear Mixed Model (LMM)
#> (also known as) Multilevel Linear Model (MLM)
#> 
#> Model Information:
#> Formula: Preference ~ Sweetness + Age + Frequency + (1 | Consumer)
#> Level-1 Observations: N = 1230
#> Level-2 Groups/Clusters: Consumer, 103
#> 
#> Model Fit:
#> AIC = 3328.442
#> BIC = 3384.705
#> R_(m)² = 0.32908  (Marginal R²: fixed effects)
#> R_(c)² = 0.52156  (Conditional R²: fixed + random effects)
#> Omega² = 0.50163  (= 1 - proportion of unexplained variance)
#> 
#> ANOVA Table:
#> ────────────────────────────────────────────────────────
#>            Sum Sq Mean Sq NumDF   DenDF      F     p    
#> ────────────────────────────────────────────────────────
#> Sweetness  487.13  487.13  1.00 1208.87 660.42 <.001 ***
#> Age          4.11    1.37  3.00   93.39   1.86  .142    
#> Frequency    1.80    0.45  4.00   93.75   0.61  .655    
#> ────────────────────────────────────────────────────────
#> 
#> Fixed Effects:
#> Unstandardized Coefficients (b or γ):
#> Outcome Variable: Preference
#> ──────────────────────────────────────────────────────────────────
#>                 b/γ    S.E.     t     df     p     [95% CI of b/γ]
#> ──────────────────────────────────────────────────────────────────
#> (Intercept)   2.626 (0.220) 11.93  119.9 <.001 *** [ 2.190, 3.061]
#> Sweetness     0.508 (0.020) 25.70 1208.9 <.001 *** [ 0.469, 0.547]
#> Age2          0.395 (0.249)  1.58   93.6  .117     [-0.100, 0.890]
#> Age3          0.465 (0.214)  2.17   93.7  .033 *   [ 0.039, 0.891]
#> Age4          0.531 (0.234)  2.27   93.6  .026 *   [ 0.066, 0.996]
#> Frequency2    0.149 (0.162)  0.92   93.4  .360     [-0.172, 0.469]
#> Frequency3    0.101 (0.217)  0.47   93.6  .642     [-0.330, 0.532]
#> Frequency4   -0.501 (0.444) -1.13   93.2  .262     [-1.382, 0.380]
#> Frequency5   -0.027 (0.376) -0.07   94.7  .943     [-0.774, 0.720]
#> ──────────────────────────────────────────────────────────────────
#> 'df' is estimated by Satterthwaite approximation.
#> 
#> Standardized Coefficients (β):
#> Outcome Variable: Preference
#> ─────────────────────────────────────────────────────────────────
#>                  β    S.E.     t     df     p       [95% CI of β]
#> ─────────────────────────────────────────────────────────────────
#> Sweetness    0.609 (0.024) 25.70 1208.9 <.001 *** [ 0.562, 0.655]
#> Age2         0.122 (0.077)  1.58   93.6  .117     [-0.031, 0.275]
#> Age3         0.198 (0.091)  2.17   93.7  .033 *   [ 0.017, 0.380]
#> Age4         0.197 (0.087)  2.27   93.6  .026 *   [ 0.025, 0.369]
#> Frequency2   0.047 (0.051)  0.92   93.4  .360     [-0.055, 0.149]
#> Frequency3   0.024 (0.052)  0.47   93.6  .642     [-0.080, 0.129]
#> Frequency4  -0.059 (0.052) -1.13   93.2  .262     [-0.163, 0.045]
#> Frequency5  -0.004 (0.053) -0.07   94.7  .943     [-0.108, 0.101]
#> ─────────────────────────────────────────────────────────────────
#> 
#> Random Effects:
#> ──────────────────────────────────────────
#>  Cluster  K   Parameter   Variance     ICC
#> ──────────────────────────────────────────
#>  Consumer 103 (Intercept)  0.29673 0.28688
#>  Residual                  0.73761        
#> ──────────────────────────────────────────
#> 
HLM_summary(hlm.2)
#> 
#> Hierarchical Linear Model (HLM)
#> (also known as) Linear Mixed Model (LMM)
#> (also known as) Multilevel Linear Model (MLM)
#> 
#> Model Information:
#> Formula: Preference ~ Sweetness + Age + Frequency + (Sweetness | Consumer) + (1 | Product)
#> Level-1 Observations: N = 1230
#> Level-2 Groups/Clusters: Consumer, 103; Product, 12
#> 
#> Model Fit:
#> AIC = 3281.886
#> BIC = 3353.493
#> R_(m)² = 0.31128  (Marginal R²: fixed effects)
#> R_(c)² = 0.55254  (Conditional R²: fixed + random effects)
#> Omega² = 0.57170  (= 1 - proportion of unexplained variance)
#> 
#> ANOVA Table:
#> ───────────────────────────────────────────────────────
#>            Sum Sq Mean Sq NumDF  DenDF      F     p    
#> ───────────────────────────────────────────────────────
#> Sweetness  184.22  184.22  1.00 103.72 277.74 <.001 ***
#> Age          2.51    0.84  3.00  83.12   1.26  .293    
#> Frequency    2.43    0.61  4.00  80.03   0.92  .458    
#> ───────────────────────────────────────────────────────
#> 
#> Fixed Effects:
#> Unstandardized Coefficients (b or γ):
#> Outcome Variable: Preference
#> ─────────────────────────────────────────────────────────────────
#>                 b/γ    S.E.     t    df     p     [95% CI of b/γ]
#> ─────────────────────────────────────────────────────────────────
#> (Intercept)   2.817 (0.221) 12.76 149.7 <.001 *** [ 2.381, 3.253]
#> Sweetness     0.485 (0.029) 16.67 103.7 <.001 *** [ 0.428, 0.543]
#> Age2          0.212 (0.222)  0.96  78.6  .341     [-0.229, 0.654]
#> Age3          0.297 (0.189)  1.57  76.3  .120     [-0.080, 0.674]
#> Age4          0.388 (0.207)  1.87  77.6  .065 .   [-0.025, 0.801]
#> Frequency2    0.181 (0.150)  1.21  92.9  .230     [-0.117, 0.479]
#> Frequency3    0.095 (0.203)  0.47  96.4  .639     [-0.307, 0.497]
#> Frequency4   -0.516 (0.383) -1.35  68.4  .183     [-1.281, 0.249]
#> Frequency5   -0.055 (0.327) -0.17  70.4  .867     [-0.707, 0.597]
#> ─────────────────────────────────────────────────────────────────
#> 'df' is estimated by Satterthwaite approximation.
#> 
#> Standardized Coefficients (β):
#> Outcome Variable: Preference
#> ────────────────────────────────────────────────────────────────
#>                  β    S.E.     t    df     p       [95% CI of β]
#> ────────────────────────────────────────────────────────────────
#> Sweetness    0.581 (0.035) 16.67 103.7 <.001 *** [ 0.512, 0.650]
#> Age2         0.066 (0.069)  0.96  78.6  .341     [-0.071, 0.202]
#> Age3         0.127 (0.081)  1.57  76.3  .120     [-0.034, 0.287]
#> Age4         0.144 (0.077)  1.87  77.6  .065 .   [-0.009, 0.297]
#> Frequency2   0.058 (0.048)  1.21  92.9  .230     [-0.037, 0.152]
#> Frequency3   0.023 (0.049)  0.47  96.4  .639     [-0.074, 0.120]
#> Frequency4  -0.061 (0.045) -1.35  68.4  .183     [-0.151, 0.029]
#> Frequency5  -0.008 (0.046) -0.17  70.4  .867     [-0.099, 0.083]
#> ────────────────────────────────────────────────────────────────
#> 
#> Random Effects:
#> ──────────────────────────────────────────
#>  Cluster  K   Parameter   Variance     ICC
#> ──────────────────────────────────────────
#>  Consumer 103 (Intercept)  1.11977 0.62512
#>               Sweetness    0.04438        
#>  Product   12 (Intercept)  0.00825 0.00460
#>  Residual                  0.66328        
#> ──────────────────────────────────────────
#> 
```
