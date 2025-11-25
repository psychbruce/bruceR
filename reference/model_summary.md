# Tidy report of regression models.

Tidy report of regression models (most model types are supported). This
function uses:

- [`texreg::screenreg()`](https://rdrr.io/pkg/texreg/man/screenreg.html)

- [`texreg::htmlreg()`](https://rdrr.io/pkg/texreg/man/htmlreg.html)

- [`MuMIn::std.coef()`](https://rdrr.io/pkg/MuMIn/man/std.coef.html)

- [`MuMIn::r.squaredGLMM()`](https://rdrr.io/pkg/MuMIn/man/r.squaredGLMM.html)

- [`performance::r2_mcfadden()`](https://easystats.github.io/performance/reference/r2_mcfadden.html)

- [`performance::r2_nagelkerke()`](https://easystats.github.io/performance/reference/r2_nagelkerke.html)

## Usage

``` r
model_summary(
  model.list,
  std = FALSE,
  digits = 3,
  file = NULL,
  check = TRUE,
  zero = ifelse(std, FALSE, TRUE),
  modify.se = NULL,
  modify.head = NULL,
  line = TRUE,
  bold = 0,
  ...
)
```

## Arguments

- model.list:

  A single model or a list of (various types of) models. Most types of
  regression models are supported!

- std:

  Standardized coefficients? Defaults to `FALSE`. Only applicable to
  linear models and linear mixed models. Not applicable to generalized
  linear (mixed) models.

- digits:

  Number of decimal places of output. Defaults to `3`.

- file:

  File name of MS Word (`".doc"`).

- check:

  If there is only one model in `model.list`, it checks for
  multicollinearity using
  [`performance::check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.html).
  You may turn it off by setting `check=FALSE`.

- zero:

  Display "0" before "."? Defaults to `TRUE`.

- modify.se:

  Modify standard errors. Useful if you need to change raw SEs to robust
  SEs. New SEs should be provided as a list of numeric vectors. See
  usage in
  [`texreg::screenreg()`](https://rdrr.io/pkg/texreg/man/screenreg.html).

- modify.head:

  Modify model names.

- line:

  Lines look like true line (`TRUE`) or `=== --- ===` (`FALSE`). Only
  effective in R Console output.

- bold:

  The *p*-value threshold below which the coefficients will be formatted
  in bold.

- ...:

  Arguments passed on to
  [`texreg::screenreg()`](https://rdrr.io/pkg/texreg/man/screenreg.html)
  or [`texreg::htmlreg()`](https://rdrr.io/pkg/texreg/man/htmlreg.html).

## Value

Invisibly return the output (character string).

## See also

[`print_table()`](https://psychbruce.github.io/bruceR/reference/print_table.md)
(print simple table)

[`GLM_summary()`](https://psychbruce.github.io/bruceR/reference/GLM_summary.md)

[`HLM_summary()`](https://psychbruce.github.io/bruceR/reference/HLM_summary.md)

[`med_summary()`](https://psychbruce.github.io/bruceR/reference/med_summary.md)

[`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md)

[`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)

## Examples

``` r
#### Example 1: Linear Model ####
lm1 = lm(Temp ~ Month + Day, data=airquality)
lm2 = lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
model_summary(lm1)
#> 
#> Model Summary
#> 
#> ────────────────────────
#>              (1) Temp   
#> ────────────────────────
#> (Intercept)   60.406 ***
#>               (3.718)   
#> Month          2.806 ***
#>               (0.490)   
#> Day           -0.136    
#>               (0.078)   
#> ────────────────────────
#> R^2            0.193    
#> Adj. R^2       0.183    
#> Num. obs.    153        
#> ────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
#> # Check for Multicollinearity
#> 
#> Low Correlation
#> 
#>   Term  VIF  VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#>  Month 1.00 [1.00, Inf]     1.00      1.00     [0.00, 1.00]
#>    Day 1.00 [1.00, Inf]     1.00      1.00     [0.00, 1.00]
#> 
model_summary(lm2)
#> 
#> Model Summary
#> 
#> ────────────────────────
#>              (1) Temp   
#> ────────────────────────
#> (Intercept)   68.770 ***
#>               (4.391)   
#> Month          2.225 ***
#>               (0.441)   
#> Day           -0.084    
#>               (0.070)   
#> Wind          -1.003 ***
#>               (0.176)   
#> Solar.R        0.027 ***
#>               (0.007)   
#> ────────────────────────
#> R^2            0.387    
#> Adj. R^2       0.369    
#> Num. obs.    146        
#> ────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
#> # Check for Multicollinearity
#> 
#> Low Correlation
#> 
#>     Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#>    Month 1.03 [1.00,  4.91]     1.02      0.97     [0.20, 1.00]
#>      Day 1.02 [1.00, 22.58]     1.01      0.98     [0.04, 1.00]
#>     Wind 1.03 [1.00,  6.15]     1.02      0.97     [0.16, 1.00]
#>  Solar.R 1.03 [1.00,  5.32]     1.02      0.97     [0.19, 1.00]
#> 
model_summary(list(lm1, lm2))
#> 
#> Model Summary
#> 
#> ─────────────────────────────────────
#>              (1) Temp     (2) Temp   
#> ─────────────────────────────────────
#> (Intercept)   60.406 ***   68.770 ***
#>               (3.718)      (4.391)   
#> Month          2.806 ***    2.225 ***
#>               (0.490)      (0.441)   
#> Day           -0.136       -0.084    
#>               (0.078)      (0.070)   
#> Wind                       -1.003 ***
#>                            (0.176)   
#> Solar.R                     0.027 ***
#>                            (0.007)   
#> ─────────────────────────────────────
#> R^2            0.193        0.387    
#> Adj. R^2       0.183        0.369    
#> Num. obs.    153          146        
#> ─────────────────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
model_summary(list(lm1, lm2), std=TRUE, digits=2)
#> 
#> Model Summary
#> 
#> ─────────────────────────────────
#>            (1) Temp    (2) Temp  
#> ─────────────────────────────────
#> Month         .42 ***     .34 ***
#>              (.07)       (.07)   
#> Day          -.13        -.08    
#>              (.07)       (.07)   
#> Wind                     -.38 ***
#>                          (.07)   
#> Solar.R                   .27 ***
#>                          (.07)   
#> ─────────────────────────────────
#> R^2           .19         .39    
#> Adj. R^2      .18         .37    
#> Num. obs.  153         146       
#> ─────────────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
model_summary(list(lm1, lm2), file="OLS Models.doc")
#> ✔ Table saved to '/home/runner/work/bruceR/bruceR/docs/reference/OLS Models.doc'
#> 
unlink("OLS Models.doc")  # delete file for code check

#### Example 2: Generalized Linear Model ####
glm1 = glm(case ~ age + parity,
           data=infert, family=binomial)
glm2 = glm(case ~ age + parity + education + spontaneous + induced,
           data=infert, family=binomial)
model_summary(list(glm1, glm2))  # "std" is not applicable to glm
#> 
#> Model Summary
#> 
#> ───────────────────────────────────────
#>                   (1) case  (2) case   
#> ───────────────────────────────────────
#> (Intercept)        -0.754    -1.149    
#>                    (0.836)   (1.412)   
#> age                 0.001     0.040    
#>                    (0.026)   (0.031)   
#> parity              0.015    -0.828 ***
#>                    (0.108)   (0.196)   
#> education6-11yrs             -1.044    
#>                              (0.793)   
#> education12+ yrs             -1.403    
#>                              (0.834)   
#> spontaneous                   2.046 ***
#>                              (0.310)   
#> induced                       1.289 ***
#>                              (0.301)   
#> ───────────────────────────────────────
#> McFadden's R^2      0.000     0.185    
#> Nagelkerke's R^2    0.000     0.291    
#> AIC               322.150   271.798    
#> BIC               332.690   296.392    
#> Num. obs.         248       248        
#> ───────────────────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
model_summary(list(glm1, glm2), file="GLM Models.doc")
#> ✔ Table saved to '/home/runner/work/bruceR/bruceR/docs/reference/GLM Models.doc'
#> 
unlink("GLM Models.doc")  # delete file for code check

#### Example 3: Linear Mixed Model ####
library(lmerTest)
hlm1 = lmer(Reaction ~ (1 | Subject), data=sleepstudy)
hlm2 = lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
hlm3 = lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
model_summary(list(hlm1, hlm2, hlm3))
#> 
#> Model Summary
#> 
#> ───────────────────────────────────────────────────────────────────────
#>                                (1) Reaction  (2) Reaction  (3) Reaction
#> ───────────────────────────────────────────────────────────────────────
#> (Intercept)                     298.508 ***   251.405 ***   251.405 ***
#>                                  (9.050)       (9.747)       (6.825)   
#> Days                                           10.467 ***    10.467 ***
#>                                                (0.804)       (1.546)   
#> ───────────────────────────────────────────────────────────────────────
#> Marginal R^2                      0.000         0.280         0.279    
#> Conditional R^2                   0.395         0.704         0.799    
#> AIC                            1910.327      1794.465      1755.628    
#> BIC                            1919.905      1807.237      1774.786    
#> Num. obs.                       180           180           180        
#> Num. groups: Subject             18            18            18        
#> Var: Subject (Intercept)       1278.338      1378.179       612.100    
#> Var: Residual                  1958.865       960.457       654.940    
#> Var: Subject Days                                            35.072    
#> Cov: Subject (Intercept) Days                                 9.604    
#> ───────────────────────────────────────────────────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
model_summary(list(hlm1, hlm2, hlm3), std=TRUE)
#> 
#> Model Summary
#> 
#> ───────────────────────────────────────────────────────────────────────
#>                                (1) Reaction  (2) Reaction  (3) Reaction
#> ───────────────────────────────────────────────────────────────────────
#> Days                                             .535 ***      .535 ***
#>                                                 (.041)        (.079)   
#> ───────────────────────────────────────────────────────────────────────
#> Marginal R^2                       .000          .280          .279    
#> Conditional R^2                    .395          .704          .799    
#> AIC                            1910.327      1794.465      1755.628    
#> BIC                            1919.905      1807.237      1774.786    
#> Num. obs.                       180           180           180        
#> Num. groups: Subject             18            18            18        
#> Var: Subject (Intercept)       1278.338      1378.179       612.100    
#> Var: Residual                  1958.865       960.457       654.940    
#> Var: Subject Days                                            35.072    
#> Cov: Subject (Intercept) Days                                 9.604    
#> ───────────────────────────────────────────────────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
model_summary(list(hlm1, hlm2, hlm3), file="HLM Models.doc")
#> ✔ Table saved to '/home/runner/work/bruceR/bruceR/docs/reference/HLM Models.doc'
#> 
unlink("HLM Models.doc")  # delete file for code check

#### Example 4: Generalized Linear Mixed Model ####
library(lmerTest)
data.glmm = MASS::bacteria
glmm1 = glmer(y ~ trt + week + (1 | ID), data=data.glmm, family=binomial)
glmm2 = glmer(y ~ trt + week + hilo + (1 | ID), data=data.glmm, family=binomial)
model_summary(list(glmm1, glmm2))  # "std" is not applicable to glmm
#> 
#> Model Summary
#> 
#> ─────────────────────────────────────────────
#>                      (1) y        (2) y      
#> ─────────────────────────────────────────────
#> (Intercept)            3.144 ***    3.399 ***
#>                       (0.622)      (0.736)   
#> trtdrug               -1.320 *     -0.928    
#>                       (0.642)      (0.802)   
#> trtdrug+              -0.795       -1.061    
#>                       (0.652)      (0.751)   
#> week                  -0.144 **    -0.144 ** 
#>                       (0.051)      (0.051)   
#> hilolo                             -0.656    
#>                                    (0.876)   
#> ─────────────────────────────────────────────
#> AIC                  207.771      209.217    
#> BIC                  224.739      229.579    
#> Num. obs.            220          220        
#> Num. groups: ID       50           50        
#> Var: ID (Intercept)    1.314        1.254    
#> ─────────────────────────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
model_summary(list(glmm1, glmm2), file="GLMM Models.doc")
#> ✔ Table saved to '/home/runner/work/bruceR/bruceR/docs/reference/GLMM Models.doc'
#> 
unlink("GLMM Models.doc")  # delete file for code check

#### Example 5: Multinomial Logistic Model ####
library(nnet)
d = airquality
d$Month = as.factor(d$Month)  # Factor levels: 5, 6, 7, 8, 9
mn1 = multinom(Month ~ Temp, data=d, Hess=TRUE)
#> # weights:  15 (8 variable)
#> initial  value 246.244001 
#> iter  10 value 196.357608
#> final  value 194.725454 
#> converged
mn2 = multinom(Month ~ Temp + Wind + Ozone, data=d, Hess=TRUE)
#> # weights:  25 (16 variable)
#> initial  value 186.694798 
#> iter  10 value 168.210554
#> iter  20 value 139.276111
#> iter  30 value 137.478576
#> final  value 137.466340 
#> converged
model_summary(mn1)
#> 
#> Model Summary
#> 
#> ───────────────────────────────────────────────────────────────
#>              6            7            8            9          
#> ───────────────────────────────────────────────────────────────
#> (Intercept)  -20.275 ***  -29.489 ***  -29.629 ***  -16.484 ***
#>               (4.089)      (4.730)      (4.738)      (3.731)   
#> Temp           0.280 ***    0.393 ***    0.395 ***    0.231 ***
#>               (0.056)      (0.063)      (0.063)      (0.052)   
#> ───────────────────────────────────────────────────────────────
#> AIC          405.451      405.451      405.451      405.451    
#> BIC          429.694      429.694      429.694      429.694    
#> Num. obs.    153          153          153          153        
#> K              5            5            5            5        
#> ───────────────────────────────────────────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
#> Not enough model terms in the conditional part of the model to check for
#>   multicollinearity.
model_summary(mn2)
#> 
#> Model Summary
#> 
#> ───────────────────────────────────────────────────────────────
#>              6            7            8            9          
#> ───────────────────────────────────────────────────────────────
#> (Intercept)  -27.752 ***  -27.079 ***  -27.212 ***  -17.641 ***
#>               (7.316)      (5.944)      (5.948)      (4.772)   
#> Temp           0.346 ***    0.364 ***    0.363 ***    0.261 ***
#>               (0.094)      (0.076)      (0.076)      (0.063)   
#> Wind           0.223        0.027        0.040       -0.007    
#>               (0.147)      (0.138)      (0.137)      (0.120)   
#> Ozone         -0.039       -0.016       -0.014       -0.032    
#>               (0.027)      (0.016)      (0.015)      (0.017)   
#> ───────────────────────────────────────────────────────────────
#> AIC          306.933      306.933      306.933      306.933    
#> BIC          350.990      350.990      350.990      350.990    
#> Num. obs.    116          116          116          116        
#> K              5            5            5            5        
#> ───────────────────────────────────────────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
#> # Check for Multicollinearity
#> 
#> Low Correlation
#> 
#>   Term  VIF   VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#>   Temp 2.84 [2.24, 3.72]     1.68      0.35     [0.27, 0.45]
#>   Wind 1.95 [1.60, 2.52]     1.40      0.51     [0.40, 0.63]
#>  Ozone 2.36 [1.89, 3.06]     1.54      0.42     [0.33, 0.53]
#> 
model_summary(mn2, file="Multinomial Logistic Model.doc")
#> ✔ Table saved to '/home/runner/work/bruceR/bruceR/docs/reference/Multinomial Logistic Model.doc'
#> 
unlink("Multinomial Logistic Model.doc")  # delete file for code check
```
