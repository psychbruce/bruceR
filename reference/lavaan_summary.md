# Tidy report of lavaan model.

Tidy report of lavaan model.

## Usage

``` r
lavaan_summary(
  lavaan,
  ci = c("raw", "boot", "bc.boot", "bca.boot"),
  nsim = 100,
  seed = NULL,
  digits = 3,
  print = TRUE,
  covariance = FALSE,
  file = NULL
)
```

## Arguments

- lavaan:

  Model object fitted by
  [`lavaan`](https://rdrr.io/pkg/lavaan/man/lavaan-class.html).

- ci:

  Method for estimating standard error (SE) and 95% confidence interval
  (CI).

  Defaults to `"raw"` (the standard approach of `lavaan`). Other
  options:

  `"boot"`

  :   Percentile Bootstrap

  `"bc.boot"`

  :   Bias-Corrected Percentile Bootstrap

  `"bca.boot"`

  :   Bias-Corrected and Accelerated (BCa) Percentile Bootstrap

- nsim:

  Number of simulation samples (bootstrap resampling) for estimating SE
  and 95% CI. In formal analyses, **`nsim=1000` (or larger)** is
  strongly suggested.

- seed:

  Random seed for obtaining reproducible results. Defaults to `NULL`.

- digits:

  Number of decimal places of output. Defaults to `3`.

- print:

  Print results. Defaults to `TRUE`.

- covariance:

  Print (co)variances. Defaults to `FALSE`.

- file:

  File name of MS Word (`.doc`).

## Value

Invisibly return a list of results:

- `fit`:

  Model fit indices.

- `measure`:

  Latent variable measures.

- `regression`:

  Regression paths.

- `covariance`:

  Variances and/or covariances.

- `effect`:

  Defined effect estimates.

## See also

[`PROCESS`](https://psychbruce.github.io/bruceR/reference/PROCESS.md),
[`CFA`](https://psychbruce.github.io/bruceR/reference/CFA.md)

## Examples

``` r
## Simple Mediation:
## Solar.R (X) => Ozone (M) => Temp (Y)

# PROCESS(airquality, y="Temp", x="Solar.R",
#         meds="Ozone", ci="boot", nsim=1000, seed=1)

model = "
Ozone ~ a*Solar.R
Temp ~ c.*Solar.R + b*Ozone
Indirect := a*b
Direct := c.
Total := c. + a*b
"
lv = lavaan::sem(model=model, data=airquality)
lavaan::summary(lv, fit.measure=TRUE, ci=TRUE, nd=3)  # raw output
#> lavaan 0.6-20 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>                                                   Used       Total
#>   Number of observations                           111         153
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> Model Test Baseline Model:
#> 
#>   Test statistic                                89.294
#>   Degrees of freedom                                 3
#>   P-value                                        0.000
#> 
#> User Model versus Baseline Model:
#> 
#>   Comparative Fit Index (CFI)                    1.000
#>   Tucker-Lewis Index (TLI)                       1.000
#> 
#> Loglikelihood and Information Criteria:
#> 
#>   Loglikelihood user model (H0)               -908.632
#>   Loglikelihood unrestricted model (H1)       -908.632
#>                                                       
#>   Akaike (AIC)                                1827.265
#>   Bayesian (BIC)                              1840.812
#>   Sample-size adjusted Bayesian (SABIC)       1825.011
#> 
#> Root Mean Square Error of Approximation:
#> 
#>   RMSEA                                          0.000
#>   90 Percent confidence interval - lower         0.000
#>   90 Percent confidence interval - upper         0.000
#>   P-value H_0: RMSEA <= 0.050                       NA
#>   P-value H_0: RMSEA >= 0.080                       NA
#> 
#> Standardized Root Mean Square Residual:
#> 
#>   SRMR                                           0.000
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>   Ozone ~                                                               
#>     Solar.R    (a)    0.127    0.032    3.915    0.000    0.064    0.191
#>   Temp ~                                                                
#>     Solar.R   (c.)    0.006    0.008    0.800    0.424   -0.009    0.021
#>     Ozone      (b)    0.194    0.021    9.390    0.000    0.154    0.235
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>    .Ozone           964.164  129.421    7.450    0.000  710.504 1217.825
#>    .Temp             45.821    6.151    7.450    0.000   33.766   57.876
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>     Indirect          0.025    0.007    3.614    0.000    0.011    0.038
#>     Direct            0.006    0.008    0.800    0.424   -0.009    0.021
#>     Total             0.031    0.009    3.242    0.001    0.012    0.049
#> 
lavaan_summary(lv)
#> 
#> Fit Measures (lavaan):
#> χ²(0, N = 111) = 0.000, p = 1.000    
#> χ²/df = NaN (saturated model)
#> AIC = 1827.265 (Akaike Information Criterion)
#> BIC = 1840.812 (Bayesian Information Criterion)
#> CFI = 1.000 (Comparative Fit Index)
#> TLI = 1.000 (Tucker-Lewis Index; Non-Normed Fit Index, NNFI)
#> NFI = 1.000 (Normed Fit Index)
#> IFI = 1.000 (Incremental Fit Index)
#> GFI = 1.000 (Goodness-of-Fit Index)
#> AGFI = 1.000 (Adjusted Goodness-of-Fit Index)
#> RMSEA = 0.000, 90% CI [0.000, 0.000] (Root Mean Square Error of Approximation)
#> SRMR = 0.000 (Standardized Root Mean Square Residual)
#> 
#> Model Estimates (lavaan):
#> ───────────────────────────────────────────────────────────────────────────
#>                         Estimate    S.E.     z     p       LLCI  ULCI  Beta
#> ───────────────────────────────────────────────────────────────────────────
#> Regression Paths:                                                          
#>   Ozone <- Solar.R (a)     0.127 (0.032) 3.915 <.001 ***  0.064 0.191 0.348
#>   Temp <- Solar.R (c.)     0.006 (0.008) 0.800  .424     -0.009 0.021 0.058
#>   Temp <- Ozone (b)        0.194 (0.021) 9.390 <.001 ***  0.154 0.235 0.678
#> Defined Effects:                                                           
#>   Indirect                 0.025 (0.007) 3.614 <.001 ***  0.011 0.038 0.236
#>   Direct                   0.006 (0.008) 0.800  .424     -0.009 0.021 0.058
#>   Total                    0.031 (0.009) 3.242  .001 **   0.012 0.049 0.294
#> ───────────────────────────────────────────────────────────────────────────
#> Note. Raw (Standard) Confidence Interval (CI) and SE.
#> 
# lavaan_summary(lv, ci="boot", nsim=1000, seed=1)


## Serial Multiple Mediation:
## Solar.R (X) => Ozone (M1) => Wind(M2) => Temp (Y)

# PROCESS(airquality, y="Temp", x="Solar.R",
#         meds=c("Ozone", "Wind"),
#         med.type="serial", ci="boot", nsim=1000, seed=1)

model0 = "
Ozone ~ a1*Solar.R
Wind ~ a2*Solar.R + d12*Ozone
Temp ~ c.*Solar.R + b1*Ozone + b2*Wind
Indirect_All := a1*b1 + a2*b2 + a1*d12*b2
Ind_X_M1_Y := a1*b1
Ind_X_M2_Y := a2*b2
Ind_X_M1_M2_Y := a1*d12*b2
Direct := c.
Total := c. + a1*b1 + a2*b2 + a1*d12*b2
"
lv0 = lavaan::sem(model=model0, data=airquality)
lavaan::summary(lv0, fit.measure=TRUE, ci=TRUE, nd=3)  # raw output
#> lavaan 0.6-20 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         9
#> 
#>                                                   Used       Total
#>   Number of observations                           111         153
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> Model Test Baseline Model:
#> 
#>   Test statistic                               144.974
#>   Degrees of freedom                                 6
#>   P-value                                        0.000
#> 
#> User Model versus Baseline Model:
#> 
#>   Comparative Fit Index (CFI)                    1.000
#>   Tucker-Lewis Index (TLI)                       1.000
#> 
#> Loglikelihood and Information Criteria:
#> 
#>   Loglikelihood user model (H0)              -1178.664
#>   Loglikelihood unrestricted model (H1)      -1178.664
#>                                                       
#>   Akaike (AIC)                                2375.329
#>   Bayesian (BIC)                              2399.715
#>   Sample-size adjusted Bayesian (SABIC)       2371.273
#> 
#> Root Mean Square Error of Approximation:
#> 
#>   RMSEA                                          0.000
#>   90 Percent confidence interval - lower         0.000
#>   90 Percent confidence interval - upper         0.000
#>   P-value H_0: RMSEA <= 0.050                       NA
#>   P-value H_0: RMSEA >= 0.080                       NA
#> 
#> Standardized Root Mean Square Residual:
#> 
#>   SRMR                                           0.000
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>   Ozone ~                                                               
#>     Solar.R   (a1)    0.127    0.032    3.915    0.000    0.064    0.191
#>   Wind ~                                                                
#>     Solar.R   (a2)    0.004    0.003    1.234    0.217   -0.002    0.010
#>     Ozone    (d12)   -0.069    0.008   -8.134    0.000   -0.086   -0.052
#>   Temp ~                                                                
#>     Solar.R   (c.)    0.007    0.008    0.965    0.334   -0.007    0.022
#>     Ozone     (b1)    0.172    0.026    6.637    0.000    0.121    0.223
#>     Wind      (b2)   -0.323    0.229   -1.410    0.159   -0.772    0.126
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>    .Ozone           964.164  129.421    7.450    0.000  710.504 1217.825
#>    .Wind              7.732    1.038    7.450    0.000    5.698    9.766
#>    .Temp             45.014    6.042    7.450    0.000   33.172   56.857
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>     Indirect_All      0.023    0.007    3.378    0.001    0.010    0.037
#>     Ind_X_M1_Y        0.022    0.006    3.372    0.001    0.009    0.035
#>     Ind_X_M2_Y       -0.001    0.001   -0.928    0.353   -0.004    0.001
#>     Ind_X_M1_M2_Y     0.003    0.002    1.309    0.190   -0.001    0.007
#>     Direct            0.007    0.008    0.965    0.334   -0.007    0.022
#>     Total             0.031    0.009    3.242    0.001    0.012    0.049
#> 
lavaan_summary(lv0)
#> 
#> Fit Measures (lavaan):
#> χ²(0, N = 111) = 0.000, p = 1.000    
#> χ²/df = NaN (saturated model)
#> AIC = 2375.329 (Akaike Information Criterion)
#> BIC = 2399.715 (Bayesian Information Criterion)
#> CFI = 1.000 (Comparative Fit Index)
#> TLI = 1.000 (Tucker-Lewis Index; Non-Normed Fit Index, NNFI)
#> NFI = 1.000 (Normed Fit Index)
#> IFI = 1.000 (Incremental Fit Index)
#> GFI = 1.000 (Goodness-of-Fit Index)
#> AGFI = 1.000 (Adjusted Goodness-of-Fit Index)
#> RMSEA = 0.000, 90% CI [0.000, 0.000] (Root Mean Square Error of Approximation)
#> SRMR = 0.000 (Standardized Root Mean Square Residual)
#> 
#> Model Estimates (lavaan):
#> ───────────────────────────────────────────────────────────────────────────────
#>                          Estimate    S.E.      z     p       LLCI   ULCI   Beta
#> ───────────────────────────────────────────────────────────────────────────────
#> Regression Paths:                                                              
#>   Ozone <- Solar.R (a1)     0.127 (0.032)  3.915 <.001 ***  0.064  0.191  0.348
#>   Wind <- Solar.R (a2)      0.004 (0.003)  1.234  .217     -0.002  0.010  0.098
#>   Wind <- Ozone (d12)      -0.069 (0.008) -8.134 <.001 *** -0.086 -0.052 -0.647
#>   Temp <- Solar.R (c.)      0.007 (0.008)  0.965  .334     -0.007  0.022  0.070
#>   Temp <- Ozone (b1)        0.172 (0.026)  6.637 <.001 ***  0.121  0.223  0.600
#>   Temp <- Wind (b2)        -0.323 (0.229) -1.410  .159     -0.772  0.126 -0.121
#> Defined Effects:                                                               
#>   Indirect_All              0.023 (0.007)  3.378 <.001 ***  0.010  0.037  0.224
#>   Ind_X_M1_Y                0.022 (0.006)  3.372 <.001 ***  0.009  0.035  0.209
#>   Ind_X_M2_Y               -0.001 (0.001) -0.928  .353     -0.004  0.001 -0.012
#>   Ind_X_M1_M2_Y             0.003 (0.002)  1.309  .190     -0.001  0.007  0.027
#>   Direct                    0.007 (0.008)  0.965  .334     -0.007  0.022  0.070
#>   Total                     0.031 (0.009)  3.242  .001 **   0.012  0.049  0.294
#> ───────────────────────────────────────────────────────────────────────────────
#> Note. Raw (Standard) Confidence Interval (CI) and SE.
#> 
# lavaan_summary(lv0, ci="boot", nsim=1000, seed=1)

model1 = "
Ozone ~ a1*Solar.R
Wind ~ d12*Ozone
Temp ~ c.*Solar.R + b1*Ozone + b2*Wind
Indirect_All := a1*b1 + a1*d12*b2
Ind_X_M1_Y := a1*b1
Ind_X_M1_M2_Y := a1*d12*b2
Direct := c.
Total := c. + a1*b1 + a1*d12*b2
"
lv1 = lavaan::sem(model=model1, data=airquality)
lavaan::summary(lv1, fit.measure=TRUE, ci=TRUE, nd=3)  # raw output
#> lavaan 0.6-20 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         8
#> 
#>                                                   Used       Total
#>   Number of observations                           111         153
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 1.512
#>   Degrees of freedom                                 1
#>   P-value (Chi-square)                           0.219
#> 
#> Model Test Baseline Model:
#> 
#>   Test statistic                               144.974
#>   Degrees of freedom                                 6
#>   P-value                                        0.000
#> 
#> User Model versus Baseline Model:
#> 
#>   Comparative Fit Index (CFI)                    0.996
#>   Tucker-Lewis Index (TLI)                       0.978
#> 
#> Loglikelihood and Information Criteria:
#> 
#>   Loglikelihood user model (H0)              -1179.420
#>   Loglikelihood unrestricted model (H1)      -1178.664
#>                                                       
#>   Akaike (AIC)                                2374.840
#>   Bayesian (BIC)                              2396.517
#>   Sample-size adjusted Bayesian (SABIC)       2371.235
#> 
#> Root Mean Square Error of Approximation:
#> 
#>   RMSEA                                          0.068
#>   90 Percent confidence interval - lower         0.000
#>   90 Percent confidence interval - upper         0.273
#>   P-value H_0: RMSEA <= 0.050                    0.281
#>   P-value H_0: RMSEA >= 0.080                    0.631
#> 
#> Standardized Root Mean Square Residual:
#> 
#>   SRMR                                           0.028
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>   Ozone ~                                                               
#>     Solar.R   (a1)    0.127    0.032    3.915    0.000    0.064    0.191
#>   Wind ~                                                                
#>     Ozone    (d12)   -0.065    0.008   -8.164    0.000   -0.081   -0.050
#>   Temp ~                                                                
#>     Solar.R   (c.)    0.007    0.007    0.972    0.331   -0.007    0.022
#>     Ozone     (b1)    0.172    0.025    6.784    0.000    0.122    0.222
#>     Wind      (b2)   -0.323    0.227   -1.420    0.156   -0.769    0.123
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>    .Ozone           964.164  129.421    7.450    0.000  710.504 1217.825
#>    .Wind              7.838    1.052    7.450    0.000    5.776    9.900
#>    .Temp             45.014    6.042    7.450    0.000   33.172   56.857
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>     Indirect_All      0.025    0.007    3.611    0.000    0.011    0.038
#>     Ind_X_M1_Y        0.022    0.006    3.391    0.001    0.009    0.035
#>     Ind_X_M1_M2_Y     0.003    0.002    1.317    0.188   -0.001    0.007
#>     Direct            0.007    0.007    0.972    0.331   -0.007    0.022
#>     Total             0.032    0.009    3.380    0.001    0.013    0.050
#> 
lavaan_summary(lv1)
#> 
#> Fit Measures (lavaan):
#> χ²(1, N = 111) = 1.512, p = 0.219    
#> χ²/df = 1.512
#> AIC = 2374.840 (Akaike Information Criterion)
#> BIC = 2396.517 (Bayesian Information Criterion)
#> CFI = 0.996 (Comparative Fit Index)
#> TLI = 0.978 (Tucker-Lewis Index; Non-Normed Fit Index, NNFI)
#> NFI = 0.990 (Normed Fit Index)
#> IFI = 0.996 (Incremental Fit Index)
#> GFI = 0.991 (Goodness-of-Fit Index)
#> AGFI = 0.911 (Adjusted Goodness-of-Fit Index)
#> RMSEA = 0.068, 90% CI [0.000, 0.273] (Root Mean Square Error of Approximation)
#> SRMR = 0.028 (Standardized Root Mean Square Residual)
#> 
#> Model Estimates (lavaan):
#> ───────────────────────────────────────────────────────────────────────────────
#>                          Estimate    S.E.      z     p       LLCI   ULCI   Beta
#> ───────────────────────────────────────────────────────────────────────────────
#> Regression Paths:                                                              
#>   Ozone <- Solar.R (a1)     0.127 (0.032)  3.915 <.001 ***  0.064  0.191  0.348
#>   Wind <- Ozone (d12)      -0.065 (0.008) -8.164 <.001 *** -0.081 -0.050 -0.612
#>   Temp <- Solar.R (c.)      0.007 (0.007)  0.972  .331     -0.007  0.022  0.070
#>   Temp <- Ozone (b1)        0.172 (0.025)  6.784 <.001 ***  0.122  0.222  0.600
#>   Temp <- Wind (b2)        -0.323 (0.227) -1.420  .156     -0.769  0.123 -0.120
#> Defined Effects:                                                               
#>   Indirect_All              0.025 (0.007)  3.611 <.001 ***  0.011  0.038  0.235
#>   Ind_X_M1_Y                0.022 (0.006)  3.391 <.001 ***  0.009  0.035  0.209
#>   Ind_X_M1_M2_Y             0.003 (0.002)  1.317  .188     -0.001  0.007  0.026
#>   Direct                    0.007 (0.007)  0.972  .331     -0.007  0.022  0.070
#>   Total                     0.032 (0.009)  3.380 <.001 ***  0.013  0.050  0.304
#> ───────────────────────────────────────────────────────────────────────────────
#> Note. Raw (Standard) Confidence Interval (CI) and SE.
#> 
# lavaan_summary(lv1, ci="boot", nsim=1000, seed=1)
```
