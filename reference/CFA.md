# Confirmatory Factor Analysis (CFA).

An extension of
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

## Usage

``` r
CFA(
  data,
  model = "A =~ a[1:5]; B =~ b[c(1,3,5)]; C =~ c1 + c2 + c3",
  estimator = "ML",
  highorder = "",
  orthogonal = FALSE,
  missing = "listwise",
  digits = 3,
  file = NULL
)
```

## Arguments

- data:

  Data frame.

- model:

  Model formula. See examples.

- estimator:

  The estimator to be used (for details, see [lavaan
  options](https://rdrr.io/pkg/lavaan/man/lavOptions.html)).

  Defaults to `"ML"`. Can be one of the following:

  - `"ML"`: Maximum Likelihood (can be extended to `"MLM"`, `"MLMV"`,
    `"MLMVS"`, `"MLF"`, or `"MLR"` for robust standard errors and robust
    test statistics)

  - `"GLS"`: Generalized Least Squares

  - `"WLS"`: Weighted Least Squares

  - `"ULS"`: Unweighted Least Squares

  - `"DWLS"`: Diagonally Weighted Least Squares

  - `"DLS"`: Distributionally-weighted Least Squares

- highorder:

  High-order factor. Defaults to `""`.

- orthogonal:

  Defaults to `FALSE`. If `TRUE`, all covariances among latent variables
  are set to zero.

- missing:

  Defaults to `"listwise"`. Alternative is `"fiml"` ("Full Information
  Maximum Likelihood").

- digits:

  Number of decimal places of output. Defaults to `3`.

- file:

  File name of MS Word (`".doc"`).

## Value

A list of results returned by
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

## See also

[`Alpha()`](https://psychbruce.github.io/bruceR/reference/Alpha.md)

[`EFA()`](https://psychbruce.github.io/bruceR/reference/EFA.md)

[`lavaan_summary()`](https://psychbruce.github.io/bruceR/reference/lavaan_summary.md)

## Examples

``` r
data.cfa=lavaan::HolzingerSwineford1939
CFA(data.cfa, "Visual =~ x[1:3]; Textual =~ x[c(4,5,6)]; Speed =~ x7 + x8 + x9")
#> 
#> Model Syntax (lavaan):
#> Visual =~ x1 + x2 + x3
#> Textual =~ x4 + x5 + x6
#> Speed =~ x7 + x8 + x9
#> 
#> Fit Measures (lavaan):
#> χ²(24, N = 301) = 85.306, p = 9e-09 ***
#> χ²/df = 3.554
#> AIC = 7517.490 (Akaike Information Criterion)
#> BIC = 7595.339 (Bayesian Information Criterion)
#> CFI = 0.931 (Comparative Fit Index)
#> TLI = 0.896 (Tucker-Lewis Index; Non-Normed Fit Index, NNFI)
#> NFI = 0.907 (Normed Fit Index)
#> IFI = 0.931 (Incremental Fit Index)
#> GFI = 0.943 (Goodness-of-Fit Index)
#> AGFI = 0.894 (Adjusted Goodness-of-Fit Index)
#> RMSEA = 0.092, 90% CI [0.071, 0.114] (Root Mean Square Error of Approximation)
#> SRMR = 0.065 (Standardized Root Mean Square Residual)
#> 
#> Model Estimates (lavaan):
#> ──────────────────────────────────────────────────────────────────────
#>                    Estimate    S.E.      z     p      LLCI  ULCI  Beta
#> ──────────────────────────────────────────────────────────────────────
#> Latent Variables:                                                     
#>   Visual =~ x1        0.900 (0.081) 11.128 <.001 *** 0.741 1.058 0.772
#>   Visual =~ x2        0.498 (0.077)  6.429 <.001 *** 0.346 0.650 0.424
#>   Visual =~ x3        0.656 (0.074)  8.817 <.001 *** 0.510 0.802 0.581
#>   Textual =~ x4       0.990 (0.057) 17.474 <.001 *** 0.879 1.101 0.852
#>   Textual =~ x5       1.102 (0.063) 17.576 <.001 *** 0.979 1.224 0.855
#>   Textual =~ x6       0.917 (0.054) 17.082 <.001 *** 0.811 1.022 0.838
#>   Speed =~ x7         0.619 (0.070)  8.903 <.001 *** 0.483 0.756 0.570
#>   Speed =~ x8         0.731 (0.066) 11.090 <.001 *** 0.602 0.860 0.723
#>   Speed =~ x9         0.670 (0.065) 10.305 <.001 *** 0.543 0.797 0.665
#> ──────────────────────────────────────────────────────────────────────
#> Note. Raw (Standard) Confidence Interval (CI) and SE.
#> 
#> Estimator: ML
#> 
CFA(data.cfa, model="
    Visual =~ x[1:3]
    Textual =~ x[c(4,5,6)]
    Speed =~ x7 + x8 + x9
    ", highorder="Ability")
#> 
#> Model Syntax (lavaan):
#> Visual =~ x1 + x2 + x3
#> Textual =~ x4 + x5 + x6
#> Speed =~ x7 + x8 + x9
#> Ability =~ Visual + Textual + Speed
#> Ability ~~ Ability
#> 
#> Fit Measures (lavaan):
#> χ²(24, N = 301) = 85.306, p = 9e-09 ***
#> χ²/df = 3.554
#> AIC = 7517.490 (Akaike Information Criterion)
#> BIC = 7595.339 (Bayesian Information Criterion)
#> CFI = 0.931 (Comparative Fit Index)
#> TLI = 0.896 (Tucker-Lewis Index; Non-Normed Fit Index, NNFI)
#> NFI = 0.907 (Normed Fit Index)
#> IFI = 0.931 (Incremental Fit Index)
#> GFI = 0.943 (Goodness-of-Fit Index)
#> AGFI = 0.894 (Adjusted Goodness-of-Fit Index)
#> RMSEA = 0.092, 90% CI [0.071, 0.114] (Root Mean Square Error of Approximation)
#> SRMR = 0.065 (Standardized Root Mean Square Residual)
#> 
#> Model Estimates (lavaan):
#> ───────────────────────────────────────────────────────────────────────────
#>                        Estimate    S.E.      z     p       LLCI  ULCI  Beta
#> ───────────────────────────────────────────────────────────────────────────
#> Latent Variables:                                                          
#>   Visual =~ x1            0.439 (0.194)  2.257  .024 *    0.058 0.819 0.772
#>   Visual =~ x2            0.243 (0.108)  2.253  .024 *    0.032 0.454 0.424
#>   Visual =~ x3            0.320 (0.138)  2.326  .020 *    0.050 0.589 0.581
#>   Textual =~ x4           0.842 (0.064) 13.251 <.001 ***  0.718 0.967 0.852
#>   Textual =~ x5           0.937 (0.071) 13.293 <.001 ***  0.799 1.076 0.855
#>   Textual =~ x6           0.780 (0.060) 13.084 <.001 ***  0.663 0.897 0.838
#>   Speed =~ x7             0.522 (0.066)  7.908 <.001 ***  0.392 0.651 0.570
#>   Speed =~ x8             0.616 (0.067)  9.129 <.001 ***  0.484 0.748 0.723
#>   Speed =~ x9             0.564 (0.064)  8.808 <.001 ***  0.439 0.690 0.665
#>   Ability =~ Visual       1.791 (0.990)  1.809  .070 .   -0.149 3.732 0.873
#>   Ability =~ Textual      0.617 (0.129)  4.798 <.001 ***  0.365 0.869 0.525
#>   Ability =~ Speed        0.640 (0.143)  4.489 <.001 ***  0.360 0.919 0.539
#> ───────────────────────────────────────────────────────────────────────────
#> Note. Raw (Standard) Confidence Interval (CI) and SE.
#> 
#> Estimator: ML
#> 

data.bfi = na.omit(psych::bfi)
CFA(data.bfi, "E =~ E[1:5]; A =~ A[1:5]; C =~ C[1:5]; N =~ N[1:5]; O =~ O[1:5]")
#> 
#> Model Syntax (lavaan):
#> E =~ E1 + E2 + E3 + E4 + E5
#> A =~ A1 + A2 + A3 + A4 + A5
#> C =~ C1 + C2 + C3 + C4 + C5
#> N =~ N1 + N2 + N3 + N4 + N5
#> O =~ O1 + O2 + O3 + O4 + O5
#> 
#> Fit Measures (lavaan):
#> χ²(265, N = 2236) = 3843.296, p < 1e-99 ***
#> χ²/df = 14.503
#> AIC = 182698.556 (Akaike Information Criterion)
#> BIC = 183041.303 (Bayesian Information Criterion)
#> CFI = 0.780 (Comparative Fit Index)
#> TLI = 0.751 (Tucker-Lewis Index; Non-Normed Fit Index, NNFI)
#> NFI = 0.768 (Normed Fit Index)
#> IFI = 0.780 (Incremental Fit Index)
#> GFI = 0.862 (Goodness-of-Fit Index)
#> AGFI = 0.830 (Adjusted Goodness-of-Fit Index)
#> RMSEA = 0.078, 90% CI [0.076, 0.080] (Root Mean Square Error of Approximation)
#> SRMR = 0.076 (Standardized Root Mean Square Residual)
#> 
#> Model Estimates (lavaan):
#> ──────────────────────────────────────────────────────────────────────────
#>                    Estimate    S.E.       z     p       LLCI   ULCI   Beta
#> ──────────────────────────────────────────────────────────────────────────
#> Latent Variables:                                                         
#>   E =~ E1             0.907 (0.035)  26.094 <.001 ***  0.838  0.975  0.560
#>   E =~ E2             1.114 (0.033)  33.914 <.001 ***  1.049  1.178  0.694
#>   E =~ E3            -0.866 (0.028) -30.936 <.001 *** -0.920 -0.811 -0.645
#>   E =~ E4            -1.027 (0.030) -34.530 <.001 *** -1.085 -0.968 -0.704
#>   E =~ E5            -0.746 (0.029) -26.147 <.001 *** -0.802 -0.691 -0.561
#>   A =~ A1             0.460 (0.032)  14.328 <.001 ***  0.397  0.523  0.330
#>   A =~ A2            -0.734 (0.025) -29.758 <.001 *** -0.782 -0.685 -0.634
#>   A =~ A3            -0.955 (0.027) -35.954 <.001 *** -1.008 -0.903 -0.741
#>   A =~ A4            -0.728 (0.032) -22.664 <.001 *** -0.791 -0.665 -0.503
#>   A =~ A5            -0.852 (0.026) -32.282 <.001 *** -0.903 -0.800 -0.678
#>   C =~ C1             0.652 (0.028)  23.697 <.001 ***  0.598  0.706  0.536
#>   C =~ C2             0.758 (0.029)  25.826 <.001 ***  0.700  0.815  0.578
#>   C =~ C3             0.707 (0.029)  24.376 <.001 ***  0.650  0.764  0.550
#>   C =~ C4            -0.949 (0.030) -32.012 <.001 *** -1.008 -0.891 -0.697
#>   C =~ C5            -1.014 (0.036) -28.108 <.001 *** -1.084 -0.943 -0.622
#>   N =~ N1             1.284 (0.029)  43.743 <.001 ***  1.227  1.342  0.821
#>   N =~ N2             1.222 (0.029)  41.954 <.001 ***  1.165  1.279  0.796
#>   N =~ N3             1.152 (0.031)  36.823 <.001 ***  1.091  1.214  0.722
#>   N =~ N4             0.891 (0.033)  27.399 <.001 ***  0.827  0.955  0.571
#>   N =~ N5             0.825 (0.035)  23.897 <.001 ***  0.757  0.892  0.509
#>   O =~ O1             0.629 (0.027)  23.245 <.001 ***  0.576  0.682  0.562
#>   O =~ O2            -0.666 (0.038) -17.641 <.001 *** -0.740 -0.592 -0.431
#>   O =~ O3             0.861 (0.029)  29.360 <.001 ***  0.804  0.919  0.722
#>   O =~ O4             0.260 (0.029)   8.850 <.001 ***  0.203  0.318  0.221
#>   O =~ O5            -0.633 (0.032) -19.604 <.001 *** -0.696 -0.570 -0.476
#> ──────────────────────────────────────────────────────────────────────────
#> Note. Raw (Standard) Confidence Interval (CI) and SE.
#> 
#> Estimator: ML
#> 
```
