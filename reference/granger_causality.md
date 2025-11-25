# Granger causality test (multivariate).

Granger test of predictive causality (between multivariate time series)
based on vector autoregression model using
[`vars::VAR()`](https://rdrr.io/pkg/vars/man/VAR.html). Its output
resembles the output of the `vargranger` command in Stata (but here
using an \\F\\ test).

## Usage

``` r
granger_causality(
  varmodel,
  var.y = NULL,
  var.x = NULL,
  test = c("F", "Chisq"),
  file = NULL,
  check.dropped = FALSE
)
```

## Arguments

- varmodel:

  VAR model fitted using
  [`vars::VAR()`](https://rdrr.io/pkg/vars/man/VAR.html).

- var.y, var.x:

  \[Optional\] Defaults to `NULL` (all variables). If specified, then
  perform tests for specific variables. Values can be a single variable
  (e.g., `"X"`), a vector of variables (e.g., `c("X1", "X2")`), or a
  string containing regular expression (e.g., `"X1|X2"`).

- test:

  \\F\\ test and/or Wald \\\chi^2\\ test. Defaults to both:
  `c("F", "Chisq")`.

- file:

  File name of MS Word (`".doc"`).

- check.dropped:

  Check dropped variables. Defaults to `FALSE`.

## Value

A data frame of results.

## Details

Granger causality test (based on VAR model) examines whether the lagged
values of a predictor (or predictors) help to predict an outcome when
controlling for the lagged values of the outcome itself. Granger
causality does not represent a true causal effect.

## See also

[`ccf_plot()`](https://psychbruce.github.io/bruceR/reference/ccf_plot.md)

[`granger_test()`](https://psychbruce.github.io/bruceR/reference/granger_test.md)

## Examples

``` r
# R package "vars" should be installed
library(vars)
#> Loading required package: MASS
#> 
#> Attaching package: ‘MASS’
#> The following object is masked from ‘package:dplyr’:
#> 
#>     select
#> Loading required package: strucchange
#> Loading required package: zoo
#> 
#> Attaching package: ‘zoo’
#> The following objects are masked from ‘package:data.table’:
#> 
#>     yearmon, yearqtr
#> The following objects are masked from ‘package:base’:
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: sandwich
#> 
#> Attaching package: ‘strucchange’
#> The following object is masked from ‘package:stringr’:
#> 
#>     boundary
#> Loading required package: urca
#> Loading required package: lmtest
data(Canada)
VARselect(Canada)
#> $selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      3      2      1      3 
#> 
#> $criteria
#>                   1            2            3            4           5
#> AIC(n) -6.191599834 -6.621627919 -6.709002047 -6.512701777 -6.30174681
#> HQ(n)  -5.943189052 -6.174488511 -6.063134014 -5.668105118 -5.25842152
#> SC(n)  -5.568879538 -5.500731387 -5.089929279 -4.395452772 -3.68632157
#> FPE(n)  0.002048239  0.001337721  0.001237985  0.001534875  0.00195439
#>                   6            7            8            9           10
#> AIC(n) -6.194596715 -6.011720944 -6.054479536 -5.912126222 -5.867271844
#> HQ(n)  -4.952542805 -4.570938409 -4.414968375 -4.073886435 -3.830303432
#> SC(n)  -3.080995238 -2.399943231 -1.944525586 -1.303996035 -0.760965421
#> FPE(n)  0.002278812  0.002924622  0.003073249  0.004015164  0.004961704
#> 
vm = VAR(Canada, p=3)
model_summary(vm)
#> 
#> Model Summary
#> 
#> ────────────────────────────────────────────────────────────────
#>            e             prod          rw            U          
#> ────────────────────────────────────────────────────────────────
#> e.l1          1.753 ***    -0.149        -0.472       -0.618 ***
#>              (0.151)       (0.289)       (0.337)      (0.125)   
#> prod.l1       0.170 **      1.148 ***    -0.065       -0.098    
#>              (0.062)       (0.119)       (0.139)      (0.052)   
#> rw.l1        -0.083         0.024         0.909 ***    0.015    
#>              (0.053)       (0.101)       (0.118)      (0.044)   
#> U.l1          0.100        -0.658        -0.001        0.660 ***
#>              (0.197)       (0.379)       (0.442)      (0.164)   
#> e.l2         -1.184 ***    -0.182         0.667        0.518 ** 
#>              (0.235)       (0.451)       (0.526)      (0.195)   
#> prod.l2      -0.106        -0.196        -0.216        0.088    
#>              (0.094)       (0.181)       (0.211)      (0.078)   
#> rw.l2        -0.024        -0.203        -0.146        0.070    
#>              (0.070)       (0.133)       (0.156)      (0.058)   
#> U.l2         -0.051         0.822        -0.301       -0.081    
#>              (0.245)       (0.470)       (0.549)      (0.203)   
#> e.l3          0.587 ***     0.575        -0.129       -0.030    
#>              (0.164)       (0.315)       (0.367)      (0.136)   
#> prod.l3       0.011         0.044         0.214       -0.011    
#>              (0.064)       (0.122)       (0.143)      (0.053)   
#> rw.l3         0.038         0.093         0.190       -0.039    
#>              (0.054)       (0.103)       (0.120)      (0.044)   
#> U.l3          0.341         0.401         0.151        0.067    
#>              (0.205)       (0.394)       (0.459)      (0.170)   
#> const      -150.687 *    -195.870       -11.669      114.367 *  
#>             (61.009)     (116.958)     (136.454)     (50.598)   
#> ────────────────────────────────────────────────────────────────
#> R^2           0.999         0.980         0.999        0.974    
#> Adj. R^2      0.999         0.976         0.999        0.969    
#> Num. obs.    81            81            81           81        
#> ────────────────────────────────────────────────────────────────
#> Note. * p < .05, ** p < .01, *** p < .001.
#> 
granger_causality(vm)
#> 
#> Granger Causality Test (Multivariate)
#> 
#> F test and Wald χ² test based on VAR(3) model:
#> ─────────────────────────────────────────────────────────
#>                    F df1 df2     p     Chisq df     p    
#> ─────────────────────────────────────────────────────────
#>  -----------                                             
#>  e <= prod      5.90   3  68  .001 **  17.69  3 <.001 ***
#>  e <= rw        3.29   3  68  .026 *    9.86  3  .020 *  
#>  e <= U         2.13   3  68  .105      6.39  3  .094 .  
#>  e <= ALL       3.86   9  68 <.001 *** 34.77  9 <.001 ***
#>  -----------                                             
#>  prod <= e      1.90   3  68  .137      5.71  3  .127    
#>  prod <= rw     1.81   3  68  .154      5.42  3  .144    
#>  prod <= U      3.22   3  68  .028 *    9.67  3  .022 *  
#>  prod <= ALL    2.36   9  68  .022 *   21.21  9  .012 *  
#>  -----------                                             
#>  rw <= e        0.75   3  68  .527      2.25  3  .523    
#>  rw <= prod     1.97   3  68  .126      5.92  3  .115    
#>  rw <= U        0.18   3  68  .912      0.53  3  .913    
#>  rw <= ALL      3.58   9  68  .001 **  32.24  9 <.001 ***
#>  -----------                                             
#>  U <= e         9.53   3  68 <.001 *** 28.60  3 <.001 ***
#>  U <= prod      1.58   3  68  .203      4.74  3  .192    
#>  U <= rw        2.19   3  68  .098 .    6.56  3  .087 .  
#>  U <= ALL       5.81   9  68 <.001 *** 52.26  9 <.001 ***
#> ─────────────────────────────────────────────────────────
#> 
```
