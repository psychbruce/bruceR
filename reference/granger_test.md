# Granger causality test (bivariate).

Granger test of predictive causality (between two time series) using
[`lmtest::grangertest()`](https://rdrr.io/pkg/lmtest/man/grangertest.html).

## Usage

``` r
granger_test(formula, data, lags = 1:5, test.reverse = TRUE, file = NULL, ...)
```

## Arguments

- formula:

  Model formula like `y ~ x`.

- data:

  Data frame.

- lags:

  Time lags. Defaults to `1:5`.

- test.reverse:

  Whether to test reverse causality. Defaults to `TRUE`.

- file:

  File name of MS Word (`".doc"`).

- ...:

  Arguments passed on to
  [`lmtest::grangertest()`](https://rdrr.io/pkg/lmtest/man/grangertest.html).
  For example, you may use *robust* standard errors by specifying the
  `vcov` argument (see [GitHub Issue
  \#23](https://github.com/psychbruce/bruceR/issues/23)).

## Value

A data frame of results.

## Details

Granger causality test examines whether the lagged values of a predictor
have an incremental role in predicting (i.e., help to predict) an
outcome when controlling for the lagged values of the outcome. Granger
causality does not represent a true causal effect.

## See also

[`ccf_plot()`](https://psychbruce.github.io/bruceR/reference/ccf_plot.md)

[`granger_causality()`](https://psychbruce.github.io/bruceR/reference/granger_causality.md)

## Examples

``` r
granger_test(chicken ~ egg, data=lmtest::ChickEgg)
#> 
#> Granger Causality Test (Bivariate)
#> 
#> Hypothesized direction:
#> chicken ~ chicken[1:Lags] + egg[1:Lags]
#> Lags = 1:    F(1, 50) = 1.21, p = 0.277    
#> Lags = 2:    F(2, 47) = 8.82, p = 6e-04 ***
#> Lags = 3:    F(3, 44) = 5.40, p = 0.003 ** 
#> Lags = 4:    F(4, 41) = 4.26, p = 0.006 ** 
#> Lags = 5:    F(5, 38) = 4.73, p = 0.002 ** 
#> 
#> Reverse direction:
#> egg ~ egg[1:Lags] + chicken[1:Lags]
#> Lags = 1:    F(1, 50) = 0.05, p = 0.829    
#> Lags = 2:    F(2, 47) = 0.88, p = 0.422    
#> Lags = 3:    F(3, 44) = 0.59, p = 0.624    
#> Lags = 4:    F(4, 41) = 0.39, p = 0.813    
#> Lags = 5:    F(5, 38) = 0.29, p = 0.913    
#> 
granger_test(chicken ~ egg, data=lmtest::ChickEgg, lags=1:10, file="Granger.doc")
#> 
#> Granger Causality Test (Bivariate)
#> 
#> Hypothesized direction:
#> chicken ~ chicken[1:Lags] + egg[1:Lags]
#> Lags = 1:    F(1, 50) = 1.21, p = 0.277    
#> Lags = 2:    F(2, 47) = 8.82, p = 6e-04 ***
#> Lags = 3:    F(3, 44) = 5.40, p = 0.003 ** 
#> Lags = 4:    F(4, 41) = 4.26, p = 0.006 ** 
#> Lags = 5:    F(5, 38) = 4.73, p = 0.002 ** 
#> Lags = 6:    F(6, 35) = 3.65, p = 0.006 ** 
#> Lags = 7:    F(7, 32) = 4.06, p = 0.003 ** 
#> Lags = 8:    F(8, 29) = 3.15, p = 0.011 *  
#> Lags = 9:    F(9, 26) = 4.37, p = 0.001 ** 
#> Lags = 10:   F(10, 23) = 3.90, p = 0.003 ** 
#> 
#> Reverse direction:
#> egg ~ egg[1:Lags] + chicken[1:Lags]
#> Lags = 1:    F(1, 50) = 0.05, p = 0.829    
#> Lags = 2:    F(2, 47) = 0.88, p = 0.422    
#> Lags = 3:    F(3, 44) = 0.59, p = 0.624    
#> Lags = 4:    F(4, 41) = 0.39, p = 0.813    
#> Lags = 5:    F(5, 38) = 0.29, p = 0.913    
#> Lags = 6:    F(6, 35) = 0.36, p = 0.898    
#> Lags = 7:    F(7, 32) = 0.61, p = 0.747    
#> Lags = 8:    F(8, 29) = 0.61, p = 0.762    
#> Lags = 9:    F(9, 26) = 0.56, p = 0.820    
#> Lags = 10:   F(10, 23) = 0.38, p = 0.944    
#> 
#> ✔ Table saved to "/home/runner/work/bruceR/bruceR/docs/reference/Granger.doc"
#> 
unlink("Granger.doc")  # delete file for code check
```
