# Test the difference between two correlations.

Test the difference between two correlations.

## Usage

``` r
cor_diff(r1, n1, r2, n2, n = NULL, rcov = NULL)
```

## Arguments

- r1, r2:

  Correlation coefficients (Pearson's \\r\\).

- n, n1, n2:

  Sample sizes.

- rcov:

  \[Optional\] Only for nonindependent \\r\\s:

  `r1` is r(X,Y),

  `r2` is r(X,Z),

  then, as Y and Z are also correlated,

  we should also consider `rcov`: r(Y,Z)

## Value

Invisibly return the *p* value.

## Examples

``` r
# two independent rs (X~Y vs. Z~W)
cor_diff(r1=0.20, n1=100, r2=0.45, n2=100)
#> r1 = 0.200 (N = 100)
#> r2 = 0.450 (N = 100)
#> Difference of correlation: z = -1.96, p = 0.050 *  

# two nonindependent rs (X~Y vs. X~Z, with Y and Z also correlated [rcov])
cor_diff(r1=0.20, r2=0.45, n=100, rcov=0.80)
#> r1 = 0.200
#> r2 = 0.450
#> (N = 100, r_cov = 0.800)
#> Difference of correlation: t(97) = -4.56, p = 1e-05 ***
```
