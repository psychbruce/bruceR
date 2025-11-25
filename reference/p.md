# Compute *p* value.

Compute *p* value.

## Usage

``` r
p(
  z = NULL,
  t = NULL,
  f = NULL,
  r = NULL,
  chi2 = NULL,
  n = NULL,
  df = NULL,
  df1 = NULL,
  df2 = NULL,
  digits = 2
)

p.z(z)

p.t(t, df)

p.f(f, df1, df2)

p.r(r, n)

p.chi2(chi2, df)
```

## Arguments

- z, t, f, r, chi2:

  \\z\\, \\t\\, \\F\\, \\r\\, \\\chi^2\\ value.

- n, df, df1, df2:

  Sample size or degree of freedom.

- digits:

  Number of decimal places of output. Defaults to `2`.

## Value

*p* value statistics.

## Functions

- `p.z()`: Two-tailed *p* value of \\z\\.

- `p.t()`: Two-tailed *p* value of \\t\\.

- `p.f()`: One-tailed *p* value of \\F\\. (Note: \\F\\ test is
  one-tailed only.)

- `p.r()`: Two-tailed *p* value of \\r\\.

- `p.chi2()`: One-tailed *p* value of \\\chi^2\\. (Note: \\\chi^2\\ test
  is one-tailed only.)

## Examples

``` r
p.z(1.96)
#> [1] 0.04999579
p.t(2, 100)
#> [1] 0.04821218
p.f(4, 1, 100)
#> [1] 0.04821218
p.r(0.2, 100)
#> [1] 0.04603629
p.chi2(3.84, 1)
#> [1] 0.05004352

p(z=1.96)
#> z = 1.96, p = 0.050 *  
p(t=2, df=100)
#> t(100) = 2.00, p = 0.048 *  
p(f=4, df1=1, df2=100)
#> F(1, 100) = 4.00, p = 0.048 *  
p(r=0.2, n=100)
#> r(98) = 0.20, p = 0.046 *  
p(chi2=3.84, df=1)
#> χ²(1) = 3.84, p = 0.050 .  
```
