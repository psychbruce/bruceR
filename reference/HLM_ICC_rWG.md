# Tidy report of HLM indices: ICC(1), ICC(2), and rWG/rWG(J).

Compute ICC(1) (non-independence of data), ICC(2) (reliability of group
means), and \\r\_{WG} / r\_{WG(J)}\\ (within-group agreement for
single-item/multi-item measures) in multilevel analysis (HLM).

## Usage

``` r
HLM_ICC_rWG(
  data,
  group,
  icc.var,
  rwg.vars = icc.var,
  rwg.levels = 0,
  digits = 3
)
```

## Arguments

- data:

  Data frame.

- group:

  Grouping variable.

- icc.var:

  Key variable for analysis (usually the dependent variable).

- rwg.vars:

  Defaults to `icc.var`. It can be:

  - A single variable (*single-item* measure), then computing rWG.

  - Multiple variables (*multi-item* measure), then computing rWG(J),
    where J = the number of items.

- rwg.levels:

  As \\r\_{WG} / r\_{WG(J)}\\ compares the actual group variance to the
  expected random variance (i.e., the variance of uniform distribution,
  \\\sigma\_{EU}^2\\), it is required to specify which type of uniform
  distribution is.

  - For *continuous* uniform distribution, \\\sigma\_{EU}^2 = (max -
    min)^2 / 12\\. Then `rwg.levels` is not useful and will be set to
    `0` (default).

  - For *discrete* uniform distribution, \\\sigma\_{EU}^2 = (A^2 - 1) /
    12\\, where A is the number of response options (levels). Then
    `rwg.levels` should be provided (= A in the above formula). For
    example, if the measure is a 5-point Likert scale, you should set
    `rwg.levels=5`.

- digits:

  Number of decimal places of output. Defaults to `3`.

## Value

Invisibly return a list of results.

## Statistical Details

### ICC(1) (intra-class correlation, or non-independence of data)

ICC(1) = var.u0 / (var.u0 + var.e) = \\\sigma\_{u0}^2 /
(\sigma\_{u0}^2 + \sigma\_{e}^2)\\

ICC(1) is the ICC we often compute and report in multilevel analysis
(usually in the Null Model, where only the random intercept of group is
included). It can be interpreted as either **"the proportion of variance
explained by groups"** (i.e., *heterogeneity* between groups) or **"the
expectation of correlation coefficient between any two observations
within any group"** (i.e., *homogeneity* within groups).

### ICC(2) (reliability of group means)

ICC(2) = mean(var.u0 / (var.u0 + var.e / n.k)) =
\\\Sigma\[\sigma\_{u0}^2 / (\sigma\_{u0}^2 + \sigma\_{e}^2 / n_k)\] /
K\\

ICC(2) is a measure of **"the representativeness of group-level
aggregated means for within-group individual values"** or **"the degree
to which an individual score can be considered a reliable assessment of
a group-level construct"**.

### \\r\_{WG}\\/\\r\_{WG(J)}\\ (within-group agreement for single-item/multi-item measures)

\\r\_{WG} = 1 - \sigma^2 / \sigma\_{EU}^2\\

\\r\_{WG(J)} = 1 - (\sigma\_{MJ}^2 / \sigma\_{EU}^2) / \[J \* (1 -
\sigma\_{MJ}^2 / \sigma\_{EU}^2) + \sigma\_{MJ}^2 / \sigma\_{EU}^2\]\\

\\r\_{WG} / r\_{WG(J)}\\ is a measure of within-group agreement or
consensus. Each group has an \\r\_{WG} / r\_{WG(J)}\\.

### Notes for the above formulas

- \\\sigma\_{u0}^2\\: between-group variance (i.e., tau00)

- \\\sigma\_{e}^2\\: within-group variance (i.e., residual variance)

- \\n_k\\: group size of the k-th group

- \\K\\: number of groups

- \\\sigma^2\\: actual group variance of the k-th group

- \\\sigma\_{MJ}^2\\: mean value of actual group variance of the k-th
  group across all J items

- \\\sigma\_{EU}^2\\: expected random variance (i.e., the variance of
  uniform distribution)

- \\J\\: number of items

## References

Bliese, P. D. (2000). Within-group agreement, non-independence, and
reliability: Implications for data aggregation and Analysis. In K. J.
Klein & S. W. Kozlowski (Eds.), *Multilevel theory, research, and
methods in organizations* (pp. 349–381). San Francisco, CA: Jossey-Bass,
Inc.

James, L.R., Demaree, R.G., & Wolf, G. (1984). Estimating within-group
interrater reliability with and without response bias. *Journal of
Applied Psychology, 69*, 85–98.

## See also

[`cor_multilevel()`](https://psychbruce.github.io/bruceR/reference/cor_multilevel.md)

[R package `multilevel`](https://CRAN.R-project.org/package=multilevel)

## Examples

``` r
data = lme4::sleepstudy  # continuous variable
HLM_ICC_rWG(data, group="Subject", icc.var="Reaction")
#> 
#> ------ Sample Size Information ------
#> 
#> Level 1: N = 180 observations ("Reaction")
#> Level 2: K = 18 groups ("Subject")
#> 
#>        n (group sizes)
#> Min.                10
#> Median              10
#> Mean                10
#> Max.                10
#> 
#> ------ ICC(1), ICC(2), and rWG ------
#> 
#> ICC variable: "Reaction"
#> 
#> ICC(1) = 0.395 (non-independence of data)
#> ICC(2) = 0.867 (reliability of group means)
#> 
#> rWG variable: "Reaction"
#> 
#> rWG (within-group agreement for single-item measures)
#> ─────────────────────────────────────────────
#>       Min. 1st Qu. Median  Mean 3rd Qu.  Max.
#> ─────────────────────────────────────────────
#> rWG  0.000   0.482  0.778 0.684   0.876 0.981
#> ─────────────────────────────────────────────
#> 

data = lmerTest::carrots  # 7-point scale
HLM_ICC_rWG(data, group="Consumer", icc.var="Preference",
            rwg.vars="Preference",
            rwg.levels=7)
#> 
#> ------ Sample Size Information ------
#> 
#> Level 1: N = 1233 observations ("Preference")
#> Level 2: K = 103 groups ("Consumer")
#> 
#>        n (group sizes)
#> Min.          11.00000
#> Median        12.00000
#> Mean          11.97087
#> Max.          12.00000
#> 
#> ------ ICC(1), ICC(2), and rWG ------
#> 
#> ICC variable: "Preference"
#> 
#> ICC(1) = 0.143 (non-independence of data)
#> ICC(2) = 0.666 (reliability of group means)
#> 
#> rWG variable: "Preference"
#> 
#> rWG (within-group agreement for single-item measures)
#> ─────────────────────────────────────────────
#>       Min. 1st Qu. Median  Mean 3rd Qu.  Max.
#> ─────────────────────────────────────────────
#> rWG  0.000   0.631  0.752 0.711   0.815 1.000
#> ─────────────────────────────────────────────
#> 
HLM_ICC_rWG(data, group="Consumer", icc.var="Preference",
            rwg.vars=c("Sweetness", "Bitter", "Crisp"),
            rwg.levels=7)
#> 
#> ------ Sample Size Information ------
#> 
#> Level 1: N = 1233 observations ("Preference")
#> Level 2: K = 103 groups ("Consumer")
#> 
#>        n (group sizes)
#> Min.          11.00000
#> Median        12.00000
#> Mean          11.97087
#> Max.          12.00000
#> 
#> ------ ICC(1), ICC(2), and rWG(J) ------
#> 
#> ICC variable: "Preference"
#> 
#> ICC(1) = 0.143 (non-independence of data)
#> ICC(2) = 0.666 (reliability of group means)
#> 
#> rWG(J) variables: "Sweetness", "Bitter", "Crisp"
#> 
#> rWG(J) (within-group agreement for multi-item measures)
#> ────────────────────────────────────────────────
#>          Min. 1st Qu. Median  Mean 3rd Qu.  Max.
#> ────────────────────────────────────────────────
#> rWG(J)  0.170   0.807  0.871 0.841   0.908 0.983
#> ────────────────────────────────────────────────
#> 
```
