# Reliability analysis (Cronbach's \\\alpha\\ and McDonald's \\\omega\\).

An extension of
[`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html) and
[`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html), reporting
(1) scale statistics (Cronbach's \\\alpha\\ and McDonald's \\\omega\\)
and (2) item statistics (item-rest correlation \[i.e., corrected
item-total correlation\] and Cronbach's \\\alpha\\ if item deleted).

Three options to specify variables:

1.  **`var` + `items`**: common and unique parts of variable names
    (suggested).

2.  **`vars`**: a character vector of variable names (suggested).

3.  **`varrange`**: starting and stopping positions of variables (NOT
    suggested).

## Usage

``` r
Alpha(data, var, items, vars = NULL, varrange = NULL, rev = NULL, digits = 3)
```

## Arguments

- data:

  Data frame.

- var:

  **\[Option 1\]** Common part across variables: e.g., `"RSES"`,
  `"XX.{i}.pre"` (if `var` string has any placeholder in braces `{...}`,
  then `items` will be pasted into the braces, see examples)

- items:

  **\[Option 1\]** Unique part across variables: e.g., `1:10`,
  `c("a", "b", "c")`

- vars:

  **\[Option 2\]** Character vector specifying variables: e.g.,
  `c("X1", "X2", "X3", "X4", "X5")`

- varrange:

  **\[Option 3\]** Character string specifying positions
  (`"start:stop"`) of variables: e.g., `"A1:E5"`

- rev:

  \[Optional\] Variables that need to be reversed. It can be (1) a
  character vector specifying the reverse-scoring variables
  (recommended), or (2) a numeric vector specifying the item number of
  reverse-scoring variables (not recommended).

- digits:

  Number of decimal places of output. Defaults to `3`.

## Value

A list of results obtained from
[`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html) and
[`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html).

## See also

[`MEAN()`](https://psychbruce.github.io/bruceR/reference/grapes-grapes-COMPUTE-grapes-grapes.md)

[`EFA()`](https://psychbruce.github.io/bruceR/reference/EFA.md)

[`CFA()`](https://psychbruce.github.io/bruceR/reference/CFA.md)

## Examples

``` r
# ?psych::bfi
data = psych::bfi
Alpha(data, "E", 1:5)   # "E1" & "E2" should be reversed
#> 
#> Reliability Analysis
#> 
#> Summary:
#> Total Items: 5
#> Scale Range: 1 ~ 6
#> Total Cases: 2800
#> Valid Cases: 2713 (96.9%)
#> 
#> Scale Statistics:
#> Mean = 3.791
#> S.D. = 0.542
#> Cronbach’s α = -0.624
#> McDonald’s ω = 0.150
#> 
#> Warning: Scale reliability is low. You may check item codings.
#> Items E1, E2 correlate negatively with the scale and may be reversed.
#> You can specify this argument: rev=c("E1", "E2")
#> 
#> Item Statistics (Cronbach’s α If Item Deleted):
#> ─────────────────────────────────────────────
#>      Mean    S.D. Item-Rest Cor. Cronbach’s α
#> ─────────────────────────────────────────────
#> E1  2.972 (1.632)         -0.263       -0.270
#> E2  3.144 (1.607)         -0.355       -0.074
#> E3  4.000 (1.352)         -0.002       -0.881
#> E4  4.421 (1.461)         -0.202       -0.423
#> E5  4.418 (1.337)         -0.047       -0.765
#> ─────────────────────────────────────────────
#> Item-Rest Cor. = Corrected Item-Total Correlation
#> 
Alpha(data, "E", 1:5, rev=1:2)            # correct
#> 
#> Reliability Analysis
#> 
#> Summary:
#> Total Items: 5
#> Scale Range: 1 ~ 6
#> Total Cases: 2800
#> Valid Cases: 2713 (96.9%)
#> 
#> Scale Statistics:
#> Mean = 4.145
#> S.D. = 1.060
#> Cronbach’s α = 0.761
#> McDonald’s ω = 0.763
#> 
#> Item Statistics (Cronbach’s α If Item Deleted):
#> ───────────────────────────────────────────────────
#>            Mean    S.D. Item-Rest Cor. Cronbach’s α
#> ───────────────────────────────────────────────────
#> E1 (rev)  4.028 (1.632)          0.513        0.725
#> E2 (rev)  3.856 (1.607)          0.606        0.688
#> E3        4.000 (1.352)          0.501        0.728
#> E4        4.421 (1.461)          0.578        0.701
#> E5        4.418 (1.337)          0.455        0.742
#> ───────────────────────────────────────────────────
#> Item-Rest Cor. = Corrected Item-Total Correlation
#> 
Alpha(data, "E", 1:5, rev=cc("E1, E2"))   # also correct
#> 
#> Reliability Analysis
#> 
#> Summary:
#> Total Items: 5
#> Scale Range: 1 ~ 6
#> Total Cases: 2800
#> Valid Cases: 2713 (96.9%)
#> 
#> Scale Statistics:
#> Mean = 4.145
#> S.D. = 1.060
#> Cronbach’s α = 0.761
#> McDonald’s ω = 0.763
#> 
#> Item Statistics (Cronbach’s α If Item Deleted):
#> ───────────────────────────────────────────────────
#>            Mean    S.D. Item-Rest Cor. Cronbach’s α
#> ───────────────────────────────────────────────────
#> E1 (rev)  4.028 (1.632)          0.513        0.725
#> E2 (rev)  3.856 (1.607)          0.606        0.688
#> E3        4.000 (1.352)          0.501        0.728
#> E4        4.421 (1.461)          0.578        0.701
#> E5        4.418 (1.337)          0.455        0.742
#> ───────────────────────────────────────────────────
#> Item-Rest Cor. = Corrected Item-Total Correlation
#> 
Alpha(data, vars=cc("E1, E2, E3, E4, E5"), rev=cc("E1, E2"))
#> 
#> Reliability Analysis
#> 
#> Summary:
#> Total Items: 5
#> Scale Range: 1 ~ 6
#> Total Cases: 2800
#> Valid Cases: 2713 (96.9%)
#> 
#> Scale Statistics:
#> Mean = 4.145
#> S.D. = 1.060
#> Cronbach’s α = 0.761
#> McDonald’s ω = 0.763
#> 
#> Item Statistics (Cronbach’s α If Item Deleted):
#> ───────────────────────────────────────────────────
#>            Mean    S.D. Item-Rest Cor. Cronbach’s α
#> ───────────────────────────────────────────────────
#> E1 (rev)  4.028 (1.632)          0.513        0.725
#> E2 (rev)  3.856 (1.607)          0.606        0.688
#> E3        4.000 (1.352)          0.501        0.728
#> E4        4.421 (1.461)          0.578        0.701
#> E5        4.418 (1.337)          0.455        0.742
#> ───────────────────────────────────────────────────
#> Item-Rest Cor. = Corrected Item-Total Correlation
#> 
Alpha(data, varrange="E1:E5", rev=cc("E1, E2"))
#> 
#> Reliability Analysis
#> 
#> Summary:
#> Total Items: 5
#> Scale Range: 1 ~ 6
#> Total Cases: 2800
#> Valid Cases: 2713 (96.9%)
#> 
#> Scale Statistics:
#> Mean = 4.145
#> S.D. = 1.060
#> Cronbach’s α = 0.761
#> McDonald’s ω = 0.763
#> 
#> Item Statistics (Cronbach’s α If Item Deleted):
#> ───────────────────────────────────────────────────
#>            Mean    S.D. Item-Rest Cor. Cronbach’s α
#> ───────────────────────────────────────────────────
#> E1 (rev)  4.028 (1.632)          0.513        0.725
#> E2 (rev)  3.856 (1.607)          0.606        0.688
#> E3        4.000 (1.352)          0.501        0.728
#> E4        4.421 (1.461)          0.578        0.701
#> E5        4.418 (1.337)          0.455        0.742
#> ───────────────────────────────────────────────────
#> Item-Rest Cor. = Corrected Item-Total Correlation
#> 

# using dplyr::select()
data %>% select(E1, E2, E3, E4, E5) %>%
  Alpha(vars=names(.), rev=cc("E1, E2"))
#> 
#> Reliability Analysis
#> 
#> Summary:
#> Total Items: 5
#> Scale Range: 1 ~ 6
#> Total Cases: 2800
#> Valid Cases: 2713 (96.9%)
#> 
#> Scale Statistics:
#> Mean = 4.145
#> S.D. = 1.060
#> Cronbach’s α = 0.761
#> McDonald’s ω = 0.763
#> 
#> Item Statistics (Cronbach’s α If Item Deleted):
#> ───────────────────────────────────────────────────
#>            Mean    S.D. Item-Rest Cor. Cronbach’s α
#> ───────────────────────────────────────────────────
#> E1 (rev)  4.028 (1.632)          0.513        0.725
#> E2 (rev)  3.856 (1.607)          0.606        0.688
#> E3        4.000 (1.352)          0.501        0.728
#> E4        4.421 (1.461)          0.578        0.701
#> E5        4.418 (1.337)          0.455        0.742
#> ───────────────────────────────────────────────────
#> Item-Rest Cor. = Corrected Item-Total Correlation
#> 
```
