# Cross-correlation analysis.

Plot the results of cross-correlation analysis using `ggplot2`.

## Usage

``` r
ccf_plot(
  formula,
  data,
  lag.max = 30,
  sig.level = 0.05,
  xbreaks = seq(-100, 100, 10),
  ybreaks = seq(-1, 1, 0.2),
  ylim = NULL,
  alpha.ns = 1,
  pos.color = "black",
  neg.color = "black",
  ci.color = "blue",
  title = NULL,
  subtitle = NULL,
  xlab = "Lag",
  ylab = "Cross-Correlation"
)
```

## Arguments

- formula:

  Model formula like `y ~ x`.

- data:

  Data frame.

- lag.max:

  Maximum time lag. Defaults to `30`.

- sig.level:

  Significance level. Defaults to `0.05`.

- xbreaks:

  X-axis breaks.

- ybreaks:

  Y-axis breaks.

- ylim:

  Y-axis limits. Defaults to `NULL` to automatically estimate.

- alpha.ns:

  Color transparency (opacity: 0~1) for non-significant values. Defaults
  to `1` for no transparency (i.e., opaque color).

- pos.color:

  Color for positive values. Defaults to `"black"`.

- neg.color:

  Color for negative values. Defaults to `"black"`.

- ci.color:

  Color for upper and lower bounds of significant values. Defaults to
  `"blue"`.

- title:

  Plot title. Defaults to an illustration of the formula.

- subtitle:

  Plot subtitle.

- xlab:

  X-axis title. Defaults to `"Lag"`.

- ylab:

  Y-axis title. Defaults to `"Cross-Correlation"`.

## Value

A `ggplot` object, which can be further modified using `ggplot2` syntax
and saved using
[`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Details

Significant correlations with *negative time lags* suggest shifts in a
predictor *precede* shifts in an outcome.

## See also

[`granger_test()`](https://psychbruce.github.io/bruceR/reference/granger_test.md)

## Examples

``` r
# resemble the default plot output by `ccf()`
p1 = ccf_plot(chicken ~ egg, data=lmtest::ChickEgg)
p1


# a more colorful plot
p2 = ccf_plot(chicken ~ egg, data=lmtest::ChickEgg, alpha.ns=0.3,
              pos.color="#CD201F",
              neg.color="#21759B",
              ci.color="black")
p2

```
