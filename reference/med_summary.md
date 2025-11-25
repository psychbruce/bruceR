# Tidy report of mediation analysis.

Tidy report of mediation analysis, which is performed using
[`mediation::mediate()`](https://rdrr.io/pkg/mediation/man/mediate.html).

## Usage

``` r
med_summary(model, digits = 3, file = NULL)
```

## Arguments

- model:

  Mediation model built with
  [`mediation::mediate()`](https://rdrr.io/pkg/mediation/man/mediate.html).

- digits:

  Number of decimal places of output. Defaults to `3`.

- file:

  File name of MS Word (`".doc"`).

## Value

Invisibly return a data frame containing the results.

## See also

[`PROCESS()`](https://psychbruce.github.io/bruceR/reference/PROCESS.md)

## Examples

``` r
if (FALSE) { # \dontrun{

library(mediation)
# ?mediation::mediate

## Example 1: OLS Regression
## Bias-corrected and accelerated (BCa) bootstrap confidence intervals

## Hypothesis: Solar radiation -> Ozone -> Daily temperature
lm.m = lm(Ozone ~ Solar.R + Month + Wind, data=airquality)
lm.y = lm(Temp ~ Ozone + Solar.R + Month + Wind, data=airquality)
set.seed(123)  # set a random seed for reproduction
med = mediate(lm.m, lm.y,
            treat="Solar.R", mediator="Ozone",
            sims=1000, boot=TRUE, boot.ci.type="bca")
med_summary(med)

## Example 2: Multilevel Linear Model (Linear Mixed Model)
## (models must be fit using "lme4::lmer" rather than "lmerTest::lmer")
## Monte Carlo simulation (quasi-Bayesian approximation)
## (bootstrap method is not applicable to "lmer" models)

## Hypothesis: Crips -> Sweetness -> Preference (for carrots)
data = lmerTest::carrots  # long-format data
data = na.omit(data)  # omit missing values
lmm.m = lme4::lmer(Sweetness ~ Crisp + Gender + Age + (1 | Consumer), data=data)
lmm.y = lme4::lmer(Preference ~ Sweetness + Crisp + Gender + Age + (1 | Consumer), data=data)
set.seed(123)  # set a random seed for reproduction
med.lmm = mediate(lmm.m, lmm.y,
                  treat="Crisp", mediator="Sweetness",
                  sims=1000)
med_summary(med.lmm)
} # }
```
