## Examples for `bruceR::PROCESS()` Function

``` r
library(bruceR)

#### NOTE ####
## In the following examples, I set nsim=100 to save time.
## In formal analyses, nsim=1000 (or larger) is suggested!

#### Demo Data ####
# ?mediation::student
data=mediation::student %>%
  dplyr::select(SCH_ID, free, smorale, pared, income,
                gender, work, attachment, fight, late, score)
names(data)[2:3]=c("SCH_free", "SCH_morale")
names(data)[4:7]=c("parent_edu", "family_inc", "gender", "partjob")
data$gender01=1-data$gender  # 0 = female, 1 = male
# dichotomous X: as.factor()
data$gender=factor(data$gender01, levels=0:1, labels=c("Female", "Male"))
# dichotomous Y: as.factor()
data$pass=as.factor(ifelse(data$score>=50, 1, 0))

#### Descriptive Statistics and Correlation Analyses ####
Freq(data$gender)
Freq(data$pass)
Describe(data)     # Describe(data, file="xxx.doc")
Corr(data[,4:11])  # Corr(data, file="xxx.doc")

#### PROCESS Analyses ####

## Model 1 ##
PROCESS(data, y="score", x="late", mods="gender")  # continuous Y
PROCESS(data, y="pass", x="late", mods="gender")   # dichotomous Y

# (multilevel moderation)
PROCESS(data, y="score", x="late", mods="gender",  # continuous Y (LMM)
        clusters="SCH_ID")
PROCESS(data, y="pass", x="late", mods="gender",   # dichotomous Y (GLMM)
        clusters="SCH_ID")

# (Johnson-Neyman (J-N) interval and plot)
PROCESS(data, y="score", x="gender", mods="late")->P
P$results[[1]]$jn[[1]]       # Johnson-Neyman interval
P$results[[1]]$jn[[1]]$plot  # Johnson-Neyman plot (ggplot object)
GLM_summary(P$model.y)       # detailed results of regression

# (allows multicategorical moderator)
d=airquality
d$Month=as.factor(d$Month)  # moderator: factor with levels "5"~"9"
PROCESS(d, y="Temp", x="Solar.R", mods="Month")

## Model 2 ##
PROCESS(data, y="score", x="late",
        mods=c("gender", "family_inc"),
        mod.type="2-way")  # or omit "mod.type", default is "2-way"

## Model 3 ##
PROCESS(data, y="score", x="late",
        mods=c("gender", "family_inc"),
        mod.type="3-way")
PROCESS(data, y="pass", x="gender",
        mods=c("late", "family_inc"),
        mod1.val=c(1, 3, 5),     # moderator 1: late
        mod2.val=seq(1, 15, 2),  # moderator 2: family_inc
        mod.type="3-way")

## Model 4 ##
# Percentile Bootstrap CI
PROCESS(data, y="score", x="parent_edu",
        meds="family_inc", covs="gender",
        ci="boot", nsim=100, seed=1)
# Bias-Corrected Percentile Bootstrap CI
PROCESS(data, y="score", x="parent_edu",
        meds="family_inc", covs="gender",
        ci="bc.boot", nsim=100, seed=1)
# Bias-Corrected and Accelerated Percentile Bootstrap CI
PROCESS(data, y="score", x="parent_edu",
        meds="family_inc", covs="gender",
        ci="bca.boot", nsim=100, seed=1)
# Monte Carlo CI
PROCESS(data, y="score", x="parent_edu",
        meds="family_inc", covs="gender",
        ci="mcmc", nsim=100, seed=1)

# (allows an infinite number of multiple mediators in parallel)
PROCESS(data, y="score", x="parent_edu",
        meds=c("family_inc", "late"),
        covs=c("gender", "partjob"),
        ci="boot", nsim=100, seed=1)

# (multilevel mediation)
PROCESS(data, y="score", x="SCH_free",
        meds="late", clusters="SCH_ID",
        ci="mcmc", nsim=100, seed=1)

## Model 5 / 5.2 / 5.3 ##
PROCESS(data, y="score", x="fight",
        meds="late",
        mods="gender",
        covs="parent_edu",
        mod.path="x-y",
        ci="boot", nsim=100, seed=1)
PROCESS(data, y="score", x="fight",
        meds=c("late", "attachment"),
        mods=c("gender", "partjob"),
        covs=c("parent_edu", "family_inc"),
        mod.path="x-y",
        mod.type="3-way",
        ci="boot", nsim=100, seed=1)

## Model 6 ##
PROCESS(data, y="score", x="parent_edu",
        meds=c("family_inc", "late"),
        covs=c("gender", "partjob"),
        med.type="serial",
        ci="boot", nsim=100, seed=1)

## Model 8 ##
PROCESS(data, y="score", x="fight",
        meds="late",
        mods="gender",
        mod.path=c("x-m", "x-y"),
        ci="boot", nsim=100, seed=1)

## Model 10 ##
PROCESS(data, y="score", x="fight",
        meds="late",
        mods=c("gender", "family_inc"),
        mod.path=c("x-m", "x-y"),
        mod.type="2-way",
        ci="boot", nsim=100, seed=1)

## Model 12 ##
PROCESS(data, y="score", x="fight",
        meds="late",
        mods=c("gender", "family_inc"),
        mod.path=c("x-m", "x-y"),
        mod.type="3-way",
        ci="boot", nsim=100, seed=1)

## Index of Moderated Mediation (based on Model 8) ##
pro=PROCESS(data, y="score", x="fight",
            meds="late",
            mods="gender",
            mod.path=c("x-m", "x-y"),
            ci="boot", nsim=1000, seed=1)
mediation::test.modmed(
  mediation::mediate(
    model.m=pro$model.m[[1]],
    model.y=pro$model.y,
    treat="fight",
    mediator="late",
    boot=TRUE,
    sims=1000),
  covariates.1=list(gender="Female"),
  covariates.2=list(gender="Male"))
# For usage, see ?mediation::test.modmed
# The results can be found under the title:
# Test of ACME(covariates.1) - ACME(covariates.2) = 0
# ...
```
