# bruceR

**BR**oadly **U**seful **C**ollections and **E**xtensions of **R** functions

![](https://img.shields.io/badge/R-package-success)
![](https://img.shields.io/badge/Version-0.3.0-success)
![](https://img.shields.io/github/license/psychbruce/bruceR?label=License&color=success)
[![](https://img.shields.io/github/stars/psychbruce/bruceR?style=social)](https://github.com/psychbruce/bruceR/stargazers)

[![](https://img.shields.io/badge/Follow%20me%20on-Zhihu-blue)](https://www.zhihu.com/people/psychbruce/ "Personal profile on Zhihu.com")


## Install from GitHub
```r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("psychbruce/bruceR")
```

Then, you may see these messages in console:
```
These packages have more recent versions available.
Which would you like to update?

 1: All
 2: CRAN packages only
 3: None
 ...

Enter one or more numbers, or an empty line to skip updates:
```
You can update all the packages that `bruceR` depends on or just enter an **empty line** to skip this step.


## Potential Bugs During Installation

All the bugs listed below have **no** relationship with `bruceR` *per se*. They are actually related to the installation and updates of the dependency packages of `bruceR`.

### Bug #1:
```
WARNING: Rtools is required to build R packages, but is not currently installed.

Please download and install Rtools 3.5 from http://cran.r-project.org/bin/windows/Rtools/

Error in parse_repo_spec(repo) : Invalid git repo specification: 'bruceR'
```
#### Solution:
Download and install it. (Note: [Rtools](http://cran.r-project.org/bin/windows/Rtools/) is **not** an R package.)

### Bug #2:
```
Error: Failed to install 'bruceR' from GitHub:
  (converted from warning) installation of package ‘rlang’ had non-zero exit status
```
#### Solution:
This bug may happen when you choose to update the dependency packages (e.g., `rlang`) of `bruceR`. You may first update it by using `install.packages("rlang")` before you install `bruceR`.

Yet there might still be some bugs when you update them. For example, you may see `There is a binary version available but the source version is later` in the console and `Do you want to install from sources the package which needs compilation` in a dialog box. You can try whether the "source version" can be successfully installed. If not, you can install the "binary version". In general, an R package has a "source version" newer than a "binary version", but you can wait until the "source version" gets a corresponding "binary version", which will make installation easier and faster.

### Other bugs:
For any other potential bugs during installation, please read carefully the warning messages. I believe you can solve them if you follow the instruction.


## User Guide
> *We are all standing on the shoulders of giants.*

### Loading `bruceR`
`bruceR` depends on some important packages (listed as follows) and also automatically installs many other useful packages for users. Many functions in `bruceR` are extensions of the functions in these packages.

Once you load `bruceR` to the global environment with `library()`, it will also load the packages most commonly used:
- `rio`: Data input/output for many file formats within one function (`import`/`export`).
- `dplyr`: Data manipulation and preprocessing.
- `data.table`: Enhanced 'data.frame' with higher efficiency.
- `psych`: Toolbox for psychological research.
- `stringr`: Toolbox for dealing with strings and regular expressions.
- `lubridate`: Toolbox for dealing with dates and times.
- `performance`: Toolbox for assessing many indexes of regression models.
- `ggplot2`: Data visualization.

No need to load each one with its own call. Loading `bruceR` is enough.
```r
library(bruceR)

## Overview of Package
?bruceR

## See Help Pages of Functions
?MANOVA
?EMMEANS
?GLM_summary
?HLM_summary

## Check Update
check_update()

## Run Some Examples
example("Print")
example("Describe")
example("Freq")
example("Corr")

example("MEAN")
example("LOOKUP")

example("GLM_summary")
example("HLM_summary")
example("model_check")
```

### Main functions in `bruceR`
- [x] Basic use and analyses (e.g., correlation matrix with plot)
  + `Print()`, `Describe()`, `Freq()`, `Corr()`, ...
  + `set.wd()`, `set.seeds()`, `dtime()`, ...
  + `%notin%`, `%partin%`, ...
  + `LOOKUP()`, ...
- [x] Multivariate computing (e.g., scale mean score with reverse scoring)
  + `RECODE()`, `RESCALE()`
  + `COUNT()`, `MODE()`, `SUM()`, `MEAN()`, `STD()`, `CONSEC()`
- [x] Reliability and validity analyses (e.g., Cronbach's α, exploratory/confirmatory factor analysis)
  + `Alpha()`
  + `EFA()`, `CFA()`
- [x] *t*-test, ANOVA, simple-effect analyses, and multiple comparisons
  + `MANOVA()`, `EMMEANS()`
- [x] Advanced toolbox and output for general/generalized ordinary/multilevel linear models
  + `grand_mean_center()`, `group_mean_center()`
  + `GLM_summary()`, `HLM_summary()`, `regress()`, `model_check()`
  + `med_mc()`, `simple_slope()`
- [x] Nice themes of `ggplot2` ready for scientific publication
  + `theme_bruce()`


## Release Notes
### Current version: `0.3.0`
### Major changes:
+ `0.3.0` - 30 October 2019
  + Added new functions for ANOVA, simple-effect analyses, and multiple comparisons
  + General bug-fixes and improvements
+ `0.2.0` - 30 August 2019
  + Added help pages
  + General bug-fixes and improvements
+ `0.1.0` - 30 June 2019
  + Initial commit


## Author
[Han-Wu-Shuang (Bruce) Bao - 包寒吴霜](https://www.zhihu.com/people/psychbruce/ "Personal profile on Zhihu.com")

E-mail: baohws@psych.ac.cn or psychbruce@qq.com
