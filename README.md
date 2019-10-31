# bruceR

**BR**oadly **U**seful **C**ollections and **E**xtensions of **R** functions

![](https://img.shields.io/badge/R-package-success)
![](https://img.shields.io/badge/Version-0.3.0-success)
![](https://img.shields.io/github/license/psychbruce/bruceR?label=License&color=success)
[![](https://img.shields.io/github/stars/psychbruce/bruceR?style=social)](https://github.com/psychbruce/bruceR/stargazers)

[![](https://img.shields.io/badge/Follow%20me%20on-Zhihu-blue)](https://www.zhihu.com/people/psychbruce/ "Personal profile on Zhihu.com")


## Install
```r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("psychbruce/bruceR")
```
> Please do NOT use `install.packages("bruceR")`, because this package has not been put on the [CRAN](https://cran.r-project.org/). Currently, it is only available from GitHub.

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

**NONE** of the installation bugs have relationships with `bruceR` *per se*.

### Bug #01:
```
> install.packages("bruceR")
Warning in install.packages :
  package ‘bruceR’ is not available (for R version 3.6.1)
```
#### Solution:
Use `devtools::install_github("psychbruce/bruceR")`.
Do not use `install.packages("bruceR")`.

### Bug #02:
```
> devtools::install_github("psychbruce/bruceR")
Downloading GitHub repo psychbruce/bruceR@master
Error in utils::download.file(url, path, method = method, quiet = quiet,  : 
  cannot open URL 'https://api.github.com/repos/psychbruce/bruceR/tarball/master'
```
#### Solution:
Check your network connections.

### Bug #03:
```
WARNING: Rtools is required to build R packages, but is not currently installed.

Please download and install Rtools 3.5 from http://cran.r-project.org/bin/windows/Rtools/

Error in parse_repo_spec(repo) : Invalid git repo specification: 'bruceR'
```
#### Solution:
Download and install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).

### Bug #04:
```
* installing *source* package 'bruceR' ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
错误: (由警告转换成)程辑包'rio'是用R版本3.6.1 来建造的
停止执行
ERROR: lazy loading failed for package 'bruceR'
Error: Failed to install 'bruceR' from GitHub:
  (converted from warning) installation of package ‘bruceR’ had non-zero exit status
```
or
```
* installing *source* package 'bruceR' ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
错误: (由警告转换成)程辑包'dplyr'是用R版本3.6.1 来建造的
停止执行
ERROR: lazy loading failed for package 'bruceR'
Error: Failed to install 'bruceR' from GitHub:
  (converted from warning) installation of package ‘bruceR’ had non-zero exit status
```
#### Solution:
Update R to the [newest version](https://cran.r-project.org/), because the latest versions of some packages (e.g., `rio`, `dplyr`) also require the latest version of R.

Tips: You can use the `installr` package to copy all your installed packages from the old folder to the new one.
```r
install.packages("installr")
library(installr)
copy.packages.between.libraries(ask=TRUE)
```

### Bug #05:
```
Error: Failed to install 'bruceR' from GitHub:
  (converted from warning) installation of package ‘rlang’ had non-zero exit status
```
#### Solution:
Some dependency packages of `bruceR` (e.g., `rlang`) should be responsible for this bug. Use the menu bar of RStudio (in the panes, not on the top: `Package -> Update`) to update such packages before you install `bruceR`. Or you can skip updating such packages.

### Bug #N:
There may be other bugs.
#### Solution:
Read the warning messages. Follow the instruction. Search the Internet. Get the answer.


## User Guide
> *We are all standing on the shoulders of giants.*

### Loading `bruceR`
`bruceR` depends on some important packages and also automatically installs many other useful packages for users. Many functions in `bruceR` are extensions of the functions in these packages.

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
