# bruceR

**BR**oadly **U**seful **C**ollections and **E**xtensions of **R** functions

[![](https://img.shields.io/badge/R-package-success)](https://github.com/psychbruce/bruceR "bruceR")
![](https://img.shields.io/badge/Release-v0.2.2-success)
![](https://img.shields.io/github/license/psychbruce/bruceR?label=License&color=success)
[![](https://img.shields.io/github/stars/psychbruce/bruceR?style=social)](https://github.com/psychbruce/bruceR/stargazers)

[![](https://img.shields.io/badge/Follow%20me%20on-Zhihu-blue)](https://www.zhihu.com/people/psychbruce/ "Personal profile on Zhihu.com")


## Install from GitHub
```r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("psychbruce/bruceR")
```

During installation, you might see these in console:
```
These packages have more recent versions available.
Which would you like to update?

 1: All
 2: CRAN packages only
 3: None
 ...

Enter one or more numbers, or an empty line to skip updates:
```
You may enter an **empty line** to skip updating other packages.

In a few cases, you might also see these warning messages:
```
WARNING: Rtools is required to build R packages, but is not currently installed.

Please download and install Rtools 3.5 from http://cran.r-project.org/bin/windows/Rtools/

Error in parse_repo_spec(repo) : Invalid git repo specification: 'bruceR'
```
Just download and install it. (Note: [Rtools](http://cran.r-project.org/bin/windows/Rtools/) is **not** an R package.)

For any other potential bugs during installation, please read carefully the warning messages. I believe you can solve them if you follow the instruction.


## User Guide
> *We are all standing on the shoulders of giants.*

### Loading `bruceR`
`bruceR` depends on some important packages (listed as follows) and also automatically installs many other useful packages for users. Many functions in `bruceR` are extensions of the functions in these packages.

Once you load `bruceR` to the global environment with `library()`, it will also load 6 packages most commonly used:
- `rio`: Data input/output for many file formats within one function (`import`/`export`).
- `dplyr`: Data manipulation and preprocessing.
- `stringr`: String operation with regular expressions.
- `data.table`: Enhanced 'data.frame' with higher efficiency.
- `psych`: Functions for psychological research.
- `ggplot2`: Data visualization.

No need to load them separately. You can just load `bruceR`:
```r
library(bruceR)

## overview of package
?bruceR

## check update
check_update()

## run some examples
example("Print")
example("Describe")
example("Freq")
example("Corr")
example("MEAN")
example("LOOKUP")
example("GLM_summary")
example("HLM_summary")
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
- [x] Reliability and validity analyses (e.g., Cronbach's alpha, exploratory/confirmatory factor analysis)
  + `Alpha()`
  + `EFA()`, `CFA()`
- [ ] *t*-test, ANOVA, simple-effect analysis, and multiple comparison
  + (*coming soon...*)
- [x] Advanced toolbox and output for general/generalized ordinary/multilevel linear models
  + `grand_mean_center()`, `group_mean_center()`
  + `GLM_summary()`, `HLM_summary()`, `regress()`
  + `med_mc()`, `simple_slope()`
- [x] Nice themes of `ggplot2` ready for scientific publication
  + `theme_bruce()`


## Release Notes
### Current version: `0.2.2`
### Major changes:
+ `0.2.0` - 30 August 2019
  + Added help pages
  + General bug-fixes and improvements
+ `0.1.0` - 30 June 2019
  + Initial commit


## Author
[Han-Wu-Shuang (Bruce) Bao](https://www.zhihu.com/people/psychbruce/ "Personal profile on Zhihu.com")

E-mail: baohws@psych.ac.cn or psychbruce@qq.com
