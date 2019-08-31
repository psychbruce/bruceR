# bruceR
**`R package`**

**BR**oadly **U**seful **C**ollections and **E**xtensions of **R** functions


## Install
```r
install.packages("devtools")
devtools::install_github("psychbruce/bruceR")
```

You might see these messages in console:
```
These packages have more recent versions available.
Which would you like to update?

 1: All
 2: CRAN packages only
 3: None
 ...

Enter one or more numbers, or an empty line to skip updates:
```
Just type **`3`** (i.e., not to update dependency packages).


## User Guide
> *We are all standing on the shoulders of giants.*

### Loading `bruceR`
`bruceR` depends on some important packages (listed as follows) and also automatically installs many other useful packages for users. Many functions in `bruceR` are extensions of the functions in these packages.

Once you load `bruceR` to the global environment with `library()`, it will also load 7 packages most commonly used:
- `rio`: Data input/output for many file formats within one function.
- `glue`: Paste and print in an elegant manner.
- `dplyr`: Data manipulation and preprocessing.
- `stringr`: String operation and regular expression.
- `data.table`: Enhanced 'data.frame' with higher efficiency.
- `psych`: Functions for psychological research.
- `ggplot2`: Data visualization.

No need to load them separately. You can just load `bruceR`:
```r
library(bruceR)

## overview of package
?bruceR

## run some examples
example("Print")
example("MEAN")
example("LOOKUP")
example("GLM_summary")
example("HLM_summary")
```

### Main functions in `bruceR`
- [x] Basic use and analyses (e.g., correlation matrix with plot)
  + `Print()`, `set.wd()`, `dtime()`, `%notin%`, `%partin%`, ...
  + `Describe()`, `Freq()`, `Corr()`, `CI()`, ...
- [x] Multivariate computing (e.g., scale mean score with reverse scoring)
  + `SUM()`, `MEAN()`, `RECODE()`, `RESCALE()`, `LOOKUP()`, ...
- [x] Reliability and validity analyses (e.g., Cronbach's alpha, exploratory/confirmatory factor analysis)
  + `Alpha()`, `EFA()`, `CFA()`
- [ ] *t*-test, ANOVA, simple-effect analysis, and multiple comparison
  + (*coming soon...*)
- [x] Advanced toolbox and output for general/generalized ordinary/multilevel linear models
  + `grand_mean_center()`, `group_mean_center()`, ...
  + `GLM_summary()`, `HLM_summary()`, `regress()`, ...
- [x] Nice themes of `ggplot2` ready for scientific publication
  + `theme_bruce()`


## Release Notes
### Current version: `0.2.0`
### Major changes:
+ `0.2.0` - 30 August 2019
  + Added help pages
  + General bug-fixes and improvements
+ `0.1.0` - 30 June 2019
  + Initial commit


## Author
[Han-Wu-Shuang (Bruce) Bao](https://www.zhihu.com/people/psychbruce/ "Personal profile on Zhihu.com")

E-mail: baohws@psych.ac.cn; psychbruce@qq.com
