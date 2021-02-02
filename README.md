# bruceR

**BR**oadly **U**seful **C**ollections and **E**xtensions of **R** functions

![](https://img.shields.io/badge/R-package-success)
![](https://img.shields.io/badge/Version-0.5.6-success)
![](https://img.shields.io/github/license/psychbruce/bruceR?label=License&color=success)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://img.shields.io/github/stars/psychbruce/bruceR?style=social)](https://github.com/psychbruce/bruceR/stargazers)

<a href="https://en.wikipedia.org/wiki/Creative_Commons_license"><img src="https://s1.ax1x.com/2020/07/28/aAjUJg.jpg" width="120px" height="42px"></a>

- 复制、修改、使用、分享本代码库，必须遵守<b>「创作共用许可协议 CC BY-NC-SA」（原作者署名-非商业用途使用-相同方式共享）</b>


## Author

[包寒吴霜 \| Bao H.-W.-S.](https://psychbruce.github.io)

E-mail: [baohws@foxmail.com](mailto:baohws@foxmail.com)

Website: [psychbruce.github.io](https://psychbruce.github.io)

[ResearchGate](https://www.researchgate.net/profile/Han_Wu_Shuang_Bao) |
[GitHub](https://github.com/psychbruce) |
[知乎](https://www.zhihu.com/people/psychbruce)


## Citation

Bao, H.-W.-S. (2020). bruceR: Broadly useful collections and extensions of R functions [R package]. https://github.com/psychbruce/bruceR


## User Guide

### Install
```r
#### 安装步骤 ####

## 第1步：安装基础R包（如果已经安装过，请忽略）
install.packages("devtools")
install.packages("tidyverse")
install.packages("ggstatsplot")

## 第2步：更新所有R包（为了避免第3步频繁出错）
update.packages(ask=F)

## 第3步：安装bruceR包
devtools::install_github("psychbruce/bruceR")
```
- Please DO NOT use `install.packages("bruceR")`, because this package HAS NOT BEEN put on the [CRAN](https://cran.r-project.org/).
- [Installation Bugs and Solutions (安装遇到问题时请阅读！)](https://github.com/psychbruce/bruceR/blob/master/Installation%20Bugs%20and%20Solutions.md)


### Package dependency

`bruceR` depends on some important packages and also automatically installs many other useful packages for users. Many functions in `bruceR` are extensions of the functions in these packages.

Once you load `bruceR` to the global environment with `library()`, it will also load these packages:
- `rio`: Data input/output for all file formats within one function (`import`/`export`).
- `dplyr`: Data manipulation and preprocessing.
- `stringr`: String operations and regular expressions.
- `data.table`: Advanced 'data.frame' for higher efficiency.
- `psych`: Toolbox for psychological and psychometric research.
- `performance`: Checking model performance.
- `ggplot2`: Data visualization.
- `cowplot`: Advanced toolbox for ggplot2.

No need to load each one with its own call. Loading `bruceR` is enough.


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
- [x] Analysis of variance, simple-effect analyses, and multiple comparisons
  + `MANOVA()`, `EMMEANS()`
- [x] Advanced toolbox and output for regression models
  + `grand_mean_center()`, `group_mean_center()`
  + `model_summary()`, `GLM_summary()`, `HLM_summary()`, `regress()`
  + `med_mc()`, `simple_slope()`
- [x] Themes for `ggplot2`
  + `theme_bruce()`


### Learn more from help pages
```r
library(bruceR)

## Overview
?bruceR

## See help pages of functions
## (use `?function` or `help(function)`)
?MANOVA
?EMMEANS
?model_summary
?GLM_summary
?HLM_summary
...
```


## Release Notes

### Current version: `0.5.6`
### Major changes:
- `0.5.0` - 2020.08
  + Automatically checking updates when `library(bruceR)`
  + Requiring R version 4.0+
  + Improved many functions
  + Fixed many bugs
- `0.4.0` - 2019.12
  + Added citation information
  + General bug-fixes and improvements
- `0.3.0` - 2019.10
  + Added new functions for ANOVA, simple-effect analyses, and multiple comparisons
  + General bug-fixes and improvements
- `0.2.0` - 2019.08
  + Added help pages
  + General bug-fixes and improvements
- `0.1.0` - 2019.06
  + Initial commit
