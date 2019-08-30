# bruceR
**`R package`**
**BR**oadly **U**seful **C**ollections and **E**xtensions of **R** functions

## Install
```r
install.packages("devtools")
devtools::install_github("psychbruce/bruceR")
```

## User guide
`bruceR` helps users work with R more efficiently.

Once you load bruceR, it will also load the following packages that are most commonly used:
- **rio**: Data input/output for many file formats within one function.
- **glue**: Paste and print in an elegant manner.
- **dplyr**: Data manipulation and preprocessing.
- **stringr**: String operation and regular expression.
- **data.table**: Enhanced 'data.frame' with higher efficiency.
- **psych**: Functions for psychological research.
- **ggplot2**: Data visualization.
No need to load them using `library()`. You can just load bruceR: `library(bruceR)`

## Release notes
### Current version: 0.2.0
### Major changes:
#### 0.2.0 - 30 August 2019
- Added help pages.
- General bug-fixes and improvements.
#### 0.1.0 - 30 June 2019
- Initial commit.

## Functions
- [x] Basic use and analyses (e.g., correlation matrix with plot)
- [x] Multivariate computing (e.g., scale mean score with reverse scoring)
- [x] Reliability and validity analyses (e.g., Cronbach's alpha, EFA, CFA)
- [ ] *t*-test, ANOVA, simple-effect analysis, and multiple comparison (*coming soon...*)
- [x] Advanced toolbox and output for general/generalized ordinary/multilevel linear models
- [x] Nice themes of `ggplot2` ready for scientific publication
