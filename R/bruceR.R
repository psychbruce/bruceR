#### For Developer ####

# Install Package: 'Ctrl + Shift + B'
# Check Package:   'Ctrl + Shift + E'
# Test Package:    'Ctrl + Shift + T'

if(FALSE) {
  devtools::check()
  devtools::spell_check()
  devtools::check_rhub()
  devtools::check_mac_release()
  devtools::check_win_release()
  devtools::check_win_devel()

  devtools::release()
  devtools::submit_cran()

  usethis::use_data_table()

  usethis::use_github_actions()  # R-CMD-check

  ## Build Site on GitHub
  usethis::use_pkgdown()
  # usethis::use_pkgdown_github_pages()  # with a bug!
  # usethis::use_github_pages()  # with a bug!
  usethis::use_github_action("pkgdown")  # GitHub Pages
  # Set "GitHub Pages" after pushing the above changes:
  # Branch: gh-pages, / (root)
  # Push another change in main branch
  # Done

  ## Build Site Locally
  pkgdown::build_site()
  pkgdown::init_site()
  pkgdown::build_home_index()

  usethis::use_coverage()  # Test Coverage
  usethis::use_github_action("test-coverage")  # Test Coverage

  ## Logo & Favicon
  usethis::use_logo("ignore/logo/bruceR-logo.png")
  pkgdown::build_favicons()  # pkgdown/favicon
  # pkgdown should not be included in .gitignore
}


#### Package Description ####


#' bruceR: **BR**oadly **U**seful **C**onvenient and **E**fficient **R** functions
#'
#' @details
#'
#' # Main Functions in `bruceR`
#'
#' ## 1. Basic R Programming
#'
#' - [set.wd()] (alias: [set_wd()])
#' - [import()]
#' - [export()]
#' - [cc()]
#' - [pkg_depend()]
#' - [formatF()]
#' - [formatN()]
#' - [print_table()]
#' - [Print()]
#' - [Glue()]
#' - [Run()]
#' - [`%^%`]
#' - [`%notin%`]
#' - [`%allin%`]
#' - [`%anyin%`]
#' - [`%nonein%`]
#' - [`%partin%`]
#'
#' ## 2. Multivariate Computation
#'
#' - [add()]
#' - [added()]
#' - [.sum()]
#' - [.mean()]
#' - [SUM()]
#' - [MEAN()]
#' - [STD()]
#' - [MODE()]
#' - [COUNT()]
#' - [CONSEC()]
#' - [RECODE()]
#' - [RESCALE()]
#' - [LOOKUP()]
#'
#' ## 3. Reliability and Factor Analyses
#'
#' - [Alpha()]
#' - [EFA()]
#' - [PCA()]
#' - [CFA()]
#'
#' ## 4. Descriptive Statistics and Correlation Analyses
#'
#' - [Describe()]
#' - [Freq()]
#' - [Corr()]
#' - [cor_diff()]
#' - [cor_multilevel()]
#'
#' ## 5. T-Test, Multi-Factor ANOVA, Simple-Effect Analysis, and Post-Hoc Multiple Comparison
#'
#' - [TTEST()]
#' - [MANOVA()]
#' - [EMMEANS()]
#'
#' ## 6. Tidy Report of Regression Models
#'
#' - [model_summary()]
#' - [lavaan_summary()]
#' - [GLM_summary()]
#' - [HLM_summary()]
#' - [HLM_ICC_rWG()]
#' - [regress()]
#'
#' ## 7. Mediation and Moderation Analyses
#'
#' - [PROCESS()]
#' - [med_summary()]
#' - [lavaan_summary()]
#'
#' ## 8. Additional Toolbox for Statistics and Graphics
#'
#' - [grand_mean_center()]
#' - [group_mean_center()]
#' - [ccf_plot()]
#' - [granger_test()]
#' - [granger_causality()]
#' - [theme_bruce()]
#' - [show_colors()]
#'
## @keywords internal
"_PACKAGE"


#### Package Loading Information ####


#' @import stringr
#' @import ggplot2
#' @importFrom stats sd var cor median na.omit complete.cases
#' @importFrom stats p.adjust pnorm pt pf pchisq qnorm qt quantile rnorm anova update terms drop1
#' @importFrom stats lm coef confint residuals df.residual sigma
#' @importFrom stats as.formula terms.formula model.response model.frame
#' @importFrom dplyr %>% select left_join sym group_by summarise mutate transmute across rename
#' @importFrom data.table data.table is.data.table as.data.table
#' @importFrom data.table := .BY .EACHI .GRP .I .N .NGRP .SD
#' @importFrom crayon bold italic underline inverse hidden
#' @importFrom crayon black red green yellow blue magenta cyan white silver
.onAttach = function(libname, pkgname) {
  inst.ver = as.character(utils::packageVersion("bruceR"))
  pkgs = c(
    "dplyr",
    "tidyr",
    "stringr",
    "forcats",
    "data.table",
    "emmeans",
    "effectsize",
    "performance",
    "lmerTest",
    "interactions",
    "ggplot2",
    "cowplot"
  )
  suppressMessages({
    suppressWarnings({
      loaded = sapply(pkgs, require, character.only=TRUE)
    })
  })
  if(all(loaded)) {
    # LOGO=c(bell="\ud83d\udd14",
    #        bulb="\ud83d\udca1",
    #        gift="\ud83c\udf81",
    #        bolt="\u26a1",
    #        star="\u2b50")
    # logo=sample(LOGO, 1)
    # yes: \u2714 \u221a
    # star: \u2605
    packageStartupMessage(
    glue::glue_col("

    {magenta bruceR (v{inst.ver})}
    {blue Broadly Useful Convenient and Efficient R functions}

    {magenta Packages also loaded:}
    {green
    \u2714 dplyr     \t\u2714 data.table
    \u2714 tidyr     \t\u2714 emmeans
    \u2714 stringr   \t\u2714 lmerTest
    \u2714 forcats   \t\u2714 effectsize
    \u2714 ggplot2   \t\u2714 performance
    \u2714 cowplot   \t\u2714 interactions
    }

    {magenta Main functions of `bruceR`:}
    {cyan
    cc()          \tDescribe() \tTTEST()
    add()         \tFreq()     \tMANOVA()
    .mean()       \tCorr()     \tEMMEANS()
    set.wd()      \tAlpha()    \tPROCESS()
    import()      \tEFA()      \tmodel_summary()
    print_table() \tCFA()      \tlavaan_summary()
    }

    {magenta For full functionality, please install all dependencies:}
    install.packages(\"bruceR\", dep=TRUE)

    {magenta Online documentation:}
    {underline https://psychbruce.github.io/bruceR}

    {magenta To use this package in publications, please cite:}
    Bao, H. W. S. (2021). {italic bruceR: Broadly useful convenient and efficient R functions} (Version {inst.ver}) [Computer software]. {underline https://doi.org/10.32614/CRAN.package.bruceR}

    "))
  } else {
    packageStartupMessage(
    glue::glue_col("

    These R packages have not been installed:
    {paste(pkgs[loaded==FALSE], collapse=', ')}

    ***** Please Install All Dependencies *****
    install.packages(\"bruceR\", dep=TRUE)

    "))
  }

  ## Check Dependencies
  try({ check_depend("bruceR") }, silent=TRUE)
}


check_depend = function(pkg) {
  pkgs = utils::installed.packages()
  deps = cc(pkgs[pkg, "Imports"], pkgs[pkg, "Suggests"])
  need = deps[deps %notin% pkgs]
  if(length(need) > 0)
    packageStartupMessage(
    glue::glue_col("

    These packages are dependencies but not yet installed:
    - {paste(need, collapse=', ')}

    ***** Install All Dependencies *****
    install.packages(\"{pkg}\", dep=TRUE)

    "))
}

