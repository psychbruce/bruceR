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


#### Main Description ####


#' bruceR: \strong{BR}oadly \strong{U}seful \strong{C}onvenient and \strong{E}fficient \strong{R} functions
#'
#' @description
#' \strong{BR}oadly
#' \strong{U}seful
#' \strong{C}onvenient
#' and
#' \strong{E}fficient
#' \strong{R}
#' functions that
#' \strong{BR}ing
#' \strong{U}sers
#' \strong{C}oncise
#' and
#' \strong{E}legant
#' \strong{R}
#' data analyses.
#'
#' Package homepage: \url{https://psychbruce.github.io/bruceR/}
#'
#' Install the latest \href{https://github.com/psychbruce/bruceR}{development version}
#' from GitHub:
#' \code{devtools::install_github("psychbruce/bruceR")}
#'
#' Report bugs at \href{https://github.com/psychbruce/bruceR/issues}{GitHub Issues}.
#'
#' @section Main Functions in \code{bruceR}:
#'
#' \describe{
#'   \item{\strong{(1) Basic R Programming}}{
#'       \code{\link{set.wd}} (alias: \code{\link{set_wd}})
#'
#'       \code{\link{import}},
#'       \code{\link{export}}
#'
#'       \code{\link{cc}}
#'
#'       \code{\link{pkg_depend}},
#'       \code{\link{pkg_install_suggested}}
#'
#'       \code{\link{formatF}},
#'       \code{\link{formatN}}
#'
#'       \code{\link{print_table}}
#'
#'       \code{\link{Print}},
#'       \code{\link{Glue}},
#'       \code{\link{Run}}
#'
#'       \code{\link{\%^\%}}
#'
#'       \code{\link{\%notin\%}}
#'
#'       \code{\link{\%allin\%}},
#'       \code{\link{\%anyin\%}},
#'       \code{\link{\%nonein\%}},
#'       \code{\link{\%partin\%}}
#'   }
#'
#'   \item{\strong{(2) Multivariate Computation}}{
#'       \code{\link{add}},
#'       \code{\link{added}}
#'
#'       \code{\link{.sum}},
#'       \code{\link{.mean}}
#'
#'       \code{\link{SUM}},
#'       \code{\link{MEAN}},
#'       \code{\link{STD}},
#'       \code{\link{MODE}},
#'       \code{\link{COUNT}},
#'       \code{\link{CONSEC}}
#'
#'       \code{\link{RECODE}},
#'       \code{\link{RESCALE}}
#'
#'       \code{\link{LOOKUP}}
#'   }
#'
#'   \item{\strong{(3) Reliability and Factor Analyses}}{
#'       \code{\link{Alpha}}
#'
#'       \code{\link{EFA}} / \code{\link{PCA}}
#'
#'       \code{\link{CFA}}
#'   }
#'
#'   \item{\strong{(4) Descriptive Statistics and Correlation Analyses}}{
#'       \code{\link{Describe}}
#'
#'       \code{\link{Freq}}
#'
#'       \code{\link{Corr}}
#'
#'       \code{\link{cor_diff}}
#'
#'       \code{\link{cor_multilevel}}
#'   }
#'
#'   \item{\strong{(5) T-Test, Multi-Factor ANOVA, Simple-Effect Analysis, and Post-Hoc Multiple Comparison}}{
#'       \code{\link{TTEST}}
#'
#'       \code{\link{MANOVA}}
#'
#'       \code{\link{EMMEANS}}
#'   }
#'
#'   \item{\strong{(6) Tidy Report of Regression Models}}{
#'       \code{\link{model_summary}}
#'
#'       \code{\link{lavaan_summary}}
#'
#'       \code{\link{GLM_summary}}
#'
#'       \code{\link{HLM_summary}}
#'
#'       \code{\link{HLM_ICC_rWG}}
#'
#'       \code{\link{regress}}
#'   }
#'
#'   \item{\strong{(7) Mediation and Moderation Analyses}}{
#'       \code{\link{PROCESS}}
#'
#'       \code{\link{med_summary}}
#'   }
#'
#'   \item{\strong{(8) Additional Toolbox for Statistics and Graphics}}{
#'       \code{\link{grand_mean_center}}
#'
#'       \code{\link{group_mean_center}}
#'
#'       \code{\link{ccf_plot}}
#'
#'       \code{\link{granger_test}}
#'
#'       \code{\link{granger_causality}}
#'
#'       \code{\link{theme_bruce}}
#'
#'       \code{\link{show_colors}}
#'   }
#' }
#'
"_PACKAGE"


#### Package-Loading Information ####


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
  ## Version Check
  new = FALSE
  inst.ver = as.character(utils::packageVersion("bruceR"))
  pkg.date = substr(utils::packageDate("bruceR"), 1, 4)
  xml = suppressWarnings({
    try({
      readLines("https://cran.r-project.org/web/packages/bruceR/index.html")
    }, silent=TRUE)
  })

  ## Update Message
  if(!inherits(xml, "try-error")) {
    try({
      cran.ver = xml[grep("Version:", xml, fixed=TRUE) + 1]
      cran.ymd = xml[grep("Published:", xml, fixed=TRUE) + 1]
      if(!is.na(cran.ver) & length(cran.ver)==1) {
        cran.ver = substr(cran.ver, 5, nchar(cran.ver) - 5)
        cran.ymd = substr(cran.ymd, 5, nchar(cran.ymd) - 5)
        if(numeric_version(inst.ver) < numeric_version(cran.ver))
          new = TRUE
      }
    }, silent=TRUE)
  }

  ## Loaded Package
  pkgs = c(
    "dplyr", "tidyr", "stringr", "data.table",
    "emmeans", "effectsize", "performance", "lmerTest", "interactions",
    "ggplot2"
  )

  suppressMessages({
    suppressWarnings({
      loaded = sapply(pkgs, require, character.only=TRUE, exclude="%notin%")
    })
  })

  ## Welcome Message
  if(all(loaded)) {
    # LOGO=c(bell="\ud83d\udd14",
    #        bulb="\ud83d\udca1",
    #        gift="\ud83c\udf81",
    #        bolt="\u26a1",
    #        star="\u2b50")
    # logo=sample(LOGO, 1)
    # yes: \u2714 \u221a
    # star: \u2605
    packageStartupMessage(glue::glue_col("

    {magenta bruceR (v{inst.ver})}
    {blue Broadly Useful Convenient and Efficient R functions}

    {magenta Packages also loaded:}
    {green
    \u2714 data.table\t\u2714 emmeans
    \u2714 dplyr     \t\u2714 lmerTest
    \u2714 tidyr     \t\u2714 effectsize
    \u2714 stringr   \t\u2714 performance
    \u2714 ggplot2   \t\u2714 interactions
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
    Bao, H.-W.-S. ({pkg.date}). "),
    glue::glue_col("{italic bruceR: Broadly useful convenient and efficient R functions}"),
    glue::glue_col(" (Version {inst.ver}) [Computer software]. "),
    glue::glue_col("{underline https://CRAN.R-project.org/package=bruceR}"),
    "\n")
  } else {
    packageStartupMessage(glue::glue_col("

    These R packages have not been installed:
    {paste(pkgs[loaded==FALSE], collapse=', ')}

    Please install them.

    "))
  }

  ## Update Info
  if(new)
    packageStartupMessage(glue::glue_col("

    NEWS: A new version of bruceR ({cran.ver}) is available ({cran.ymd})!

    ***** Please update *****
    install.packages(\"bruceR\", dep=TRUE)

    "))

  ## Check Dependencies
  try({ check_depend("bruceR") }, silent=TRUE)
}


check_depend = function(pkg) {
  pkgs = utils::installed.packages()
  deps = cc(pkgs[pkg, "Imports"], pkgs[pkg, "Suggests"])
  need = deps[deps %notin% pkgs]
  if(length(need)>0)
    packageStartupMessage(glue::glue_col("

    These packages are dependencies of `{pkg}` but not installed:
    - {paste(need, collapse=', ')}

    ***** Install all dependencies *****
    install.packages(\"{pkg}\", dep=TRUE)

    "))
}

