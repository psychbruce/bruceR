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

  usethis::use_logo("ignore/logo/bruceR-logo.png")
}


#### Main Description ####


#' bruceR: \strong{BR}oadly \strong{U}seful \strong{C}onvenient and \strong{E}fficient \strong{R} functions
#'
#' @description
#' \if{html}{\figure{logo.png}{options: align='right' alt='logo' width='120'}}
#'
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
#' @details
#' Loading \code{bruceR} by \code{library(bruceR)} will also load these R packages for you:
#'
#' \strong{[Data]:}
#' \itemize{
#'   \item \strong{\code{dplyr}}: Data manipulation and processing.
#'   \item \strong{\code{tidyr}}: Data cleaning and reshaping.
#'   \item \strong{\code{stringr}}: Toolbox for string operation (with regular expressions).
#'   \item \strong{\code{forcats}}: Toolbox for factor manipulation (for categorical variables).
#'   \item \strong{\code{data.table}}: Advanced \code{data.frame} with higher efficiency.
#' }
#'
#' \strong{[Stat]:}
#' \itemize{
#'   \item \strong{\code{emmeans}}: Estimates of marginal means and multiple contrasts.
#'   \item \strong{\code{effectsize}}: Estimates of effect sizes and standardized parameters.
#'   \item \strong{\code{performance}}: Estimates of regression models performance.
#'   \item \strong{\code{lmerTest}}: Tests of linear mixed effects models (LMM, also known as HLM and multilevel models).
#' }
#'
#' \strong{[Plot]:}
#' \itemize{
#'   \item \strong{\code{ggplot2}}: Data visualization.
#'   \item \strong{\code{ggtext}}: Markdown/HTML rich text format for \code{ggplot2} (geoms and themes).
#'   \item \strong{\code{cowplot}}: Advanced toolbox for \code{ggplot2} (arrange multiple plots and add labels).
#'   \item \strong{\code{see}}: Advanced toolbox for \code{ggplot2} (geoms, scales, themes, and color palettes).
#' }
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
#' @author
#' \href{https://psychbruce.github.io}{Han-Wu-Shuang (Bruce) Bao}
#'
#' @docType package
#' @name bruceR-package
#' @aliases bruceR
NULL


#### Package-Loading Information ####


#' @import stringr
#' @import ggplot2
#' @importFrom stats sd var cor median na.omit complete.cases
#' @importFrom stats p.adjust pnorm pt pf pchisq qnorm qt quantile rnorm anova update terms drop1
#' @importFrom stats lm coef confint residuals df.residual sigma as.formula terms.formula model.response model.frame
#' @importFrom dplyr %>% select left_join sym group_by summarise mutate transmute across
#' @importFrom data.table data.table is.data.table as.data.table
#' @importFrom data.table := .BY .EACHI .GRP .I .N .NGRP .SD
#' @importFrom glue glue glue_col
#' @importFrom crayon bold italic underline reset blurred inverse hidden strikethrough
#' @importFrom crayon black white silver red green blue yellow cyan magenta
#' @importFrom crayon bgBlack bgWhite bgRed bgGreen bgBlue bgYellow bgCyan bgMagenta
.onAttach = function(libname, pkgname) {
  ## Version Check
  inst.ver = as.character(utils::packageVersion("bruceR"))
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
          packageStartupMessage(Glue("
          \n
          NEWS: A new version of bruceR (version {cran.ver}) is available on {cran.ymd}!
          Please update:
          install.packages(\"bruceR\")
          update.packages(ask=FALSE)
          "))
      }
    }, silent=TRUE)
  }

  ## Loaded Package
  pkgs = c(
    ## DATA ##
    "dplyr", "tidyr", "stringr", "forcats", "data.table",
    ## STAT ##
    "emmeans", "effectsize", "performance", "lmerTest",
    ## PLOT ##
    "ggplot2", "ggtext", "cowplot", "see"
  )

  # suppressWarnings({
  #   loaded = pacman::p_load(char=pkgs, character.only=TRUE, install=FALSE)
  # })
  suppressMessages({
    suppressWarnings({
      loaded = sapply(pkgs, require, character.only=TRUE)
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
    Print("
    \n
    <<bold bruceR (version {inst.ver})>>
    <<blue <<underline BR>>oadly <<underline U>>seful <<underline C>>onvenient and <<underline E>>fficient <<underline R>> functions>>

    <<bold Packages also loaded:>>
    <<green
    \u221a dplyr     \t\u221a emmeans     \t\u221a ggplot2
    \u221a tidyr     \t\u221a effectsize  \t\u221a ggtext
    \u221a stringr   \t\u221a performance \t\u221a cowplot
    \u221a forcats   \t\u221a lmerTest    \t\u221a see
    \u221a data.table
    >>

    <<bold Main functions of `bruceR`:>>
    <<cyan
    cc()          \tDescribe() \tTTEST()
    add()         \tFreq()     \tMANOVA()
    .mean()       \tCorr()     \tEMMEANS()
    set.wd()      \tAlpha()    \tPROCESS()
    import()      \tEFA()      \tmodel_summary()
    print_table() \tCFA()      \tlavaan_summary()
    >>

    https://psychbruce.github.io/bruceR/
    \n
    ")
  } else {
    packageStartupMessage(Glue("
    \n
    These R packages are not installed:
    {paste(pkgs[loaded==FALSE], collapse=', ')}

    Please install them.
    \n
    "))
  }
}

