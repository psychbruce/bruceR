#### For Developer ####
# Install Package: 'Ctrl + Shift + B'
# Check Package:   'Ctrl + Shift + E'
# Test Package:    'Ctrl + Shift + T'

# devtools::check()
# devtools::spell_check()
# devtools::check_rhub()
# devtools::check_win_devel()

# devtools::release()
# devtools::submit_cran()


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
#' Install the latest \href{https://github.com/psychbruce/bruceR}{development version} by \code{devtools::install_github("psychbruce/bruceR")}
#'
#' Check updates in \href{https://github.com/psychbruce/bruceR/blob/master/NEWS.md}{Release Notes}.
#'
#' Report bugs in \href{https://github.com/psychbruce/bruceR/issues}{GitHub Issues}.
#'
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
#'       \code{\link{set.wd}} (alias: \code{set_wd})
#'
#'       \code{\link{import}},
#'       \code{\link{export}}
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
#' @note
#' Please always use \href{https://www.rstudio.com/products/rstudio/download/preview/}{RStudio}
#' as an \href{https://en.wikipedia.org/wiki/Integrated_development_environment}{IDE} instead of using the raw R software.
#'
#' @author
#' \href{https://psychbruce.github.io}{Han-Wu-Shuang (Bruce) Bao}
#'
#' E-mail: \email{baohws@@foxmail.com}
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
#' @importFrom dplyr %>% select left_join sym group_by summarise mutate across
#' @importFrom glue glue glue_col
#' @importFrom crayon bold italic underline reset blurred inverse hidden strikethrough
#' @importFrom crayon black white silver red green blue yellow cyan magenta
#' @importFrom crayon bgBlack bgWhite bgRed bgGreen bgBlue bgYellow bgCyan bgMagenta
.onAttach=function(libname, pkgname) {
  ## Version Check
  xml=suppressWarnings({
    try({
      readLines("https://cran.r-project.org/web/packages/bruceR/index.html")
    }, silent=TRUE)
  })

  ## Loaded Package
  pkgs=c(
    ## DATA ##
    "dplyr", "tidyr", "stringr", "forcats", "data.table",
    ## STAT ##
    "emmeans", "effectsize", "performance", "lmerTest",
    ## PLOT ##
    "ggplot2", "ggtext", "cowplot", "see"
  )

  # suppressWarnings({
  #   loaded=pacman::p_load(char=pkgs, character.only=TRUE, install=FALSE)
  # })
  suppressMessages({
    suppressWarnings({
      loaded=sapply(pkgs, require, character.only=TRUE)
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
    inst.ver=as.character(utils::packageVersion("bruceR"))
    Print("
    \n
    <<bold
    \ud83c\udf81 bruceR (version {inst.ver})
    <<underline BR>>oadly <<underline U>>seful <<underline C>>onvenient and <<underline E>>fficient <<underline R>> functions
    >>

    <<bold Packages also been loaded:>>
    <<blue
    <<green \u2714>> dplyr     \t<<green \u2714>> emmeans     \t<<green \u2714>> ggplot2
    <<green \u2714>> tidyr     \t<<green \u2714>> effectsize  \t<<green \u2714>> ggtext
    <<green \u2714>> stringr   \t<<green \u2714>> performance \t<<green \u2714>> cowplot
    <<green \u2714>> forcats   \t<<green \u2714>> lmerTest    \t<<green \u2714>> see
    <<green \u2714>> data.table
    >>

    <<bold Key functions of `bruceR`:>>
    <<cyan
    set_wd()      \tDescribe() \tTTEST()
    import()      \tFreq()     \tMANOVA()
    export()      \tCorr()     \tEMMEANS()
    print_table() \tAlpha()    \tPROCESS()
    MEAN()        \tEFA()      \tmodel_summary()
    LOOKUP()      \tCFA()      \tlavaan_summary()
    >>
    \n
    ")
  } else {
    packageStartupMessage(Glue("
    \n
    These R packages have not been installed:
    {paste(pkgs[loaded==FALSE], collapse=', ')}
    \n
    "))
  }

  ## Update Message
  if(!inherits(xml, "try-error")) {
    try({
      cran.ver=xml[grep("Version:", xml, fixed=TRUE)+1]
      # cran.ymd=xml[grep("Published:", xml, fixed=TRUE)+1]
      if(!is.na(cran.ver) & length(cran.ver)==1) {
        cran.ver=substr(cran.ver, 5, nchar(cran.ver)-5)
        # cran.ymd=substr(cran.ymd, 5, nchar(cran.ymd)-5)
        if(numeric_version(inst.ver)<numeric_version(cran.ver))
          packageStartupMessage(Glue("
          NEWS: An updated version of bruceR (version {cran.ver}) is available!
          Please update: install.packages(\"bruceR\")
          \n
          "))
      }
    }, silent=TRUE)
  }
}

