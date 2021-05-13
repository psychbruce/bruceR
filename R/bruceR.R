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
#'   \item \strong{\code{rio}}: Data import and export (for all file formats). (\code{\link[rio]{import}} / \code{\link[rio]{export}})
#'   \item \strong{\code{dplyr}}: Data manipulation and processing.
#'   \item \strong{\code{tidyr}}: Data cleaning and reshaping.
#'   \item \strong{\code{stringr}}: Toolbox for string operation (with regular expressions).
#'   \item \strong{\code{forcats}}: Toolbox for factor manipulation (for categorical variables).
#'   \item \strong{\code{data.table}}: Advanced \code{data.frame} with higher efficiency.
#' }
#'
#' \strong{[Stat]:}
#' \itemize{
#'   \item \strong{\code{psych}}: Toolbox for psychological and psychometric research.
#'   \item \strong{\code{emmeans}}: Toolbox for estimated marginal means and contrasts.
#'   \item \strong{\code{effectsize}}: Indices of effect size and standardized parameters.
#'   \item \strong{\code{performance}}: Assessment of regression models performance.
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
#'       \code{\link{set.wd}}
#'
#'       \code{\link{pkg_depend}},
#'       \code{\link{pkg_install_suggested}}
#'
#'       \code{\link{formatF}},
#'       \code{\link{formatN}}
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
#'       \code{\link{RESCALE}},
#'       \code{\link{RANDBETWEEN}}
#'
#'       \code{\link{LOOKUP}}
#'   }
#'
#'   \item{\strong{(3) Reliability and Factor Analyses}}{
#'       \code{\link{Alpha}}
#'
#'       \code{\link{EFA}}
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
#'   \item{\strong{(5) Multi-Factor ANOVA, Simple-Effect Analysis, and Post-Hoc Multiple Comparison}}{
#'       \code{\link{MANOVA}}
#'
#'       \code{\link{EMMEANS}}
#'   }
#'
#'   \item{\strong{(6) Tidy Report of Regression Models}}{
#'       \code{\link{model_summary}}
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
#'       \code{\link{med_summary}}
#'
#'       \code{PROCESS} (\emph{coming soon...})
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

.onAttach=function(libname, pkgname) {
  # packageStartupMessage("Welcome to my package")

  suppressMessages({
    pacman::p_load(
      ## DATA ##
      "rio", "dplyr", "tidyr", "stringr", "forcats", "data.table",
      ## STAT ##
      "psych", "emmeans", "effectsize", "performance",
      ## PLOT ##
      "ggplot2", "ggtext", "cowplot", "see")
  })

  # {rep_char('=', 56)}
  # \u2714 yes
  # \u2501 hyphen
  LOGO=c(bell="\ud83d\udd14",
         bulb="\ud83d\udca1",
         gift="\ud83c\udf81",
         bolt="\u26a1",
         star="\u2b50")
  logo=sample(LOGO, 1)
  Print("
  \n
  <<bold <<magenta
  {logo} bruceR: <<underline BR>>oadly <<underline U>>seful <<underline C>>onvenient and <<underline E>>fficient <<underline R>> functions
  >>>>

  <<bold <<blue Loaded R packages:>>>>
  <<green <<yellow [Data]:>> rio / dplyr / tidyr / stringr / forcats / data.table>>
  <<green <<yellow [Stat]:>> psych / emmeans / effectsize / performance>>
  <<green <<yellow [Plot]:>> ggplot2 / ggtext / cowplot / see>>

  <<bold <<blue Frequently used functions in `bruceR`:>>>>
  <<cyan set.wd() / Describe() / Freq() / Corr() / Alpha() / MEAN()>>
  <<cyan MANOVA() / EMMEANS() / model_summary() / theme_bruce()>>
  \n
  ")
}
