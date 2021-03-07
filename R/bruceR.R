#### For Developer ####
# Install Package: 'Ctrl + Shift + B'
# Check Package:   'Ctrl + Shift + E'
# Test Package:    'Ctrl + Shift + T'

#### For User ####
# devtools::install_github("psychbruce/bruceR")
# remove.packages("bruceR")

#### Version Control ####
# DESCRIPTION, README.md


#### Main Description ####
#' bruceR: \strong{BR}oadly \strong{U}seful \strong{C}ollections and \strong{E}xtensions of \strong{R} functions
#'
#' @description
#' Packing many useful R packages and functions into one package.
#' Making data analyses and statistics more elegant.
#'
#' Check updates on \url{https://github.com/psychbruce/bruceR}
#'
#' Once you load \code{bruceR} with \code{library()}, it will also load these packages:
#'
#' \itemize{
#'   \item \strong{\code{rio}}: Data input and output (for all file formats within one function). (see \code{\link[rio]{import}} / \code{\link[rio]{export}})
#'   \item \strong{\code{dplyr}}: Data manipulation and preprocessing.
#'   \item \strong{\code{tidyr}}: Data cleaning and reshaping.
#'   \item \strong{\code{stringr}}: Toolbox for string operation (with regular expressions).
#'   \item \strong{\code{forcats}}: Toolbox for factor manipulation (for categorical variables).
#'   \item \strong{\code{data.table}}: Advanced 'data.frame' with higher efficiency.
#'   \item \strong{\code{psych}}: Toolbox for psychological and psychometric research.
#'   \item \strong{\code{emmeans}}: Toolbox for estimated marginal means and contrasts.
#'   \item \strong{\code{lmerTest}}: Tests for linear mixed effects models.
#'   \item \strong{\code{effectsize}}: Indices of effect size and standardized parameters.
#'   \item \strong{\code{performance}}: Assessment of regression models performance. (see \code{\link[performance]{model_performance}})
#'   \item \strong{\code{ggplot2}}: Data visualization.
#'   \item \strong{\code{ggstatsplot}}: Extension of 'ggplot2' with statistical details.
#'   \item \strong{\code{cowplot}}: Advanced toolbox for 'ggplot2' (arrange multiple plots and add labels).
#'   \item \strong{\code{ggthemes}}: Advanced toolbox for 'ggplot2' (extra geoms, scales, themes, and color palettes).
#'   \item \strong{\code{see}}: Advanced toolbox for 'ggplot2' (extra geoms, scales, themes, and color palettes).
#' }
#'
#' Loading \code{bruceR} is enough:
#' \strong{\code{library(bruceR)}}
#'
#' @section Main Functions in \code{bruceR}:
#' \code{bruceR} includes functions for
#' \strong{(1)} basic use and analyses (e.g., correlation matrix with plot),
#' \strong{(2)} multivariate computation (e.g., scale mean score with reverse scoring),
#' \strong{(3)} reliability and validity analyses (e.g., Cronbach's \eqn{\alpha}, EFA, CFA),
#' \strong{(4)} MANOVA, simple-effect analysis, and multiple comparison,
#' \strong{(5)} advanced toolbox and output for regression models, and
#' \strong{(6)} nice themes for \code{ggplot2}.
#'
#' \describe{
#'   \item{\strong{(1) Basic Use and Analyses}}{
#'       \code{\link{Print}}  (print texts to console with rich formats and colors)
#'
#'       \code{\link{Describe}}  (descriptive statistics)
#'
#'       \code{\link{Freq}}  (frequency statistics with histogram plot)
#'
#'       \code{\link{Corr}}  (correlation analysis with correlation-matrix plot)
#'
#'       \code{\link{p}}  (compute \emph{p}-values from statistics: \emph{z, t, F, r, chi2})
#'
#'       \code{\link{set.wd}}  (a simple extension of \code{\link{setwd}})
#'
#'       \code{\link{pkg_depend}}  (check package dependencies)
#'
#'       \code{\link{dtime}}  (compute time difference)
#'
#'       \code{\link{\%notin\%}}  (the reverse of \code{\%in\%}, return a logical vector specifying values not in a table)
#'
#'       \code{\link{\%allin\%}}  (return whether all Xs are in a vector)
#'
#'       \code{\link{\%anyin\%}}  (return whether any of Xs is in a vector)
#'
#'       \code{\link{\%nonein\%}}  (return whether none of Xs is in a vector)
#'
#'       \code{\link{\%partin\%}}  (use regular expression to judge whether a pattern exists in a vector)
#'
#'       \code{\link{RANDBETWEEN}}  (random sampling, like Excel's function \code{RANDBETWEEN})
#'
#'       \code{\link{LOOKUP}}  (search, match, and look up values, like Excel's functions \code{INDEX + MATCH})
#'   }
#'   \item{\strong{(2) Multivariate Computation}}{
#'       \code{\link{RECODE}}  (recode a variable)
#'
#'       \code{\link{RESCALE}}  (rescale a variable; e.g., from 5-point to 7-point scale)
#'
#'       \code{\link{COUNT}}  (count values across variables)
#'
#'       \code{\link{MODE}}  (compute multivariate mode)
#'
#'       \code{\link{SUM}}  (compute multivariate sum)
#'
#'       \code{\link{MEAN}}  (compute multivariate mean)
#'
#'       \code{\link{STD}}  (compute multivariate standard deviation)
#'
#'       \code{\link{CONSEC}}  (count "consecutive identical digits" across variables)
#'   }
#'   \item{\strong{(3) Reliability and Validity Analyses}}{
#'       \code{\link{Alpha}}  (reliability analysis, Cronbach's \eqn{\alpha})
#'
#'       \code{\link{EFA}}  (exploratory factor analysis)
#'
#'       \code{\link{CFA}}  (confirmatory factor analysis)
#'   }
#'   \item{\strong{(4) MANOVA, Simple-Effect Analysis, & Multiple Comparison}}{
#'       \code{\link{MANOVA}}  (between-subjects, within-subjects, and mixed design ANOVA)
#'
#'       \code{\link{EMMEANS}}  (simple-effect analysis and post-hoc multiple comparison)
#'   }
#'   \item{\strong{(5) Advanced Toolbox for Statistics}}{
#'       \code{\link{grand_mean_center}}  (center variable(s) on \strong{grand} mean(s))
#'
#'       \code{\link{group_mean_center}}  (center variable(s) on \strong{group} mean(s))
#'
#'       \code{\link{regress}}  (fast perform OLS, logistic, poisson, and multilevel regression analyses)
#'
#'       \code{\link{GLM_summary}}  (advanced report of general/generalized linear models)
#'
#'       \code{\link{HLM_summary}}  (advanced report of multilevel/hierarchical linear models)
#'
#'       \code{\link{model_summary}}  (tidy report of regression models)
#'
#'       \code{\link{med_summary}}  (tidy report of mediation analyses based on the \code{mediation} package)
#'
#'       \code{\link{ccf_plot}}  (cross-correlation analysis with a \code{ggplot2} plot)
#'
#'       \code{\link{granger_test}}  (Granger causality test based on the \code{lmtest::\link[lmtest]{grangertest}} function)
#'   }
#'   \item{\strong{(6) Theme for \code{ggplot2}}}{
#'       \code{\link{theme_bruce}}  (a set of nice themes for scientific publication)
#'   }
#' }
#'
#' @note
#' Please always use \href{https://www.rstudio.com/products/rstudio/download/preview/}{RStudio} as an \href{https://en.wikipedia.org/wiki/Integrated_development_environment}{IDE} instead of using the raw R software.
#'
#' The "truly" latest version of \href{https://www.rstudio.com/products/rstudio/download/preview/}{RStudio} can be accessed from \url{https://www.rstudio.com/products/rstudio/download/preview/}
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

  # options(contrasts=c("contr.sum", "contr.poly"))
  # message("Contrasts have been changed to the orthogonal 'sum-to-zero' contrasts.")

  suppressMessages({
    library(MASS)  # to prevent `dplyr::select` being masked by `MASS::select`

    library(rio)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(forcats)
    library(data.table)

    library(psych)
    library(emmeans)
    library(lmerTest)
    library(effectsize)
    library(performance)

    library(ggplot2)
    library(ggstatsplot)
    library(cowplot)
    library(ggthemes)
    library(see)
  })

  user.ver=curr.ver=as.character(packageVersion("bruceR"))
  try({
    curr.ver=rvest::html_text(rvest::html_node(xml2::read_html("https://github.com/psychbruce/bruceR"), "h2+ h3 code"))
  }, silent=T)

  if(user.ver==curr.ver) {
    update_msg="\n"
  } else {
    update_msg=Glue("
    \n
    <<bold <<red \u26a0 NEWS: A new version of 'bruceR' (<<underline v{curr.ver}>>) is now available!>>>>
    <<bold <<green \u25b6 Run this to update:>>>>
    devtools::install_github(\"psychbruce/bruceR\")
    \n\n\n")
  }

  # {rep_char('=', 56)}
  # bell: \ud83d\udd14
  # bulb: \ud83d\udca1
  # gift: \ud83c\udf81
  # bolt: \u26a1
  # star: \u2b50
  LOGO=c(bell="\ud83d\udd14",
         bulb="\ud83d\udca1",
         gift="\ud83c\udf81",
         bolt="\u26a1",
         star="\u2b50")
  logo=sample(LOGO, 1)
  Print("
  \n
  <<bold <<magenta
  {logo} bruceR: <<underline BR>>oadly <<underline U>>seful <<underline C>>ollections and <<underline E>>xtensions of <<underline R>> functions
  >>>>

  <<bold <<blue Loaded R packages:>>>>
  <<green \u2714 <<yellow [Data]:>> rio / dplyr / tidyr / stringr / forcats / data.table>>
  <<green \u2714 <<yellow [Stat]:>> psych / emmeans / lmerTest / effectsize / performance>>
  <<green \u2714 <<yellow [Plot]:>> ggplot2 / ggstatsplot / cowplot / ggthemes / see>>

  <<bold <<blue Suggested R packages:>>>>
  <<green \u2501 mediation / interactions / processR / lavaan / metafor / apaTables>>

  <<bold <<blue Frequently used functions in `bruceR`:>>>>
  <<cyan \u2501 Describe() / Freq() / Corr() / Alpha() / SUM() / MEAN()>>
  <<cyan \u2501 MANOVA() / EMMEANS() / model_summary() / theme_bruce()>>

  <<bold <<blue Citation:>>>>
  Bao, H.-W.-S. (2021). bruceR: Broadly useful collections and extensions of R functions [R package v{as.character(packageVersion('bruceR'))}]. <<underline https://github.com/psychbruce/bruceR>>
  {update_msg}")
}
