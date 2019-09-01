#### For Developer ####
# Install Package: 'Ctrl + Shift + B'
# Check Package:   'Ctrl + Shift + E'
# Test Package:    'Ctrl + Shift + T'

#### For User ####
# devtools::install_github("psychbruce/bruceR")
# remove.packages("bruceR")


#### Main Description ####
#' bruceR: \strong{BR}oadly \strong{U}seful \strong{C}ollections and \strong{E}xtensions of \strong{R} functions
#'
#' @description
#' Packing many useful R packages and functions into one package.
#' Making data analyses and statistics more elegant.
#'
#' Check updates on \url{https://github.com/psychbruce/bruceR}
#'
#' Once you load \code{bruceR}, it will also load the following packages:
#'
#' \itemize{
#'   \item \code{\strong{\link[rio]{rio}}}: Data input/output for many file formats within one function (\code{import}/\code{export}).
#'   \item \code{\strong{\link[dplyr]{dplyr}}}: Data manipulation and preprocessing.
#'   \item \code{\strong{\link[stringr]{stringr}}}: String operation with regular expressions.
#'   \item \code{\strong{\link[data.table]{data.table}}}: Enhanced 'data.frame' with higher efficiency.
#'   \item \code{\strong{\link[psych]{psych}}}: Functions for psychological research.
#'   \item \code{\strong{\link[ggplot2]{ggplot2}}}: Data visualization.
#' }
#'
#' No need to load them separately.
#' You can just load \code{bruceR}:
#' \strong{\code{library(bruceR)}}
#'
## If you dislike \code{bruceR}, you can of course uninstall it,
## and your decision will not influence how convenient it is for me.
#'
#' @section Main Functions in \code{bruceR}:
#' \code{bruceR} includes functions for
#' \strong{1)} basic use and analyses (e.g., correlation matrix with plot),
#' \strong{2)} multivariate computing (e.g., scale mean score with reverse scoring),
#' \strong{3)} reliability and validity analyses (e.g., Cronbach's \eqn{\alpha}, EFA, CFA),
#' \strong{4)} \emph{t}-test, ANOVA, simple-effect analysis, and multiple comparison (\emph{coming soon...}),
#' \strong{5)} advanced toolbox and output for general/generalized ordinary/multilevel linear models, and
#' \strong{6)} nice themes of \code{ggplot2} ready for scientific publication.
#'
#' \describe{
#'   \item{\strong{1) Basic Functions}}{
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
#'       \code{\link{set.seeds}}  (a simple extension of \code{\link{set.seed}})
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
#'       \code{\link{RANDBETWEEN}}  (randomly sampling, like Excel's function \code{RANDBETWEEN})
#'
#'       \code{\link{LOOKUP}}  (search, match, and look up values, like Excel's functions \code{INDEX + MATCH})
#'   }
#'   \item{\strong{2) Multivariate Computing}}{
#'       \code{\link{RECODE}}  (recode a variable)
#'
#'       \code{\link{RESCALE}}  (rescale a variable; e.g., from 5-point to 7-point scale)
#'
#'       \code{\link{COUNT}}  (count values across variables)
#'
#'       \code{\link{SUM}}  (compute multivariate sum)
#'
#'       \code{\link{MEAN}}  (compute multivariate mean)
#'
#'       \code{\link{STD}}  (compute multivariate standard deviation)
#'
#'       \code{\link{CONSEC}}  (count "consecutive identical digits" across variables)
#'   }
#'   \item{\strong{3) Reliability and Validity Analyses}}{
#'       \code{\link{Alpha}}  (reliability analysis, Cronbach's \eqn{\alpha})
#'
#'       \code{\link{EFA}}  (exploratory factor analysis)
#'
#'       \code{\link{CFA}}  (confirmatory factor analysis)
#'   }
#'   \item{\strong{4) MANOVA, Simple Effect \emph{(for interaction effect)}, & Multiple Comparison \emph{(for factor with >= 3 levels)}}}{
#'       \emph{(Coming soon...)}
#'   }
#'   \item{\strong{5) Advanced Toolbox and Output for Linear Models}}{
#'       \code{\link{grand_mean_center}}  (center variable(s) on \strong{grand} mean(s))
#'
#'       \code{\link{group_mean_center}}  (center variable(s) on \strong{group} mean(s))
#'
#'       \code{\link{GLM_summary}}  (advanced output for General and Generalized Linear Models)
#'
#'       \code{\link{HLM_summary}}  (advanced output for Multilevel/Hierarchical Linear Models)
#'
#'       \code{\link{regress}}  (fast do OLS, logistic, poisson, and multilevel regression analyses)
#'
#'       \code{\link{med_mc}}  (mediation analysis based on \emph{b} and \emph{SE} with Sobel test and Monte Carlo simulation)
#'
#'       \code{\link{simple_slope}}  (simple-slope analysis based on \emph{b} and \emph{SE})
#'   }
#'   \item{\strong{6) Plotting with \code{ggplot2}}}{
#'       \code{\link{theme_bruce}}  (a set of nice themes ready for scientific publication)
#'   }
#' }
#'
#' @note
#' Please always use \href{https://www.rstudio.com/products/rstudio/download/preview/}{RStudio} as an \href{https://en.wikipedia.org/wiki/Integrated_development_environment}{IDE} instead of using the raw R.
#'
#' The "truly" newest version of \href{https://www.rstudio.com/products/rstudio/download/preview/}{RStudio} can be accessed from this website: \url{https://www.rstudio.com/products/rstudio/download/preview/}
#'
#' @author
#' \href{https://www.zhihu.com/people/psychbruce/}{Han-Wu-Shuang (Bruce) Bao} (personal profile on Zhihu.com)
#'
#' E-mail: \email{baohws@@psych.ac.cn} or \email{psychbruce@@qq.com}
#'
#' @docType package
#' @name bruceR-package
#' @aliases bruceR
NULL



#### Package-Loading Information ####

.onAttach=function(libname, pkgname) {
  # packageStartupMessage("Welcome to my package")

  # pkgs=.packages(all.available=T)
  # pkgs.bruceR=c("tidyverse", "ggstatsplot", "MBESS", "MuMIn",
  #               "jtools", "summarytools", "texreg", "semPlot")
  # for(pkg in pkgs.bruceR) {
  #   if(pkg %notin% pkgs) {
  #     Print("Package '<<red {pkg}>>' will be installed...")
  #     install.packages(pkg)
  #   }
  # }
  # rm(pkg, pkgs, pkgs.bruceR)

  Print("
  <<bold <<blue
  <<magenta
  {rep_char('=', 56)}
  <<underline BR>>oadly <<underline U>>seful <<underline C>>ollections and <<underline E>>xtensions of <<underline R>> functions
  {rep_char('=', 56)}
  >>
  Loaded packages:
  <<green \u2714 bruceR (version 0.2.0)>>
  <<green \u2714 rio, dplyr, stringr, data.table, psych, ggplot2>>
  Check updates on <<underline https://github.com/psychbruce/bruceR>>
  <<silver
  <<blue To update:>> devtools::install_github(\"psychbruce/bruceR\")
  >>
  >>>>
  ")
}
  # <<blue Overview:>>  ?bruceR
  # <<blue Examples:>>  example(\"Print\"); example(\"regress\"); ...
  # <<red (This version is not yet complete. Use at your own risk.)>>
