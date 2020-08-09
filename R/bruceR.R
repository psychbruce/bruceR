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
#'   \item \code{\strong{\link[rio]{rio}}}: Data input/output for all file formats within one function. (see \code{\link[rio]{import}} / \code{\link[rio]{export}})
#'   \item \code{\strong{\link[dplyr]{dplyr}}}: Data manipulation and preprocessing.
#'   \item \code{\strong{\link[stringr]{stringr}}}: String operations and regular expressions.
#'   \item \code{\strong{\link[data.table]{data.table}}}: Advanced 'data.frame' for higher efficiency.
#'   \item \code{\strong{\link[psych]{psych}}}: Toolbox for psychological and psychometric research.
#'   \item \code{\strong{\link[performance]{performance}}}: Checking regression model performance. (see \code{\link[performance]{model_performance}})
#'   \item \code{\strong{\link[ggplot2]{ggplot2}}}: Data visualization.
#'   \item \code{\strong{\link[cowplot]{cowplot}}}: Advanced toolbox for ggplot2.
#' }
#'
#' No need to load each one with its own call.
#'
#' Loading \code{bruceR} is enough:
#' \strong{\code{library(bruceR)}}
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
#'   \item{\strong{3) Reliability and Validity Analyses}}{
#'       \code{\link{Alpha}}  (reliability analysis, Cronbach's \eqn{\alpha})
#'
#'       \code{\link{EFA}}  (exploratory factor analysis)
#'
#'       \code{\link{CFA}}  (confirmatory factor analysis)
#'   }
#'   \item{\strong{4) MANOVA, Simple Effect (\emph{for interactions}), & Multiple Comparison (\emph{for factors with >= 3 levels})}}{
#'       \code{\link{MANOVA}}  (between-subjects, within-subjects, and mixed design ANOVA)
#'
#'       \code{\link{EMMEANS}}  (simple-effect analysis and post-hoc multiple comparison)
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
    library(rio)
    library(dplyr)
    library(stringr)
    library(data.table)
    library(psych)
    library(performance)
    library(ggplot2)
    library(cowplot)
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

  <<bold <<blue Loaded packages:>>>>
  <<green \u2714 bruceR>>
  <<green \u2714 rio, dplyr, stringr, data.table>>
  <<green \u2714 psych, performance, ggplot2, cowplot>>

  <<bold <<blue Citation:>>>>
  Bao, H.-W.-S. (2020). bruceR: Broadly useful collections and extensions of R functions [R package v{as.character(packageVersion('bruceR'))}]. <<underline https://github.com/psychbruce/bruceR>>
  {update_msg}")
}
