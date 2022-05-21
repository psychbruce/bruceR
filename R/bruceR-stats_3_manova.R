#### Demo Data ####


# library(rio)
# between.1 = import("data-raw/between.1.sav", haven=F); names(between.1)[2]="SCORE"
# between.2 = import("data-raw/between.2.sav", haven=F)
# between.3 = import("data-raw/between.3.sav", haven=F)
# usethis::use_data(between.1, overwrite=TRUE)
# usethis::use_data(between.2, overwrite=TRUE)
# usethis::use_data(between.3, overwrite=TRUE)
# within.1 = import("data-raw/within.1.sav", haven=F)
# within.2 = import("data-raw/within.2.sav", haven=F)
# within.3 = import("data-raw/within.3.sav", haven=F)
# usethis::use_data(within.1, overwrite=TRUE)
# usethis::use_data(within.2, overwrite=TRUE)
# usethis::use_data(within.3, overwrite=TRUE)
# mixed.2_1b1w = import("data-raw/mixed.2_1b1w.sav", haven=F)
# mixed.3_1b2w = import("data-raw/mixed.3_1b2w.sav", haven=F)
# mixed.3_2b1w = import("data-raw/mixed.3_2b1w.sav", haven=F)
# usethis::use_data(mixed.2_1b1w, overwrite=TRUE)
# usethis::use_data(mixed.3_1b2w, overwrite=TRUE)
# usethis::use_data(mixed.3_2b1w, overwrite=TRUE)


#' Demo data.
#'
#' @description
#' Demo datasets of multi-factor ANOVA as examples to show how the functions
#' \code{\link{MANOVA}} and \code{\link{EMMEANS}} work.
#'
#' @format
#' \describe{
#'   \item{\strong{1. Between-Subjects Design}}{
#'     \itemize{
#'       \item \code{between.1} - A(4)
#'       \item \code{between.2} - A(2) * B(3)
#'       \item \code{between.3} - A(2) * B(2) * C(2)
#'     }
#'   }
#'   \item{\strong{2. Within-Subjects Design}}{
#'     \itemize{
#'       \item \code{within.1} - A(4)
#'       \item \code{within.2} - A(2) * B(3)
#'       \item \code{within.3} - A(2) * B(2) * C(2)
#'     }
#'   }
#'   \item{\strong{3. Mixed Design}}{
#'     \itemize{
#'       \item \code{mixed.2_1b1w} - A(2, between) * B(3, within)
#'       \item \code{mixed.3_1b2w} - A(2, between) * B(2, within) * C(2, within)
#'       \item \code{mixed.3_2b1w} - A(2, between) * B(2, within) * C(2, between)
#'     }
#'   }
#' }
#'
#' @source
#' \href{https://book.douban.com/subject/1195181/}{Multi-Factor Experimental Design in Psychology and Education}
#'
#' @name bruceR-demodata
#' @keywords internal
#' @aliases
#' between.1 between.2 between.3
#' mixed.2_1b1w mixed.3_1b2w mixed.3_2b1w
#' within.1 within.2 within.3
NULL




#### MANOVA ####


## Levene's Test for Homogeneity of Variance
levene_test = function(data, id, dvs, ivs.between) {
  ## data should be wide-format
  Print("\n\n\nLevene\u2019s Test for Homogeneity of Variance:")
  if(is.null(ivs.between)) {
    Print("No between-subjects factors. No need to do the Levene\u2019s test.")
  } else {
    data = data.table::as.data.table(data[c(id, dvs, ivs.between)])
    data = unique(data, by=id) %>% as.data.frame()
    for(iv in ivs.between)
      data[[iv]] = as.factor(data[[iv]])
    levene = data.frame()
    for(dv in dvs) {
      f = as.formula(Glue("{dv} ~ {paste(ivs.between, collapse='*')}"))
      lev = car::leveneTest(f, data, center=mean)
      lev = cbind(lev[1, "F value"],
                  lev[1, "Df"],
                  lev[2, "Df"],
                  lev[1, "Pr(>F)"]) %>% as.data.frame()
      names(lev) = c("Levene\u2019s F", "df1", "df2", "pval")
      row.names(lev) = paste("DV:", dv)
      levene = rbind(levene, lev)
    }
    print_table(levene, nsmalls=c(3, 0, 0, 0))
  }
}


fix_long_data = function(data.long, ivs) {
  # Ensure Factorized Variables
  for(iv in ivs) {
    data.long[[iv]] = as.factor(data.long[[iv]])
    suppressWarnings({
      levels = levels(data.long[[iv]])
      levels.num = as.numeric(as.character(levels))
    })
    if(all(is.na(levels.num))==FALSE) {
      # numeric levels ==> character levels ("varnum")
      data.long[[iv]] = factor(data.long[[iv]],
                               levels=levels,
                               labels=paste0(iv, levels))
    }
  }
  return(data.long)
}


#' Multi-factor ANOVA.
#'
#' @description
#' Multi-factor ANOVA (between-subjects, within-subjects, and mixed designs),
#' with and without covariates (ANCOVA).
#'
#' This function is based on and extends \code{\link[afex:aov_car]{afex::aov_ez()}}.
#' You only need to specify the data, dependent variable(s), and factors
#' (between-subjects and/or within-subjects).
#' Almost all results you need will be displayed together,
#' including effect sizes (partial \eqn{\eta^2}) and their confidence intervals (CIs).
#' 90\% CIs for partial \eqn{\eta^2} (two-sided) are reported, following Steiger (2004).
#' In addition to partial \eqn{\eta^2}, it also reports generalized \eqn{\eta^2}, following Olejnik & Algina (2003).
#'
#' How to prepare your data and specify the arguments of \code{MANOVA}?
#' \itemize{
#'   \item \strong{Wide-format data} (one person in one row, and repeated measures in multiple columns):
#'   \describe{
#'     \item{Betweem-subjects design}{\code{MANOVA(data=, dv=, between=, ...)}}
#'     \item{Within-subjects design}{\code{MANOVA(data=, dvs=, dvs.pattern=, within=, ...)}}
#'     \item{Mixed design}{\code{MANOVA(data=, dvs=, dvs.pattern=, between=, within=, ...)}}
#'   }
#'   \item \strong{Long-format data} (one person in multiple rows, and repeated measures in one column):
#'   \describe{
#'     \item{Betweem-subjects design}{(not applicable)}
#'     \item{Within-subjects design}{\code{MANOVA(data=, subID=, dv=, within=, ...)}}
#'     \item{Mixed design}{\code{MANOVA(data=, subID=, dv=, between=, within=, ...)}}
#'   }
#' }
#'
#' @details
#' If observations are not uniquely identified in user-defined long-format data,
#' the function takes averages across those multiple observations for each case.
#' In technical details, it specifies \code{fun_aggregate=mean} in \code{\link[afex:aov_car]{afex::aov_ez()}}
#' and \code{values_fn=mean} in \code{\link[tidyr:pivot_wider]{tidyr::pivot_wider()}}.
#'
#' @param data Data frame. Both \strong{wide-format} and \strong{long-format} are supported.
#' @param subID Subject ID (the column name). Only necessary for \strong{long-format} data.
#' @param dv Dependent variable.
#' \itemize{
#'   \item For \strong{wide-format} data, \code{dv} only can be used for between-subjects designs.
#'   For within-subjects and mixed designs, please use \code{dvs} and \code{dvs.pattern}.
#'   \item For \strong{long-format} data, \code{dv} is the outcome variable.
#' }
#' @param dvs Repeated measures. Only for \strong{wide-format} data (within-subjects or mixed designs).
#'
#' Can be:
#' \itemize{
#'   \item \code{"start:stop"} to specify the range of variables
#'   (sensitive to the order of variables):
#'
#'   e.g., \code{"A1B1:A2B3"} is matched to all variables in the data
#'   between \code{"A1B1"} and \code{"A2B3"}
#'
#'   \item a character vector to directly specify variables
#'   (insensitive to the order of variables):
#'
#'   e.g., \code{c("Cond1", "Cond2", "Cond3")} or \code{cc("Cond1, Cond2, Cond3")}
#'
#'   See \code{\link{cc}} for its usage.
#' }
#' @param dvs.pattern If you use \code{dvs}, you should also specify the pattern of variable names
#' using \emph{regular expression}.
#'
#' Examples:
#' \itemize{
#'   \item \code{"Cond(.)"} extracts levels from \code{"Cond1", "Cond2", "Cond3", ...}
#'   You may rename the factor using the \code{within} argument (e.g., \code{within="Condition"})
#'   \item \code{"X(..)Y(..)"} extracts levels from \code{"X01Y01", "X02Y02", "XaaYbc", ...}
#'   \item \code{"X(.+)Y(.+)"} extracts levels from \code{"X1Y1", "XaYb", "XaY002", ...}
#' }
#'
#' Tips on regular expression:
#' \itemize{
#'   \item \code{"(.)"} extracts any single character (number, letter, and other symbols)
#'   \item \code{"(.+)"} extracts >= 1 character(s)
#'   \item \code{"(.*)"} extracts >= 0 character(s)
#'   \item \code{"([0-9])"} extracts any single number
#'   \item \code{"([a-z])"} extracts any single letter
#'   \item More information: \href{https://regexr.com/}{Link 1 (in English)} and
#'         \href{https://www.jb51.net/shouce/jquery1.82/regexp.html}{Link 2 (in Chinese)}
#' }
#'
#' @param between Between-subjects factor(s). Multiple variables should be included in a character vector \code{c()}.
#' @param within Within-subjects factor(s). Multiple variables should be included in a character vector \code{c()}.
#' @param covariate Covariates. Multiple variables should be included in a character vector \code{c()}.
#' @param ss.type Type of sums of squares (SS) for ANOVA. Default is \code{"III"}.
#' Possible values are \code{"II"}, \code{"III"}, \code{2}, or \code{3}.
#' @param sph.correction [Only for repeated measures with >= 3 levels]
#'
#' Sphericity correction method for adjusting the degrees of freedom (\emph{df}) when the sphericity assumption is violated. Default is \code{"none"}.
#' If Mauchly's test of sphericity is significant, you may set it to \code{"GG"} (Greenhouse-Geisser) or \code{"HF"} (Huynh-Feldt).
#' @param aov.include Include the \code{aov} object in the returned object?
#' Default is \code{FALSE}, as suggested by \code{\link[afex:aov_car]{afex::aov_ez()}}
#' (please see the \code{include_aov} argument in this help page, which provides a detailed explanation).
#' If \code{TRUE}, you should also specify \code{model.type="univariate"} in \code{\link{EMMEANS}}.
#' @param digits,nsmall Number of decimal places of output. Default is \code{3}.
#' @param file File name of MS Word (\code{.doc}).
## @param which.observed \strong{[only effective for computing generalized \eqn{\eta^2}]}
##
## Factors that are observed or measured (e.g., gender, age group, measured covariates) but not experimentally manipulated. Default is \code{NULL}.
## The generalized \eqn{\eta^2} requires correct specification of the observed (vs. manipulated) variables.
## (If all the variables in \code{between} and \code{within} are set to \code{observed}, then generalized \eqn{\eta^2} will be equal to \eqn{\eta^2}.)
#'
#' @return
#' A result object (list) returned by
#' \code{\link[afex:aov_car]{afex::aov_ez()}},
#' along with several other elements:
#' \code{between}, \code{within},
#' \code{data.wide}, \code{data.long}.
#'
#' @section Interaction Plot:
#' You can save the returned object and use the \code{\link[emmeans:emmip]{emmeans::emmip()}} function
#' to create an interaction plot (based on the fitted model and a formula specification).
#' For usage, please see the help page of \code{\link[emmeans:emmip]{emmeans::emmip()}}.
#' It returns an object of class \code{ggplot}, which can be easily modified and saved using \code{ggplot2} syntax.
#'
#' @examples
#' #### Between-Subjects Design ####
#'
#' between.1
#' MANOVA(between.1, dv="SCORE", between="A")
#'
#' between.2
#' MANOVA(between.2, dv="SCORE", between=c("A", "B"))
#'
#' between.3
#' MANOVA(between.3, dv="SCORE", between=c("A", "B", "C"))
#'
#' ## How to create an interaction plot using `emmeans::emmip()`?
#' ## See help page for its usage: ?emmeans::emmip()
#' m = MANOVA(between.2, dv="SCORE", between=c("A", "B"))
#' emmip(m, ~ A | B, CIs=TRUE)
#' emmip(m, ~ B | A, CIs=TRUE)
#' emmip(m, B ~ A, CIs=TRUE)
#' emmip(m, A ~ B, CIs=TRUE)
#'
#'
#' #### Within-Subjects Design ####
#'
#' within.1
#' MANOVA(within.1, dvs="A1:A4", dvs.pattern="A(.)",
#'        within="A")
#' ## the same:
#' MANOVA(within.1, dvs=c("A1", "A2", "A3", "A4"), dvs.pattern="A(.)",
#'        within="MyFactor")  # renamed the within-subjects factor
#'
#' within.2
#' MANOVA(within.2, dvs="A1B1:A2B3", dvs.pattern="A(.)B(.)",
#'        within=c("A", "B"))
#'
#' within.3
#' MANOVA(within.3, dvs="A1B1C1:A2B2C2", dvs.pattern="A(.)B(.)C(.)",
#'        within=c("A", "B", "C"))
#'
#'
#' #### Mixed Design ####
#'
#' mixed.2_1b1w
#' MANOVA(mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
#'        between="A", within="B")
#' MANOVA(mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
#'        between="A", within="B", sph.correction="GG")
#'
#' \donttest{mixed.3_1b2w
#' MANOVA(mixed.3_1b2w, dvs="B1C1:B2C2", dvs.pattern="B(.)C(.)",
#'        between="A", within=c("B", "C"))
#'
#' mixed.3_2b1w
#' MANOVA(mixed.3_2b1w, dvs="B1:B2", dvs.pattern="B(.)",
#'        between=c("A", "C"), within="B")
#'
#'
#' #### Other Examples ####
#'
#' data.new = mixed.3_1b2w
#' names(data.new) = c("Group", "Cond_01", "Cond_02", "Cond_03", "Cond_04")
#' MANOVA(data.new,
#'        dvs="Cond_01:Cond_04",
#'        dvs.pattern="Cond_(..)",
#'        between="Group",
#'        within="Condition")  # rename the factor
#'
#' # ?afex::obk.long
#' MANOVA(afex::obk.long,
#'        subID="id",
#'        dv="value",
#'        between=c("treatment", "gender"),
#'        within=c("phase", "hour"),
#'        cov="age",
#'        sph.correction="GG")
#' }
#' @references
#' Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared statistics: Measures of effect size for some common research designs.
#' \emph{Psychological Methods, 8}(4), 434-447.
#'
#' Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis.
#' \emph{Psychological Methods, 9}(2), 164-182.
#'
#' @seealso \code{\link{TTEST}}, \code{\link{EMMEANS}}, \code{\link{bruceR-demodata}}
#'
#' @export
MANOVA = function(data, subID=NULL, dv=NULL,
                  dvs=NULL, dvs.pattern=NULL,
                  between=NULL, within=NULL, covariate=NULL,
                  ss.type="III",
                  sph.correction="none",
                  # which.observed=NULL,
                  aov.include=FALSE,
                  digits=3, nsmall=digits,
                  file=NULL) {
  ## Initialize
  data = as.data.frame(data)
  if(is.null(within)) {
    if(is.null(between)) {
      stop("Either `between` or `within` or both should be specified.\nSee: help(MANOVA)", call.=FALSE)
    } else {
      design = "Between-Subjects Design"
      format = "wide"
      if(is.null(dv))
        stop("`dv` should be specified.\nSee: help(MANOVA)", call.=FALSE)
      if(!is.null(dvs) | !is.null(dvs.pattern))
        stop("`dvs` should not be used for between-subjects designs. Please use `dv` instead.\nSee: help(MANOVA)", call.=FALSE)
    }
  } else {
    if(is.null(between)) {
      design = "Within-Subjects Design"
    } else {
      design = "Mixed Design"
    }
    if((!is.null(dv) & is.null(subID)) | (!is.null(dvs) & is.null(dvs.pattern))) {
      stop(Glue("
      Wrong usage of MANOVA().
      - For wide-format data, please specify both `dvs` and `dvs.pattern`.
      - For long-format data, please specify both `dv` and `subID`.
      See: help(MANOVA)"), call.=FALSE)
    }
    format = ifelse(!is.null(dv) & !is.null(subID),
                    "long", "wide")
  }
  if(is.null(subID)) {
    data$bruceR.ID = 1:nrow(data)
    subID = "bruceR.ID"
  }

  ## Wide and Long Data
  ## Wide: dv (between) | dvs, dvs.pattern
  ## Long: dv (within)
  if(is.null(dv)) {
    data.wide = data
    if(length(dvs)==1 & any(grepl(":", dvs))) {
      DVS = dv.vars = convert2vars(data, varrange=dvs)$vars.raw
      message("\n", Glue("Note:\ndvs=\"{dvs}\" is matched to variables:"),
              "\n", paste(DVS, collapse=", "))
    } else {
      DVS = dv.vars = dvs
    }
    dv = "bruceR.Y"  # "Y" will generate an error when dvs are like "X1Y1"
    data.long = tidyr::pivot_longer(
      data, cols=dv.vars,
      names_to=within,
      names_pattern=dvs.pattern,
      values_to=dv) %>% as.data.frame()
    data.long = fix_long_data(data.long, c(between, within))
  } else {
    dv.vars = dv
    data.long = data
    data.long = fix_long_data(data.long, c(between, within))
    if(is.null(within)) {
      data.wide = data.long
      DVS = dv
    } else {
      data.wide = tidyr::pivot_wider(
        data.long[c(subID, dv, between, within, covariate)],
        names_from=within,
        values_from=dv,
        values_fn=mean) %>% as.data.frame()
      message("
    * Data are aggregated to mean (across items/trials)
    if there are >=2 observations per subject and cell.
    You may use Linear Mixed Model to analyze the data,
    e.g., with subjects and items as level-2 clusters.\n")
      DVS = base::setdiff(names(data.wide), c(subID, between, covariate))
    }
  }

  ## Main ANOVA Function
  try({
    err = TRUE
    suppressMessages({
      aov.ez = afex::aov_ez(
        data=data.long,
        id=subID,  # "bruceR.ID"
        dv=dv,  # "bruceR.Y"
        between=between,
        within=within,
        covariate=covariate,
        type=ss.type,
        # observed=which.observed,
        anova_table=list(correction=sph.correction, es="ges"),
        fun_aggregate=mean,
        include_aov=aov.include,
        factorize=FALSE,
        print.formula=FALSE)
    })
    err = FALSE
  }, silent=TRUE)
  if(err) {
    cat("\n")
    stop("
    Failed to perform MANOVA.
    Please follow the correct usage.
    See: help(MANOVA)", call.=FALSE)
  }
  at = aov.ez$anova_table
  names(at)[1:2] = c("df1", "df2")
  at$MS = at$`F`*at$`MSE`
  eta2 = effectsize::F_to_eta2(at$`F`, at$df1, at$df2,
                               ci=0.90, alternative="two.sided")
  at$p.eta2 = cc_m_ci(eta2$Eta2_partial, eta2$CI_low, eta2$CI_high, nsmall) %>%
    str_replace_all("0\\.", ".")
  at$g.eta2 = str_replace_all(formatF(at$ges, nsmall), "0\\.", ".")
  at = at[c("MS", "MSE", "df1", "df2", "F", "Pr(>F)", "p.eta2", "g.eta2")]
  names(at)[7:8] = c("\u03b7\u00b2p [90% CI of \u03b7\u00b2p]",
                     "\u03b7\u00b2G")
  row.names(at) = row.names(aov.ez$anova_table) %>%
    str_replace_all(":", " * ")
  df.nsmall = ifelse(sph.correction=="none", 0, nsmall)
  at.nsmalls = c(nsmall, nsmall, df.nsmall, df.nsmall, nsmall, 0, 0, 0)

  ## Descriptive Statistics
  nsub = nrow(data.wide)
  ncom = complete.cases(data.long[c(between, within, covariate)])
  nmis = length(ncom)-sum(ncom)
  N.info = Glue("{nsub}{ifelse(nmis>0, Glue(' ({nmis} missing observations deleted)'), '')}")
  data.long$bruceR.dv = data.long[[dv]]
  nmsd = plyr::ddply(
    .data=aov.ez$data$long,
    .variables=plyr::as.quoted(c(between, within)),
    .fun=summarise,
    bruceR.Mean=mean(!!sym(dv), na.rm=TRUE),
    bruceR.S.D.=sd(!!sym(dv), na.rm=TRUE),
    bruceR.n=length(!!sym(dv)))
  ncol.nmsd = length(nmsd)
  names(nmsd)[1:(ncol.nmsd-3)] = "\"" %^% names(nmsd)[1:(ncol.nmsd-3)] %^% "\""
  names(nmsd)[(ncol.nmsd-2):ncol.nmsd] = c("Mean", "S.D.", "n")

  cat("\n")
  Print("<<cyan ====== ANOVA ({design}) ======>>")
  cat("\n")
  Print("Descriptives:")
  print_table(nmsd, row.names=FALSE,
              nsmalls=c(rep(nsmall, ncol.nmsd-1), 0))
  Print("Total sample size: <<italic N>> = {N.info}")
  cat("\n")

  nmsd$Mean = formatF(nmsd$Mean, nsmall)
  nmsd$S.D. = formatF(nmsd$S.D., nsmall)
  names(nmsd)[(ncol.nmsd-2):ncol.nmsd] = c("<i>M</i>", "<i>SD</i>", "<i>n</i>")
  nmsd.html = paste0(
    "<p><br/><br/></p>",
    "<p><b>Descriptive Statistics:</b></p>",
    df_to_html(
      nmsd,
      align.head=c(rep("left", times=ncol.nmsd-3),
                   rep("right"), times=3),
      align.text=c(rep("left", times=ncol.nmsd-3),
                   rep("right"), times=3))$TABLE,
    "<p>Total sample size: <i>N</i> = ", N.info, "</p>"
  )

  DEP = ifelse(is.null(within), dv, paste(dv.vars, collapse=", "))
  BET = ifelse(is.null(between), "\u2013", paste(between, collapse=", "))
  WIT = ifelse(is.null(within), "\u2013", paste(within, collapse=", "))
  COV = ifelse(is.null(covariate), "\u2013", paste(covariate, collapse=", "))
  Print("
  ANOVA Table:
  Dependent variable(s):      {DEP}
  Between-subjects factor(s): {BET}
  Within-subjects factor(s):  {WIT}
  Covariate(s):               {COV}
  ")
  print_table(at, nsmalls=at.nsmalls)
  if(sph.correction %in% c("GG", "HF")) {
    sph.text=switch(sph.correction,
                    "GG"="GG (Greenhouse-Geisser)",
                    "HF"="HF (Huynh-Feldt)")
    Print("<<green Sphericity correction method: {sph.text}>>")
  }
  Print("<<blue MSE = mean square error (the residual variance of the linear model)>>")

  ## All Other Effect-Size Measures (deprecated; please use `effectsize` package)
  # https://github.com/strengejacke/sjstats/blob/master/R/anova_stats.R#L116
  Print("<<magenta
  \u03b7\u00b2p = partial eta-squared = SS / (SS + SSE) = F * df1 / (F * df1 + df2)
  \u03c9\u00b2p = partial omega-squared = (F - 1) * df1 / (F * df1 + df2 + 1)
  \u03b7\u00b2G = generalized eta-squared (see Olejnik & Algina, 2003)
  Cohen\u2019s <<italic f>>\u00b2 = \u03b7\u00b2p / (1 - \u03b7\u00b2p)
  >>")

  ## Levene's Test for Homogeneity of Variance
  try({ levene_test(data.wide, subID, DVS, between) })

  ## Mauchly's Test of Sphericity
  if(!is.null(within)) {
    Print("\n\n\nMauchly\u2019s Test of Sphericity:")
    suppressWarnings({
      sph = summary(aov.ez$Anova)$sphericity.tests
    })
    if(length(sph)==0) {
      Print("The repeated measures have only two levels. The assumption of sphericity is always met.")
    } else {
      class(sph) = "matrix"
      sph = as.data.frame(sph)
      names(sph) = c("Mauchly's W", "pval")
      row.names(sph) = str_replace_all(row.names(sph), ":", " * ")
      print_table(sph, nsmalls=4)
      if(min(sph[,2])<.05 & sph.correction=="none") {
        Print("<<red The sphericity assumption is violated.
              You may specify: sph.correction=\"GG\" (or =\"HF\")>>")
      }
    }
  }
  cat("\n")

  if(!is.null(file)) {
    print_table(
      at,
      nsmalls=at.nsmalls,
      col.names=c("<i>MS</i>",
                  "<i>MSE</i>",
                  "<i>df</i><sub>1</sub>",
                  "<i>df</i><sub>2</sub>",
                  "<i>F</i>",
                  "<i>p</i>",
                  " ",
                  "\u03b7<sup>2</sup><sub>p</sub> [90% CI]",
                  "\u03b7<sup>2</sup><sub>G</sub>"),
      file=file,
      file.align.text=c("left",
                        "right", "right",
                        "right", "right",
                        "right", "right", "left",
                        "right", "right"),
      title=paste0(
        "<b>ANOVA Table:</b></p>\n<p>",
        "<pre>Dependent variable(s):&#9;", DEP, "</pre></p>\n<p>",
        "<pre>Between-subjects factor(s):&#9;", BET, "</pre></p>\n<p>",
        "<pre>Within-subjects factor(s):&#9;", WIT, "</pre></p>\n<p>",
        "<pre>Covariate(s):&#9;&#9;", COV, "</pre>"
      ),
      note=paste0(
        "<i>Note</i>. MSE = Mean Square Error.",
        ifelse(
          sph.correction %in% c("GG", "HF"),
          " Sphericity correction method: " %^% sph.text %^% ".",
          "")
      ),
      append=nmsd.html)
  }

  ## Return
  aov.ez$between = between
  aov.ez$within = within
  aov.ez$data.wide = data.wide
  aov.ez$data.long = data.long
  invisible(aov.ez)
}


#' Simple-effect analysis and post-hoc multiple comparison.
#'
#' @description
#' Perform (1) simple-effect (and simple-simple-effect) analyses,
#' including both simple main effects and simple interaction effects,
#' and (2) post-hoc multiple comparisons (e.g., pairwise, sequential, polynomial),
#' with \emph{p} values adjusted for factors with >= 3 levels.
#'
#' This function is based on and extends
#' (1) \code{\link[emmeans:joint_tests]{emmeans::joint_tests()}},
#' (2) \code{\link[emmeans:emmeans]{emmeans::emmeans()}}, and
#' (3) \code{\link[emmeans:contrast]{emmeans::contrast()}}.
#' You only need to specify the model object, to-be-tested effect(s), and moderator(s).
#' Almost all results you need will be displayed together,
#' including effect sizes (partial \eqn{\eta^2} and Cohen's \emph{d}) and their confidence intervals (CIs).
#' 90\% CIs for partial \eqn{\eta^2} and 95\% CIs for Cohen's \emph{d} are reported.
#'
#' By default, the \emph{root mean square error} (RMSE) is used to compute the pooled \emph{SD} for Cohen's \emph{d}.
#' Specifically, it uses:
#' \enumerate{
#'   \item the square root of \emph{mean square error} (MSE) for between-subjects designs;
#'   \item the square root of \emph{mean variance of all paired differences of the residuals of repeated measures} for within-subjects and mixed designs.
#' }
#'
#' \strong{\emph{Disclaimer}:}
#' There is substantial disagreement on the appropriate pooled \emph{SD} to use in computing the effect size.
#' For alternative methods, see \code{\link[emmeans:eff_size]{emmeans::eff_size()}} and \code{\link[effectsize:t_to_r]{effectsize::t_to_d()}}.
#' Users should \emph{not} take the default output as the only right results and are completely responsible for specifying \code{sd.pooled}.
#'
#' @section Interaction Plot (See Examples Below):
#' You can save the returned object and use the \code{\link[emmeans:emmip]{emmeans::emmip()}} function
#' to create an interaction plot (based on the fitted model and a formula).
#' See examples below for the usage.
#'
#' Note: \code{\link[emmeans:emmip]{emmeans::emmip()}} returns a \code{ggplot} object,
#' which can be modified and saved with \code{ggplot2} syntax.
#'
#' @section Statistical Details:
#'
#' Some may confuse the statistical terms "simple effects", "post-hoc tests", and "multiple comparisons".
#' Such a confusion is not uncommon. Here I explain what these terms actually refer to.
#' \describe{
#'   \item{\strong{1. Simple Effect}}{
#'     When we speak of "simple effect", we are referring to ...
#'     \itemize{
#'       \item simple main effect
#'       \item simple interaction effect (only for designs with 3 or more factors)
#'       \item simple simple effect (only for designs with 3 or more factors)
#'     }
#'     When the interaction effect in ANOVA is significant,
#'     we should then perform a "simple-effect analysis".
#'     In regression, we call this "simple-slope analysis".
#'     They are identical in statistical principles.
#'
#'     In a two-factors design, we only test \strong{"simple main effect"}.
#'     That is, at different levels of a factor "B", the main effects of "A" would be different.
#'     However, in a three-factors (or more) design, we may also test \strong{"simple interaction effect"} and \strong{"simple simple effect"}.
#'     That is, at different combinations of levels of factors "B" and "C", the main effects of "A" would be different.
#'
#'     To note, simple effects \emph{per se} never require \emph{p}-value adjustment, because what we test in simple-effect analyses are still "omnibus \emph{F}-tests".
#'   }
#'   \item{\strong{2. Post-Hoc Test}}{
#'     The term "post-hoc" means that the tests are performed after ANOVA. Given this, some may (wrongly) regard simple-effect analyses also as a kind of post-hoc tests.
#'     However, these two terms should be distinguished. In many situations,
#'     "post-hoc tests" only refer to \strong{"post-hoc comparisons"} using \emph{t}-tests and some \emph{p}-value adjustment techniques.
#'     We need post-hoc comparisons \strong{only when there are factors with 3 or more levels}.
#'
#'     Post-hoc tests are totally \strong{independent of} whether there is a significant interaction effect. \strong{It only deals with factors with multiple levels.}
#'     In most cases, we use pairwise comparisons to do post-hoc tests. See the next part for details.
#'   }
#'   \item{\strong{3. Multiple Comparison}}{
#'     As mentioned above, multiple comparisons are indeed post-hoc tests but have no relationship with simple-effect analyses.
#'     Post-hoc multiple comparisons are \strong{independent of} interaction effects and simple effects.
#'     Furthermore, if a simple main effect contains 3 or more levels, we also need to do multiple comparisons \emph{within} the simple-effect analysis.
#'     In this situation, we also need \emph{p}-value adjustment with methods such as Bonferroni, Tukey's HSD (honest significant difference), FDR (false discovery rate), and so forth.
#'
#'     Options for multiple comparison:
#'     \itemize{
#'       \item \code{"pairwise"} - Pairwise comparisons (default is "higher level - lower level")
#'       \item \code{"seq"} or \code{"consec"} - Consecutive (sequential) comparisons
#'       \item \code{"poly"} - Polynomial contrasts (linear, quadratic, cubic, quartic, ...)
#'       \item \code{"eff"} - Effect contrasts (vs. the grand mean)
#'     }
#'   }
#' }
#'
#' @param model The model object returned by \code{\link{MANOVA}}.
#' @param effect Effect(s) you want to test.
#' If set to a character string (e.g., \code{"A"}),
#' it reports the results of omnibus test or simple main effect.
#' If set to a character vector (e.g., \code{c("A", "B")}),
#' it also reports the results of simple interaction effect.
#' @param by Moderator variable(s). Default is \code{NULL}.
#' @param contrast Contrast method for multiple comparisons.
#' Default is \code{"pairwise"}.
#'
#' Alternatives can be \code{"pairwise"} (\code{"revpairwise"}),
#' \code{"seq"} (\code{"consec"}), \code{"poly"}, \code{"eff"}.
#' For details, see \code{?emmeans::`contrast-methods`}.
#' @param reverse The order of levels to be contrasted.
#' Default is \code{TRUE} (higher level vs. lower level).
#' @param p.adjust Adjustment method of \emph{p} values for multiple comparisons.
#' Default is \code{"bonferroni"}.
#' For polynomial contrasts, default is \code{"none"}.
#'
#' Alternatives can be \code{"none"}, \code{"fdr"}, \code{"hochberg"},
#' \code{"hommel"}, \code{"holm"}, \code{"tukey"}, \code{"mvt"},
#' \code{"dunnettx"}, \code{"sidak"}, \code{"scheffe"}, \code{"bonferroni"}.
#' For details, see \code{\link[stats:p.adjust]{stats::p.adjust()}} and
#' \code{\link[emmeans:summary.emmGrid]{emmeans::summary()}}.
#' @param sd.pooled By default, it uses \strong{\code{sqrt(MSE)}} (root mean square error, RMSE)
#' as the pooled \emph{SD} to compute Cohen's \emph{d}.
#' Users may specify this argument as the \emph{SD} of a reference group,
#' or use \code{\link[effectsize:sd_pooled]{effectsize::sd_pooled()}} to obtain a pooled \emph{SD}.
#' For an issue about the computation method of Cohen's \emph{d}, see \emph{Disclaimer} above.
#' @param model.type \code{"multivariate"} returns the results of pairwise comparisons identical to SPSS,
#' which uses the \code{lm} (rather than \code{aov}) object of the \code{model}
#' for \code{\link[emmeans:joint_tests]{emmeans::joint_tests()}} and \code{\link[emmeans:emmeans]{emmeans::emmeans()}}.
#'
#' \code{"univariate"} requires also specifying \code{aov.include=TRUE} in \code{\link{MANOVA}}
#' (not recommended by the \code{afex} package; for details, see \code{\link[afex:aov_car]{afex::aov_ez()}}).
#' @param digits,nsmall Number of decimal places of output. Default is \code{3}.
#'
#' @return
#' The same model object as returned by
#' \code{\link{MANOVA}} (for recursive use),
#' along with a list of tables:
#' \code{sim} (simple effects),
#' \code{emm} (estimated marginal means),
#' \code{con} (contrasts).
#'
#' Each \code{EMMEANS(...)} appends one list to the returned object.
#'
#' @examples
#' #### Between-Subjects Design ####
#'
#' between.1
#' MANOVA(between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A")
#' \donttest{MANOVA(between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A", p.adjust="tukey")
#' MANOVA(between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A", contrast="seq")
#' MANOVA(between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A", contrast="poly")
#' }
#' between.2
#' MANOVA(between.2, dv="SCORE", between=c("A", "B")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B", by="A")
#' ## How to create an interaction plot using `emmeans::emmip()`?
#' ## See help page for its usage: ?emmeans::emmip()
#' m = MANOVA(between.2, dv="SCORE", between=c("A", "B"))
#' emmip(m, ~ A | B, CIs=TRUE)
#' emmip(m, ~ B | A, CIs=TRUE)
#' emmip(m, B ~ A, CIs=TRUE)
#' emmip(m, A ~ B, CIs=TRUE)
#'
#' between.3
#' MANOVA(between.3, dv="SCORE", between=c("A", "B", "C")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#' ## Just to name a few...
#' ## You may test other combinations...
#'
#'
#' #### Within-Subjects Design ####
#'
#' within.1
#' MANOVA(within.1, dvs="A1:A4", dvs.pattern="A(.)",
#'        within="A") %>%
#'   EMMEANS("A")
#'
#' within.2
#' MANOVA(within.2, dvs="A1B1:A2B3", dvs.pattern="A(.)B(.)",
#'        within=c("A", "B")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B", by="A")  # singular error matrix
#' # :::::::::::::::::::::::::::::::::::::::
#' # This would produce a WARNING because of
#' # the linear dependence of A2B2 and A2B3.
#' # See: Corr(within.2[c("A2B2", "A2B3")])
#'
#' \donttest{within.3
#' MANOVA(within.3, dvs="A1B1C1:A2B2C2", dvs.pattern="A(.)B(.)C(.)",
#'        within=c("A", "B", "C")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#' ## Just to name a few...
#' ## You may test other combinations...
#' }
#'
#' #### Mixed Design ####
#'
#' mixed.2_1b1w
#' MANOVA(mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
#'        between="A", within="B", sph.correction="GG") %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B", by="A")
#'
#' \donttest{mixed.3_1b2w
#' MANOVA(mixed.3_1b2w, dvs="B1C1:B2C2", dvs.pattern="B(.)C(.)",
#'        between="A", within=c("B", "C")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#' ## Just to name a few...
#' ## You may test other combinations...
#'
#' mixed.3_2b1w
#' MANOVA(mixed.3_2b1w, dvs="B1:B2", dvs.pattern="B(.)",
#'        between=c("A", "C"), within="B") %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("A", by="C") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("B", by=c("A", "C"))
#' ## Just to name a few...
#' ## You may test other combinations...
#'
#'
#' #### Other Examples ####
#'
#' air = airquality
#' air$Day.1or2 = ifelse(air$Day %% 2 == 1, 1, 2) %>%
#'   factor(levels=1:2, labels=c("odd", "even"))
#' MANOVA(air, dv="Temp", between=c("Month", "Day.1or2"),
#'        covariate=c("Solar.R", "Wind")) %>%
#'   EMMEANS("Month", contrast="seq") %>%
#'   EMMEANS("Month", by="Day.1or2", contrast="poly")
#' }
#' @seealso \code{\link{TTEST}}, \code{\link{MANOVA}}, \code{\link{bruceR-demodata}}
#'
#' @export
EMMEANS = function(model, effect=NULL, by=NULL,
                   contrast="pairwise",
                   reverse=TRUE,
                   p.adjust="bonferroni",
                   sd.pooled=NULL,
                   model.type="multivariate",
                   digits=3, nsmall=digits) {
  # IMPORTANT: If include 'aov', the 'emmeans' results of
  # within-subjects design will not be equal to those in SPSS!
  # So we do not include 'aov' object but instead use 'lm' and 'mlm'
  # objects to do the follow-up 'emmeans' analyses!

  # Bug: For within-sub 'effect' and between-sub 'by' in mixed design
  # if(!is.null(effect) & !is.null(model$within) & effect %anyin% model$within &
  #    !is.null(by) & !is.null(model$between) & by %anyin% model$between) {
  #   model=model.raw
  # }

  if(is.null(model))
    stop("`model` is invalid. Run MANOVA() without EMMEANS() to check.", call.=FALSE)

  if(!inherits(model, "afex_aov"))
    stop("EMMEANS() should be used with MANOVA()!", call.=FALSE)

  effect.text = paste(effect, collapse='\" & \"')
  Print("<<cyan ------ EMMEANS (effect = \"{effect.text}\") ------>>")
  cat("\n")

  ## Simple Effect (omnibus)
  # see 'weights' in ?emmeans
  suppressMessages({
    sim = emmeans::joint_tests(
      object=model, by=by,
      weights="equal",
      model=model.type)
  })
  if(is.null(sim))
    stop("`model` or `by` is invalid. Please check your code.", call.=FALSE)
  sim$note = NULL
  names(sim)[1] = "Effect"
  sim$Effect = str_replace_all(sim$Effect, ":", " * ")
  eta2 = effectsize::F_to_eta2(sim$F.ratio, sim$df1, sim$df2,
                               ci=0.90, alternative="two.sided")
  sim$p.eta2 = cc_m_ci(eta2$Eta2_partial, eta2$CI_low, eta2$CI_high, nsmall) %>%
    str_replace_all("0\\.", ".")
  if(length(by)>0) {
    vns = names(sim)[2:(length(by)+1)]
    names(sim)[2:(length(by)+1)] = "\"" %^% vns %^% "\""
  }
  names(sim)[(length(by)+4):(length(by)+6)] =
    c("F", "pval", "\u03b7\u00b2p [90% CI of \u03b7\u00b2p]")

  Print("Joint Tests of \"{effect.text}\":")
  print_table(sim, nsmalls=c(rep(0, length(by)+3),
                             nsmall, 0, 0),
              row.names=FALSE)
  Print("
  <<italic Note>>. Simple effects of <<italic repeated measures>> with 3 or more levels
  are <<italic different>> from the results obtained with SPSS MANOVA syntax.
  ")
  cat("\n")

  ## SPSS GLM EMMEANS Univariate/Multivariate Tests
  try({
    phtest = phia::testInteractions(
      model=model$lm,
      across=effect,
      fixed=by,
      idata=model$Anova$idata,
      adjustment="none")
    if(grepl("Multivariate", attr(phtest, "heading"))) {
      pht = as.data.frame(phtest)[c(
        "test stat",
        "num Df",
        "den Df",
        "approx F",
        "Pr(>F)")]
      names(pht) = c(
        "Pillai\u2019s trace",
        "Hypoth. df",
        "Error df",
        "Exact F",
        "pval")
      row.names(pht) = str_replace_all(row.names(pht), " : ", " & ") %^%
        ": " %^% Glue("\"{effect.text}\"")
      print_table(pht, nsmalls=nsmall,
                  title=Glue("Multivariate Tests of \"{effect.text}\":"),
                  note=Glue("<<italic Note>>. Identical to the results obtained with SPSS GLM EMMEANS syntax."))
      cat("\n")
    } else {
      pht = as.data.frame(phtest)[c(
        "Sum of Sq", "Df", "F", "Pr(>F)")]
      pht$MS = pht[,1] / pht[,2]
      pht = pht[, c(1, 2, 5, 3, 4)]
      names(pht) = c("Sum of Squares", "df", "Mean Square", "F", "pval")
      row.names(pht)[1:(nrow(pht)-1)] = str_replace_all(row.names(pht)[1:(nrow(pht)-1)], " : ", " & ") %^%
        ": " %^% Glue("\"{effect.text}\"")
      print_table(pht, nsmalls=c(nsmall, 0, nsmall, nsmall, 0),
                  title=Glue("Univariate Tests of \"{effect.text}\":"),
                  note=Glue("<<italic Note>>. Identical to the results obtained with SPSS GLM EMMEANS syntax."))
      cat("\n")
    }
  }, silent=TRUE)

  ## Estimated Marginal Means (emmeans)
  suppressMessages({
    emm0 = emm = emmeans::emmeans(
      object=model, specs=effect, by=by,
      weights="equal",
      model=model.type)
  })
  emm = summary(emm)  # to a data.frame (class 'summary_emm')
  emm$MeanCI = cc_m_ci(emm$emmean, emm$lower.CL, emm$upper.CL, nsmall)
  vns = names(emm)[1:(length(by)+1)]
  names(emm)[1:(length(by)+1)] = "\"" %^% vns %^% "\""
  emm = cbind(emm[1:(length(by)+1)], emm[c("MeanCI", "SE")])
  names(emm)[length(emm)-1] = "Mean [95% CI of Mean]"

  Print("Estimated Marginal Means of \"{effect.text}\":")
  print_table(emm, nsmalls=nsmall, row.names=FALSE)
  cat(paste(attr(emm, "mesg"), collapse="\n"))
  cat("\n")

  ## Multiple Comparison (pairwise or other methods)
  # see: ?contrast, ?pairs.emmGrid, ?pairwise.emmc
  contr.method = switch(
    contrast,
    pairwise=,
    revpairwise="Pairwise Comparisons",
    consec=,
    seq="Consecutive (Sequential) Comparisons",
    poly="Polynomial Contrasts",
    eff="Effect Contrasts (vs. Grand Mean)",
    "Multiple Comparisons")
  if(contrast=="pairwise" & reverse==TRUE) contrast = "revpairwise"
  if(contrast=="seq") contrast = "consec"
  if(contrast=="consec") reverse = FALSE
  if(contrast=="poly") p.adjust = "none"
  con0 = con = emmeans::contrast(
    object=emm0, method=contrast,
    adjust=p.adjust, reverse=reverse)
  # pairs(emm, simple="each", reverse=TRUE, combine=TRUE)
  conCI = confint(con)
  con = summary(con)  # to a data.frame (class 'summary_emm')

  ## Cohen's d
  all_paired_diffs = function(v) {
    combns = utils::combn(v, 2)
    combns[1,] - combns[2,]
  }
  if(is.null(sd.pooled)) {
    sigma = stats::sigma(model$lm)
    if(length(sigma)==1) {
      sd.pooled = sigma  # = sqrt(MSE), i.e., RMSE
    } else {
      res = residuals(model$lm)  # matrix
      D = apply(res, 1, all_paired_diffs)
      if(is.matrix(D))
        sd.pooled = sqrt(mean(apply(D, 1, var)))  # RMSE
      else
        sd.pooled = sd(D)
    }
  }
  # rn = row.names(model$anova_table)
  # term = c()
  # for(i in rn) if(i %in% effect) term = c(term, i)
  # term = paste(term, collapse=":")
  # if(is.null(sd.pooled))
  #   sd.pooled = sqrt(model$anova_table[term, "MSE"])
  if(contrast!="poly")
    attr(con, "mesg") = c(
      Glue("Pooled SD for computing Cohen\u2019s d: {formatF(sd.pooled, nsmall)}"),
      attr(con, "mesg"))
  con$d = cc_m_ci(con$estimate/sd.pooled,
                  conCI$lower.CL/sd.pooled,
                  conCI$upper.CL/sd.pooled,
                  nsmall)
  if(length(by)>0) {
    vns = names(con)[2:(length(con)-6)]
    names(con)[2:(length(con)-6)] = "\"" %^% vns %^% "\""
  }
  names(con)[c(1, (length(con)-5):length(con))] =
    c("Contrast", "Estimate", "SE", "df", "t", "pval",
      "Cohen\u2019s d [95% CI of d]")
  p.mesg.index = grepl("^P value adjustment", attr(con, "mesg"))
  if(any(p.mesg.index)) {
    p.mesg = attr(con, "mesg")[which(p.mesg.index)]
    method.mesg = str_extract(p.mesg, "(?<=: ).+(?= method)")
    if(method.mesg %in% c("fdr", "mvt"))
      method.mesg.new = toupper(method.mesg)
    else
      method.mesg.new = capitalize(method.mesg)
    p.mesg.new = paste0("P-value adjustment: ", method.mesg.new, strsplit(p.mesg, method.mesg)[[1]][2], ".")
    attr(con, "mesg")[which(p.mesg.index)] = p.mesg.new
  } else if(p.adjust!="none") {
    attr(con, "mesg") = c(attr(con, "mesg"),
                          "No need to adjust p values.")
  }
  if(contrast=="poly") con[c("Cohen\u2019s d [95% CI of d]")] = NULL

  Print("{contr.method} of \"{effect.text}\":")
  con$df = formatF(con$df, 0)
  print_table(con, nsmalls=nsmall, row.names=FALSE)
  cat(paste(attr(con, "mesg"), collapse="\n"))
  cat("\n\n")
  Print("<<green Disclaimer:
  By default, pooled SD is <<italic Root Mean Square Error>> (RMSE).
  There is much disagreement on how to compute Cohen\u2019s d.
  You are completely responsible for setting `sd.pooled`.
  You might also use `effectsize::t_to_d()` to compute d.
  >>")
  cat("\n")
  # if(con0@misc[["famSize"]] > 2 & p.adjust != "none")
  #   cat("\n")
  # if(any(grepl("averaged|Pooled SD", attr(con, "mesg"))) & any(p.mesg.index)==FALSE)
  #   cat("\n")

  ## Return (return the raw model for recycling across '%>%' pipelines)
  model$EMMEANS = c(model$EMMEANS, list(list(sim=sim, emm=emm, con=con)))
  invisible(model)
}

