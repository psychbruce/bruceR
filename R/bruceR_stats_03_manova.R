#### Demo Data ####


# library(rio)
# between.1=import("data-raw/between.1.sav", haven=F); names(between.1)[2]="SCORE"
# between.2=import("data-raw/between.2.sav", haven=F)
# between.3=import("data-raw/between.3.sav", haven=F)
# usethis::use_data(between.1, overwrite=TRUE)
# usethis::use_data(between.2, overwrite=TRUE)
# usethis::use_data(between.3, overwrite=TRUE)
# within.1=import("data-raw/within.1.sav", haven=F)
# within.2=import("data-raw/within.2.sav", haven=F)
# within.3=import("data-raw/within.3.sav", haven=F)
# usethis::use_data(within.1, overwrite=TRUE)
# usethis::use_data(within.2, overwrite=TRUE)
# usethis::use_data(within.3, overwrite=TRUE)
# mixed.2_1b1w=import("data-raw/mixed.2_1b1w.sav", haven=F)
# mixed.3_1b2w=import("data-raw/mixed.3_1b2w.sav", haven=F)
# mixed.3_2b1w=import("data-raw/mixed.3_2b1w.sav", haven=F)
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
#' @aliases
#' between.1 between.2 between.3
#' mixed.2_1b1w mixed.3_1b2w mixed.3_2b1w
#' within.1 within.2 within.3
NULL




#### MANOVA ####


#' Multi-factor ANOVA.
#'
#' @description
#' Easily perform multi-factor ANOVA (between-subjects, within-subjects, and mixed designs),
#' with or without covariates (ANCOVA). Print results to R Console (and MS Word).
#'
#' This function is based on and extends the \code{\link[afex:aov_car]{afex::aov_ez()}} function.
#' You only need to specify the data, dependent variable(s), and factors (between-subjects and/or within-subjects).
#' Almost all results you need will be displayed in an elegant manner, including effect sizes (partial \eqn{\eta^2}) and their confidence intervals (CIs).
#' 90\% CIs for partial \eqn{\eta^2} are reported, following the suggestion by Steiger (2004).
#'
#' @param data Data frame. Both \strong{long-format} and \strong{wide-format} can be used.
#' \itemize{
#'   \item If using \strong{long-format} data, please also set \strong{subID}.
#'   \item If using \strong{wide-format} data (i.e., one subject occupies one row, and repeated measures occupy multiple columns),
#'   the function can \strong{\emph{automatically}} transform the data into \strong{long-format}.
#' }
#' @param subID Subject ID.
#' \itemize{
#'   \item If using \strong{long-format} data, you should set the subject ID.
#'   \item If using \strong{wide-format} data, no need to set this parameter.
#' }
#' @param dv Variable name of dependent variable.
#' \itemize{
#'   \item If using \strong{long-format} data, then \code{dv} is the outcome variable.
#'   \item If using \strong{wide-format} data, then \code{dv} can only be used for complete between-subjects design.
#'   For designs with repeated measures, please use \code{dvs} and \code{dvs.pattern}.
#' }
#' @param dvs \strong{[only for "wide-format" data and designs with repeated measures]}
#'
#' Variable names of repeated measures.
#' \itemize{
#'   \item You can use \code{":"} to specify a range of variables: e.g., \code{"A1B1:A2B3"}
#'   (similar to the SPSS syntax "TO"; the variables should be put in order)
#'   \item You can also use a character vector to specify variable names: e.g., \code{c("Cond1", "Cond2", "Cond3")}
#' }
#' @param dvs.pattern \strong{[only for "wide-format" data and designs with repeated measures]}
#'
#' If you set \code{dvs}, you must also set the pattern of variable names by using \href{https://www.jb51.net/shouce/jquery1.82/regexp.html}{regular expressions}.
#'
#' \strong{Examples:}
#' \itemize{
#'   \item \code{"Cond(.)"} can extract levels from \code{"Cond1", "Cond2", "Cond3", ...}
#'
#'   \strong{You can rename the factor name} by using \code{within}: e.g., \code{within="Condition"}
#'   \item \code{"X(..)Y(..)"} can extract levels from \code{"X01Y01", "X02Y02", "XaaYbc", ...}
#'   \item \code{"X(.+)Y(.+)"} can extract levels from \code{"X1Y1", "XaYb", "XaY002", ...}
#' }
#'
#' \strong{Tips on regular expression:}
#' \itemize{
#'   \item \code{"(.)"} extracts any single character (can be number, letter, or other symbols)
#'   \item \code{"(.+)"} extracts >= 1 character(s)
#'   \item \code{"(.*)"} extracts >= 0 character(s)
#'   \item \code{"([0-9])"} extracts any single number
#'   \item \code{"([a-z])"} extracts any single letter
#'   \item each pair of \code{"()"} extracts levels for each factor
#' }
#' @param between Between-subjects factors. Character string (e.g., \code{"A"}) or vector (e.g., \code{c("A", "B")}). Default is \code{NULL}.
#' @param within Within-subjects factors. Character string (e.g., \code{"A"}) or vector (e.g., \code{c("A", "B")}). Default is \code{NULL}.
#' @param covariate Covariates (if necessary). Character string (e.g., \code{"age"}) or vector (e.g., \code{c("gender", "age", "edu")}). Default is \code{NULL}.
#' @param sph.correction \strong{[only effective for repeated measures with >= 3 levels]}
#'
#' Sphericity correction method to adjust the degrees of freedom (\emph{df}) when the sphericity assumption is violated. Default is \code{"none"}.
#' If Mauchly's test of sphericity is significant, you may set it to \code{"GG"} (Greenhouse-Geisser) or \code{"HF"} (Huynh-Feldt).
#' @param file File name of MS Word (\code{.doc}).
## @param which.observed \strong{[only effective for computing generalized \eqn{\eta^2}]}
##
## Factors that are observed or measured (e.g., gender, age group, measured covariates) but not experimentally manipulated. Default is \code{NULL}.
## The generalized \eqn{\eta^2} requires correct specification of the observed (vs. manipulated) variables.
## (If all the variables in \code{between} and \code{within} are set to \code{observed}, then generalized \eqn{\eta^2} will be equal to \eqn{\eta^2}.)
#' @param nsmall Number of decimal places of output. Default is \code{2}.
#'
#' @return
#' A result object returned by \code{\link[afex:aov_car]{afex::aov_ez()}}.
#'
#' @examples
#' \donttest{#### Between-Subjects Design ####
#'
#' between.1
#' MANOVA(data=between.1, dv="SCORE", between="A")
#'
#' between.2
#' MANOVA(data=between.2, dv="SCORE", between=c("A", "B"))
#'
#' between.3
#' MANOVA(data=between.3, dv="SCORE", between=c("A", "B", "C"))
#'
#'
#' #### Within-Subjects Design ####
#'
#' within.1
#' MANOVA(data=within.1, dvs="A1:A4", dvs.pattern="A(.)",
#'        within="A")
#' ## the same:
#' MANOVA(data=within.1, dvs=c("A1", "A2", "A3", "A4"), dvs.pattern="A(.)",
#'        within="MyFactor")  # renamed the within-subjects factor
#'
#' within.2
#' MANOVA(data=within.2, dvs="A1B1:A2B3", dvs.pattern="A(.)B(.)",
#'        within=c("A", "B"))
#'
#' within.3
#' MANOVA(data=within.3, dvs="A1B1C1:A2B2C2", dvs.pattern="A(.)B(.)C(.)",
#'        within=c("A", "B", "C"))
#'
#'
#' #### Mixed Design ####
#'
#' mixed.2_1b1w
#' MANOVA(data=mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
#'        between="A", within="B")
#' MANOVA(data=mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
#'        between="A", within="B", sph.correction="GG")
#'
#' mixed.3_1b2w
#' MANOVA(data=mixed.3_1b2w, dvs="B1C1:B2C2", dvs.pattern="B(.)C(.)",
#'        between="A", within=c("B", "C"))
#'
#' mixed.3_2b1w
#' MANOVA(data=mixed.3_2b1w, dvs="B1:B2", dvs.pattern="B(.)",
#'        between=c("A", "C"), within="B")
#'
#'
#' #### Other Examples ####
#' data.new=mixed.3_1b2w
#' names(data.new)=c("Group", "Cond_01", "Cond_02", "Cond_03", "Cond_04")
#' MANOVA(data=data.new, dvs="Cond_01:Cond_04", dvs.pattern="Cond_(..)",
#'        between="Group", within="Condition")  # renamed the within-subjects factor
#'
#' ?afex::obk.long
#' MANOVA(data=afex::obk.long, subID="id", dv="value",
#'        between=c("treatment", "gender"), within=c("phase", "hour"), cov="age",
#'        sph.correction="GG")
#' }
#' @references
#' Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared statistics: Measures of effect size for some common research designs.
#' \emph{Psychological Methods, 8}(4), 434-447. \doi{10.1037/1082-989X.8.4.434}
#'
#' Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis.
#' \emph{Psychological Methods, 9}(2), 164-182. \doi{10.1037/1082-989X.9.2.164}
#'
#' @seealso \code{\link{EMMEANS}}, \code{\link{bruceR-demodata}}
#'
#' @importFrom stats complete.cases
#' @export
MANOVA=function(data, subID=NULL, dv=NULL,
                dvs=NULL, dvs.pattern="",
                between=NULL, within=NULL, covariate=NULL,
                sph.correction="none",
                file=NULL,
                # which.observed=NULL,
                nsmall=2) {
  data0=data=as.data.frame(data)
  design=ifelse(is.null(within), "Between-Subjects Design",
                ifelse(is.null(between), "Within-Subjects Design",
                       "Mixed Design"))
  Print("<<yellow ====== MANOVA Output ({design}) ======>>")
  cat("\n")

  ## Add Participant ID (if necessary)
  if(is.null(subID)) {
    data$bruceR.ID=1:nrow(data)
    subID="bruceR.ID"
  }
  nsub=length(unique(data[[subID]]))

  ## Wide to Long (if necessary)
  if(is.null(dv)) {
    if(is.null(within)) {
      stop("Please specify the dependent variable.")
    } else {
      if(length(dvs)==1 & any(grepl(":", dvs)))
        dv.vars=convert2vars(data, varrange=dvs)$vars.raw
      else
        dv.vars=dvs
      dv="bruceY"  # "Y" will generate an error when dvs are like "X1Y1"
      data=tidyr::pivot_longer(data, cols=dv.vars,
                               names_to=within,
                               names_pattern=dvs.pattern,
                               values_to=dv)
      data=as.data.frame(data)
    }
  } else {
    dv.vars=dv
  }
  ncom=complete.cases(data[c(between, within, covariate)])
  nmis=length(ncom)-sum(ncom)

  ## Ensure Factorized Variables
  for(iv in c(between, within))
    data[[iv]]=as.factor(data[[iv]])

  ## Descriptive Statistics
  Print("<<underline Descriptive Statistics:>>")
  nmsd=eval(parse(text=Glue("
    plyr::ddply(data,
    plyr::.({paste(c(between, within), collapse=', ')}),
    dplyr::summarise,
    M=mean({dv}, na.rm=TRUE),
    SD=sd({dv}, na.rm=TRUE),
    n=length({dv}))")))
  N.info=Glue("{nsub}{ifelse(nmis>0, Glue(' ({nmis} missing observations deleted)'), '')}")
  print_table(nmsd, row.names=FALSE, nsmalls=nsmall)
  Print("Total sample size: <<italic N>> = {N.info}")
  cat("\n")

  nmsd$M=formatF(nmsd$M, nsmall)
  nmsd$SD=formatF(nmsd$SD, nsmall)
  names(nmsd)[(ncol(nmsd)-2):ncol(nmsd)]=c("<i>M</i>", "<i>SD</i>", "<i>n</i>")
  nmsd.html=paste0(
    "<p><br/><br/></p>",
    "<p><b>Descriptive Statistics:</b></p>",
    df_to_html(
      nmsd,
      align.head=c(rep("left", times=ncol(nmsd)-3),
                   rep("right"), times=3),
      align.text=c(rep("left", times=ncol(nmsd)-3),
                   rep("right"), times=3))$TABLE,
    "<p>Total sample size: <i>N</i> = ", N.info, "</p>"
  )

  ## Main MANOVA Functions
  suppressMessages({
    aov.ez=afex::aov_ez(
      data=data, id=subID, dv=dv,
      between=between,
      within=within,
      covariate=covariate,
      type="III",
      # observed=which.observed,
      anova_table=list(correction=sph.correction, es="ges"),
      fun_aggregate=mean,
      include_aov=FALSE,
      factorize=FALSE,
      print.formula=FALSE)
  })
  at=aov.ez$anova_table
  names(at)[1:2]=c("df1", "df2")
  at$MS=at$`F`*at$`MSE`
  eta2=effectsize::F_to_eta2(at$`F`, at$df1, at$df2)
  at$p.eta2=paste0(formatF(eta2$Eta2_partial, nsmall+1), " [",
                   formatF(eta2$CI_low, nsmall+1), ", ",
                   formatF(eta2$CI_high, nsmall+1), "]") %>%
    stringr::str_replace_all("0\\.", ".")
  at0=at=at[c("MS", "MSE", "df1", "df2", "F", "Pr(>F)", "p.eta2")]
  names(at)[7]=c("\u03b7\u00b2p [90% CI]")
  row.names(at)=row.names(aov.ez$anova_table)
  df.nsmall=ifelse(sph.correction=="none", 0, nsmall)
  at.nsmalls=c(nsmall, nsmall, df.nsmall, df.nsmall, nsmall, 0, 0)
  DEP=ifelse(is.null(within), dv, paste(dv.vars, collapse=", "))
  BET=ifelse(is.null(between), "\u2013", paste(between, collapse=", "))
  WIT=ifelse(is.null(within), "\u2013", paste(within, collapse=", "))
  COV=ifelse(is.null(covariate), "\u2013", paste(covariate, collapse=", "))
  Print("
  <<underline ANOVA Table:>>
  Dependent variable(s):      {DEP}
  Between-subjects factor(s): {BET}
  Within-subjects factor(s):  {WIT}
  Covariate(s):               {COV}
  ")
  print_table(at, nsmalls=at.nsmalls)
  Print("<<blue MSE = Mean Square Error (an estimate of population variance \u03c3\u00b2).>>")

  if(sph.correction %in% c("GG", "HF")) {
    if(sph.correction=="GG")
      sph.text="GG (Greenhouse-Geisser)"
    if(sph.correction=="HF")
      sph.text="HF (Huynh-Feldt)"
    Print("<<green Sphericity correction method: {sph.text}>>")
  }

  ## All Other Effect-Size Measures (deprecated; please use `effectsize` package)
  # https://github.com/strengejacke/sjstats/blob/master/R/anova_stats.R#L116
  Print("\n\n\n<<magenta
  \u03c9\u00b2 = omega-squared = (SS - df1 * MSE) / (SST + MSE)
  \u03b7\u00b2 = eta-squared = SS / SST
  \u03b7\u00b2G = generalized eta-squared (see Olejnik & Algina, 2003)
  \u03b7\u00b2p = partial eta-squared = SS / (SS + SSE) = F * df1 / (F * df1 + df2)
  Cohen\u2019s <<italic f>> = sqrt( \u03b7\u00b2p / (1 - \u03b7\u00b2p) )
  >>")

  ## Levene's Test for Homogeneity of Variance
  try({ levene_test(dv.vars, between, data0) }, silent=TRUE)

  ## Mauchly's Test of Sphericity
  if(!is.null(within)) {
    Print("\n\n\n<<underline Mauchly\u2019s Test of Sphericity:>>")
    suppressWarnings({
      sph=summary(aov.ez$Anova)$sphericity.tests
    })
    colnames(sph)=c("Mauchly's W", "p")
    if(length(sph)==0) {
      message("No factors have more than 2 levels, so no need to do the sphericity test.")
    } else {
      print(sph)
      if(min(sph[,2])<.05 & sph.correction=="none") {
        Print("<<red The sphericity assumption is violated.
              You may set 'sph.correction' to 'GG' or 'HF'.>>")
      }
    }
  }
  cat("\n")

  if(!is.null(file)) {
    print_table(
      at,
      nsmalls=at.nsmalls,
      col.names=c("<i>MS</i>", "<i>MSE</i>",
                  "<i>df</i><sub>1</sub>", "<i>df</i><sub>2</sub>",
                  "<i>F</i>", "<i>p</i>", " ",
                  "\u03b7<sup>2</sup><sub><i>p</i></sub> [90% CI]"),
      file=file,
      file.align.text=c("left",
                        "right", "right",
                        "right", "right",
                        "right", "right",
                        "left", "right"),
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
  aov.ez$between=between
  aov.ez$within=within
  invisible(aov.ez)
}


#' Simple-effect analysis and post-hoc multiple comparison.
#'
#' @description
#' Easily perform (1) simple-effect (and simple-simple-effect) analyses,
#' including both simple main effects and simple interaction effects,
#' and (2) post-hoc multiple comparisons (e.g., pairwise, sequential, polynomial),
#' with \emph{p} values adjusted for factors with >= 3 levels.
#'
#' This function is based on and extends the
#' (1) \code{\link[emmeans:joint_tests]{emmeans::joint_tests()}},
#' (2) \code{\link[emmeans:emmeans]{emmeans::emmeans()}}, and
#' (3) \code{\link[emmeans:contrast]{emmeans::contrast()}} functions.
#' You only need to specify the model object, to-be-tested effect(s), and moderator(s).
#' Almost all results you need will be displayed in an elegant manner,
#' including effect sizes (partial \eqn{\eta^2} and Cohen's \emph{d}) and their confidence intervals (CIs).
#' 90\% CIs for partial \eqn{\eta^2} and 95\% CIs for Cohen's \emph{d} are reported.
#'
#' To compute Cohen's \emph{d} and its 95\% CI in pairwise comparisons,
#' this function uses the pooled \emph{SD}:
#' \strong{\code{SD_pooled = sqrt(MSE)}}, where \code{MSE} is of the effect term extracted from ANOVA table.
#'
#' \strong{\emph{Disclaimer}:}
#' There is substantial disagreement on what is the appropriate pooled \emph{SD} to use in computing effect sizes.
#' For alternative methods, see \code{\link[emmeans:eff_size]{emmeans::eff_size()}} and \code{\link[effectsize:t_to_r]{effectsize::t_to_d()}}.
#' Users should \emph{not} take the default output as the only right results and are completely responsible for specifying \code{sd.pooled}.
#'
#' @section Statistical Details:
#'
#' Some may confuse the statistical terms "simple effects", "post-hoc tests", and "multiple comparisons".
#' Such a confusion is not uncommon. Here, I explain what these terms actually refer to.
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
#'     In ANOVA, we call it "simple-effect analysis";
#'     in regression, we also call it "simple-slope analysis".
#'     They are identical in statistical principles.
#'     Nonetheless, the situations in ANOVA can be a bit more complex because we sometimes have a three-factors design.
#'
#'     In a regular two-factors design, we only test \strong{"simple main effects"}.
#'     That is, on the different levels of a factor "B", the main effects of "A" would be different.
#'     However, in a three-factors (or more) design, we may also test \strong{"simple interaction effects"} and \strong{"simple simple effects"}.
#'     That is, on the different combinations of levels of factors "B" and "C", the main effects of "A" would be different.
#'
#'     In SPSS, we usually use the \code{MANOVA} and/or the \code{GLM + /EMMEANS} syntax to perform such analyses.
#'     Tutorials (in Chinese) for the SPSS syntax can be found in:
#'     \href{https://zhuanlan.zhihu.com/p/30037168}{Tutorial #1},
#'     \href{https://zhuanlan.zhihu.com/p/31863288}{Tutorial #2}, and
#'     \href{https://zhuanlan.zhihu.com/p/35011046}{Tutorial #3}.
#'
#'     Here, the R function \code{EMMEANS} can do the same thing as in SPSS and can do much better and easier (just see the section "Examples").
#'
#'     To note, simple effects \emph{per se} do NOT need any form of \emph{p}-value adjustment, because what we test in simple-effect analyses are still "omnibus \emph{F}-tests".
#'   }
#'   \item{\strong{2. Post-Hoc Test}}{
#'     The term "post-hoc" means that the tests are performed after ANOVA. Given this, some may (wrongly) regard simple-effect analyses also as a kind of post-hoc tests.
#'     However, these two terms should be distinguished. In many situations and softwares,
#'     "post-hoc tests" only refer to \strong{"post-hoc comparisons"} using \emph{t}-tests and some \emph{p}-value adjustment techniques.
#'     We need post-hoc comparisons \strong{only when there are factors with 3 or more levels}.
#'     For example, we can perform the post-hoc comparisons of mean values (1) across multiple levels of one factor in a pairwise way or (2) particularly between the two conditions "A1B1" and "A2B2".
#'
#'     Post-hoc tests are totally \strong{independent of} whether there is a significant interaction effect. \strong{It only deals with factors with multiple levels.}
#'     In most cases, we use pairwise comparisons to do post-hoc tests. See the next part for details.
#'   }
#'   \item{\strong{3. Multiple Comparison}}{
#'     As mentioned above, multiple comparisons are post-hoc tests by its nature but do NOT have any relationship with simple-effect analyses.
#'     In other words, "(post-hoc) multiple comparisons" are \strong{independent of} "interaction effects" and "simple effects".
#'     What's more, when the simple main effect is of a factor with 3 or more levels, we also need to do multiple comparisons (e.g., pairwise comparisons) \emph{within} the simple-effect analysis.
#'     In this situation (i.e., >= 3 levels), we need \emph{p}-value adjustment methods such as Bonferroni, Tukey's HSD (honest significant difference), FDR (false discovery rate), and so forth.
#'
#'     There are many ways to do multiple comparisons. All these methods are included in the current \code{EMMEANS} function.
#'     If you are familiar with SPSS syntax, you may feel that the current R functions \code{MANOVA} and \code{EMMEANS} are a nice combination of the SPSS syntax \code{MANOVA} and \code{GLM + /EMMEANS}.
#'     Yes, they are. More importantly, they outperform the SPSS syntax, either for its higher convenience or for its more fruitful results.
#'
#'     \itemize{
#'       \item \code{"pairwise"} - Pairwise comparisons (default is "higher level - lower level")
#'       \item \code{"seq"} or \code{"consec"} - Consecutive (sequential) comparisons
#'       \item \code{"poly"} - Polynomial contrasts (linear, quadratic, cubic, quartic, ...)
#'       \item \code{"eff"} - Effect contrasts (vs. the grand mean)
#'     }
#'   }
#' }
#'
#' @param model A model object returned by \code{\link{MANOVA}}.
#' @param effect The effect(s) you want to test.
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
#' @param sd.pooled By default, it uses \strong{\code{sqrt(MSE)}} to compute Cohen's \emph{d}.
#' Users may also manually set it (e.g., the \emph{SD} of a reference group, or using \code{\link[effectsize:sd_pooled]{effectsize::sd_pooled()}}).
#' @param spss Return results identical to SPSS.
#' Default is \code{TRUE}, which uses the \code{lm} (rather than \code{aov}) object in \code{model}
#' for \code{\link[emmeans:joint_tests]{emmeans::joint_tests()}} and \code{\link[emmeans:emmeans]{emmeans::emmeans()}}.
#' @param nsmall Number of decimal places of output. Default is \code{2}.
#'
#' @return
#' The same object as returned by \code{\link{MANOVA}} (for recursive use).
#'
#' @examples
#' \donttest{#### Between-Subjects Design ####
#'
#' between.1
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A")
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A", p.adjust="tukey")
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A", contrast="seq")
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A", contrast="poly")
#'
#' between.2
#' MANOVA(data=between.2, dv="SCORE", between=c("A", "B")) %>%
#'   EMMEANS("A") %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B") %>%
#'   EMMEANS("B", by="A")
#'
#' between.3
#' MANOVA(data=between.3, dv="SCORE", between=c("A", "B", "C")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#' ## just to name a few
#' ## you can test many other combinations of effects
#'
#'
#' #### Within-Subjects Design ####
#'
#' within.1
#' MANOVA(data=within.1, dvs="A1:A4", dvs.pattern="A(.)",
#'        within="A") %>%
#'   EMMEANS("A")
#'
#' within.2
#' MANOVA(data=within.2, dvs="A1B1:A2B3", dvs.pattern="A(.)B(.)",
#'        within=c("A", "B")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B", by="A")  # singular error matrix
#'
#' within.3
#' MANOVA(data=within.3, dvs="A1B1C1:A2B2C2", dvs.pattern="A(.)B(.)C(.)",
#'        within=c("A", "B", "C")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#'
#'
#' #### Mixed Design ####
#'
#' mixed.2_1b1w
#' MANOVA(data=mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
#'        between="A", within="B", sph.correction="GG") %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B", by="A")
#'
#' mixed.3_1b2w
#' MANOVA(data=mixed.3_1b2w, dvs="B1C1:B2C2", dvs.pattern="B(.)C(.)",
#'        between="A", within=c("B", "C")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#'
#' mixed.3_2b1w
#' MANOVA(data=mixed.3_2b1w, dvs="B1:B2", dvs.pattern="B(.)",
#'        between=c("A", "C"), within="B") %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("A", by="C") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("B", by=c("A", "C"))
#'
#'
#' #### Other Examples ####
#' air=airquality
#' air$Day.1or2=ifelse(air$Day %% 2 == 1, 1, 2) %>%
#'   factor(levels=1:2, labels=c("odd", "even"))
#' MANOVA(data=air, dv="Temp", between=c("Month", "Day.1or2"),
#'        covariate=c("Solar.R", "Wind")) %>%
#'   EMMEANS("Month", contrast="seq") %>%
#'   EMMEANS("Month", by="Day.1or2", contrast="poly")
#' }
#' @seealso \code{\link{MANOVA}}, \code{\link{bruceR-demodata}}
#'
#' @export
EMMEANS=function(model, effect=NULL, by=NULL,
                 contrast="pairwise",
                 reverse=TRUE,
                 p.adjust="bonferroni",
                 sd.pooled=NULL,
                 spss=TRUE,
                 nsmall=2) {
  # model.raw=model
  # if(spss) model$aov=NULL

  # IMPORTANT: If include 'aov', the 'emmeans' results of
  # within-subjects design will not be equal to those in SPSS!
  # So we do not include 'aov' object but instead use 'lm' and 'mlm'
  # objects to do the follow-up 'emmeans' analyses!

  # Bug: For within-sub 'effect' and between-sub 'by' in mixed design
  # if(!is.null(effect) & !is.null(model$within) & effect %anyin% model$within &
  #    !is.null(by) & !is.null(model$between) & by %anyin% model$between) {
  #   model=model.raw
  # }

  ## Simple Effect (omnibus)
  # see 'weights' in ?emmeans
  try({
    sim=NULL
    suppressMessages({
      sim=emmeans::joint_tests(object=model, by=by,
                               weights="equal",
                               model=ifelse(spss, "multivariate", "univariate"))
    })
    names(sim)[1]="Effect"
    eta2=effectsize::F_to_eta2(sim$F.ratio, sim$df1, sim$df2)
    sim$sig=sig.trans(sim$p.value)
    sim$p.eta2=paste0(formatF(eta2$Eta2_partial, nsmall+1), " [",
                      formatF(eta2$CI_low, nsmall+1), ", ",
                      formatF(eta2$CI_high, nsmall+1), "]")
    sim$df1=formatF(sim$df1, 0)
    sim$df2=formatF(sim$df2, 0)
    sim$F.ratio=formatF(sim$F.ratio, nsmall)
    sim$p.value=p.trans(sim$p.value)
    names(sim)[(length(by)+4):(length(by)+7)]=
      c("F", "p", " ", "\u03b7\u00b2p [90% CI]")
    if(!is.null(by)) {
      for(i in 1:length(by)) {
        sim[, i+1]=paste0("(", names(sim)[i+1], " = ", sim[, i+1], ")")
        names(sim)[i+1]=paste0("(By: ", names(sim)[i+1], ")")
      }
    }
  }, silent=TRUE)

  effect.text=paste(effect, collapse='\" & \"')
  Print("<<yellow ------ EMMEANS Output (effect = \"{effect.text}\") ------>>")
  cat("\n")
  Print("<<underline {ifelse(is.null(by), 'Omnibus Test', 'Simple Effects')} of \"{effect.text}\":>>")
  if(is.null(sim) | "note" %in% names(sim))
    message("Warning:
    WITHIN CELLS error matrix is SINGULAR.
    Some variables are LINEARLY DEPENDENT.
    The simple effect could be misleading.")
  else
    print_table(sim, nsmalls=2, row.names=FALSE)
  cat("\n")

  ## Estimated Marginal Means (emmeans)
  suppressMessages({
    emm0=emm=emmeans::emmeans(object=model, specs=effect, by=by,
                              weights="equal",
                              model=ifelse(spss, "multivariate", "univariate"))
  })
  emm=summary(emm)  # to a data.frame (class 'summary_emm')
  emm$emmean=formatF(emm$emmean, nsmall)
  emm$SE=paste0("(", formatF(emm$SE, nsmall), ")")
  emm$CI=paste0("[",
                formatF(emm$lower.CL, nsmall), ", ",
                formatF(emm$upper.CL, nsmall), "]")
  emm$df=NULL
  emm$lower.CL=NULL
  emm$upper.CL=NULL
  names(emm)[(length(emm)-2):length(emm)]=
    c("Mean", "S.E.", "[95% CI of Mean]")
  attr(emm, "mesg")[which(grepl("^Confidence", attr(emm, "mesg")))]=
    "Estimated means use an equally weighted average."

  Print("<<underline Estimated Marginal Means of \"{effect.text}\":>>")
  print(emm)
  cat("\n")

  ## Multiple Comparison (pairwise or other methods)
  # see: ?contrast, ?pairs.emmGrid, ?pairwise.emmc
  contr.method=switch(contrast,
                      pairwise=,
                      revpairwise="Pairwise Comparisons",
                      consec=,
                      seq="Consecutive (Sequential) Comparisons",
                      poly="Polynomial Contrasts",
                      eff="Effect Contrasts (vs. grand mean)",
                      "Multiple Comparisons")
  if(contrast=="pairwise" & reverse==TRUE) contrast="revpairwise"
  if(contrast=="seq") contrast="consec"
  if(contrast=="consec") reverse=FALSE
  if(contrast=="poly") p.adjust="none"
  con0=con=emmeans::contrast(emm0, method=contrast, adjust=p.adjust, reverse=reverse)
  # pairs(emm, simple="each", reverse=TRUE, combine=TRUE)
  conCI=stats::confint(con)
  con=summary(con)  # to a data.frame (class 'summary_emm')
  con$sig=sig.trans(con$p.value)

  # Cohen's d
  rn=row.names(model$anova_table)
  term=c()
  for(i in rn) if(i %in% effect) term=c(term, i)
  term=paste(term, collapse=":")
  if(is.null(sd.pooled))
    sd.pooled=sqrt(model$anova_table[term, "MSE"])
  if(contrast!="poly")
    attr(con, "mesg")=c(Glue("SD_pooled for computing Cohen\u2019s d: {formatF(sd.pooled, nsmall)}"),
                        attr(con, "mesg"))
  # es=emmeans::eff_size(emm0, method=contrast,
  #                      sigma=stats::sigma(model$lm),
  #                      edf=stats::df.residual(model$lm))
  # es=summary(es)
  # con$d=paste0(formatF(es$effect.size, nsmall), " [",
  #              formatF(es$lower.CL, nsmall), ", ",
  #              formatF(es$upper.CL, nsmall), "]")
  con$d=paste0(formatF(con$estimate/sd.pooled, nsmall), " [",
               formatF(conCI$lower.CL/sd.pooled, nsmall), ", ",
               formatF(conCI$upper.CL/sd.pooled, nsmall), "]")
  con$estimate=formatF(con$estimate, nsmall)
  con$SE=paste0("(", formatF(con$SE, nsmall), ")")
  con$t.ratio=formatF(con$t.ratio, nsmall)
  con$p.value=p.trans(con$p.value)
  p.mesg.index=grepl("^P value adjustment", attr(con, "mesg"))
  names(con)[c(1, (length(con)-6):length(con))]=
    c("Contrast", "b", "S.E.", "df", "t",
      ifelse(any(p.mesg.index), "p*", "p"),
      " ", "Cohen's d [95% CI]")
  if(any(p.mesg.index)) {
    p.mesg=attr(con, "mesg")[which(p.mesg.index)]
    method.mesg=stringr::str_extract(p.mesg, "(?<=: ).+(?= method)")
    if(method.mesg %in% c("fdr", "mvt"))
      method.mesg.new=toupper(method.mesg)
    else
      method.mesg.new=capitalize(method.mesg)
    p.mesg.new=paste0("P-value adjustment: ", method.mesg.new, strsplit(p.mesg, method.mesg)[[1]][2], ".")
    attr(con, "mesg")[which(p.mesg.index)]=p.mesg.new
  } else if(p.adjust!="none") {
    attr(con, "mesg")=c(attr(con, "mesg"),
                        "No need to adjust p values.")
  }
  if(contrast=="poly") con[c("Cohen's d [95% CI]")]=NULL

  Print("<<underline {contr.method} of \"{effect.text}\":>>")
  print(con)
  Print("<<yellow Disclaimer (about Cohen\u2019s d):>>
  <<cyan
  There is considerable disagreement on how to compute Cohen\u2019s d.
  You should <<italic not>> take the above output as the only right results.
  You are completely responsible for setting the \"sd.pooled\".
  >>")
  if(con0@misc[["famSize"]] > 2 & p.adjust != "none")
    cat("\n")
  if(any(grepl("averaged|SD_pooled", attr(con, "mesg"))) & any(p.mesg.index)==FALSE)
    cat("\n")

  ## Return (return the raw model for recycling across '%>%' pipelines)
  invisible(model)
}


## Levene's Test for Homogeneity of Variance
levene_test=function(dvs, ivs.between, data) {
  Print("\n\n\n<<underline Levene\u2019s Test for Homogeneity of Variance:>>")
  if(is.null(ivs.between)) {
    message("No between-subjects factors, so no need to do the Levene's test.")
  } else {
    for(iv in ivs.between)
      data[[iv]]=as.factor(data[[iv]])
    for(dv in dvs) {
      f=as.formula(Glue("{dv} ~ {paste(ivs.between, collapse='*')}"))
      test1=car::leveneTest(f, data, center=mean)
      test2=car::leveneTest(f, data, center=median)
      test=rbind(test1[1,], test2[1,])
      test=cbind(test[2], test[1], df2=c(test1[2,"Df"], test2[2,"Df"]), test[3])
      names(test)=c("Levene's F", "df1", "df2", "p")
      test$sig=sig.trans(test$p)
      test$p=p.trans(test$p)
      test$`Levene's F`=formatF(test$`Levene's F`, 2)
      row.names(test)=c("Based on Mean", "Based on Median")
      Print("DV = {dv}:")
      print(test)
      if(which(dv==dvs) < length(dvs)) cat("\n")
    }
  }
}

