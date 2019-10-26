#### t test ####



#### MANOVA ####

## Demo data
if(FALSE) {
  library(rio)
  between.1=import("data-raw/between.1.sav", haven=F); names(between.1)[2]="SCORE"
  between.2=import("data-raw/between.2.sav", haven=F)
  between.3=import("data-raw/between.3.sav", haven=F)
  usethis::use_data(between.1, overwrite=TRUE)
  usethis::use_data(between.2, overwrite=TRUE)
  usethis::use_data(between.3, overwrite=TRUE)
  within.1=import("data-raw/within.1.sav", haven=F)
  within.2=import("data-raw/within.2.sav", haven=F)
  within.3=import("data-raw/within.3.sav", haven=F)
  usethis::use_data(within.1, overwrite=TRUE)
  usethis::use_data(within.2, overwrite=TRUE)
  usethis::use_data(within.3, overwrite=TRUE)
  mixed.2_1b1w=import("data-raw/mixed.2_1b1w.sav", haven=F)
  mixed.3_1b2w=import("data-raw/mixed.3_1b2w.sav", haven=F)
  mixed.3_2b1w=import("data-raw/mixed.3_2b1w.sav", haven=F)
  usethis::use_data(mixed.2_1b1w, overwrite=TRUE)
  usethis::use_data(mixed.3_1b2w, overwrite=TRUE)
  usethis::use_data(mixed.3_2b1w, overwrite=TRUE)
}


#' Multivariate ANOVA
#'
#' Easily perform MANOVA (between-subjects, within-subjects, and mixed design).
#'
#' This function is based on and extends the \code{\link[afex]{aov_ez}} function in the R package \code{afex}.
#' You only need to specify the data, dependent variable(s), and factors (between-subjects and/or within-subjects).
#' Then, almost all the outputs you need will be displayed in an elegant manner, including effect sizes (partial \eqn{\eta^2}) and their confidence intervals (CIs).
#' 90\% CIs for partial \eqn{\eta^2} are reported, following the suggestion by Steiger (2004).
#'
#' In addition to partial \eqn{\eta^2}, it will also output many other effect-size measures:
#' \eqn{\eta^2}, generalized \eqn{\eta^2}, \eqn{\omega^2}, and Cohen's \emph{f}.
#' For statistical details, see \url{https://en.wikipedia.org/wiki/Effect_size}
#'
#' \strong{Demo Datasets:}
#'
#' The demo datasets were obtained from a course of "multifactor experimental design" in \emph{Beijing Normal University} (2016).
#' In this course, we used a book written by Prof. Hua Shu (\strong{\emph{"Factorial Experimental Design in Psychological and Educational Research"}}).
#' The book provided a seires of demo datasets to show different experimental designs and how to do MANOVA in SPSS.
#' Here, we reuse these excellent demo datasets to show how the R function \code{MANOVA} can easily handle almost all types of designs.
#' Many thanks go to the contributors of the datasets.
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
#' @import afex
#' @importFrom tidyr pivot_longer
#' @importFrom sjstats anova_stats
#' @param data \code{data.frame} or \code{data.table}. You can directly input a "wide-format" data (i.e., one subject occupies one row, and repeated measures occupy multiple columns).
#' Then, if your data have any repeated measures, the function will automatically transform it into a long-format data for further analyses.
#'
#' If you input a long-format data, please also specify the variable name of subID in your data (see below, e.g., \code{subID="ID"}).
#'
#' However, in general, you can just input a wide-format data (like in SPSS) and enjoy the convenience of this function!
#' @param dv \strong{[for purely between-subjects design or for long-format data]} Variable name of the dependent variable in your data.
#' @param dvs \strong{[for designs with repeated measures]} Variable names of repeated measures in your data. You can use \code{":"} to specify a range of vairables (e.g., \code{"A1B1:A2B3"}).
#' @param dvs.pattern \strong{[for designs with repeated measures]} If you set \code{dvs}, you must also specify the pattern of these variables. Regular expressions are used here.
#' For example, \code{"A(.)B(.)"} will extract the factor levels and leave "A" and "B" as new variables in a long-format data.
#' @param between Between-subjects factors. Can be either a character string (e.g., \code{"A"}) or a character vector (e.g., \code{c("A", "B")}). Default is \code{NULL}.
#' @param within Within-subjects factors. Can be either a character string (e.g., \code{"A"}) or a character vector (e.g., \code{c("A", "B")}). Default is \code{NULL}.
#' @param covariate Covariates (if necessary). Can be either a character string or a character vector. Default is \code{NULL}.
#' @param sphericity.correction \strong{[only effective for repeated measures with >= 3 levels]} Sphericity correction method to adjust the degrees of freedom (\emph{df}) when the sphericity assumption is violated. Default is \code{"none"}.
#' If Mauchly's test of sphericity is significant, you may set it to \code{"GG"} (Greenhouse-Geisser) or \code{"HF"} (Huynh-Feldt).
#' @param subID As mentioned above, if your input is already a long-format data, then you should specify the subject ID because one subject occupies multiple rows.
#' But in most cases, you don't need to do that.
#' @param factorize Just leave this parameter as default. For details, see \code{afex::\link[afex]{aov_ez}}.
#' @examples
#' ## Between-Subjects Design
#'
#' View(between.1)
#' MANOVA(data=between.1, dv="SCORE", between="A")
#'
#' View(between.2)
#' MANOVA(data=between.2, dv="SCORE", between=c("A", "B"))
#'
#' View(between.3)
#' MANOVA(data=between.3, dv="SCORE", between=c("A", "B", "C"))
#'
#'
#' ## Within-Subjects Design
#'
#' View(within.1)
#' MANOVA(data=within.1, dvs="A1:A4", dvs.pattern="A(.)",
#'        within="A")
#'
#' View(within.2)
#' MANOVA(data=within.2, dvs="A1B1:A2B3", dvs.pattern="A(.)B(.)",
#'        within=c("A", "B"))
#'
#' View(within.3)
#' MANOVA(data=within.3, dvs="A1B1C1:A2B2C2", dvs.pattern="A(.)B(.)C(.)",
#'        within=c("A", "B", "C"))
#'
#'
#' ## Mixed Design
#'
#' View(mixed.2_1b1w)
#' MANOVA(data=mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
#'        between="A", within="B")
#' MANOVA(data=mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
#'        between="A", within="B", sphericity.correction="GG")
#'
#' View(mixed.3_1b2w)
#' MANOVA(data=mixed.3_1b2w, dvs="B1C1:B2C2", dvs.pattern="B(.)C(.)",
#'        between="A", within=c("B", "C"))
#'
#' View(mixed.3_2b1w)
#' MANOVA(data=mixed.3_2b1w, dvs="B1:B2", dvs.pattern="B(.)",
#'        between=c("A", "C"), within="B")
#' @references
#' Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared statistics: Measures of effect size for some common research designs. \emph{Psychological Methods, 8}(4), 434-447. \url{https://doi.org/10.1037/1082-989X.8.4.434}
#'
#' Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. \emph{Psychological Methods, 9}(2), 164-182. \url{https://doi.org/10.1037/1082-989X.9.2.164}
#' @seealso \code{\link{EMMEANS}}
#' @export
MANOVA=function(data, dv=NULL, dvs=NULL, dvs.pattern="",
                between=NULL, within=NULL, covariate=NULL,
                sphericity.correction="none", subID=NULL,
                factorize=ifelse(is.null(covariate), TRUE, FALSE)) {
  data=as.data.frame(data)
  design=ifelse(is.null(within), "Between-Subjects Design",
                ifelse(is.null(between), "Within-Subjects Design",
                       "Mixed Design"))
  Print("<<yellow ------ MANOVA Output ({design}) ------>>")
  cat("\n")

  ## Add Participant ID (if necessary)
  if(is.null(subID)) {
    data$ID=1:nrow(data)
    subID="ID"
  }
  nsub=data[[subID]] %>% unique() %>% length()

  ## Wide to Long (if necessary)
  if(is.null(dv)) {
    if(is.null(within)) {
      stop("Please specify the dependent variable.")
    } else {
      vars=convert2vars(data, varrange=dvs)$vars.raw
      data=pivot_longer(data, cols=vars,
                        names_to=within,
                        names_pattern=dvs.pattern,
                        values_to="Y") %>% as.data.frame()
      dv="Y"
    }
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
    Mean=mean({dv}, na.rm=T),
    S.D.=sd({dv}, na.rm=T),
    N=length({dv}))")))
  print_table(nmsd, row.names=FALSE, nsmalls=2)
  Print("Total sample size: <<italic N>> = {nsub}{ifelse(nmis>0, Glue(' ({nmis} missing observations deleted)'), '')}")
  cat("\n")

  ## Main MANOVA Functions
  aov.ez=aov_ez(data=data, id=subID, dv=dv,
                between=between, within=within, covariate=covariate,
                factorize=factorize,
                anova_table=list(correction=sphericity.correction,
                                 es="ges"),
                include_aov=TRUE,  # see EMMEANS, default will be FALSE
                print.formula=FALSE)
  at=aov.ez$anova_table
  names(at)[1:2]=c("df1", "df2")
  at=mutate(at, MS=`F`*MSE,
            g.eta2=at$ges,
            p.eta2=mapply(eta_sq_ci, `F`, df1, df2, return="eta2"),
            LLCI=mapply(eta_sq_ci, `F`, df1, df2, return="LLCI"),
            ULCI=mapply(eta_sq_ci, `F`, df1, df2, return="ULCI"))
  at0=at=at[c("MS", "MSE", "df1", "df2", "F", "Pr(>F)",
              "g.eta2", "p.eta2", "LLCI", "ULCI")]
  at$g.eta2=NULL
  names(at)[7:9]=c("  \u03b7\u00b2p", "[90% ", "  CI]")
  row.names(at)=row.names(aov.ez$anova_table)
  df.nsmall=ifelse(sphericity.correction=="none", 0, 2)
  Print("
  <<underline ANOVA Table:>>
  Dependent variable(s):      {ifelse(is.null(within), dv, paste0(vars, collapse=', '))}
  Between-subjects factor(s): {ifelse(is.null(between), '-', paste0(between, collapse=', '))}
  Within-subjects factor(s):  {ifelse(is.null(within), '-', paste0(within, collapse=', '))}
  Covariate(s):               {ifelse(is.null(covariate), '-', paste0(covariate, collapse=', '))}
  ")
  print_table(at, nsmalls=c(3, 3, df.nsmall, df.nsmall,
                            2, 0, 3, 3, 3))
  Print("<<blue MSE = Mean Square Error (an estimate of the population variance \u03c3\u00b2)>>")
  if(sphericity.correction=="GG")
    Print("<<green Sphericity correction method: GG (Greenhouse-Geisser)>>")
  if(sphericity.correction=="HF")
    Print("<<green Sphericity correction method: HF (Huynh-Feldt)>>")

  ## All Other Effect-Size Measures
  # https://github.com/strengejacke/sjstats/blob/master/R/anova_stats.R#L116
  # Replace partial.etasq and cohens.f, due to their wrong results
  Print("\n\n\n<<underline ANOVA Effect Size:>>")
  effsize=anova_stats(aov.ez$aov)
  effsize$stratum=NULL
  effsize=effsize[-which(effsize$term=="Residuals"),]
  effsize=mutate(effsize,
                 partial.etasq=round(at0$p.eta2, 3),
                 cohens.f=round(sqrt(partial.etasq/(1-partial.etasq)), 3),
                 generalized.etasq=round(at0$g.eta2, 3))
  names(effsize)=c("Term", "df", "Sum Sq", "Mean Sq", "F", "p",
                   "     \u03b7\u00b2",  # eta2
                   "  \u03b7\u00b2[p]",  # eta2_p
                   "     \u03c9\u00b2",  # omega2
                   "  \u03c9\u00b2[p]",  # omega2_p
                   "     \u03b5\u00b2",  # epsilon2
                   "Cohen's f", "Post-Hoc Power",
                   "  \u03b7\u00b2[G]")  # eta2_G
  row.names(effsize)=effsize$Term
  print(effsize[c(9, 7, 14, 8, 12)])  # omega2, eta2, eta2g, eta2p, f
  Print("\n\n\n<<blue
  \u03c9\u00b2: omega-squared = (SS - df1 * MSE) / (SST + MSE)
  \u03b7\u00b2: eta-squared = SS / SST
  \u03b7\u00b2G: generalized eta-squared (see Olejnik & Algina, 2003)
  \u03b7\u00b2p: partial eta-squared = SS / (SS + SSE) <<bold <<magenta = >>>><<bold <<magenta F * df1 / (F * df1 + df2)>>>>
  Cohen\u2019s <<italic f>>: = sqrt( \u03b7\u00b2p / (1 - \u03b7\u00b2p) )
  >>")

  ## Mauchly's Test of Sphericity
  if(!is.null(within)) {
    Print("\n\n\n<<underline Mauchly Test of Sphericity:>>")
    sph=summary(aov.ez$Anova)$sphericity.tests
    colnames(sph)=c("Mauchly's W", "p")
    if(length(sph)==0) {
      message("No factors have more than 2 levels, so no need to do the sphericity test.")
    } else {
      print(sph)
      if(min(sph[,2])<.05 & sphericity.correction=="none") {
        Print("<<red The sphericity assumption is violated.
              You may set 'sphericity.correction' to 'GG' or 'HF'.>>")
      }
    }
  }
  cat("\n")

  ## Return
  invisible(aov.ez)
}


#' Simple-effect analyses (for interactions) and post-hoc multiple comparisons (for factors with >= 3 levels)
#'
#' Easily perform 1) simple-effect and simple-simple-effect analyses, including both simple main effects and simple interaction effects,
#' and 2) post-hoc multiple comparisons (e.g., pairwise, sequential, polynomial), with \emph{p}-value adjustment for factors with >= 3 levels
#' (using methods such as Bonferroni, Tukey's HSD, and FDR).
#'
#' This function is based on and extends the 1) \code{\link[emmeans]{joint_tests}}, 2) \code{\link[emmeans]{emmeans}}, and 3) \code{\link[emmeans]{contrast}} functions in the R package \code{emmeans}.
#' You only need to specify the model object, to-be-tested effect(s), and moderator(s).
#' Then, almost all the outputs you need will be displayed in an elegant manner, including effect sizes (partial \eqn{\eta^2} and Cohen's \emph{d}) and their confidence intervals (CIs).
#' 90\% CIs for partial \eqn{\eta^2} and 95\% CIs for Cohen's \emph{d} are reported.
#'
#' \strong{Statistical Details:}
#'
#' Some may confuse the statistical terms "simple effects", "post-hoc tests", and "multiple comparisons". Unfortunately, such a confusion is not uncommon.
#' Here, I explain what these terms actually refer to.
#' \describe{
#'   \item{\strong{1. Simple Effects}}{
#'     When we speak of "simple effects", we are referring to ...
#'     \itemize{
#'       \item simple main effects
#'       \item simple interaction effects (only for designs with 3 or more factors)
#'       \item simple simple effects (only for designs with 3 or more factors)
#'     }
#'     When the interaction effects in ANOVA are significant, we should then perform a "simple-effect analysis".
#'     In ANOVA, we call it "simple-effect analysis"; in regression, we also call it "simple-slope analysis".
#'     They are identical in statistical principles. Nonetheless, the situations in ANOVA can be a bit more complex because we sometimes have a three-factors design.
#'
#'     In a regular two-factors design, we only test \strong{"simple main effects"}.
#'     That is, on the different levels of a factor "B", the main effects of "A" would be different.
#'     However, in a three-factors (or more) design, we may also test \strong{"simple interaction effects"} and \strong{"simple simple effects"}.
#'     That is, on the different combinations of levels of factors "B" and "C", the main effects of "A" would be different.
#'
#'     In SPSS, we usually use the \code{MANOVA} and/or the \code{GLM + /EMMEANS} syntax to perform such analyses.
#'     Tutorials of the SPSS syntax (in Chinese) can be found in my personal profile on Zhihu.com:
#'     \href{https://zhuanlan.zhihu.com/p/30037168}{Tutorial #1},
#'     \href{https://zhuanlan.zhihu.com/p/31863288}{Tutorial #2}, and
#'     \href{https://zhuanlan.zhihu.com/p/35011046}{Tutorial #3}.
#'
#'     Here, the R function \code{EMMEANS} can do the same thing as in SPSS and can do much better and easier (just see the section "Examples").
#'     The outputs include tidy tables as well as the estimates for effect sizes (partial \eqn{\eta^2}) and their 90\% CIs.
#'
#'     To note, simple effects \emph{per se} do NOT need any form of \emph{p}-value adjustment, because what we test in simple-effect analyses are still "omnibus \emph{F}-tests".
#'   }
#'   \item{\strong{2. Post-Hoc Tests}}{
#'     The term "post-hoc" means that the tests are performed after ANOVA. Given this, some may (wrongly) regard simple-effect analyses also as a kind of post-hoc tests.
#'     However, these two terms should be distinguished. In many situations and softwares, "post-hoc tests" only refer to \strong{"post-hoc comparisons"} using \emph{t}-tests and some \emph{p}-value adjustment techniques.
#'     We need post-hoc comparisons \strong{only when there are factors with 3 or more levels}.
#'     For example, we can perform the post-hoc comparisons of mean values 1) across multiple levels of one factor in a pairwise way or 2) particularly between the two conditions "A1B1" and "A2B2".
#'
#'     Post-hoc tests are totally \strong{independent of} whether there is a significant interaction effect. \strong{It only deals with factors with multiple levels.}
#'     In most cases, we use pairwise comparisons to do post-hoc tests. See the next part for details.
#'   }
#'   \item{\strong{3. Multiple Comparisons}}{
#'     As mentioned above, multiple comparisons are post-hoc tests by its nature but do NOT have any relationship with simple-effect analyses.
#'     In other words, "(post-hoc) multiple comparisons" are \strong{independent of} "interaction effects" and "simple effects".
#'     What's more, when the simple main effect is of a factor with 3 or more levels, we also need to do multiple comparisons (e.g., pairwise comparisons) \emph{within} the simple-effect analysis.
#'     In this situation (i.e., >= 3 levels), we need \emph{p}-value adjustment methods such as Bonferroni, Tukey's HSD (honest significant difference), FDR (false discovery rate), and so forth.
#'
#'     There are many ways to do multiple comparisons. All these methods are included in the current \code{EMMEANS} function.
#'     If you are familiar with SPSS syntax, you may feel that the current R functions \code{MANOVA} and \code{EMMEANS} are a nice combination of the SPSS syntax \code{MANOVA} and \code{GLM + /EMMEANS}.
#'     Yes, they are. More importantly, they outperform the SPSS syntax, either for its higher convenience or for its more fruitful outputs.
#'
#'     Now, you can forget the overcomplicated SPSS syntax and join in the warm family of R. Welcome!
#'     \itemize{
#'       \item \code{"pairwise"} - Pairwise comparisons (default is "higher level - lower level")
#'       \item \code{"seq"} or \code{"consec"} - Consecutive (sequential) comparisons
#'       \item \code{"poly"} - Polynomial contrasts (linear, quadratic, cubic, quartic, ...)
#'       \item \code{"eff"} - Effect contrasts (vs. the grand mean)
#'     }
#'   }
#' }
#' @import emmeans
#' @param model A model fitted by \code{\link{MANOVA}} or \code{\link[afex]{aov_ez}}.
#' @param effect The effect(s) you want to test. If set to a character string (e.g., \code{"A"}), it will output the results of omnibus tests or simple main effects.
#' If set to a character vector (e.g., \code{c("A", "B")}), it will also output the results of simple interaction effects.
#' @param by Moderator variable(s). Default is \code{NULL}.
#' @param contrast Contrast method for multiple comparisons. Default is \code{"pairwise"}.
#' Alternatives can be \code{"pairwise" ("revpairwise"), "seq" ("consec"), "poly", "eff"}.
#' For details, see \code{emmeans::\link[emmeans]{contrast-methods}}.
#' @param p.adjust Adjustment method (of \emph{p} values) for multiple comparisons. Default is \code{"bonferroni"}.
#' Alternatives can be \code{"none", "fdr", "hochberg", "hommel", "holm", "tukey", "mvt", "bonferroni"}.
#' For polynomial contrasts, the default is \code{"none"}.
#' For details, see \code{stats::\link[stats]{p.adjust}}.
#' @param cohen.d Method to compute Cohen's \emph{d} in multiple comparisons.
#' Default is \code{"eff_size"}, which will use the function \code{\link[emmeans]{eff_size}} in the latest \code{emmeans} package (version 1.4.2 released on 2019-10-24).
#' For details, see \href{https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html}{Comparisons and contrasts in emmeans}.
#' An alternative can be \code{"t2d"}, which will estimate Cohen's \emph{d} by the \emph{t}-to-\emph{r} (\code{\link[psych]{t2r}}) and \emph{r}-to-\emph{d} (\code{\link[psych]{r2d}}) transformations.
#' In general, \code{"eff_size"} gives more reasonable estimates and so is highly suggested.
#' @param reverse The order of levels to be contrasted. Default is \code{TRUE} ("higher level vs. lower level").
#' @param repair In a few cases, some problems in your data may generate some errors in output (see \code{within.2} in Examples).
#' Then, you may set \code{repair="TRUE"} to have the adjusted results.
#' @examples
#' ## Between-Subjects Design
#'
#' View(between.1)
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A")
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A", contrast="seq")
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A", contrast="seq", p.adjust="tukey")
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A", contrast="poly")
#'
#' View(between.2)
#' MANOVA(data=between.2, dv="SCORE", between=c("A", "B")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B", by="A")
#'
#' View(between.3)
#' MANOVA(data=between.3, dv="SCORE", between=c("A", "B", "C")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#'
#'
#' ## Within-Subjects Design
#'
#' View(within.1)
#' MANOVA(data=within.1, dvs="A1:A4", dvs.pattern="A(.)",
#'        within="A") %>%
#'   EMMEANS("A")
#'
#' View(within.2)
#' MANOVA(data=within.2, dvs="A1B1:A2B3", dvs.pattern="A(.)B(.)",
#'        within=c("A", "B")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B", by="A") %>%  # with some errors
#'   EMMEANS("B", by="A", repair=TRUE)
#'
#' View(within.3)
#' MANOVA(data=within.3, dvs="A1B1C1:A2B2C2", dvs.pattern="A(.)B(.)C(.)",
#'        within=c("A", "B", "C")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#'
#'
#' ## Mixed Design
#'
#' View(mixed.2_1b1w)
#' MANOVA(data=mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
#'        between="A", within="B", sphericity.correction="GG") %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B", by="A")
#'
#' View(mixed.3_1b2w)
#' MANOVA(data=mixed.3_1b2w, dvs="B1C1:B2C2", dvs.pattern="B(.)C(.)",
#'        between="A", within=c("B", "C")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#'
#' View(mixed.3_2b1w)
#' MANOVA(data=mixed.3_2b1w, dvs="B1:B2", dvs.pattern="B(.)",
#'        between=c("A", "C"), within="B") %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("A", by="C") %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("B", by=c("A", "C"))
#'
#'
#' ## Other Examples
#' air=airquality
#' air$Day.1or2=ifelse(air$Day %% 2 == 1, 1, 2) %>%
#'   factor(levels=1:2, labels=c("odd", "even"))
#' MANOVA(data=air, dv="Temp", between=c("Month", "Day.1or2"),
#'        covariate=c("Solar.R", "Wind")) %>%
#'   EMMEANS("Month", contrast="seq") %>%
#'   EMMEANS("Month", by="Day.1or2", contrast="poly")
#' @seealso \code{\link{MANOVA}}
#' @export
EMMEANS=function(model, effect=NULL, by=NULL,
                 contrast="pairwise",
                 p.adjust="bonferroni",
                 cohen.d="eff_size",
                 reverse=TRUE,
                 repair=FALSE) {
  model.raw=model

  # IMPORTANT: If include 'aov', the 'emmeans' results of
  # within-subjects design will not be equal to those of SPSS!
  # So we do not include 'aov' object but instead use 'lm' and 'mlm'
  # objects to do the follow-up 'emmeans' analyses!
  if(!repair) model$aov=NULL
  repair.msg="NOTE: Repaired results are shown below.\nThe function works with the 'aov' object."

  effect.text=paste(effect, collapse='\" & \"')
  Print("<<yellow ------ EMMEANS Output (effect = \"{effect.text}\") ------>>")
  if(repair) message(repair.msg)
  cat("\n")

  ## Simple Effect (omnibus)
  # see 'weights' in ?emmeans
  Print("<<underline {ifelse(is.null(by), 'Omnibus Test', 'Simple Effects')} of \"{effect.text}\":>>")
  tryCatch({
    err=TRUE
    suppressMessages({
      sim=joint_tests(model, by=by, weights="equal")
    })
    err=FALSE
  }, error=function(e) {
    message(repair.msg)
  }, silent=TRUE)
  if(err) sim=joint_tests(model.raw, by=by, weights="equal")
  if("note" %in% names(sim)) {
    error=TRUE
    message("\nWarning message:
    Please check your data!
    The WITHIN CELLS error matrix is SINGULAR.
    Some variables are LINEARLY DEPENDENT.
    So the tests below are misleading.
    You may set 'repair = TRUE'.
    ")
  } else {
    error=FALSE
    sim$sig=sig.trans(sim$p.value)
    sim$p.eta2=mapply(eta_sq_ci, sim$F.ratio, sim$df1, sim$df2, return="eta2") %>% formatF(3)
    sim$LLCI=mapply(eta_sq_ci, sim$F.ratio, sim$df1, sim$df2, return="LLCI") %>% formatF(3) %>% paste0("[", ., ",")
    sim$ULCI=mapply(eta_sq_ci, sim$F.ratio, sim$df1, sim$df2, return="ULCI") %>% formatF(3) %>% paste0(., "]")
    sim$F.ratio=round(sim$F.ratio, 2)
    names(sim)[c(1,(length(by)+2):(length(by)+9))]=
      c("----", "df1", "df2", "F", "p", "sig",
        "  \u03b7\u00b2p", " [90%", "  CI]")
  }
  print(sim)
  if(error) cat("\n")

  ## Estimated Marginal Means (emmeans)
  Print("<<underline Estimated Marginal Means of \"{effect.text}\":>>")
  suppressMessages({
    emm0=emm=emmeans(model, specs=effect, by=by, weights="equal")
  })
  emm=summary(emm)  # to a data.frame (class 'summary_emm')
  emm$emmean=round(emm$emmean, 2)
  emm$SE=formatF(emm$SE, 3) %>% paste0("(", ., ")")
  emm$lower.CL=formatF(emm$lower.CL, 2) %>% paste0("[", ., ",")
  emm$upper.CL=formatF(emm$upper.CL, 2) %>% paste0(., "]")
  names(emm)[(length(emm)-4):length(emm)]=
    c("EM.Mean", "   S.E.", "df", " [95%", "  CI]")
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
  Print("<<underline {contr.method} of \"{effect.text}\":>>")
  if(contrast=="pairwise" & reverse==TRUE) contrast="revpairwise"
  if(contrast=="seq") contrast="consec"
  if(contrast=="consec") reverse=FALSE
  if(contrast=="poly") p.adjust="none"
  con0=con=contrast(emm0, method=contrast, adjust=p.adjust, reverse=reverse)
  conCI=confint(con)
  con=summary(con)  # to a data.frame (class 'summary_emm')
  # com=pairs(emm, simple="each", adjust=p.adjust, reverse=TRUE, combine=TRUE)
  con$sig=sig.trans(con$p.value)
  if(cohen.d=="t2d") {
    # WARNING: NOT EXACTLY!
    if(contrast!="poly")
      message("NOTE: Cohen's d was estimated by 't-to-r' and 'r-to-d' transformations.")
    con$d=r2d(t2r(con$t.ratio, con$df))
    con$d.LLCI=(conCI$lower.CL*con$d/con$estimate) %>% formatF(2) %>% paste0("[", ., ",")
    con$d.ULCI=(conCI$upper.CL*con$d/con$estimate) %>% formatF(2) %>% paste0(., "]")
  } else if(cohen.d=="eff_size") {
    # if(contrast!="poly")
    #   message("NOTE: Cohen's d was estimated by 'eff_size()' in the 'emmeans' package.")
    rn=row.names(model$anova_table)
    term=c()
    for(i in rn) if(i %in% effect) term=c(term, i)
    term=paste(term, collapse=":")
    es=eff_size(emm0, method=contrast,
                sigma=sqrt(model$anova_table[term, "MSE"]),
                edf=df.residual(model$lm)) %>% summary()
    con$d=es$effect.size
    con$d.LLCI=es$lower.CL %>% formatF(2) %>% paste0("[", ., ",")
    con$d.ULCI=es$upper.CL %>% formatF(2) %>% paste0(., "]")
  } else {
    stop("Please set cohen.d = 'eff_size' or 't2d', see ?EMMEANS")
  }
  con$estimate=round(con$estimate, 2)
  con$SE=formatF(con$SE, 3) %>% paste0("(", ., ")")
  con$t.ratio=formatF(con$t.ratio, 2)
  con$p.value=p.trans(con$p.value)
  names(con)[c(1, (length(con)-8):length(con))]=
    c("Contrast", "b", "   S.E.", "df", "    t", "    p", "sig",
      "Cohen's d", " [95%", "  CI]")
  if(contrast=="poly")
    con[c("Cohen's d", " [95%", "  CI]")]=NULL
  print(con)
  if(con0@misc[["famSize"]] > 2 & p.adjust != "none")
    cat("\n")
  if(length(attr(con, "mesg"))==1)
    if(grepl("averaged", attr(con, "mesg")))
      cat("\n")

  ## Return (return the raw model for recycling across '%>%' pipelines)
  invisible(model.raw)
}


#' Compute the confidence interval (CI) of partial eta^2 in ANOVA
#' @importFrom MBESS F2Rsquare ci.R2
#' @export
eta_sq_ci=function(F.value, df.1, df.2, conf.level=0.90, return="all") {
  eta2=F2Rsquare(F.value=F.value, df.1=df.1, df.2=df.2)
  ci=ci.R2(F.value=F.value, df.1=df.1, df.2=df.2,
           Random.Predictors=FALSE, conf.level=conf.level)
  if(return=="all")
    return(c(eta2, ci$Lower.Conf.Limit.R2, ci$Upper.Conf.Limit.R2))
  if(return=="eta2")
    return(eta2)
  if(return=="LLCI")
    return(ci$Lower.Conf.Limit.R2)
  if(return=="ULCI")
    return(ci$Upper.Conf.Limit.R2)
}
