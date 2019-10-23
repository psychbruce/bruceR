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
#' Easily perform MANOVA (between-subject, within-subject, and mixed design).
#'
#' \strong{Demo datasets:}
#' \describe{
#'   \item{1. Between-subject design}{
#'     \itemize{
#'       \item \code{between.1} - 1
#'       \item \code{between.2} - 2
#'       \item \code{between.3} - 3
#'     }
#'   }
#'   \item{2. Within-subject design}{
#'     \itemize{
#'       \item \code{within.1} - 1
#'       \item \code{within.2} - 2
#'       \item \code{within.3} - 3
#'     }
#'   }
#'   \item{3. Mixed design}{
#'     \itemize{
#'       \item \code{mixed.2_1b1w} - 1
#'       \item \code{mixed.3_1b2w} - 2
#'       \item \code{mixed.3_2b1w} - 3
#'     }
#'   }
#' }
#' @import afex
#' @importFrom tidyr pivot_longer
#' @param correction Sphericity correction method to adjust the degrees of freedom (\emph{df}) when the sphericity assumption is violated. Default is \code{"none"}.
#' If Mauchly's test of sphericity is significant, you may set it to \code{"GG"} (Greenhouse-Geisser) or \code{"HF"} (Huynh-Feldt).
#' @examples
#' ## Between-Subject Design
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
#' ## Within-Subject Design
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
#'        between="A", within="B", correction="GG")
#' @seealso \code{\link{EMMEANS}}
#' @export
MANOVA=function(data, id=NULL, dv=NULL, dvs=NULL, dvs.pattern="",
                between=NULL, within=NULL, covariate=NULL,
                factorize=ifelse(is.null(covariate), TRUE, FALSE),
                correction="none") {
  data=as.data.frame(data)
  design=ifelse(is.null(within), "Between-Subject Design",
                ifelse(is.null(between), "Within-Subject Design",
                       "Mixed Design"))
  Print("<<yellow ------ MANOVA Output ({design}) ------>>")
  cat("\n")

  ## Add Participant ID (if necessary)
  if(is.null(id)) {
    data$ID=1:nrow(data)
    id="ID"
  }

  ## Wide to Long (if necessary)
  if(is.null(dv)) {
    if(is.null(within)) {
      stop("Please specify the dependent variable.")
    } else {
      data=pivot_longer(data, cols=convert2vars(data, varrange=dvs)$vars.raw,
                        names_to=within,
                        names_pattern=dvs.pattern,
                        values_to="Y") %>% as.data.frame()
      dv="Y"
    }
  }

  ## Ensure Factorized Variables
  for(iv in c(between, within))
    data[[iv]]=as.factor(data[[iv]])

  ## Main MANOVA Functions
  aov.ez=aov_ez(data=data, id=id, dv=dv,
                between=between, within=within, covariate=covariate,
                factorize=factorize,
                anova_table=list(correction=correction, es="pes"),
                print.formula=FALSE)
  at=aov.ez$anova_table
  names(at)[1:2]=c("df1", "df2")
  at=mutate(at, MS=`F`*MSE,
            p.eta2=mapply(eta_sq_ci, `F`, df1, df2, return="eta2"),
            LLCI=mapply(eta_sq_ci, `F`, df1, df2, return="LLCI"),
            ULCI=mapply(eta_sq_ci, `F`, df1, df2, return="ULCI"),
            cohen.f=sqrt(p.eta2/(1-p.eta2)))
  at=at[c("MS", "MSE", "df1", "df2", "F", "Pr(>F)",
          "p.eta2", "LLCI", "ULCI", "cohen.f")]
  names(at)[7:10]=c("  \u03b7\u00b2p", "[90% ", "  CI]", "Cohen's f")
  row.names(at)=row.names(aov.ez$anova_table)
  df.nsmall=ifelse(correction=="none", 0, 2)
  Print("<<underline ANOVA Table:>>
         Outcome variable: {dv}")
  print_table(at, nsmalls=c(3, 3, df.nsmall, df.nsmall,
                            2, 0, 3, 3, 3, 3))
  if(correction=="GG")
    Print("<<green Sphericity correction method: GG (Greenhouse-Geisser)>>")
  if(correction=="HF")
    Print("<<green Sphericity correction method: HF (Huynh-Feldt)>>")
  Print("<<blue
  Tips: \u03b7\u00b2p (partial eta-squared) = F * df1 / (F * df1 + df2)
  >>")

  ## Mauchly's Test of Sphericity
  if(!is.null(within)) {
    Print("\n\n\n<<underline Mauchly Test of Sphericity:>>")
    sph=summary(aov.ez$Anova)$sphericity.tests
    colnames(sph)=c("Mauchly's W", "p")
    print(sph)
    if(min(sph[,2])<.05 & correction=="none")
      Print("<<red The sphericity assumption is violated. You may set 'correction' to 'GG' or 'HF'.>>")
  }
  cat("\n")

  ## Return
  invisible(aov.ez)
}


#' Simple-effect analysis (for interaction) and post-hoc pairwise comparison (for factors with 3 or more levels)
#' @import emmeans
#' @param p.adjust Adjustment of \emph{p} values for multiple comparisons. Default is \code{"bonferroni"}.
#' You may set it to \code{"none", "fdr", "hochberg", "hommel", "holm", "tukey", "bonferroni"}.
#' For details, see \code{\link[stats]{p.adjust}}.
#' @examples
#' ## Between-Subject Design
#'
#' View(between.1)
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A")
#'
#' View(between.2)
#' MANOVA(data=between.2, dv="SCORE", between=c("A", "B")) %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B", by="A")
#'
#' View(between.3)
#' MANOVA(data=between.3, dv="SCORE", between=c("A", "B", "C")) %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#'
#'
#' ## Within-Subject Design
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
#'   EMMEANS("B", by="A")
#'
#' View(within.3)
#' MANOVA(data=within.3, dvs="A1B1C1:A2B2C2", dvs.pattern="A(.)B(.)C(.)",
#'        within=c("A", "B", "C")) %>%
#'   EMMEANS(c("A", "B"), by="C") %>%
#'   EMMEANS("A", by=c("B", "C"))
#'
#'
#' ## Mixed Design
#'
#' View(mixed.2_1b1w)
#' MANOVA(data=mixed.2_1b1w, dvs="B1:B3", dvs.pattern="B(.)",
#'        between="A", within="B", correction="GG") %>%
#'   EMMEANS("A", by="B") %>%
#'   EMMEANS("B", by="A")
#' @seealso \code{\link{MANOVA}}
#' @export
EMMEANS=function(model, effect=NULL, by=NULL,
                 p.adjust="bonferroni") {
  Print("<<yellow ------ EMMEANS Output (effect = {paste(effect, collapse=', ')}) ------>>")
  cat("\n")

  ## Simple Effect (omnibus)
  sim=joint_tests(model, by=by, weights="equal")
  sim$sig=sig.trans(sim$p.value)
  sim$p.eta2=mapply(eta_sq_ci, sim$F.ratio, sim$df1, sim$df2, return="eta2") %>% formatF(3)
  sim$LLCI=mapply(eta_sq_ci, sim$F.ratio, sim$df1, sim$df2, return="LLCI") %>% formatF(3)
  sim$ULCI=mapply(eta_sq_ci, sim$F.ratio, sim$df1, sim$df2, return="ULCI") %>% formatF(3)
  names(sim)[c(1,(length(by)+2):(length(by)+9))]=
    c("----", "df1", "df2", "F", "p", "sig",
      "  \u03b7\u00b2p", "[90% ", "  CI]")
  Print("<<underline {ifelse(is.null(by), 'Omnibus Test', 'Simple Effects')} of {paste(effect, collapse=', ')}:>>")
  print(sim)

  ## Multiple Comparison (emmeans)
  Print("<<underline Estimated Marginal Means of {paste(effect, collapse=', ')}:>>")
  emm=emmeans(model, specs=effect, by=by, weights="equal")
  print(emm)
  cat("\n")

  ## Multiple Comparison (pairwise)
  Print("<<underline Pairwise Comparisons of {paste(effect, collapse=', ')}:>>")
  com=pairs(emm, adjust=p.adjust)
  print(com)
  if(com@misc[["famSize"]]>2) cat("\n")

  ## Return (return the raw model for recycling across '%>%' pipelines)
  invisible(model)
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
