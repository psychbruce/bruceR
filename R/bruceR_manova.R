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
#' @param sphericity.correction Sphericity correction method to adjust the degrees of freedom (\emph{df}) when the sphericity assumption is violated. Default is \code{"none"}.
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
#'        between="A", within="B", sphericity.correction="GG")
#'
#' View(mixed.3_1b2w)
#' MANOVA(data=mixed.3_1b2w, dvs="B1C1:B2C2", dvs.pattern="B(.)C(.)",
#'        between="A", within=c("B", "C"))
#'
#' View(mixed.3_2b1w)
#' MANOVA(data=mixed.3_2b1w, dvs="B1:B2", dvs.pattern="B(.)",
#'        between=c("A", "C"), within="B")
#' @seealso \code{\link{EMMEANS}}
#' @export
MANOVA=function(data, id=NULL, dv=NULL, dvs=NULL, dvs.pattern="",
                between=NULL, within=NULL, covariate=NULL,
                factorize=ifelse(is.null(covariate), TRUE, FALSE),
                sphericity.correction="none") {
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
      vars=convert2vars(data, varrange=dvs)$vars.raw
      data=pivot_longer(data, cols=vars,
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
                anova_table=list(correction=sphericity.correction,
                                 es="pes"),
                include_aov=TRUE,  # see EMMEANS, default will be FALSE
                print.formula=FALSE)
  at=aov.ez$anova_table
  names(at)[1:2]=c("df1", "df2")
  at=mutate(at, MS=`F`*MSE,
            p.eta2=mapply(eta_sq_ci, `F`, df1, df2, return="eta2"),
            LLCI=mapply(eta_sq_ci, `F`, df1, df2, return="LLCI"),
            ULCI=mapply(eta_sq_ci, `F`, df1, df2, return="ULCI"))
            # cohen.f=sqrt(p.eta2/(1-p.eta2))
  at=at[c("MS", "MSE", "df1", "df2", "F", "Pr(>F)",
          "p.eta2", "LLCI", "ULCI")]
  names(at)[7:9]=c("  \u03b7\u00b2p", "[90% ", "  CI]")
  row.names(at)=row.names(aov.ez$anova_table)
  df.nsmall=ifelse(sphericity.correction=="none", 0, 2)
  Print("
  <<underline ANOVA Table:>>
  Dependent variable(s):     {ifelse(is.null(within), dv, paste0(vars, collapse=', '))}
  Between-subject factor(s): {ifelse(is.null(between), '-', paste0(between, collapse=', '))}
  Within-subject factor(s):  {ifelse(is.null(within), '-', paste0(within, collapse=', '))}
  Covariate(s):              {ifelse(is.null(covariate), '-', paste0(covariate, collapse=', '))}
  ")
  print_table(at, nsmalls=c(3, 3, df.nsmall, df.nsmall,
                            2, 0, 3, 3, 3))
  if(sphericity.correction=="GG")
    Print("<<green Sphericity correction method: GG (Greenhouse-Geisser)>>")
  if(sphericity.correction=="HF")
    Print("<<green Sphericity correction method: HF (Huynh-Feldt)>>")
  Print("<<blue
  Tips: \u03b7\u00b2p (partial eta-squared) = F * df1 / (F * df1 + df2)
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
      if(min(sph[,2])<.05 & sphericity.correction=="none")
        Print("<<red The sphericity assumption is violated. You may set 'sphericity.correction' to 'GG' or 'HF'.>>")
    }
  }
  cat("\n")

  ## Return
  invisible(aov.ez)
}


#' Simple-effect analysis (for interactions) and post-hoc multiple comparison (for factors with 3 or more levels)
#' @import emmeans
#' @param contrast Contrast method for multiple comparisons. Default is \code{"pairwise"}.
#' Alternatives can be \code{"pairwise" ("revpairwise"), "seq" ("consec"), "poly", "eff"}.
#' For details, see \code{emmeans::\link[emmeans]{contrast-methods}}.
#' @param p.adjust Adjustment method (of \emph{p} values) for multiple comparisons. Default is \code{"bonferroni"}.
#' Alternatives can be \code{"none", "fdr", "hochberg", "hommel", "holm", "tukey", "mvt", "bonferroni"}.
#' For details, see \code{stats::\link[stats]{p.adjust}}.
#' @examples
#' ## Between-Subject Design
#'
#' View(between.1)
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A")
#' MANOVA(data=between.1, dv="SCORE", between="A") %>%
#'   EMMEANS("A", contrast="seq")
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
#' @seealso \code{\link{MANOVA}}
#' @export
EMMEANS=function(model, effect=NULL, by=NULL,
                 contrast="pairwise",
                 p.adjust="bonferroni",
                 reverse=TRUE,
                 repair=FALSE) {
  model.raw=model

  # IMPORTANT: If include 'aov', the 'emmeans' results of
  # within-subject design will not be equal to those of SPSS!
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
  con0=con=contrast(emm0, method=contrast, adjust=p.adjust, reverse=reverse)
  conCI=confint(con)
  con=summary(con)  # to a data.frame (class 'summary_emm')
  # com=pairs(emm, simple="each", adjust=p.adjust, reverse=TRUE, combine=TRUE)
  con$sig=sig.trans(con$p.value)
  # con$lower.CI=conCI$lower.CL %>% formatF(2) %>% paste0("[", ., ",")
  # con$upper.CI=conCI$upper.CL %>% formatF(2) %>% paste0(., "]")
  con$d=r2d(t2r(con$t.ratio, con$df))  # WARNING: NOT EXACTLY!
  con$d.LLCI=(conCI$lower.CL*con$d/con$estimate) %>% formatF(2) %>% paste0("[", ., ",")
  con$d.ULCI=(conCI$upper.CL*con$d/con$estimate) %>% formatF(2) %>% paste0(., "]")
  con$SE=formatF(con$SE, 3) %>% paste0("(", ., ")")
  con$t.ratio=formatF(con$t.ratio, 2)
  con$p.value=p.trans(con$p.value)
  names(con)[c(1, (length(con)-8):length(con))]=
    c("Contrast", "b", "   S.E.", "df", "    t", "    p", "sig",
      "Cohen's d", " [95%", "  CI]")
  if(contrast=="poly")
    con[c("Cohen's d", " [95%", "  CI]")]=NULL
  else
    message("NOTE: Cohen's d was estimated by 't-to-r' and 'r-to-d' transformations.")
  print(con)
  if(con0@misc[["famSize"]] > 2 & p.adjust != "none")
    cat("\n")
  if(!is.null(attr(con, "mesg")))
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
