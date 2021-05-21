#### PROCESS Macro (GLM and HLM) ####


interaction_F_test=function(model, data=NULL, data.name="data") {
  Run("{data.name}=data")
  df2=stats::df.residual(model)
  interms=attr(stats::terms(model), "term.labels")
  interms=interms[grepl(":", interms)]
  interms.form=as.formula(paste("~", paste(interms, collapse=" + ")))
  interms.drop=as.formula(paste(". ~ . -", paste(interms, collapse=" - ")))
  dp1=stats::drop1(model, scope=interms.form, test="F")
  dp1=dp1[!is.na(dp1$Df), c("Df", "F value", "Pr(>F)")]
  aov=stats::anova(stats::update(model, interms.drop), model)
  if(df2!=aov[2, "Res.Df"]) warning("Error!")
  aov.table=data.frame(
    `F`=c(dp1[[2]], aov[2, "F"]),
    df1=c(dp1[[1]], aov[2, "Df"]),
    df2=df2,
    pval=c(dp1[[3]], aov[2, "Pr(>F)"]))
  row.names(aov.table)=c(gsub(":", " x ", row.names(dp1)),
                         "(All Interactions)")
  return(aov.table)
}


interaction_Chi2_test=function(model, data=NULL, data.name="data") {
  Run("{data.name}=data")
  interms=attr(stats::terms(model), "term.labels")
  interms=interms[grepl(":", interms)]
  interms.form=as.formula(paste("~", paste(interms, collapse=" + ")))
  interms.drop=as.formula(paste(". ~ . -", paste(interms, collapse=" - ")))
  dp1=stats::drop1(model, scope=interms.form, test="Chisq")
  dp1=dp1[!is.na(dp1$Df), c("Df", "LRT", "Pr(>Chi)")]
  chi=stats::anova(stats::update(model, interms.drop), model, test="Chisq")
  chi.table=data.frame(
    `Chisq`=c(dp1[[2]], chi[2, "Deviance"]),
    df=c(dp1[[1]], chi[2, "Df"]),
    pval=c(dp1[[3]], chi[2, "Pr(>Chi)"]))
  row.names(chi.table)=c(gsub(":", " x ", row.names(dp1)),
                         "(All Interactions)")
  return(chi.table)
}


interaction_test=function(model, data=NULL, data.name="data") {
  if(inherits(model, "glm")) {
    table=interaction_Chi2_test(model, data=data, data.name=data.name)
  } else {
    table=interaction_F_test(model, data=data, data.name=data.name)
  }
  return(table)
}


lav_med_modeler=function(y, x,
                         meds=c(),
                         covs=c(),
                         med.type=c("parallel", "serial"),
                         cov.path=c("y", "m", "both")) {
  ids=1:length(meds)
  if(length(med.type)>1) med.type="parallel"
  if(length(meds)==1) {
    fm=meds %^% " ~ a*" %^% x
    fy=y %^% " ~ c.*"%^% x %^% " + " %^% "b*" %^% meds
    pars=paste(
      "Indirect := a*b",
      "Direct := c.",
      "Total := c. + a*b",
      sep="\n"
    )
  } else {
    if(grepl("p", med.type)) {
      x.all="a" %^% ids %^% "*" %^% x
      meds.all=paste("b" %^% ids %^% "*" %^% meds, collapse=" + ")
      ind="Indirect_X_M" %^% ids %^% "_Y := " %^% "a" %^% ids %^% "*" %^% "b" %^% ids
      fm=meds %^% " ~ " %^% x.all
      fy=y %^% " ~ c.*"%^% x %^% " + " %^% meds.all
      ind.all=paste("a" %^% ids %^% "*" %^% "b" %^% ids, collapse=" + ")
      pars=paste(
        "Indirect_All := " %^% ind.all,
        paste(ind, collapse="\n"),
        "Direct := c.",
        "Total := c. + " %^% ind.all,
        sep="\n"
      )
    }
    if(grepl("s", med.type)) {
      x.all="a" %^% 1:length(meds) %^% "*" %^% x
      meds.all=paste("b" %^% ids %^% "*" %^% meds, collapse=" + ")
      fm=meds %^% " ~ " %^% x.all
      for(mi in 2:length(meds)) {
        fm[mi]=fm[mi] %^% " + " %^%
          paste("d" %^% ids[1:(mi-1)] %^% ids[mi] %^% "*" %^% meds[1:(mi-1)], collapse=" + ")
      }
      fy=y %^% " ~ c.*"%^% x %^% " + " %^% meds.all
      if(length(meds)==2) {
        ind.all="a1*b1 + a2*b2 + a1*d12*b2"
        pars=Glue("
        Indirect_All := {ind.all}
        Ind_X_M1_Y := a1*b1
        Ind_X_M2_Y := a2*b2
        Ind_X_M1_M2_Y := a1*d12*b2
        Direct := c.
        Total := c. + {ind.all}
        ")
      }
      if(length(meds)==3) {
        ind.all=paste(
          "a1*b1",
          "a2*b2",
          "a3*b3",
          "a1*d12*b2",
          "a1*d13*b3",
          "a2*d23*b3",
          "a1*d12*d23*b3",
          sep=" + "
        )
        pars=Glue("
        Indirect_All := {ind.all}
        Ind_X_M1_Y := a1*b1
        Ind_X_M2_Y := a2*b2
        Ind_X_M3_Y := a3*b3
        Ind_X_M1_M2_Y := a1*d12*b2
        Ind_X_M1_M3_Y := a1*d13*b3
        Ind_X_M2_M3_Y := a2*d23*b3
        Ind_X_M1_M2_M3_Y := a1*d12*d23*b3
        Direct := c.
        Total := c. + {ind.all}
        ")
      }
      if(length(meds)==4) {
        ind.all=paste(
          "a1*b1",
          "a2*b2",
          "a3*b3",
          "a4*b4",
          "a1*d12*b2",
          "a1*d13*b3",
          "a1*d14*b4",
          "a2*d23*b3",
          "a2*d24*b4",
          "a3*d34*b4",
          "a1*d12*d23*b3",
          "a1*d12*d24*b4",
          "a1*d13*d34*b4",
          "a2*d23*d34*b4",
          "a1*d12*d23*d34*b4",
          sep=" + "
        )
        pars=Glue("
        Indirect_All := {ind.all}
        Ind_X_M1_Y := a1*b1
        Ind_X_M2_Y := a2*b2
        Ind_X_M3_Y := a3*b3
        Ind_X_M4_Y := a4*b4
        Ind_X_M1_M2_Y := a1*d12*b2
        Ind_X_M1_M3_Y := a1*d13*b3
        Ind_X_M1_M4_Y := a1*d14*b4
        Ind_X_M2_M3_Y := a2*d23*b3
        Ind_X_M2_M4_Y := a2*d24*b4
        Ind_X_M3_M4_Y := a3*d34*b4
        Ind_X_M1_M2_M3_Y := a1*d12*d23*b3
        Ind_X_M1_M2_M4_Y := a1*d12*d24*b4
        Ind_X_M1_M3_M4_Y := a1*d13*d34*b4
        Ind_X_M2_M3_M4_Y := a2*d23*d34*b4
        Ind_X_M1_M2_M3_M4_Y := a1*d12*d23*d34*b4
        Direct := c.
        Total := c. + {ind.all}
        ")
      }
    }
  }
  if(length(covs)>0)
    covs.all=" " %^% paste(covs, collapse=" + ") %^% " +"
  else
    covs.all=""
  if("m" %in% cov.path)
    fm=stringr::str_replace(fm, "~", "~" %^% covs.all)
  if("y" %in% cov.path)
    fy=stringr::str_replace(fy, "~", "~" %^% covs.all)
  model=paste(paste(fm, collapse="\n"),
              fy, pars,
              sep="\n")
  return(model)
}


# edit(mediation::mediate)
# edit(boot::boot.ci)
# edit(boot:::perc.ci)
# edit(boot:::bca.ci)
boot_ci=function(boot,
                 type=c("boot", "bc.boot", "bca.boot", "mcmc"),
                 true=NULL,
                 conf=0.95) {
  low=(1-conf)/2
  high=1-low
  if(length(type)>1) type="boot"
  if(type %in% c("boot", "mcmc")) {
    ci=quantile(boot, c(low, high), na.rm=TRUE)  # percentile
  } else {
    if(is.null(true)) boot0=mean(boot) else boot0=true
    p0=length(boot[boot<boot0])/length(boot)
    z0=qnorm(p0)
    z.l=qnorm(low)
    z.h=qnorm(high)
    if(type=="bca.boot") {
      U=(length(boot)-1)*(boot0-boot)
      a=sum(U^3)/(6*(sum(U^2))^1.5)  # accelerated bias-corrected
    } else {
      a=0  # bias-corrected
    }
    lower.bc=pnorm(z0+(z0+z.l)/(1-a*(z0+z.l)))
    upper.bc=pnorm(z0+(z0+z.h)/(1-a*(z0+z.h)))
    ci=quantile(boot, c(lower.bc, upper.bc), na.rm=TRUE)
  }
  return(ci)
}


#' PROCESS for mediation and/or moderation analyses.
#'
#' @param data Data frame.
#' @param y,x Variable name of outcome (Y) and predictor (X).
#'
#' It supports continuous (numeric) and dichotomous (factor) variables.
#' @param meds Variable name(s) of mediator(s) (M).
#' Use \code{c()} to combine multiple mediators.
#'
#' It supports continuous (numeric) and dichotomous (factor) variables.
#'
#' It allows an infinite number of mediators in parallel
#' or 2~4 mediators in serial.
#'
#' Order matters when \code{med.type="serial"}
#' (PROCESS Model 6: serial mediation).
#' @param mods Variable name(s) of 0~2 moderator(s) (W).
#' Use \code{c()} to combine multiple moderators.
#'
#' It supports all types of variables:
#' continuous (numeric), dichotomous (factor), and multicategorical (factor).
#'
#' Order matters when \code{mod.type="3-way"}
#' (PROCESS Models 3, 5.3, 11, 12, 18, 19, 72, and 73).
#'
#' * Do not set this parameter when \code{med.type="serial"}
#' (PROCESS Model 6).
#' @param covs Variable name(s) of covariate(s) (i.e., control variables).
#' Use \code{c()} to combine multiple covariates.
#' It supports all types of (and an infinite number of) variables.
#' @param clusters (coming soon...) HLM (multilevel) cluster(s).
#' @param hlm.rand (coming soon...) HLM (multilevel) random effect term.
#' @param hlm.type (coming soon...) HLM (multilevel) mediation type:
#' \code{"2-2-1"}, \code{"2-1-1"}, or \code{"1-1-1"}.
#' @param med.type Type of mediator:
#' \code{"parallel"} (default) or \code{"serial"}
#' (PROCESS Model 6).
#' Partial matches of \code{"p"} or \code{"s"} also work.
#' @param mod.type Type of moderator:
#' \code{"2-way"} (default) or \code{"3-way"}
#' (PROCESS Models 3, 5.3, 11, 12, 18, 19, 72, and 73).
#' Partial matches of \code{"2"} or \code{"3"} also work.
#' @param mod.path Which path(s) do the moderator(s) influence?
#' \code{"x-y"}, \code{"x-m"}, \code{"m-y"}, or any combination of them
#' (use \code{c()} to combine), or \code{"all"} (i.e., all of them).
#' @param cov.path Which path(s) do the control variable(s) influence?
#' \code{"y"}, \code{"m"}, or \code{"both"} (default).
#' @param mod1.val,mod2.val By default (\code{NULL}), it uses
#' \strong{Mean +/- SD} of a continuous moderator (numeric) or
#' \strong{all levels} of a dichotomous/multicategorical moderator (factor) to
#' perform simple slope analyses and/or conditional mediation analyses.
#' You can also manually specify the values: e.g.,
#' \code{mod1.val=c(1, 3, 5)} or \code{mod1.val=c("A", "B", "C")}.
#' @param ci Method for estimating the standard error (SE) and
#' 95\% confidence interval (CI) of indirect effect(s).
#' Default is \code{"boot"} for (generalized) linear models or
#' \code{"mcmc"} for (generalized) linear mixed models (multilevel models).
#' \describe{
#'   \item{\code{"boot"}}{Percentile Bootstrap}
#'   \item{\code{"bc.boot"}}{Bias-Corrected Percentile Bootstrap}
#'   \item{\code{"bca.boot"}}{Bias-Corrected and Accelerated (BCa) Percentile Bootstrap}
#'   \item{\code{"mcmc"}}{Markov Chain Monte Carlo (Quasi-Bayesian)}
#' }
#' @param nsim Number of simulation samples (bootstrap resampling or Monte Carlo simulation)
#' for estimating SE and 95\% CI. Default is \code{100} for running examples faster.
#' In formal analyses, however, \strong{\code{nsim=1000} (or larger)} is strongly suggested!
#' @param seed Random seed for obtaining reproducible results.
#' Default is \code{1} (just an uncountable number).
#' You may set to any number you prefer (e.g., \code{seed=5201314}).
#'
#' Note that all mediation models include random processes
#' (i.e., bootstrap resampling or Monte Carlo simulation).
#' To get exactly the same results, you have to set a random seed.
#' However, even you set the same seed number, it is unlikely to
#' get the same results across different R packages
#' (e.g., \code{\link[lavaan:lavaan-class]{lavaan}} vs. \code{\link[mediation:mediate]{mediation}})
#' or softwares (e.g., SPSS, Mplus, R, jamovi).
#' @param std Standardized coefficients? Default is \code{FALSE}.
#' If \code{TRUE}, then it will standardize all numeric (continuous) variables
#' before building regression models.
#' @param nsmall Number of decimal places of output. Default is \code{3}.
#' @param file File name of MS Word (\code{.doc}).
#' Only regression models will be saved (using \code{\link{model_summary}}).
#'
#' @seealso
#' \code{\link{model_summary}},
#' \code{\link{med_summary}},
#' \code{\link{lavaan_summary}}
#'
#' @references
#' Yzerbyt, V., Muller, D., Batailler, C., & Judd, C. M. (2018).
#' New recommendations for testing indirect effects in mediational models:
#' The need to report and test component paths.
#' \emph{Journal of Personality and Social Psychology, 115}(6), 929-943.
#' \doi{10.1037/pspa0000132}
#'
#' @examples
#' \donttest{#### NOTE ####
#' ## In the following examples, I set nsim=100 to save time.
#' ## In formal analyses, nsim=1000 (or larger) is suggested!
#'
#' #### Demo Data ####
#' # ?mediation::student
#' data=mediation::student %>%
#'   # dplyr::slice_sample(n=567) %>%
#'   dplyr::select(SCH_ID, free, smorale, pared, income,
#'                 gender, work, attachment, fight, late, score)
#' names(data)[2:3]=c("SCH_free", "SCH_morale")
#' names(data)[4:7]=c("parent_edu", "family_inc", "gender", "partjob")
#' data$gender01=1-data$gender  # 0 = female, 1 = male
#' # dichotomous X: as.factor()
#' data$gender=factor(data$gender01, levels=0:1, labels=c("Female", "Male"))
#' # dichotomous Y: as.factor()
#' data$pass=as.factor(ifelse(data$score>=50, 1, 0))
#'
#' #### Descriptive Statistics and Correlation Analyses ####
#' Freq(data$gender)
#' Freq(data$pass)
#' Describe(data)     # file="xxx.doc"
#' Corr(data[,4:11])  # file="xxx.doc"
#'
#' #### PROCESS Analyses ####
#'
#' ## Model 1 ##
#' PROCESS(data, y="score", x="late", mods="gender")  # continuous Y
#' PROCESS(data, y="pass", x="late", mods="gender")   # dichotomous Y
#'
#' # (Johnson-Neyman (J-N) interval and plot)
#' PROCESS(data, y="score", x="gender", mods="late")->P
#' P$results[[1]]$jn[[1]]       # Johnson-Neyman interval
#' P$results[[1]]$jn[[1]]$plot  # Johnson-Neyman plot (ggplot object)
#' GLM_summary(P$model.y)       # detailed results of regression
#'
#' # (allows multicategorical moderator)
#' d=airquality
#' d$Month=as.factor(d$Month)  # moderator
#' PROCESS(d, y="Temp", x="Solar.R", mods="Month")
#'
#' ## Model 2 ##
#' PROCESS(data, y="score", x="late",
#'         mods=c("gender", "family_inc"),
#'         mod.type="2-way")  # or omit this, default is "2-way"
#'
#' ## Model 3 ##
#' PROCESS(data, y="score", x="late",
#'         mods=c("gender", "family_inc"),
#'         mod.type="3-way")
#' PROCESS(data, y="pass", x="gender",
#'         mods=c("late", "family_inc"),
#'         mod.type="3-way")
#'
#' ## Model 4 ##
#' PROCESS(data, y="score", x="parent_edu",
#'         meds="family_inc", covs="gender",
#'         ci="boot", nsim=100, seed=1)
#' PROCESS(data, y="score", x="parent_edu",
#'         meds="family_inc", covs="gender",
#'         ci="bca.boot", nsim=100, seed=1)
#' PROCESS(data, y="score", x="parent_edu",
#'         meds="family_inc", covs="gender",
#'         ci="mcmc", nsim=100, seed=1)
#'
#' # (allows an infinite number of multiple mediators in parallel)
#' PROCESS(data, y="score", x="parent_edu",
#'         meds=c("family_inc", "late"),
#'         covs=c("gender", "partjob"),
#'         nsim=100, seed=1)
#'
#' ## Model 5 / 5.2 / 5.3 ##
#' PROCESS(data, y="score", x="fight",
#'         meds="late",
#'         mods="gender",
#'         covs="parent_edu",
#'         mod.path="x-y",
#'         nsim=100, seed=1)
#' PROCESS(data, y="score", x="fight",
#'         meds=c("late", "attachment"),
#'         mods=c("gender", "partjob"),
#'         covs=c("parent_edu", "family_inc"),
#'         mod.path="x-y",
#'         mod.type="3-way",
#'         nsim=100, seed=1)
#'
#' ## Model 6 ##
#' PROCESS(data, y="score", x="parent_edu",
#'         meds=c("family_inc", "late"),
#'         covs=c("gender", "partjob"),
#'         med.type="serial",
#'         nsim=100, seed=1)
#'
#' ## Model 8 ##
#' PROCESS(data, y="score", x="fight",
#'         meds="late",
#'         mods="gender",
#'         mod.path=c("x-m", "x-y"),
#'         nsim=100, seed=1)
#'
#' ## Model 10 ##
#' PROCESS(data, y="score", x="fight",
#'         meds="late",
#'         mods=c("gender", "family_inc"),
#'         mod.path=c("x-m", "x-y"),
#'         mod.type="2-way",
#'         nsim=100, seed=1)
#'
#' ## Model 12 ##
#' PROCESS(data, y="score", x="fight",
#'         meds="late",
#'         mods=c("gender", "family_inc"),
#'         mod.path=c("x-m", "x-y"),
#'         mod.type="3-way",
#'         nsim=100, seed=1)
#'
#' ## For more examples (other PROCESS models), see:
#' ## https://
#' }
#' @export
PROCESS=function(data,
                 y="",
                 x="",
                 meds=c(),
                 mods=c(),
                 covs=c(),
                 clusters=c(),
                 hlm.rand="",
                 hlm.type=c("2-2-1", "2-1-1", "1-1-1"),
                 med.type=c("parallel", "serial"),  # "p"*, "s"
                 mod.type=c("2-way", "3-way"),  # "2"*, "3"
                 mod.path=c("x-y", "x-m", "m-y", "all"),
                 cov.path=c("y", "m", "both"),
                 mod1.val=NULL,
                 mod2.val=NULL,
                 ci=c("boot", "bc.boot", "bca.boot", "mcmc"),
                 nsim=100,
                 seed=1,
                 std=FALSE,
                 nsmall=3,
                 file=NULL) {
  ## Default Setting
  warning.y.class="\n\"y\" should be a numeric variable or a factor variable with only 2 levels."
  warning.x.class="\n\"x\" should be a numeric variable or a factor variable with only 2 levels."
  warning.m.class="\n\"meds\" should be numeric variable(s) or factor variable(s) with only 2 levels."
  warning.mod.path="\nPlease also set \"mod.path\":\n    \"all\" or any combination of c(\"x-y\", \"x-m\", \"m-y\")"
  if(x=="" | y=="") stop("\n\nPlease specify both \"x\" and \"y\".")
  if(length(meds)>0 & length(mods)>0 & length(mod.path)>3)
    stop(warning.mod.path)
  if("all" %in% mod.path)
    mod.path=c("x-y", "x-m", "m-y")
  if("both" %in% cov.path)
    cov.path=c("y", "m")
  if(length(mods)>0) mod1=mods[1] else mod1=NULL
  if(length(mods)>1) mod2=mods[2] else mod2=NULL
  if(length(mods)>2) stop("\nThe number of moderators (\"mods\") should be no more than 2.")
  if(length(med.type)>1) med.type="parallel"  # default
  if(length(mod.type)>1) mod.type="2-way"     # default
  if(grepl("p", med.type)) med.type="parallel"
  if(grepl("s", med.type)) med.type="serial"
  if(grepl("2", mod.type)) mod.type="2-way"
  if(grepl("3", mod.type)) mod.type="3-way"
  if(grepl("p|s", med.type)==FALSE)
    stop("\n\"med.type\" should be \"parallel\" or \"serial\".")
  if(grepl("2|3", mod.type)==FALSE)
    stop("\n\"mod.type\" should be \"2-way\" or \"3-way\".")
  if(length(meds)>0) {
    if(length(mods)>0 & "m-y" %in% mod.path) {
      if(mod.type=="2-way")
        meds.all=" + " %^% paste(rep(meds, each=length(mods)) %^% "*" %^% mods, collapse=" + ")
      if(mod.type=="3-way")
        meds.all=" + " %^% paste(meds %^% "*" %^% paste(mods, collapse="*"), collapse=" + ")
    } else {
      meds.all=" + " %^% paste(meds, collapse=" + ")
    }
  }
  if(length(covs)>0)
    covs.all=" " %^% paste(covs, collapse=" + ") %^% " +"
  else
    covs.all=""
  if(length(ci)>1) ci="boot"  # default: percentile bootstrap
  if(grepl("p", ci) | ci=="boot") ci="boot"
  if(ci %in% c("bc", "bc.boot")) ci="bc.boot"
  if(grepl("bca", ci)) ci="bca.boot"
  if(grepl("m", ci)) ci="mcmc"
  if(ci %notin% c("boot", "bc.boot", "bca.boot", "mcmc"))
    stop("\nPlease choose either \"boot\", \"bc.boot\", \"bca.boot\", or \"mcmc\" for ci.")
  nsim.type=ifelse(grepl("boot", ci), "Bootstrap", "Monte Carlo")

  ## Data Centering and Recoding
  data=as.data.frame(data)
  data.v=na.omit(data[c(y, x, meds, mods, covs, clusters)])
  if(inherits(data.v[[y]], c("factor", "character", "logical"))) {
    if(length(unique(data.v[[y]]))==2) {
      data.v[[y]]=as.numeric(as.factor(data.v[[y]]))-1  # 0, 1
      Y01=TRUE
    } else {
      stop(warning.y.class)
    }
  } else {
    Y01=FALSE
  }
  M01=c()
  for(med in meds) {
    if(inherits(data.v[[med]], c("factor", "character", "logical"))) {
      if(length(unique(data.v[[med]]))==2) {
        data.v[[med]]=as.numeric(as.factor(data.v[[med]]))-1  # 0, 1
        M01=c(M01, TRUE)
      } else {
        stop(warning.m.class)
      }
    } else {
      M01=c(M01, FALSE)
    }
  }
  if(inherits(data.v[[x]], c("factor", "character", "logical"))) {
    if(length(unique(data.v[[x]]))==2) {
      x.levels=levels(as.factor(data.v[[x]]))
      data.v[[x]]=as.numeric(as.factor(data.v[[x]]))-1  # 0, 1
      x.trans.info=" (recoded: " %^% paste(x.levels, 0:1, sep="=", collapse=", ") %^% ")"
    } else {
      stop(warning.x.class)
    }
  } else {
    x.trans.info=""
  }
  if(std) {
    # caution !!!
    data.v=data.c=data.c.NOmed=grand_mean_center(data.v, vars=c(y, x, meds, mods, covs), std=std)
  } else {
    data.c.NOmed=grand_mean_center(data.v, vars=c(x, mods, covs), std=std)
    data.c=grand_mean_center(data.v, vars=c(x, meds, mods, covs), std=std)
  }
  nmis=nrow(data)-nrow(data.v)

  ## File Opening
  # if(!is.null(file)) {
  #   file=stringr::str_replace(file, "\\.docx$", ".doc")
  #   FILE=file(file, "a", encoding="UTF-8")
  # }

  ## Formula Building
  ft=Glue("{y} ~ {x}")
  if(length(meds)==0) {
    # (moderated) moderation
    fm=c()
    if(length(mods)==0) {
      stop("\n\nPlease specify \"meds\" (mediators) and/or \"mods\" (moderators).")
    } else if(length(mods)==1) {
      pid=1
      ptype="Simple Moderation"
      fy=Glue("{y} ~ {x}*{mod1}")
    } else if(length(mods)==2) {
      if(mod.type=="2-way") {
        pid=2
        ptype="Parallel Moderation (2 mods; 2-way)"
        fy=Glue("{y} ~ {x}*{mod1} + {x}*{mod2}")
      }
      if(mod.type=="3-way") {
        pid=3
        ptype="Moderated Moderation (2 mods; 3-way)"
        fy=Glue("{y} ~ {x}*{mod1}*{mod2}")
      }
    }
  }
  else if(length(meds)==1 | med.type=="parallel") {
    # single/parallel (moderated) mediation
    if(length(mods)==0) {
      pid=4
      ptype=ifelse(
        length(meds)==1,
        "Simple Mediation",
        Glue("Parallel Multiple Mediation ({length(meds)} meds)"))
      fm=meds %^% Glue(" ~ {x}")
      fy=Glue("{y} ~ {x}") %^% meds.all
    }
    if(length(mods)==1) {
      ptype=ifelse(
        length(meds)==1,
        "Moderated Mediation",
        Glue("Parallel Multiple Moderated Mediation ({length(meds)} meds)"))
      if("x-y" %in% mod.path) {
        fy=Glue("{y} ~ {x}*{mod1}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid=ifelse("m-y" %in% mod.path, 59, 8)
          fm=meds %^% Glue(" ~ {x}*{mod1}")
        } else {
          pid=ifelse("m-y" %in% mod.path, 15, 5)
          if(pid==5) ptype=Glue("Mediation and Moderation ({length(meds)} meds and 1 mods)")
          fm=meds %^% Glue(" ~ {x}")
        }
      } else {
        fy=Glue("{y} ~ {x}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid=ifelse("m-y" %in% mod.path, 58, 7)
          fm=meds %^% Glue(" ~ {x}*{mod1}")
        } else {
          pid=ifelse("m-y" %in% mod.path, 14, -1)
          fm=meds %^% Glue(" ~ {x}")
        }
      }
    }
    if(length(mods)==2 & mod.type=="2-way") {
      ptype=ifelse(
        length(meds)==1,
        "Moderated Mediation (2 mods; 2-way)",
        Glue("Parallel Multiple Moderated Mediation ({length(meds)} meds and 2 mods; 2-way)"))
      if("x-y" %in% mod.path) {
        fy=Glue("{y} ~ {x}*{mod1} + {x}*{mod2}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid=ifelse("m-y" %in% mod.path, 76, 10)
          fm=meds %^% Glue(" ~ {x}*{mod1} + {x}*{mod2}")
        } else {
          pid=ifelse("m-y" %in% mod.path, 17, 5.2)
          if(pid==5.2) ptype=Glue("Mediation and Parallel Moderation ({length(meds)} meds and 2 mods; 2-way)")
          fm=meds %^% Glue(" ~ {x}")
        }
      } else {
        fy=Glue("{y} ~ {x}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid=ifelse("m-y" %in% mod.path, 75, 9)
          fm=meds %^% Glue(" ~ {x}*{mod1} + {x}*{mod2}")
        } else {
          pid=ifelse("m-y" %in% mod.path, 16, -2)
          fm=meds %^% Glue(" ~ {x}")
        }
      }
    }
    if(length(mods)==2 & mod.type=="3-way") {
      ptype=ifelse(
        length(meds)==1,
        "Moderated Mediation (2 mods; 3-way)",
        Glue("Parallel Multiple Moderated Mediation ({length(meds)} meds and 2 mods; 3-way)"))
      if("x-y" %in% mod.path) {
        fy=Glue("{y} ~ {x}*{mod1}*{mod2}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid=ifelse("m-y" %in% mod.path, 73, 12)
          fm=meds %^% Glue(" ~ {x}*{mod1}*{mod2}")
        } else {
          pid=ifelse("m-y" %in% mod.path, 19, 5.3)
          if(pid==5.3) ptype=Glue("Mediation and Moderated Moderation ({length(meds)} meds and 2 mods; 3-way)")
          fm=meds %^% Glue(" ~ {x}")
        }
      } else {
        fy=Glue("{y} ~ {x}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid=ifelse("m-y" %in% mod.path, 72, 11)
          fm=meds %^% Glue(" ~ {x}*{mod1}*{mod2}")
        } else {
          pid=ifelse("m-y" %in% mod.path, 18, -3)
          fm=meds %^% Glue(" ~ {x}")
        }
      }
    }
    if(pid<0)
      stop(warning.mod.path)
    if(pid %in% 7)
      fy=Glue("{y} ~ {x} + {mod1}") %^% meds.all
    if(pid %in% c(9, 11))
      fy=Glue("{y} ~ {x} + {mod1} + {mod2}") %^% meds.all
    if(pid %in% 14:15)
      fm=meds %^% Glue(" ~ {x} + {mod1}")
    if(pid %in% 16:19)
      fm=meds %^% Glue(" ~ {x} + {mod1} + {mod2}")
  }
  else if(med.type=="serial") {
    # serial mediation
    if(length(mods)==0) {
      if(length(meds)>4)
        stop("\nPROCESS() does not support serial mediation with more than 4 mediators.")
      if(any(M01))
        stop("\nPROCESS() does not support serial mediation with dichotomous mediators.")
      pid=6
      ptype=Glue("Serial Multiple Mediation ({length(meds)} meds)")
      fy=Glue("{y} ~ {x}") %^% meds.all
      fm=meds %^% Glue(" ~ {x}")
      for(mi in 2:length(meds)) {
        fm[mi]=fm[mi] %^% " + " %^% paste(meds[1:(mi-1)], collapse=" + ")
      }
    } else {
      stop("\nPROCESS() does not support serial mediation with moderators.\nPlease remove the \"mods\" parameter from your code.")
    }
  }
  if("m" %in% cov.path)
    fm=stringr::str_replace(fm, "~", "~" %^% covs.all)
  if("y" %in% cov.path) {
    fy=stringr::str_replace(fy, "~", "~" %^% covs.all)
    ft=stringr::str_replace(ft, "~", "~" %^% covs.all)  # y ~ [covs] + x
  }

  ## Regression Model Summary
  varlist=function(vars=c()) {
    vars.text=paste(vars, collapse=', ')
    if(vars.text=="") vars.text="-"
    return(vars.text)
  }
  meds.text=varlist(meds)
  mods.text=varlist(mods)
  covs.text=varlist(covs)
  clusters.text=varlist(clusters)
  Print("
  \n
  <<bold ************ PART 1. Regression Model Summary ************>>

  <<blue PROCESS Model Code : {pid}>> <<white (Hayes, 2018; <<underline www.guilford.com/p/hayes3>>)>>
  <<blue PROCESS Model Type : {ptype}>>
  <<green
  -      Outcome (Y) : {y}
  -    Predictor (X) : {x}{x.trans.info}
  -    Mediators (M) : {meds.text}
  -   Moderators (W) : {mods.text}
  -   Covariates (C) : {covs.text}
  -     HLM Clusters : {clusters.text}
  >>
  <<yellow All numeric predictors have been {ifelse(std, 'standardized', 'mean-centered')}.>>
  \n
  ")

  if(length(meds)>0) {
    Print("<<italic Formula of Mediator>>:")
    cat("-   ", paste(fm, collapse="\n-    "))
    cat("\n")
  }
  Print("<<italic Formula of Outcome>>:")
  cat("-   ", fy)
  cat("\n")

  ## Regression Model Building
  if(Y01==FALSE) {
    FUN.y="lm"
    FML.y=""
  } else {
    FUN.y="glm"
    FML.y=", family=binomial"
  }
  Run("model.y0 = {FUN.y}({fy}, data=data.v{FML.y})")
  Run("model.y = {FUN.y}({fy}, data=data.c{FML.y})")
  Run("model.t = {FUN.y}({ft}, data=data.c{FML.y})")
  model.m=list()
  model.m0=list()
  if(pid>=4) {
    data.c.temp=data.c
    data.c=data.c.NOmed
    for(i in 1:length(fm)) {
      if(M01[i]==FALSE) {
        FUN.m="lm"
        FML.m=""
      } else {
        FUN.m="glm"
        FML.m=", family=binomial"
      }
      Run("model.m0.{i} = {FUN.m}({fm[i]}, data=data.v{FML.m})")
      Run("model.m0 = c(model.m0, list(model.m0.{i}=model.m0.{i}))")
      Run("model.m.{i} = {FUN.m}({fm[i]}, data=data.c{FML.m})")
      Run("model.m = c(model.m, list(model.m.{i}=model.m.{i}))")
    }
    data.c=data.c.temp
    rm(data.c.temp)
  }
  model_summary(c(list(model.t), model.m, list(model.y)),
                nsmall=nsmall, std=std, file=file)
  file=NULL

  ## PROCESS Model Summary
  if(pid %in% 1:3)
    pkg=Glue("\u2018interactions\u2019 (v{packageVersion('interactions')})")
  else if(pid==4)
    pkg=Glue("\u2018mediation\u2019 (v{packageVersion('mediation')})")
  else if(pid==6)
    pkg=Glue("\u2018lavaan\u2019 (v{packageVersion('lavaan')})")
  else
    pkg=Glue("\u2018mediation\u2019 (v{packageVersion('mediation')}), \u2018interactions\u2019 (v{packageVersion('interactions')})")
  Print("
  <<bold ************ PART 2. Mediation/Moderation Effect Estimate ************>>

  <<magenta
  Package Use : {pkg}
  Effect Type : {ptype} (= Model {pid})
  Sample Size : {nrow(data.v)}{ifelse(nmis>0, Glue(' ({nmis} missing observations deleted)'), '')}
  Random Seed : {ifelse(length(meds)>0, 'set.seed('%^%seed%^%')', '-')}
  Simulations : {ifelse(length(meds)>0, nsim %^% ' (' %^% nsim.type %^% ')', '-')}
  >>")
  if(length(meds)>0 & nsim<1000)
    message("\nWarning: nsim=1000 (or larger) is suggested!")
  cat("\n")

  ## PROCESS Model Building
  RES=list()
  run.process.mod.xy=function(eff.tag="") {
    text=Glue("
    res=process_mod(model.y0, model.y,
                    data.c, x, y, mod1, mod2,
                    mod1.val, mod2.val,
                    mod.type,
                    x.label=\"X\",
                    y.label=\"Y\",
                    eff.tag=\"{eff.tag}\",
                    nsmall, file=file)
    RES=c(RES, list(res))")
  }
  run.process.mod.xm=function(i, eff.tag="") {
    text=Glue("
    res=process_mod(model.m0[[i]], model.m[[i]],
                    data.c.NOmed, x, meds[i], mod1, mod2,
                    mod1.val, mod2.val,
                    mod.type,
                    x.label=\"X\",
                    y.label=\"M\",
                    eff.tag=\"{eff.tag}\",
                    nsmall, file=file)
    RES=c(RES, list(res))")
  }
  run.process.mod.my=function(i, eff.tag="") {
    text=Glue("
    res=process_mod(model.y0, model.y,
                    data.c, meds[i], y, mod1, mod2,
                    mod1.val, mod2.val,
                    mod.type,
                    x.label=\"M\",
                    y.label=\"Y\",
                    eff.tag=\"{eff.tag}\",
                    nsmall, file=file)
    RES=c(RES, list(res))")
  }
  run.process.med=function(eff.tag="") {
    text=Glue("
    res=process_med(model.m0[[i]], model.y0,
                    x, y, meds[i],
                    conditional, simple.slopes,
                    ci, nsim, seed,
                    direct=ifelse(length(mods)==0, TRUE, FALSE),
                    total=ifelse(length(meds)==1, TRUE, FALSE),
                    eff.tag=\"{eff.tag}\",
                    nsmall, file=file)
    RES=c(RES, list(res))")
  }
  conditional=NULL
  simple.slopes=NULL
  if(pid %in% 1:3) {
    # moderation
    Run(run.process.mod.xy())
  } else if(pid==4) {
    # mediation
    for(i in 1:length(meds)) Run(run.process.med())
    # res=process_lav(data.v, y, x, meds, covs,
    #                 med.type, cov.path,
    #                 ci, nsim, seed,
    #                 nsmall=nsmall,
    #                 file=file)
    # RES=c(RES, list(res))
    # lavaan ERROR: unordered factor(s) detected; make them numeric or ordered: pass gender
  } else if(pid %in% c(5, 5.2, 5.3)) {
    # mediation and moderation
    Run(run.process.mod.xy(eff.tag="(Conditional Direct Effects [c'] of X on Y)"))
    for(i in 1:length(meds)) Run(run.process.med())
  } else if(pid==6) {
    # serial mediation
    res=process_lav(data.v, y, x, meds, covs,
                    med.type, cov.path,
                    ci, nsim, seed,
                    nsmall=nsmall,
                    file=file)
    RES=c(RES, list(res))
  } else {
    # moderated mediation
    if("x-y" %in% mod.path) {
      Run(run.process.mod.xy(eff.tag="(Conditional Direct Effects [c'] of X on Y)"))
    } else {
      Print("<<cyan <<underline Direct Effect:>> \"{x}\" (X) ==> \"{y}\" (Y)>>")
      de=as.data.frame(coef(summary(model.y)))[2,]
      de$CI=paste0("[", paste(formatF(stats::confint(model.y)[2,], nsmall), collapse=", "), "]")
      names(de)[1]="Effect"
      names(de)[5]="[95% CI]"
      row.names(de)[1]="Direct (c')"
      print_table(de, nsmalls=nsmall, file=file)
      cat("\n")
    }
    for(i in 1:length(meds)) {
      if("x-m" %in% mod.path)
        Run(run.process.mod.xm(i, eff.tag="(Conditional Effects [a] of X on M)"))
      if("m-y" %in% mod.path)
        Run(run.process.mod.my(i, eff.tag="(Conditional Effects [b] of M on Y)"))
      conditional=res$conditional
      simple.slopes=res$simple.slopes
      Run(run.process.med(eff.tag="(Conditional Indirect Effects [ab] of X through M on Y)"))
    }
  }

  ## File Closing
  # if(!is.null(file)) {
  #   close(FILE)
  #   Print("<<green \u2714>> All results (plain text) are saved to <<blue '{paste0(getwd(), '/', file)}'>>")
  #   cat("\n")
  # }

  invisible(list(
    process.id=pid,
    process.type=ptype,
    model.m=model.m,
    model.y=model.y,
    results=RES
  ))
}


#' Tidy report of lavaan model.
#' @export
lavaan_summary=function(lavaan,
                        ci=c("raw", "boot", "bc.boot", "bca.boot"),
                        nsim=100,
                        seed=1) {
  if(length(ci)>1) ci="raw"
  pe=lavaan::parameterEstimates(lavaan, standardized=TRUE)
  pe.reg=pe[pe$op=="~", c("rhs", "lhs", "label", "est", "std.all")]
  pe.eff=pe[pe$op==":=", c("label", "est", "std.all")]
  if(ci!="raw") {
    set.seed(seed)
    lv.boot=lavaan::bootstrapLavaan(
      lavaan, type="nonparametric",
      FUN=function(...) { lavaan::coef(..., type="user") },
      R=nsim)
    lv.boot=as.data.frame(lv.boot)[pe.eff$label]
    EFF=as.data.frame(pe.eff[1])
    EFF$Effect=pe.eff$est
    EFF$BootSE=apply(lv.boot, 2, sd)
    EFF$z=EFF$Effect/EFF$BootSE
    EFF$pval=p.z(EFF$z)
    EFF=cbind(EFF, t(apply(lv.boot, 2, boot_ci, type=ci)), pe.eff[3])
    names(EFF)[c(1, 6, 7, 8)]=c(" ", "BootLLCI", "BootULCI", "Beta")
  } else {
    EFF=pe[pe$op==":=",
           c("label", "est", "se", "z", "pvalue",
             "ci.lower", "ci.upper", "std.all")]
    EFF$pvalue=p.z(EFF$z)
    EFF=as.data.frame(EFF)
    names(EFF)=c(" ", "Effect", "S.E.", "z", "pval", "LLCI", "ULCI", "Beta")
  }
  row.names(EFF)=EFF[[1]]
  EFF[[1]]=NULL
  invisible(EFF)
}


## Model 4 and 6
process_lav=function(data, y, x, meds, covs,
                     med.type, cov.path,
                     ci, nsim, seed,
                     nsmall=3,
                     file=NULL,
                     print=TRUE) {
  CI=switch(
    ci,
    "mcmc"="<<red Reset to:>> Percentile Bootstrap",
    "boot"="Percentile Bootstrap",
    "bc.boot"="Bias-Corrected Percentile Bootstrap",
    "bca.boot"="Bias-Corrected and Accelerated (BCa) Percentile Bootstrap")

  # ?lavaan::`lavaan-class`

  model=lav_med_modeler(y, x, meds, covs, med.type, cov.path)
  # cat(model)

  lv=lavaan::sem(model=model, data=data)
  # lavaan::summary(lv, header=FALSE,
  #                 fit.measure=TRUE,
  #                 ci=TRUE,
  #                 standardized=FALSE,
  #                 rsquare=FALSE, nd=3)
  # lvb=lavaan::sem(model=model, data=data, se="bootstrap", bootstrap=100)
  # lavaan::fitMeasures(lv)

  cat(crayon::white("Running", nsim, "simulations (lavaan model)...\n"))

  MED=lavaan_summary(lv, ci=ci, nsim=nsim, seed=seed)

  # cat("\015")
  # cat(rep_char(" ", 30))
  # cat("\015")

  if(print) {
    MED.print=MED
    MED.print$Beta=NULL
    MED.print$CI=paste0("[",
                        formatF(MED[["BootLLCI"]], nsmall), ", ",
                        formatF(MED[["BootULCI"]], nsmall), "]")
    names(MED.print)[length(MED.print)]="[Boot 95% CI]"
    MED.print$Beta=MED$Beta
    print_table(
      dplyr::select(MED.print, !c("BootLLCI", "BootULCI")),
      row.names=TRUE,
      nsmalls=nsmall,
      line.char=ifelse(is.null(file), TRUE, FALSE),
      file=file,
      title=Glue("
      <<blue <<underline LAVAAN Syntax:>>>>\n<<italic {model}>>"),
      note=Glue("
      {CI} Confidence Interval
      <<white (SEs and CIs are estimated based on {nsim} Bootstrap samples.)>>"))
    cat("\n")
  }

  return(list(lavaan.syntax=model,
              lavaan.mediation=MED))
}


extract_med=function(med, ci, nsmall=3, direct=TRUE, total=TRUE) {
  boot=ifelse(grepl("boot", ci), TRUE, FALSE)
  # MED=rbind(
  #   c(med$d.avg, sd(med$d.avg.sims), med$d.avg.ci, med$d.avg.p),
  #   c(med$z.avg, sd(med$z.avg.sims), med$z.avg.ci, med$z.avg.p),
  #   c(med$tau.coef, sd(med$tau.sims), med$tau.ci, med$tau.p))
  MED=rbind(
    c(med$d.avg, sd(med$d.avg.sims),
      boot_ci(med$d.avg.sims, ci, med$d.avg),
      med$d.avg.p),
    c(med$z.avg, sd(med$z.avg.sims),
      boot_ci(med$z.avg.sims, ci, med$z.avg),
      med$z.avg.p),
    c(med$tau.coef, sd(med$tau.sims),
      boot_ci(med$tau.sims, ci, med$tau.coef),
      med$tau.p))
  MED=as.data.frame(MED)
  row.names(MED)=c("Indirect (ab)", "Direct (c')", "Total (c)")
  names(MED)=c("Effect", "S.E.", "LLCI", "ULCI", "p")
  MED$z=MED$Effect/MED$S.E.
  MED$pval=p.z(MED$z)
  MED$CI=paste0("[",
                formatF(MED[[3]], nsmall), ", ",
                formatF(MED[[4]], nsmall), "]")
  names(MED)[8]=ifelse(boot, "[Boot 95% CI]", "[MCMC 95% CI]")
  if(direct==FALSE) total=FALSE
  if(direct==FALSE) MED=MED[which(row.names(MED)!="Direct (c')"),]
  if(total==FALSE) MED=MED[which(row.names(MED)!="Total (c)"),]
  return(MED)
}


process_med=function(model.m,
                     model.y,
                     x, y,
                     medi,
                     conditional=NULL,  # process_mod => RES0
                     simple.slopes=NULL,  # process_mod => RES
                     ci,
                     nsim=100,
                     seed=1,
                     direct=TRUE,
                     total=TRUE,
                     eff.tag="",
                     nsmall=3,
                     file=NULL,
                     print=TRUE) {
  boot=ifelse(grepl("boot", ci), TRUE, FALSE)
  CI=switch(
    ci,
    "mcmc"="Monte Carlo (Quasi-Bayesian)",
    "boot"="Percentile Bootstrap",
    "bc.boot"="Bias-Corrected Percentile Bootstrap",
    "bca.boot"="Bias-Corrected and Accelerated (BCa) Percentile Bootstrap")
  suppressMessages({
    if(is.null(conditional)) {
      # simple mediation
      cat(crayon::white("Running", nsim, "simulations...\n"))
      set.seed(seed)
      med=mediation::mediate(
        model.m=model.m,
        model.y=model.y,
        treat=x,
        mediator=medi,
        boot=boot,
        boot.ci.type="perc",  # "bca", "perc"
        sims=nsim)
      MED=extract_med(med, ci, nsmall, direct, total)
    } else {
      # moderated mediation
      COV.list=list()
      MED=data.frame()
      # DRC=data.frame()
      simple.slopes=simple.slopes[1:ncol(conditional)]
      for(i in 1:nrow(conditional)) {
        cond.list=list()
        for(j in 1:ncol(conditional)) {
          a=conditional[i, j]
          names(a)=names(conditional)[j]
          cond.list=c(cond.list, a)
        }
        COV.list=c(COV.list, list(cond.list))
      }
      cat(crayon::white("Running", nsim, "*", length(COV.list), "simulations...\n"))
      for(COV in COV.list) {
        set.seed(seed)
        med=mediation::mediate(
          model.m=model.m,
          model.y=model.y,
          treat=x,
          mediator=medi,
          covariates=COV,
          boot=boot,
          boot.ci.type="perc",  # "bca", "perc"
          sims=nsim)
        MEDi=extract_med(med, ci, nsmall, direct=FALSE, total=FALSE)
        # MEDi=cbind(data.frame(Path=row.names(MEDi)), MEDi)
        # names(MEDi)[1]=" "
        row.names(MEDi)=NULL
        MED=rbind(MED, MEDi)
        # MED=rbind(MED, MEDi[1,])
        # DRC=rbind(DRC, MEDi[2,])
      }
      # MED=cbind(rbind(simple.slopes, simple.slopes),
      #           rbind(MED, DRC))
      MED=cbind(simple.slopes, MED)
    }
  })

  # cat("\015")
  # cat(rep_char(" ", 30))
  # cat("\015")

  if(print) {
    if(eff.tag!="") eff.tag="\n" %^% eff.tag
    print_table(
      dplyr::select(MED, !c("LLCI", "ULCI", "p")),
      row.names=ifelse(is.null(conditional), TRUE, FALSE),
      nsmalls=nsmall,
      line.char=ifelse(is.null(file), TRUE, FALSE),
      title=Glue("
      <<blue <<underline Indirect Path:>> \"{x}\" (X) ==> \"{medi}\" (M) ==> \"{y}\" (Y)>>{eff.tag}"),
      note=Glue("
      {CI} Confidence Interval
      <<white ({ifelse(boot, 'SEs', 'Effects, SEs,')} and CIs are estimated based on {nsim} {ifelse(boot, 'Bootstrap samples', 'Monte Carlo samples')}.)>>"))
    cat("\n")
  }

  invisible(list(mediation=MED))
}


process_mod=function(model0,
                     model,
                     data.c,
                     x, y,
                     mod1,
                     mod2=NULL,
                     mod1.val=NULL,
                     mod2.val=NULL,
                     mod.type=c("2-way", "3-way"),
                     x.label="X",
                     y.label="Y",
                     eff.tag="",
                     nsmall=3,
                     file=NULL,
                     print=TRUE) {
  data.c=data.c
  Run("
  simple.slopes=interactions::sim_slopes(
    model=model0,
    pred={x},
    modx={mod1},
    mod2={ifelse(is.null(mod2), 'NULL', mod2)},
    modx.values=mod1.val,
    mod2.values=mod2.val,
    johnson_neyman=TRUE)
  ", silent=TRUE)
  mod1=attributes(simple.slopes)[["modx"]]
  mod2=attributes(simple.slopes)[["mod2"]]
  mod1.vals=attributes(simple.slopes)[["modx.values"]]
  mod2.vals=attributes(simple.slopes)[["mod2.values"]]
  res.sl=simple.slopes[["slopes"]]
  res.jn=simple.slopes[["jn"]]
  if(inherits(res.sl, "data.frame"))
    res.sl=list(res.sl)

  # MOD=coefficients(summary(model))
  # MOD=as.data.frame(MOD)[which(grepl(":", row.names(MOD))),]
  # row.names(MOD)=stringr::str_replace_all(row.names(MOD), ":", " x ")
  MOD=interaction_test(model, data=data.c, data.name="data.c")
  if(nrow(MOD)==2) MOD=MOD[1,]

  RES=data.frame()
  RES0=data.frame()
  for(i in 1:length(res.sl)) {
    res=res.sl[i]
    mod2.val=mod2.vals[i]
    names(mod2.val)=NULL
    if(!inherits(res, "data.frame"))
      res=as.data.frame(res) %>%
        dplyr::mutate(dplyr::across(c(-1), as.numeric))
    names(res)[2:3]=c("Effect", "S.E.")
    names(res)[4:5]=c("LLCI", "ULCI")
    names(res)[6]=stringr::str_sub(names(res)[6], 1, 1)
    names(res)[7]="pval"
    if(is.null(mod2)) {
      res0=res=cbind(data.frame(Mod1=mod1.vals), res[-1])
      if(c("- 1 SD", "Mean", "+ 1 SD") %allin% names(mod1.vals))
        res[[1]]=stringr::str_trim(formatF(res[[1]], nsmall)) %^%
          c(" (- SD)", " (Mean)", " (+ SD)")
      names(res0)[1]=mod1
      names(res)[1]="\"" %^% mod1 %^% "\""
    } else {
      res0=res=cbind(data.frame(Mod2=mod2.val), res)
      if(c("- 1 SD", "Mean", "+ 1 SD") %allin% names(mod2.vals))
        res[[1]]=stringr::str_trim(formatF(res[[1]], nsmall)) %^%
          c(" (- SD)", " (Mean)", " (+ SD)")[i]
      if(c("- 1 SD", "Mean", "+ 1 SD") %allin% names(mod1.vals))
        res[[2]]=stringr::str_trim(formatF(res[[2]], nsmall)) %^%
          c(" (- SD)", " (Mean)", " (+ SD)")
      names(res0)[1]=mod2
      names(res0)[2]=mod1
      names(res)[1]="\"" %^% mod2 %^% "\""
      names(res)[2]="\"" %^% mod1 %^% "\""
    }
    row.names(res)=row.names(res0)=NULL
    RES=rbind(RES, res)
    RES0=rbind(RES0, res0)
  }
  if(is.null(mod2))
    orders=order(RES0[[1]])
  else
    orders=order(RES0[[1]], RES0[[2]])
  RES0=RES0[orders,]
  RES=RES[orders,]
  RES$`[95% CI]`=paste0("[",
                        formatF(RES[["LLCI"]], nsmall), ", ",
                        formatF(RES[["ULCI"]], nsmall), "]")
  RES[[1]]=format(stringr::str_trim(formatF(RES[[1]], nsmall)),
                  width=nchar(names(RES)[1]))
  names(RES)[1]=format(names(RES)[1], width=max(nchar(RES[[1]])))
  if(!is.null(mod2)) {
    RES[[2]]=format(stringr::str_trim(formatF(RES[[2]], nsmall)),
                    width=nchar(names(RES)[2]))
    names(RES)[2]=format(names(RES)[2], width=max(nchar(RES[[2]])))
  }

  if(!is.null(mod2)) {
    term=row.names(MOD)[1]
    MOD.MOD=do.call(rbind, lapply(
      simple.slopes[["mods"]],
      function(model) {
        dt=model.frame(model)
        coef=interaction_test(model, data=dt, data.name="dt")
        coef=coef[term,]
        coef=cbind(data.frame(Interaction=row.names(coef)), coef)
        row.names(coef)=NULL
        return(coef)
      }))
    MOD.MOD=cbind(RES[1], MOD.MOD)
    MOD.MOD=MOD.MOD[!duplicated(MOD.MOD[[1]]),]
  } else {
    MOD.MOD=NULL
  }

  if(print) {
    if(eff.tag!="") eff.tag="\n" %^% eff.tag
    print_table(
      MOD, row.names=TRUE, nsmalls=c(2, 0, 0, 0),
      line.char=ifelse(is.null(file), TRUE, FALSE),
      title=Glue("Interaction Effect{ifelse(is.null(mod2), '', 's')} on \"{y}\" ({y.label})"))
    cat("\n")
    if(!is.null(mod2) & mod.type=="3-way") {
      print_table(
        MOD.MOD, row.names=FALSE, nsmalls=c(0, 0, 2, 0, 0, 0),
        line.char=ifelse(is.null(file), TRUE, FALSE),
        title=Glue("Conditional Interaction Effects on \"{y}\" ({y.label})"))
      cat("\n")
    }
    print_table(
      dplyr::select(RES, !c("LLCI", "ULCI")),
      row.names=FALSE, nsmalls=nsmall,
      line.char=ifelse(is.null(file), TRUE, FALSE),
      title=Glue("<<green <<underline Simple Slopes:>> \"{x}\" ({x.label}) ==> \"{y}\" ({y.label})>>{eff.tag}"))
    cat("\n")
  }

  invisible(list(mod=MOD,
                 mod.mod=MOD.MOD,
                 conditional=RES0[1:(ncol(RES0)-6)],
                 simple.slopes=RES,
                 jn=res.jn))
}


# interactions:::print.johnson_neyman
# print.johnson_neyman=function(x, raw=FALSE, ...) {
#   if(raw) {
#     interactions:::print.johnson_neyman(x, ...)
#   } else {
#     atts=attributes(x)
#     if(atts$inside==FALSE)
#       inout=crayon::inverse("OUTSIDE")
#     else
#       inout=crayon::inverse("INSIDE")
#     b_format=jtools::num_print(x$bounds, atts$digits)
#     m_range=jtools::num_print(atts$modrange, atts$digits)
#     alpha=gsub("0\\.", "\\.", as.character(atts$alpha))
#     pmsg=paste("p <", alpha)
#     cat("\nJohnson-Neyman Internal:\n\n")
#     if(all(is.finite(x$bounds))) {
#       jtools::cat_wrap(
#         "When ", atts$modx, " is ", inout,
#         " the interval [", b_format[1], ", ",
#         b_format[2], "], the slope of ", atts$pred,
#         " is ", pmsg, ".", brk="\n\n")
#       jtools::cat_wrap(
#         crayon::italic("Note: The range of observed values of",
#                        atts$modx, "is "), "[", m_range[1], ", ",
#         m_range[2], "].", brk="\n\n")
#     } else {
#       jtools::cat_wrap("The Johnson-Neyman interval could not be found.\n        Is the p value for your interaction term below\n        the specified alpha?",
#                        brk="\n\n")
#     }
#     if(atts$control.fdr==TRUE) {
#       cat("Interval calculated using false discovery rate adjusted",
#           "t =", num_print(x$t_value, atts$digits), "\n\n")
#     }
#     if(atts$plot==TRUE) {
#       print(x$plot)
#     }
#   }
# }




#### Indirect Effect: Model-Based (using the "mediation" package) ####


#' Tidy report of mediation analysis (to R Console or MS Word).
#'
#' @description
#' Tidy report of mediation analysis,
#' which is performed using the \code{\link[mediation]{mediation}} package.
#'
#' @param model Mediation model built using \code{\link[mediation:mediate]{mediation::mediate()}}.
#' @param nsmall Number of decimal places of output. Default is \code{3}.
#' @param file File name of MS Word (\code{.doc}).
## @param print.avg Just set as \code{TRUE} for a concise output.
## For details, see the "Value" section in \code{\link[mediation:mediate]{mediation::mediate()}}.
#'
#' @return Invisibly return a data frame containing the results.
#'
#' @seealso
#' \code{\link{PROCESS}}
#'
#' @examples
#' \donttest{library(mediation)
#' ?mediation::mediate
#'
#' ## Example 1: OLS Regression
#' ## Bias-corrected and accelerated (BCa) bootstrap confidence intervals
#'
#' ## Hypothesis: Solar radiation -> Ozone -> Daily temperature
#' lm.m=lm(Ozone ~ Solar.R + Month + Wind, data=airquality)
#' lm.y=lm(Temp ~ Ozone + Solar.R + Month + Wind, data=airquality)
#' set.seed(123)  # set a random seed for reproduction
#' med=mediate(lm.m, lm.y,
#'             treat="Solar.R", mediator="Ozone",
#'             sims=1000, boot=TRUE, boot.ci.type="bca")
#' med_summary(med)
#'
#' ## Example 2: Multilevel Linear Model (Linear Mixed Model)
#' ## (models must be fit using "lme4::lmer" rather than "lmerTest::lmer")
#' ## Monte Carlo simulation (quasi-Bayesian approximation)
#' ## (bootstrap method is not applicable to "lmer" models)
#'
#' ## Hypothesis: Crips -> Sweetness -> Preference (for carrots)
#' data=lmerTest::carrots  # long-format data
#' data=na.omit(data)  # omit missing values
#' lmm.m=lme4::lmer(Sweetness ~ Crisp + Gender + Age + (1 | Consumer), data=data)
#' lmm.y=lme4::lmer(Preference ~ Sweetness + Crisp + Gender + Age + (1 | Consumer), data=data)
#' set.seed(123)  # set a random seed for reproduction
#' med.lmm=mediate(lmm.m, lmm.y,
#'                 treat="Crisp", mediator="Sweetness",
#'                 sims=1000)
#' med_summary(med.lmm)
#' }
#' @export
med_summary=function(model, nsmall=3, file=NULL) {
  # for raw function, see:
  # edit(mediation::mediate)
  # edit(mediation:::print.summary.mediate)
  # edit(mediation:::pval)
  x=model
  clp=100*x$conf.level
  if(x$boot) {
    ci.type=sprintf("%s Bootstrap Confidence Interval",
                    ifelse(x$boot.ci.type=="perc",
                           "Percentile",
                           "Bias-Corrected and Accelerated (BCa)"))
  } else {
    ci.type=sprintf("%s Confidence Interval",
                    ifelse(inherits(x, "mediate.tsls"),
                           "Two-Stage Least Squares",
                           "Monte Carlo (Quasi-Bayesian)"))
  }

  cat("\n")
  Print("Mediation Analysis:")
  X=x[["treat"]]
  M=x[["mediator"]]
  Y=names(stats::model.frame(x[["model.y"]]))[1]
  Print("{X} ==> {M} ==> {Y}")

  if(!is.null(x$covariates)) {
    cat("\n")
    Print("Conditional on ...")
    conditional=data.frame(Value=unlist(x$covariates))
    conditional$Value=paste("=", conditional$Value)
    print(conditional)
    cat("\n")
  }

  smat=rbind(
    c(x$d.avg, sd(x$d.avg.sims), x$d.avg.ci, x$d.avg.p),
    c(x$z.avg, sd(x$z.avg.sims), x$z.avg.ci, x$z.avg.p),
    c(x$tau.coef, sd(x$tau.sims), x$tau.ci, x$tau.p))
  smat=as.data.frame(smat)
  row.names(smat)=c("Indirect", "Direct", "Total")
  smat$CI=paste0("[",
                 formatF(smat[[3]], nsmall), ", ",
                 formatF(smat[[4]], nsmall), "]")
  names(smat)=c("Effect",
                "S.E.",
                ifelse(x$boot, "Boot LLCI", "LLCI"),
                ifelse(x$boot, "Boot ULCI", "ULCI"),
                "pval",
                ifelse(x$boot, "[Bootstrap CI]", "[Monte Carlo CI]"))
  print_table(smat[c(1, 2, 6, 5)], nsmalls=nsmall)
  Print(ci.type)
  Print("Conf. Level: {clp}%")
  Print("Sample Size: {x$nobs}")
  Print("Simulations: {x$sims} ({ifelse(x$boot, 'Bootstrap', 'Monte Carlo')})")
  cat("\n")

  if(!is.null(file)) {
    smat.new=smat
    smat.new.names=names(smat.new)
    smat.new.names[5]="<i>p</i>"
    smat.new[[6]]=stringr::str_replace_all(smat.new[[6]], "-", "\u2013")
    print_table(
      smat.new[c(1, 2, 6, 5)], nsmalls=nsmall,
      col.names=c(smat.new.names[c(1, 2, 6, 5)], " "),
      file=file,
      file.align.head=c("left", "right", "right", "right", "right", "left"),
      file.align.text=c("left", "right", "right", "right", "right", "left"),
      title=paste0(
        "<b>",
        ifelse(is.null(x$covariates),
               "Mediation Analysis",
               "Mediation Analysis (Conditional)"),
        "</b></p>\n<p>",
        "Model Hypothesis:</p>\n<p>",
        X, " \u2192 ", M, " \u2192 ", Y
      ),
      note=paste0(
        ci.type, "</p>\n<p>",
        "Conf. Level: ", clp, "%</p>\n<p>",
        "Sample Size: ", x$nobs, "</p>\n<p>",
        "Simulations: ", x$sims, ifelse(x$boot, " (Bootstrap)", " (Monte Carlo)")
      ))
  }

  invisible(smat)
}




#### Time-Series Analyses ####


#' Cross-correlation analysis.
#'
#' @description
#' Plot the results of cross-correlation analysis using \code{ggplot2}
#' (rather than R base plot) for more flexible modification of the plot.
#'
#' @details
#' Significant correlations with \emph{negative time lags} suggest
#' shifts in a predictor \emph{precede} shifts in an outcome.
#'
#' @param formula Model formula like \code{y ~ x}.
#' @param data Data frame.
#' @param lag.max Maximum time lag. Default is \code{30}.
#' @param sig.level Significance level. Default is \code{0.05}.
#' @param xbreaks X-axis breaks.
#' @param ybreaks Y-axis breaks.
#' @param ylim Y-axis limits. Default is \code{NULL} to automatically estimate.
#' @param alpha.ns Color transparency (opacity: 0~1) for non-significant values.
#' Default is \code{1} for no transparency (i.e., opaque color).
#' @param pos.color Color for positive values. Default is \code{"black"}.
#' @param neg.color Color for negative values. Default is \code{"black"}.
#' @param ci.color Color for upper and lower bounds of significant values.
#' Default is \code{"blue"}.
#' @param title Plot title. Default is an illustration of the formula.
#' @param subtitle Plot subtitle.
#' @param xlab X-axis title. Default is \code{"Lag"}.
#' @param ylab Y-axis title. Default is \code{"Cross-Correlation"}.
#'
#' @return
#' A \code{gg} object, which you can further modify using
#' \code{ggplot2} syntax and save using \code{ggsave()}.
#'
#' @examples
#' # resemble the default plot output by `ccf()`
#' p1=ccf_plot(chicken ~ egg, data=lmtest::ChickEgg)
#'
#' # a more colorful plot
#' p2=ccf_plot(chicken ~ egg, data=lmtest::ChickEgg, alpha.ns=0.3,
#'             pos.color="#CD201F",
#'             neg.color="#21759B",
#'             ci.color="black")
#'
#' @seealso \code{\link{granger_test}}
#'
#' @import ggplot2
#' @export
ccf_plot=function(formula, data,
                  lag.max=30, sig.level=0.05,
                  xbreaks=seq(-100, 100, 10),
                  ybreaks=seq(-1, 1, 0.2),
                  ylim=NULL, alpha.ns=1,
                  pos.color="black", neg.color="black", ci.color="blue",
                  title=NULL, subtitle=NULL,
                  xlab="Lag", ylab="Cross-Correlation") {
  lag=acf=direc=sig=NULL

  x=as.character(formula)[3]
  y=as.character(formula)[2]
  data=as.data.frame(data)
  if(is.null(title)) title=Glue("{x} \u2192 {y}")

  cc=stats::ccf(x=data[[x]], y=data[[y]], lag.max=lag.max, plot=FALSE, na.action=na.omit)
  ccdata=with(cc, data.frame(lag, acf))
  n=cc$n.used
  rsig=psych::t2r(stats::qt(sig.level/2, n, lower.tail=F), n-2)
  ccdata$sig=as.factor(ifelse(abs(ccdata$acf)<rsig, 0, 1))
  ccdata$direc=as.factor(ifelse(ccdata$acf<0, 0, 1))

  p=ggplot(ccdata, aes(x=lag, y=acf, color=direc, alpha=sig)) +
    geom_segment(aes(xend=lag, yend=0), show.legend=FALSE) +
    geom_hline(aes(yintercept=0), linetype=1, color="black") +
    geom_hline(aes(yintercept=rsig), linetype=2, color=ci.color) +
    geom_hline(aes(yintercept=-rsig), linetype=2, color=ci.color) +
    scale_x_continuous(breaks=xbreaks) +
    scale_y_continuous(limits=ylim, breaks=ybreaks) +
    scale_color_manual(values=c(as.character(neg.color),
                                as.character(pos.color))) +
    scale_alpha_manual(values=c(alpha.ns, 1)) +
    labs(x=xlab, y=ylab, title=title, subtitle=subtitle) +
    theme_bruce()

  return(p)
}


#' Granger causality test (bivariate).
#'
#' @description
#' Granger test of predictive causality (between two time series)
#' using the \code{\link[lmtest:grangertest]{lmtest::grangertest()}} function.
#'
#' @details
#' The Granger causality test examines whether
#' the lagged values of a predictor
#' have any incremental role in predicting an outcome
#' if controlling for the lagged values of the outcome itself.
#'
#' @inheritParams ccf_plot
#' @param lags Time lags. Default is \code{1:5}.
#' @param test.reverse Whether to test reverse causality. Default is \code{FALSE}.
#'
#' @return No return value.
#'
#' @examples
#' granger_test(chicken ~ egg, data=lmtest::ChickEgg)
#' granger_test(chicken ~ egg, data=lmtest::ChickEgg, lags=1:10, test.reverse=TRUE)
#'
#' @seealso
#' \code{\link{ccf_plot}},
#' \code{\link{granger_causality}}
#'
#' @export
granger_test=function(formula, data, lags=1:5,
                      test.reverse=FALSE) {
  if(test.reverse) {
    formula.rev=stats::as.formula(paste(formula[3], formula[1], formula[2]))
    formulas=list(formula, formula.rev)
  } else {
    formulas=list(formula)
  }

  cat("\n")
  Print("<<yellow ====== Granger Causality Test (Bivariate) ======>>")

  Print("\n\n\nHypothesized direction:")
  Print("<<blue {formula[2]} ~ {formula[2]}[1:Lags] + <<green {formula[3]}[1:Lags]>>>>")

  for(f in formulas) {
    if(test.reverse & f!=formulas[[1]]) {
      Print("\n\n\nReverse direction:")
      Print("<<blue {formula[3]} ~ {formula[3]}[1:Lags] + <<green {formula[2]}[1:Lags]>>>>")
    }
    for(lag in lags) {
      gt=lmtest::grangertest(formula=f, data=data, order=lag, na.action=stats::na.omit)
      result=bruceR::p(f=gt[2,"F"], df1=-gt[2,"Df"], df2=gt[1,"Res.Df"])
      Print("Lags = {lag}:\t{result}")
    }
  }
}


vargranger=function(varmodel, var.y, var.x) {
  vms=varmodel[["varresult"]]
  vars=names(vms)
  if(length(var.x)==1) {
    if(var.x=="ALL")
      dropped.var=paste(paste0("^", vars[which(vars!=var.y)]), collapse="|")
    else
      dropped.var=var.x
  } else {
    dropped.var=var.x=paste(var.x, collapse="|")
  }
  vm.raw=vms[[var.y]]
  vm=stats::lm(vm.raw[["terms"]], data=vm.raw[["model"]])
  lags=names(vm[["coefficients"]])
  dropped.vars=lags[which(grepl(dropped.var, lags))]
  dropped=paste(dropped.vars, collapse=" - ")
  aov=stats::anova(stats::update(vm, as.formula(paste("~ . -", dropped))), vm)
  df1=aov[2, "Df"]
  df2=aov[2, "Res.Df"]
  chi2=aov[2, "F"]*df1
  p.chisq=p.chi2(chi2, df1)
  data.frame(Equation=var.y,
             Excluded=var.x,
             `F`=aov[2, "F"],
             df1=df1,
             df2=df2,
             p.F=aov[2, "Pr(>F)"],
             sig.F=formatF(sig.trans(aov[2, "Pr(>F)"]), 0),
             Chisq=chi2,  # F = Chisq/k where k is the difference in degrees of freedom
             df=df1,
             p.Chisq=p.chisq,
             sig.Chisq=formatF(sig.trans(p.chisq), 0),
             Dropped=paste(dropped.vars, collapse=", "))
}


#' Granger causality test (multivariate).
#'
#' @description
#' Granger test of predictive causality (between multivariate time series)
#' based on vector autoregression (\code{\link[vars:VAR]{VAR}}) model.
#' Its output resembles the output of the \code{vargranger}
#' command in Stata (but here using an \emph{F} test).
#'
#' @details
#' The Granger causality test (based on VAR model) examines whether
#' the lagged values of a predictor (or predictors)
#' have any incremental role in predicting an outcome
#' if controlling for the lagged values of the outcome itself.
#'
#' @param varmodel VAR model fitted using the \code{\link[vars:VAR]{vars::VAR()}} function.
#' @param var.y,var.x [optional] Default is \code{NULL} (all variables).
#' If specified, then perform tests for specific variables.
#' Values can be a single variable (e.g., \code{"X"}),
#' a vector of variables (e.g., \code{c("X1", "X2")}),
#' or a string containing regular expression (e.g., \code{"X1|X2"}).
#' @param test \emph{F} test and/or Wald \eqn{\chi}^2 test. Default is both: \code{c("F", "Chisq")}.
#' @param file File name of MS Word (\code{.doc}).
#' @param check.dropped Check dropped variables. Default is \code{FALSE}.
#'
#' @return A data frame of results.
#'
#' @seealso
#' \code{\link{ccf_plot}},
#' \code{\link{granger_test}}
#'
#' @examples
#' \dontrun{
#' # "vars" package should be installed and loaded.
#' library(vars)
#' data(Canada)
#' VARselect(Canada)
#' vm=VAR(Canada, p=3)
#' model_summary(vm)
#' granger_causality(vm)
#' }
#'
#' @export
granger_causality=function(varmodel, var.y=NULL, var.x=NULL,
                           test=c("F", "Chisq"),
                           file=NULL,
                           check.dropped=FALSE) {
  vars=names(varmodel[["varresult"]])
  if(is.null(var.y)) var.y=vars
  if(is.null(var.x)) {
    res=data.frame(Equation=rep(var.y, each=length(vars)+1),
                   Excluded=c(vars, "ALL"))
    res=res[which(res$Equation!=res$Excluded),]
  } else {
    if(length(var.x)==1)
      res=expand.grid(Equation=var.y,
                      Excluded=var.x)
    else
      res=expand.grid(
        Equation=var.y,
        Excluded=unique(c(
          var.x,
          paste(
            stringr::str_subset(var.x, "\\|", negate=TRUE),
            collapse="|")
        )))
    res$Equation=as.character(res$Equation)
    res$Excluded=as.character(res$Excluded)
    res=res[which(res$Excluded!=""),]
  }

  res=do.call("rbind", lapply(1:nrow(res), function(i) {
    vargranger(varmodel, res[i, "Equation"], res[i, "Excluded"])
  }))

  res$Causality=res$Equation %^% " <= " %^% res$Excluded
  nchars=max(nchar(res$Causality))
  res.check=rbind(data.frame(Dropped="Dropped variables (lags)"),
                  res["Dropped"])
  res.check$Dropped=paste(" ", format(res.check$Dropped))
  names(res.check)=" "
  row.names(res.check)=c(" ", res$Causality)
  if(check.dropped) print(res.check)
  res$Dropped=NULL

  result=data.frame()
  for(var in var.y)
    result=rbind(result,
                 data.frame(Equation=NA, Excluded=NA,
                            `F`=NA, df1=NA, df2=NA, p.F=NA, sig.F="",
                            `Chisq`=NA, df=NA, p.Chisq=NA, sig.Chisq="",
                            Causality=rep_char("-", nchars)),
                 res[which(res$Equation==var),])
  result$`F`=formatF(result$`F`, 2)
  result$`Chisq`=formatF(result$`Chisq`, 2)
  result$p.F=p.trans(result$p.F)
  result$p.Chisq=p.trans(result$p.Chisq)
  result$` `="  "
  names(result)[6]="p"
  names(result)[7]="  "
  names(result)[10]=" p"
  names(result)[11]="   "
  names(result)[12]="    "

  test.which=c()
  test.text=c()
  align.which=c("left")
  col.which=c("")
  if("F" %in% test) {
    test.which=c(test.which, 3:7)
    test.text=c(test.text, "<<italic F>> test")
    align.which=c(align.which, c("right", "right", "right", "right", "left"))
    col.which=c(col.which, c("<i>F</i>", "<i>df</i><sub>1</sub>", "<i>df</i><sub>2</sub>", "<i>p</i>", " "))
  }
  if("Chisq" %in% test) {
    test.which=c(test.which, 8:11)
    test.text=c(test.text, "Wald \u03c7\u00b2 test")
    align.which=c(align.which, c("right", "right", "right", "left"))
    col.which=c(col.which, c("\u03c7<sup>2</sup>", "<i>df</i>", " <i>p</i>", "  "))
  }

  cat("\n")
  Print("
  <<yellow ====== Granger Causality Test (Multivariate) ======>>

  {paste(test.text, collapse=' and ')} based on VAR({varmodel$p}) model:")
  print_table(result[c(12:13, test.which)],
              nsmalls=0,
              row.names=FALSE)
  cat("\n")

  if(!is.null(file)) {
    result[[12]]=result[[12]] %>%
      stringr::str_replace_all("<=", "\u2190") %>%
      stringr::str_replace_all("^-+$", "")
    print_table(
      result[c(12, test.which)],
      nsmalls=0,
      row.names=FALSE,
      col.names=col.which,
      title=paste0("<b>Table. Granger Causality Test (Multivariate) Based on VAR(", varmodel$p, ") Model.</b>"),
      note="<i>Note</i>. * <i>p</i> < .05. ** <i>p</i> < .01. *** <i>p</i> < .001.",
      file=file,
      file.align.text=align.which)
  }

  invisible(list(result=res, check.dropped=res.check))
}

