#### PROCESS Macro (GLM and HLM) ####


interaction_F_test = function(model, data=NULL, data.name="data") {
  Run("{data.name} = data")
  df2 = df.residual(model)
  interms = attr(terms(model), "term.labels")
  interms = interms[grepl(":", interms)]
  interms.form = as.formula(paste("~", paste(interms, collapse=" + ")))
  interms.drop = as.formula(paste(". ~ . -", paste(interms, collapse=" - ")))
  if(inherits(model, "lm")) {
    dp1 = drop1(model, scope=interms.form, test="F")
    dp1 = dp1[!is.na(dp1$Df), c("Df", "F value", "Pr(>F)")]
    aov = anova(update(model, interms.drop), model)
    if(df2!=aov[2, "Res.Df"]) warning("Error!", call.=TRUE)
    aov.table = data.frame(
      `F` = c(dp1[[2]], aov[2, "F"]),
      df1 = c(dp1[[1]], aov[2, "Df"]),
      df2 = df2,
      pval = c(dp1[[3]], aov[2, "Pr(>F)"]))
    row.names(aov.table) = c(gsub(":", " * ", row.names(dp1)),
                           "(All Interactions)")
  } else {
    # dp1 = drop1(model, scope=interms, test="F")
    dp1 = anova(model)[interms,]
    aov.table = data.frame(
      `F` = dp1[,"F value"],
      df1 = dp1[,"NumDF"],
      df2 = dp1[,"DenDF"],
      pval = dp1[,"Pr(>F)"])
    row.names(aov.table) = gsub(":", " * ", row.names(dp1))
  }
  return(aov.table)
}


interaction_Chi2_test = function(model, data=NULL, data.name="data") {
  Run("{data.name} = data")
  interms = attr(terms(model), "term.labels")
  interms = interms[grepl(":", interms)]
  interms.form = as.formula(paste("~", paste(interms, collapse=" + ")))
  interms.drop = as.formula(paste(". ~ . -", paste(interms, collapse=" - ")))
  if(inherits(model, "glm")) {
    dp1 = drop1(model, scope=interms.form, test="Chisq")
    dp1 = dp1[!is.na(dp1$Df), c("Df", "LRT", "Pr(>Chi)")]
    chi = anova(update(model, interms.drop), model, test="Chisq")
    chi.table = data.frame(
      `Chisq` = c(dp1[[2]], chi[2, "Deviance"]),
      df = c(dp1[[1]], chi[2, "Df"]),
      pval = c(dp1[[3]], chi[2, "Pr(>Chi)"]))
    row.names(chi.table) = c(gsub(":", " * ", row.names(dp1)),
                             "(All Interactions)")
  } else {
    dp1 = drop1(model, scope=interms.form, test="Chisq")
    dp1 = dp1[!is.na(dp1$npar), c("npar", "LRT", "Pr(Chi)")]
    chi.table = data.frame(
      Chisq = dp1[,"LRT"],
      df = dp1[,"npar"],
      pval = dp1[,"Pr(Chi)"])
    row.names(chi.table) = gsub(":", " * ", row.names(dp1))
  }
  return(chi.table)
}


interaction_test = function(model, data=NULL, data.name="data") {
  if(inherits(model, c("glm", "glmerMod"))) {
    table = interaction_Chi2_test(model, data=data, data.name=data.name)
  } else {
    table = interaction_F_test(model, data=data, data.name=data.name)
  }
  return(table)
}


lav_med_modeler = function(y, x,
                           meds=c(),
                           covs=c(),
                           med.type=c("parallel", "serial"),
                           cov.path=c("y", "m", "both")) {
  ids = 1:length(meds)
  if(length(med.type)>1) med.type = "parallel"
  if(length(meds)==1) {
    fm = meds %^% " ~ a*" %^% x
    fy = y %^% " ~ c.*"%^% x %^% " + " %^% "b*" %^% meds
    pars = paste(
      "Indirect := a*b",
      "Direct := c.",
      "Total := c. + a*b",
      sep="\n")
  } else {
    if(grepl("p", med.type)) {
      x.all = "a" %^% ids %^% "*" %^% x
      meds.all = paste("b" %^% ids %^% "*" %^% meds, collapse=" + ")
      ind = "Indirect_X_M" %^% ids %^% "_Y := " %^% "a" %^% ids %^% "*" %^% "b" %^% ids
      fm = meds %^% " ~ " %^% x.all
      fy = y %^% " ~ c.*"%^% x %^% " + " %^% meds.all
      ind.all = paste("a" %^% ids %^% "*" %^% "b" %^% ids, collapse=" + ")
      pars = paste(
        "Indirect_All := " %^% ind.all,
        paste(ind, collapse="\n"),
        "Direct := c.",
        "Total := c. + " %^% ind.all,
        sep="\n")
    }
    if(grepl("s", med.type)) {
      x.all = "a" %^% 1:length(meds) %^% "*" %^% x
      meds.all = paste("b" %^% ids %^% "*" %^% meds, collapse=" + ")
      fm = meds %^% " ~ " %^% x.all
      for(mi in 2:length(meds)) {
        fm[mi] = fm[mi] %^% " + " %^%
          paste("d" %^% ids[1:(mi-1)] %^% ids[mi] %^% "*" %^%
                  meds[1:(mi-1)], collapse=" + ")
      }
      fy = y %^% " ~ c.*"%^% x %^% " + " %^% meds.all
      if(length(meds)==2) {
        ind.all = "a1*b1 + a2*b2 + a1*d12*b2"
        pars = Glue("
        Indirect_All := {ind.all}
        Ind_X_M1_Y := a1*b1
        Ind_X_M2_Y := a2*b2
        Ind_X_M1_M2_Y := a1*d12*b2
        Direct := c.
        Total := c. + {ind.all}
        ")
      }
      if(length(meds)==3) {
        ind.all = paste(
          "a1*b1",
          "a2*b2",
          "a3*b3",
          "a1*d12*b2",
          "a1*d13*b3",
          "a2*d23*b3",
          "a1*d12*d23*b3",
          sep=" + ")
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
        ind.all = paste(
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
          sep=" + ")
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
    covs.all = " " %^% paste(covs, collapse=" + ") %^% " +"
  else
    covs.all = ""
  if("m" %in% cov.path)
    fm = str_replace(fm, "~", "~" %^% covs.all)
  if("y" %in% cov.path)
    fy = str_replace(fy, "~", "~" %^% covs.all)
  model = paste(paste(fm, collapse="\n"),
                fy, pars,
                sep="\n")
  return(model)
}


# edit(mediation::mediate)
# edit(boot::boot.ci)
# edit(boot:::perc.ci)
# edit(boot:::bca.ci)
boot_ci = function(boot,
                   type=c("boot", "bc.boot", "bca.boot", "mcmc"),
                   true=NULL,
                   conf=0.95) {
  low = (1 - conf) / 2
  high = 1 - low
  if(length(type)>1) type = "boot"
  if(type %in% c("boot", "mcmc")) {
    ci = quantile(boot, c(low, high), na.rm=TRUE)  # percentile
  } else {
    if(is.null(true)) boot0 = mean(boot) else boot0 = true
    p0 = length(boot[boot<boot0]) / length(boot)
    z0 = qnorm(p0)
    z.l = qnorm(low)
    z.h = qnorm(high)
    if(type=="bca.boot") {
      U = (length(boot) - 1) * (boot0 - boot)
      a = sum(U^3) / (6 * (sum(U^2))^1.5)  # accelerated bias-corrected
    } else {
      a = 0  # bias-corrected
    }
    lower.bc = pnorm(z0 + (z0+z.l) / (1 - a*(z0+z.l)))
    upper.bc = pnorm(z0 + (z0+z.h) / (1 - a*(z0+z.h)))
    ci = quantile(boot, c(lower.bc, upper.bc), na.rm=TRUE)
  }
  return(ci)
}


#' PROCESS for mediation and/or moderation analyses.
#'
#' @description
#' To perform mediation, moderation, and conditional process (moderated mediation) analyses,
#' people may use software like
#' \href{http://www.statmodel.com/index.shtml}{Mplus},
#' \href{https://www.processmacro.org/index.html}{SPSS "PROCESS" macro},
#' and \href{https://njrockwood.com/mlmed/}{SPSS "MLmed" macro}.
#' Some R packages can also perform such analyses separately and in a complex way, including
#' \link[mediation:mediate]{R package "mediation"},
#' \link[interactions:sim_slopes]{R package "interactions"},
#' and \link[lavaan:lavaan-class]{R package "lavaan"}.
#' Some other R packages or scripts/modules have been further developed to improve the convenience, including
#' \href{https://jamovi-amm.github.io/}{jamovi module "jAMM"} (by \emph{Marcello Gallucci}, based on the \code{lavaan} package),
#' \href{https://CRAN.R-project.org/package=processR}{R package "processR"} (by \emph{Keon-Woong Moon}, not official, also based on the \code{lavaan} package),
#' and \href{https://www.processmacro.org/download.html}{R script file "process.R"}
#' (the official PROCESS R code by \emph{Andrew F. Hayes}, but it is not yet an R package and has some bugs and limitations).
#'
#' Here, the \code{\link[bruceR:PROCESS]{bruceR::PROCESS()}} function provides
#' an alternative to performing mediation/moderation analyses in R.
#' This function supports a total of \strong{24} kinds of SPSS PROCESS models (Hayes, 2018)
#' and also supports multilevel mediation/moderation analyses.
#' Overall, it supports the most frequently used types of mediation, moderation,
#' moderated moderation (3-way interaction), and moderated mediation (conditional indirect effect) analyses
#' for \strong{(generalized) linear or linear mixed models}.
#'
#' Specifically, the \code{\link[bruceR:PROCESS]{bruceR::PROCESS()}} function
#' fits regression models based on the data, variable names, and a few other arguments
#' that users input (with \strong{no need to} specify the PROCESS model number and \strong{no need to} manually mean-center the variables).
#' The function can automatically judge the model number/type and also conduct grand-mean centering before model building
#' (using the \code{\link[bruceR:grand_mean_center]{bruceR::grand_mean_center()}} function).
#'
#' This automatic grand-mean centering can be turned off by setting \code{center=FALSE}.
#'
#' Note that this automatic grand-mean centering
#' (1) makes the results of main effects accurate for interpretation;
#' (2) does not change any results of model fit (it only affects the interpretation of main effects);
#' (3) is only conducted in "PART 1" (for an accurate estimate of main effects) but not in "PART 2" because
#' it is more intuitive and interpretable to use the raw values of variables for the simple-slope tests in "PART 2";
#' (4) is not optional to users because mean-centering should always be done when there is an interaction;
#' (5) is not conflicted with group-mean centering because after group-mean centering the grand mean of a variable will also be 0,
#' such that the automatic grand-mean centering (with mean = 0) will not change any values of the variable.
#'
#' If you need to do group-mean centering, please do this before using PROCESS.
#' \code{\link[bruceR:group_mean_center]{bruceR::group_mean_center()}} is a useful function of group-mean centering.
#' Remember that the automatic grand-mean centering in PROCESS never affects the values of a group-mean centered variable, which already has a grand mean of 0.
#'
#' The \code{\link[bruceR:PROCESS]{bruceR::PROCESS()}} function uses:
#' \enumerate{
#'   \item the \code{\link[interactions:sim_slopes]{interactions::sim_slopes()}} function to
#'   estimate simple slopes (and conditional direct effects) in moderation, moderated moderation, and moderated mediation models
#'   (PROCESS Models 1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 58, 59, 72, 73, 75, 76).
#'   \item the \code{\link[mediation:mediate]{mediation::mediate()}} function to
#'   estimate (conditional) indirect effects in (moderated) mediation models
#'   (PROCESS Models 4, 5, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 58, 59, 72, 73, 75, 76).
#'   \item the \code{\link[lavaan:sem]{lavaan::sem()}} function to perform serial multiple mediation analysis (PROCESS Model 6).
#' }
#' If you use this function in your research and report its results in your paper, please cite not only \code{bruceR} but also
#' the other R packages it uses internally (\code{mediation}, \code{interactions}, and/or \code{lavaan}).
#'
#' Two parts of results are printed:
#'
#' PART 1. Regression model summary (using \code{\link[bruceR:model_summary]{bruceR::model_summary()}} to summarize the models)
#'
#' PART 2. Mediation/moderation effect estimates (using one or a combination of the above packages and functions to estimate the effects)
#'
#' To organize the PART 2 output, the results of \strong{Simple Slopes} are titled in \strong{green},
#' whereas the results of \strong{Indirect Path} are titled in \strong{blue}.
#'
#' \strong{\emph{Disclaimer}:}
#' Although this function is named after \code{PROCESS}, Andrew F. Hayes has no role in its design, and
#' its development is independent from the official SPSS PROCESS macro and "process.R" script.
#' Any error or limitation should be attributed to the three R packages/functions that \code{bruceR::PROCESS()} uses internally.
#' Moreover, as mediation analyses include \emph{random processes} (i.e., bootstrap resampling or Monte Carlo simulation),
#' the results of mediation analyses are \emph{unlikely} to be exactly the same across different software
#' (even if you set the same random seed in different software).
#'
#' @param data Data frame.
#' @param y,x Variable name of outcome (Y) and predictor (X).
#'
#' It supports both continuous (numeric) and dichotomous (factor) variables.
#' @param meds Variable name(s) of mediator(s) (M).
#' Use \code{c()} to combine multiple mediators.
#'
#' It supports both continuous (numeric) and dichotomous (factor) variables.
#'
#' It allows an infinite number of mediators in parallel
#' or 2~4 mediators in serial.
#'
#' * Order matters when \code{med.type="serial"}
#' (PROCESS Model 6: serial mediation).
#' @param mods Variable name(s) of 0~2 moderator(s) (W).
#' Use \code{c()} to combine multiple moderators.
#'
#' It supports all types of variables:
#' continuous (numeric), dichotomous (factor), and multicategorical (factor).
#'
#' * Order matters when \code{mod.type="3-way"}
#' (PROCESS Models 3, 5.3, 11, 12, 18, 19, 72, and 73).
#'
#' ** Do not set this argument when \code{med.type="serial"}
#' (PROCESS Model 6).
#' @param covs Variable name(s) of covariate(s) (i.e., control variables).
#' Use \code{c()} to combine multiple covariates.
#' It supports all types of (and an infinite number of) variables.
#' @param clusters HLM (multilevel) cluster(s):
#' e.g., \code{"School"}, \code{c("Prov", "City")}, \code{c("Sub", "Item")}.
#' @param hlm.re.m,hlm.re.y HLM (multilevel) random effect term of M model and Y model.
#' By default, it converts \code{clusters} to \code{\link[lme4:lme4-package]{lme4}} syntax of random intercepts:
#' e.g., \code{"(1 | School)"} or \code{"(1 | Sub) + (1 | Item)"}.
#'
#' You may specify these arguments to include more complex terms:
#' e.g., random slopes \code{"(X | School)"}, or 3-level random effects \code{"(1 | Prov/City)"}.
#' @param hlm.type HLM (multilevel) mediation type (levels of "X-M-Y"):
#' \code{"1-1-1"} (default),
#' \code{"2-1-1"} (indeed the same as \code{"1-1-1"} in a mixed model),
#' or \code{"2-2-1"} (currently \emph{not fully supported}, as limited by the \code{\link[mediation:mediate]{mediation}} package).
#' In most cases, no need to set this argument.
#' @param med.type Type of mediator:
#' \code{"parallel"} (default) or \code{"serial"}
#' (only relevant to PROCESS Model 6).
#' Partial matches of \code{"p"} or \code{"s"} also work.
#' In most cases, no need to set this argument.
#' @param mod.type Type of moderator:
#' \code{"2-way"} (default) or \code{"3-way"}
#' (relevant to PROCESS Models 3, 5.3, 11, 12, 18, 19, 72, and 73).
#' Partial matches of \code{"2"} or \code{"3"} also work.
#' @param mod.path Which path(s) do the moderator(s) influence?
#' \code{"x-y"}, \code{"x-m"}, \code{"m-y"}, or any combination of them
#' (use \code{c()} to combine), or \code{"all"} (i.e., all of them).
#' No default value.
#' @param cov.path Which path(s) do the control variable(s) influence?
#' \code{"y"}, \code{"m"}, or \code{"both"} (default).
#' @param mod1.val,mod2.val By default (\code{NULL}), it uses
#' \strong{Mean +/- SD} of a continuous moderator (numeric) or
#' \strong{all levels} of a dichotomous/multicategorical moderator (factor) to
#' perform simple slope analyses and/or conditional mediation analyses.
#' You may manually specify a vector of certain values: e.g.,
#' \code{mod1.val=c(1, 3, 5)} or \code{mod1.val=c("A", "B", "C")}.
#' @param ci Method for estimating the standard error (SE) and
#' 95\% confidence interval (CI) of indirect effect(s).
#' Default is \code{"boot"} for (generalized) linear models or
#' \code{"mcmc"} for (generalized) linear mixed models (i.e., multilevel models).
#' \describe{
#'   \item{\code{"boot"}}{Percentile Bootstrap}
#'   \item{\code{"bc.boot"}}{Bias-Corrected Percentile Bootstrap}
#'   \item{\code{"bca.boot"}}{Bias-Corrected and Accelerated (BCa) Percentile Bootstrap}
#'   \item{\code{"mcmc"}}{Markov Chain Monte Carlo (Quasi-Bayesian)}
#' }
#' * Note that these methods \emph{never} apply to the estimates of simple slopes.
#' You \emph{should not} report the 95\% CIs of simple slopes as Bootstrap or Monte Carlo CIs,
#' because they are just standard CIs without any resampling method.
#' @param nsim Number of simulation samples (bootstrap resampling or Monte Carlo simulation)
#' for estimating SE and 95\% CI. Default is \code{100} for running examples faster.
#' In formal analyses, however, \strong{\code{nsim=1000} (or larger)} is strongly suggested!
#' @param seed Random seed for obtaining reproducible results.
#' Default is \code{NULL}.
#' You may set to any number you prefer
#' (e.g., \code{seed=1234}, just an uncountable number).
#'
#' * Note that all mediation models include random processes
#' (i.e., bootstrap resampling or Monte Carlo simulation).
#' To get exactly the same results between runs, you need to set a random seed.
#' However, even if you set the same seed number, it is unlikely to
#' get exactly the same results across different R packages
#' (e.g., \code{\link[lavaan:lavaan-class]{lavaan}} vs. \code{\link[mediation:mediate]{mediation}})
#' and software (e.g., SPSS, Mplus, R, jamovi).
#' @param center Centering numeric (continuous) predictors? Default is \code{TRUE} (suggested).
#' @param std Standardizing variables to get standardized coefficients? Default is \code{FALSE}.
#' If \code{TRUE}, it will standardize all numeric (continuous) variables
#' before building regression models.
#' However, it is \emph{not suggested} to set \code{std=TRUE} for \emph{generalized} linear (mixed) models.
#' @param digits,nsmall Number of decimal places of output. Default is \code{3}.
#' @param file File name of MS Word (\code{.doc}).
#' Currently, only regression model summary can be saved.
#'
#' @return
#' Invisibly return a list of results:
#' \describe{
#'   \item{\code{process.id}}{PROCESS model number.}
#'   \item{\code{process.type}}{PROCESS model type.}
#'   \item{\code{model.m}}{"Mediator" (M) models (a list of multiple models).}
#'   \item{\code{model.y}}{"Outcome" (Y) model.}
#'   \item{\code{results}}{Effect estimates and other results (unnamed list object).}
#' }
#'
#' @details
#' For more details and illustrations, see
#' \href{https://github.com/psychbruce/bruceR/tree/master/note}{PROCESS-bruceR-SPSS} (PDF and Markdown files).
#'
#' @seealso
#' \code{\link{lavaan_summary}}
#'
#' \code{\link{model_summary}}
#'
#' \code{\link{med_summary}}
#'
#' @references
#' Hayes, A. F. (2018). \emph{Introduction to mediation, moderation,
#' and conditional process analysis (second edition):
#' A regression-based approach}. Guilford Press.
#'
#' Yzerbyt, V., Muller, D., Batailler, C., & Judd, C. M. (2018).
#' New recommendations for testing indirect effects in mediational models:
#' The need to report and test component paths.
#' \emph{Journal of Personality and Social Psychology, 115}(6), 929--943.
#'
#' @examples
#' \donttest{#### NOTE ####
#' ## In the following examples, I set nsim=100 to save time.
#' ## In formal analyses, nsim=1000 (or larger) is suggested!
#'
#' #### Demo Data ####
#' # ?mediation::student
#' data = mediation::student %>%
#'   dplyr::select(SCH_ID, free, smorale, pared, income,
#'                 gender, work, attachment, fight, late, score)
#' names(data)[2:3] = c("SCH_free", "SCH_morale")
#' names(data)[4:7] = c("parent_edu", "family_inc", "gender", "partjob")
#' data$gender01 = 1 - data$gender  # 0 = female, 1 = male
#' # dichotomous X: as.factor()
#' data$gender = factor(data$gender01, levels=0:1, labels=c("Female", "Male"))
#' # dichotomous Y: as.factor()
#' data$pass = as.factor(ifelse(data$score>=50, 1, 0))
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
#' # (multilevel moderation)
#' PROCESS(data, y="score", x="late", mods="gender",  # continuous Y (LMM)
#'         clusters="SCH_ID")
#' PROCESS(data, y="pass", x="late", mods="gender",   # dichotomous Y (GLMM)
#'         clusters="SCH_ID")
#'
#' # (Johnson-Neyman (J-N) interval and plot)
#' PROCESS(data, y="score", x="gender", mods="late") -> P
#' P$results[[1]]$jn[[1]]       # Johnson-Neyman interval
#' P$results[[1]]$jn[[1]]$plot  # Johnson-Neyman plot (ggplot object)
#' GLM_summary(P$model.y)       # detailed results of regression
#'
#' # (allows multicategorical moderator)
#' d = airquality
#' d$Month = as.factor(d$Month)  # moderator: factor with levels "5"~"9"
#' PROCESS(d, y="Temp", x="Solar.R", mods="Month")
#'
#' ## Model 2 ##
#' PROCESS(data, y="score", x="late",
#'         mods=c("gender", "family_inc"),
#'         mod.type="2-way")  # or omit "mod.type", default is "2-way"
#'
#' ## Model 3 ##
#' PROCESS(data, y="score", x="late",
#'         mods=c("gender", "family_inc"),
#'         mod.type="3-way")
#' PROCESS(data, y="pass", x="gender",
#'         mods=c("late", "family_inc"),
#'         mod1.val=c(1, 3, 5),     # moderator 1: late
#'         mod2.val=seq(1, 15, 2),  # moderator 2: family_inc
#'         mod.type="3-way")
#'
#' ## Model 4 ##
#' PROCESS(data, y="score", x="parent_edu",
#'         meds="family_inc", covs="gender",
#'         ci="boot", nsim=100, seed=1)
#'
#' # (allows an infinite number of multiple mediators in parallel)
#' PROCESS(data, y="score", x="parent_edu",
#'         meds=c("family_inc", "late"),
#'         covs=c("gender", "partjob"),
#'         ci="boot", nsim=100, seed=1)
#'
#' # (multilevel mediation)
#' PROCESS(data, y="score", x="SCH_free",
#'         meds="late", clusters="SCH_ID",
#'         ci="mcmc", nsim=100, seed=1)
#'
#' ## Model 6 ##
#' PROCESS(data, y="score", x="parent_edu",
#'         meds=c("family_inc", "late"),
#'         covs=c("gender", "partjob"),
#'         med.type="serial",
#'         ci="boot", nsim=100, seed=1)
#'
#' ## Model 8 ##
#' PROCESS(data, y="score", x="fight",
#'         meds="late",
#'         mods="gender",
#'         mod.path=c("x-m", "x-y"),
#'         ci="boot", nsim=100, seed=1)
#'
#' ## For more examples and details, see the "note" subfolder at:
#' ## https://github.com/psychbruce/bruceR/tree/main/note
#' }
#' @export
PROCESS = function(data,
                   y="",
                   x="",
                   meds=c(),
                   mods=c(),
                   covs=c(),
                   clusters=c(),
                   hlm.re.m="",
                   hlm.re.y="",
                   hlm.type=c("1-1-1", "2-1-1", "2-2-1"),
                   med.type=c("parallel", "serial"),  # "p"*, "s"
                   mod.type=c("2-way", "3-way"),  # "2"*, "3"
                   mod.path=c("x-y", "x-m", "m-y", "all"),
                   cov.path=c("y", "m", "both"),
                   mod1.val=NULL,
                   mod2.val=NULL,
                   ci=c("boot", "bc.boot", "bca.boot", "mcmc"),
                   nsim=100,
                   seed=NULL,
                   center=TRUE,
                   std=FALSE,
                   digits=3,
                   nsmall=digits,
                   file=NULL) {
  ## Default Setting
  warning.y.class = "\"y\" should be a numeric variable or a factor variable with only 2 levels."
  warning.x.class = "\"x\" should be a numeric variable or a factor variable with only 2 levels."
  warning.m.class = "\"meds\" should be numeric variable(s) or factor variable(s) with only 2 levels."
  warning.mod.path = "Please also specify \"mod.path\":\n    \"all\" or any combination of c(\"x-y\", \"x-m\", \"m-y\")"
  if(x=="" | y=="") stop("Please specify both \"x\" and \"y\".", call.=TRUE)
  if(length(meds)>0 & length(mods)>0 & length(mod.path)>3)
    stop(warning.mod.path, call.=TRUE)
  if("all" %in% mod.path)
    mod.path = c("x-y", "x-m", "m-y")
  if("both" %in% cov.path)
    cov.path = c("y", "m")
  if(length(mods)>0) mod1 = mods[1] else mod1 = NULL
  if(length(mods)>1) mod2 = mods[2] else mod2 = NULL
  if(length(mods)>2) stop("The number of moderators (\"mods\") should be no more than 2.", call.=TRUE)
  if(length(med.type)>1) med.type = "parallel"  # default
  if(length(mod.type)>1) mod.type = "2-way"     # default
  if(grepl("p", med.type)) med.type = "parallel"
  if(grepl("s", med.type)) med.type = "serial"
  if(grepl("2", mod.type)) mod.type = "2-way"
  if(grepl("3", mod.type)) mod.type = "3-way"
  if(grepl("p|s", med.type)==FALSE)
    stop("\"med.type\" should be \"parallel\" or \"serial\".", call.=TRUE)
  if(grepl("2|3", mod.type)==FALSE)
    stop("\"mod.type\" should be \"2-way\" or \"3-way\".", call.=TRUE)
  if(length(meds)>0) {
    if(length(mods)>0 & "m-y" %in% mod.path) {
      if(mod.type=="2-way")
        meds.all = " + " %^% paste(rep(meds, each=length(mods)) %^% "*" %^% mods, collapse=" + ")
      if(mod.type=="3-way")
        meds.all = " + " %^% paste(meds %^% "*" %^% paste(mods, collapse="*"), collapse=" + ")
    } else {
      meds.all = " + " %^% paste(meds, collapse=" + ")
    }
  }
  if(length(covs)>0)
    covs.all = " " %^% paste(covs, collapse=" + ") %^% " +"
  else
    covs.all = ""
  if(length(ci)>1) ci = "boot"  # default: percentile bootstrap
  if(grepl("p", ci) | ci=="boot") ci = "boot"
  if(ci %in% c("bc", "bc.boot")) ci = "bc.boot"
  if(grepl("bca", ci)) ci = "bca.boot"
  if(grepl("m", ci)) ci = "mcmc"
  if(ci %notin% c("boot", "bc.boot", "bca.boot", "mcmc"))
    stop("Please choose \"boot\", \"bc.boot\", \"bca.boot\", or \"mcmc\" for ci.", call.=TRUE)
  nsim.type = ifelse(grepl("boot", ci), "Bootstrap", "Monte Carlo")
  if(length(clusters)>0) HLM = TRUE else HLM = FALSE
  if(length(hlm.type)>1) hlm.type = "1-1-1"  # default; same as "2-1-1"
  if(hlm.type %notin% c("1-1-1", "2-1-1", "2-2-1"))
    stop("\"hlm.type\" should be \"1-1-1\", \"2-1-1\", or \"2-2-1\".", call.=TRUE)
  if(HLM) {
    if(hlm.re.m=="")  # default: random intercept
      hlm.re.m = paste("(1 | " %^% clusters %^% ")", collapse=" + ")
    hlm.re.m = " + " %^% hlm.re.m
    if(hlm.re.y=="")  # default: random intercept
      hlm.re.y = paste("(1 | " %^% clusters %^% ")", collapse=" + ")
    hlm.re.y = " + " %^% hlm.re.y
    if(length(meds)>0 & ci!="mcmc")
      message("\nNOTE: \nci has been reset to \"mcmc\" because bootstrap method is not applicable to multilevel models.")
    ci = "mcmc"
  }

  ## Data Centering and Recoding
  data = as.data.frame(data)
  data.v = na.omit(data[c(y, x, meds, mods, covs, clusters)])
  if(inherits(data.v[[y]], c("factor", "character", "logical"))) {
    if(length(unique(data.v[[y]]))==2) {
      data.v[[y]] = as.numeric(as.factor(data.v[[y]])) - 1  # 0, 1
      Y01 = TRUE
    } else {
      stop(warning.y.class, call.=TRUE)
    }
  } else {
    Y01 = FALSE
  }
  M01 = c()
  for(med in meds) {
    if(inherits(data.v[[med]], c("factor", "character", "logical"))) {
      if(length(unique(data.v[[med]]))==2) {
        data.v[[med]] = as.numeric(as.factor(data.v[[med]])) - 1  # 0, 1
        M01 = c(M01, TRUE)
      } else {
        stop(warning.m.class, call.=TRUE)
      }
    } else {
      M01=c(M01, FALSE)
    }
  }
  if(inherits(data.v[[x]], c("factor", "character", "logical"))) {
    if(length(unique(data.v[[x]]))==2) {
      x.levels = levels(as.factor(data.v[[x]]))
      data.v[[x]] = as.numeric(as.factor(data.v[[x]])) - 1  # 0, 1
      x.trans.info = " (recoded: " %^% paste(x.levels, 0:1, sep="=", collapse=", ") %^% ")"
    } else {
      stop(warning.x.class, call.=TRUE)
    }
  } else {
    x.trans.info = ""
  }
  if(HLM & length(meds)>0 & hlm.type=="2-2-1") {
    if(length(clusters)>1) stop("The number of clusters should be 1.", call.=TRUE)
    dt = data.v[c(x, meds, mods, covs, clusters)]
    Run("dt1 = dplyr::summarise(dplyr::group_by(dt, {clusters}), dplyr::across(where(is.numeric), mean))",
        "dt2 = dplyr::summarise(dplyr::group_by(dt, {clusters}), dplyr::across(where(is.factor), mean))",
        "dt = dplyr::left_join(dt1, dt2, by=\"{clusters}\")")
    data.meds.L2 = as.data.frame(dt)[c(clusters, x, meds, mods, covs)]
    rm(dt)
  }
  if(std) {
    # caution !!!
    if(Y01)
      data.v = data.c = data.c.NOmed =
        grand_mean_center(data.v, vars=c(x, meds, mods, covs), std=TRUE)
    else
      data.v = data.c = data.c.NOmed =
        grand_mean_center(data.v, vars=c(y, x, meds, mods, covs), std=TRUE)
  } else if(center) {
    data.c.NOmed = grand_mean_center(data.v, vars=c(x, mods, covs), std=FALSE)
    data.c = grand_mean_center(data.v, vars=c(x, meds, mods, covs), std=FALSE)
  } else {
    data.c = data.c.NOmed = data.v
  }
  nmis = nrow(data) - nrow(data.v)

  ## File Opening
  # if(!is.null(file)) {
  #   file = str_replace(file, "\\.docx$", ".doc")
  #   FILE = file(file, "a", encoding="UTF-8")
  # }

  ## Formula Building
  ft = Glue("{y} ~ {x}")
  if(length(meds)==0) {
    # (moderated) moderation
    fm = c()
    if(length(mods)==0) {
      stop("Please specify \"meds\" (mediators) and/or \"mods\" (moderators).", call.=TRUE)
    } else if(length(mods)==1) {
      pid = 1
      ptype = "Simple Moderation"
      fy = Glue("{y} ~ {x}*{mod1}")
    } else if(length(mods)==2) {
      if(mod.type=="2-way") {
        pid = 2
        ptype = "Parallel Moderation (2 mods; 2-way)"
        fy = Glue("{y} ~ {x}*{mod1} + {x}*{mod2}")
      }
      if(mod.type=="3-way") {
        pid = 3
        ptype = "Moderated Moderation (2 mods; 3-way)"
        fy = Glue("{y} ~ {x}*{mod1}*{mod2}")
      }
    }
  }
  else if(length(meds)==1 | med.type=="parallel") {
    # single/parallel (moderated) mediation
    if(length(mods)==0) {
      pid = 4
      ptype = ifelse(
        length(meds)==1,
        "Simple Mediation",
        Glue("Parallel Multiple Mediation ({length(meds)} meds)"))
      fm = meds %^% Glue(" ~ {x}")
      fy = Glue("{y} ~ {x}") %^% meds.all
    }
    if(length(mods)==1) {
      ptype = ifelse(
        length(meds)==1,
        "Moderated Mediation",
        Glue("Parallel Multiple Moderated Mediation ({length(meds)} meds)"))
      if("x-y" %in% mod.path) {
        fy = Glue("{y} ~ {x}*{mod1}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid = ifelse("m-y" %in% mod.path, 59, 8)
          fm = meds %^% Glue(" ~ {x}*{mod1}")
        } else {
          pid = ifelse("m-y" %in% mod.path, 15, 5)
          if(pid==5) ptype = Glue("Mediation and Moderation ({length(meds)} meds and 1 mods)")
          fm = meds %^% Glue(" ~ {x}")
        }
      } else {
        fy = Glue("{y} ~ {x}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid = ifelse("m-y" %in% mod.path, 58, 7)
          fm = meds %^% Glue(" ~ {x}*{mod1}")
        } else {
          pid = ifelse("m-y" %in% mod.path, 14, -1)
          fm = meds %^% Glue(" ~ {x}")
        }
      }
    }
    if(length(mods)==2 & mod.type=="2-way") {
      ptype = ifelse(
        length(meds)==1,
        "Moderated Mediation (2 mods; 2-way)",
        Glue("Parallel Multiple Moderated Mediation ({length(meds)} meds and 2 mods; 2-way)"))
      if("x-y" %in% mod.path) {
        fy = Glue("{y} ~ {x}*{mod1} + {x}*{mod2}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid = ifelse("m-y" %in% mod.path, 76, 10)
          fm = meds %^% Glue(" ~ {x}*{mod1} + {x}*{mod2}")
        } else {
          pid = ifelse("m-y" %in% mod.path, 17, 5.2)
          if(pid==5.2) ptype = Glue("Mediation and Parallel Moderation ({length(meds)} meds and 2 mods; 2-way)")
          fm = meds %^% Glue(" ~ {x}")
        }
      } else {
        fy = Glue("{y} ~ {x}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid = ifelse("m-y" %in% mod.path, 75, 9)
          fm = meds %^% Glue(" ~ {x}*{mod1} + {x}*{mod2}")
        } else {
          pid = ifelse("m-y" %in% mod.path, 16, -2)
          fm = meds %^% Glue(" ~ {x}")
        }
      }
    }
    if(length(mods)==2 & mod.type=="3-way") {
      ptype = ifelse(
        length(meds)==1,
        "Moderated Mediation (2 mods; 3-way)",
        Glue("Parallel Multiple Moderated Mediation ({length(meds)} meds and 2 mods; 3-way)"))
      if("x-y" %in% mod.path) {
        fy = Glue("{y} ~ {x}*{mod1}*{mod2}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid = ifelse("m-y" %in% mod.path, 73, 12)
          fm = meds %^% Glue(" ~ {x}*{mod1}*{mod2}")
        } else {
          pid = ifelse("m-y" %in% mod.path, 19, 5.3)
          if(pid==5.3) ptype = Glue("Mediation and Moderated Moderation ({length(meds)} meds and 2 mods; 3-way)")
          fm = meds %^% Glue(" ~ {x}")
        }
      } else {
        fy = Glue("{y} ~ {x}") %^% meds.all
        if("x-m" %in% mod.path) {
          pid = ifelse("m-y" %in% mod.path, 72, 11)
          fm = meds %^% Glue(" ~ {x}*{mod1}*{mod2}")
        } else {
          pid = ifelse("m-y" %in% mod.path, 18, -3)
          fm = meds %^% Glue(" ~ {x}")
        }
      }
    }
    if(pid<0)
      stop(warning.mod.path, call.=TRUE)
    if(pid %in% 7)
      fy = Glue("{y} ~ {x} + {mod1}") %^% meds.all
    if(pid %in% c(9, 11))
      fy = Glue("{y} ~ {x} + {mod1} + {mod2}") %^% meds.all
    if(pid %in% 14:15)
      fm = meds %^% Glue(" ~ {x} + {mod1}")
    if(pid %in% 16:19)
      fm = meds %^% Glue(" ~ {x} + {mod1} + {mod2}")
  }
  else if(med.type=="serial") {
    # serial mediation
    if(length(mods)==0) {
      if(length(meds)>4)
        stop("PROCESS() does not support serial mediation with more than 4 mediators.", call.=TRUE)
      if(any(M01))
        stop("PROCESS() does not support serial mediation with dichotomous mediators.", call.=TRUE)
      pid = 6
      ptype = Glue("Serial Multiple Mediation ({length(meds)} meds)")
      fy = Glue("{y} ~ {x}") %^% meds.all
      fm = meds %^% Glue(" ~ {x}")
      for(mi in 2:length(meds)) {
        fm[mi] = fm[mi] %^% " + " %^% paste(meds[1:(mi-1)], collapse=" + ")
      }
    } else {
      stop("PROCESS() does not support serial mediation with moderators.\nPlease remove the \"mods\" argument from your code.", call.=TRUE)
    }
  }
  if("m" %in% cov.path)
    fm = str_replace(fm, "~", "~" %^% covs.all)
  if("y" %in% cov.path) {
    fy = str_replace(fy, "~", "~" %^% covs.all)
    ft = str_replace(ft, "~", "~" %^% covs.all)  # y ~ [covs] + x
  }
  if(HLM) {
    if(hlm.type!="2-2-1")
      fm = fm %^% hlm.re.m
    fy = fy %^% hlm.re.y
    ft = ft %^% hlm.re.y
  }

  ## Regression Model Summary
  varlist = function(vars=c()) {
    vars.text = paste(vars, collapse=', ')
    if(vars.text=="") vars.text = "-"
    return(vars.text)
  }
  meds.text = varlist(meds)
  mods.text = varlist(mods)
  covs.text = varlist(covs)
  clusters.text = varlist(clusters)
  Print("
  \n
  <<bold ****************** PART 1. Regression Model Summary ******************>>

  <<blue PROCESS Model Code : {pid}>> <<white (Hayes, 2018; <<underline www.guilford.com/p/hayes3>>)>>
  <<blue PROCESS Model Type : {ptype}>>
  <<green
  -    Outcome (Y) : {y}
  -  Predictor (X) : {x}{x.trans.info}
  -  Mediators (M) : {meds.text}
  - Moderators (W) : {mods.text}
  - Covariates (C) : {covs.text}
  -   HLM Clusters : {clusters.text}
  >>
  \n
  ")
  if(center | std) {
    Print("<<yellow
    All numeric predictors have been {ifelse(std, 'standardized', 'grand-mean centered')}.
    (For details, please see the help page of PROCESS.)
    >>
    \n
    ")
  }

  if(length(meds)>0) {
    Print("<<italic Formula of Mediator>>:")
    cat("-   ", paste(fm, collapse="\n-    "))
    cat("\n")
  }
  Print("<<italic Formula of Outcome>>:")
  cat("-   ", fy)
  cat("\n
CAUTION:
  Fixed effect (coef.) of a predictor involved in an interaction
  denotes its \"simple effect/slope\" at the other predictor = 0.
  Only when all predictors in an interaction are mean-centered
  can the fixed effect denote the \"main effect\"!
  ")

  ## Regression Model Building
  if(Y01==FALSE) {
    FUN.y = ifelse(HLM, "lmerTest::lmer", "lm")
    FML.y = ""
  } else {
    FUN.y = ifelse(HLM, "lme4::glmer", "glm")
    FML.y = ", family=binomial"
  }
  model.t = model.y = NULL
  Run("model.y0 = {FUN.y}({fy}, data=data.v{FML.y})")
  Run("model.y = {FUN.y}({fy}, data=data.c{FML.y})")
  Run("model.t = {FUN.y}({ft}, data=data.c{FML.y})")
  model.m = list()
  model.m0 = list()
  if(pid>=4) {
    data.v.temp = data.v
    data.c.temp = data.c
    data.c = data.c.NOmed
    if(HLM & hlm.type=="2-2-1") {
      data.v = data.meds.L2
      if(center) {
        data.c = data.c.NOmed =
          grand_mean_center(data.v, vars=c(x, mods, covs))
      } else {
        data.c = data.c.NOmed = data.v
      }
    }
    for(i in 1:length(fm)) {
      if(M01[i]==FALSE) {
        FUN.m = ifelse(HLM & hlm.type!="2-2-1", "lmerTest::lmer", "lm")
        FML.m = ""
      } else {
        FUN.m = ifelse(HLM & hlm.type!="2-2-1", "lme4::glmer", "glm")
        FML.m = ", family=binomial"
      }
      Run("model.m0.{i} = {FUN.m}({fm[i]}, data=data.v{FML.m})")
      Run("model.m.{i} = {FUN.m}({fm[i]}, data=data.c{FML.m})")
      Run("model.m0 = c(model.m0, list(model.m0.{i}=model.m0.{i}))")
      Run("model.m = c(model.m, list(model.m.{i}=model.m.{i}))")
    }
    data.v = data.v.temp
    data.c = data.c.temp
    rm(data.v.temp, data.c.temp)
  }
  model_summary(c(list(model.t), model.m, list(model.y)),
                nsmall=nsmall, std=std, file=file)
  file = NULL

  ## PROCESS Model Summary
  if(pid %in% 1:3)
    pkg = Glue("\u2018interactions\u2019 (v{packageVersion('interactions')})")
  else if(pid==4)
    pkg = Glue("\u2018mediation\u2019 (v{packageVersion('mediation')})")
  else if(pid==6)
    pkg = Glue("\u2018lavaan\u2019 (v{packageVersion('lavaan')})")
  else
    pkg = Glue("\u2018mediation\u2019 (v{packageVersion('mediation')}), \u2018interactions\u2019 (v{packageVersion('interactions')})")
  Print("
  <<bold ************ PART 2. Mediation/Moderation Effect Estimate ************>>

  <<magenta
  Package Use : {pkg}
  Effect Type : {ptype} (Model {pid})
  Sample Size : {nrow(data.v)}{ifelse(nmis>0, Glue(' ({nmis} missing observations deleted)'), '')}
  Random Seed : {ifelse(length(meds)>0, 'set.seed('%^%seed%^%')', '-')}
  Simulations : {ifelse(length(meds)>0, nsim %^% ' (' %^% nsim.type %^% ')', '-')}
  >>")
  if(length(meds)>0 & nsim<1000)
    message("\nWarning: nsim=1000 (or larger) is suggested!")
  cat("\n")

  ## PROCESS Model Building
  if(HLM & hlm.type=="2-2-1")
    stop("As limited by the \"mediation\" package, the estimate of \"2-2-1\" multilevel mediation is not supported currently.", call.=TRUE)
  RES = list()
  run.process.mod.xy = function(eff.tag="") {
    text = Glue("
    res = process_mod(model.y0, model.y,
                      data.c, x, y, mod1, mod2,
                      mod1.val, mod2.val,
                      mod.type,
                      x.label=\"X\",
                      y.label=\"Y\",
                      eff.tag=\"{eff.tag}\",
                      nsmall, file=file)
    RES = c(RES, list(res))")
  }
  run.process.mod.xm = function(i, eff.tag="") {
    text = Glue("
    res = process_mod(model.m0[[i]], model.m[[i]],
                      data.c.NOmed, x, meds[i], mod1, mod2,
                      mod1.val, mod2.val,
                      mod.type,
                      x.label=\"X\",
                      y.label=\"M\",
                     eff.tag=\"{eff.tag}\",
                      nsmall, file=file)
    RES = c(RES, list(res))")
  }
  run.process.mod.my = function(i, eff.tag="") {
    text = Glue("
    res = process_mod(model.y0, model.y,
                      data.c, meds[i], y, mod1, mod2,
                      mod1.val, mod2.val,
                      mod.type,
                      x.label=\"M\",
                      y.label=\"Y\",
                      eff.tag=\"{eff.tag}\",
                      nsmall, file=file)
    RES = c(RES, list(res))")
  }
  run.process.med = function(eff.tag="") {
    text = Glue("
    res = process_med(model.m0[[i]], model.y0,
                      x, y, meds[i],
                      conditional, simple.slopes,
                      ci, nsim, seed,
                      direct=ifelse(length(mods)==0, TRUE, FALSE),
                      total=ifelse(length(meds)==1, TRUE, FALSE),
                      eff.tag=\"{eff.tag}\",
                      nsmall, file=file)
    RES = c(RES, list(res))")
  }
  conditional = NULL
  simple.slopes = NULL
  if(pid %in% 1:3) {
    # moderation
    Run(run.process.mod.xy())
  } else if(pid==4) {
    # mediation
    for(i in 1:length(meds)) Run(run.process.med())
    # res = process_lav(data.v, y, x, meds, covs,
    #                   med.type, cov.path,
    #                   ci, nsim, seed,
    #                   nsmall=nsmall,
    #                   file=file)
    # RES = c(RES, list(res))
    # lavaan ERROR: unordered factor(s) detected; make them numeric or ordered: pass gender
  } else if(pid %in% c(5, 5.2, 5.3)) {
    # mediation and moderation
    Run(run.process.mod.xy(eff.tag="(Conditional Direct Effects [c'] of X on Y)"))
    for(i in 1:length(meds)) Run(run.process.med())
  } else if(pid==6) {
    # serial mediation
    res = process_lav(data.v, y, x, meds, covs, clusters,
                      med.type, cov.path,
                      ci, nsim, seed,
                      nsmall=nsmall,
                      file=file)
    RES = c(RES, list(res))
  } else {
    # moderated mediation
    if("x-y" %in% mod.path) {
      Run(run.process.mod.xy(eff.tag="(Conditional Direct Effects [c'] of X on Y)"))
    } else {
      Print("<<cyan <<underline Direct Effect:>> \"{x}\" (X) ==> \"{y}\" (Y)>>")
      de = as.data.frame(coef(summary(model.y)))
      de = de[which(row.names(de)==x),]
      de$df = NULL
      conf.int = confint(model.y)
      conf.int = conf.int[which(row.names(conf.int)==row.names(de)),]
      de$CI = paste0("[", paste(formatF(conf.int, nsmall), collapse=", "), "]")
      names(de)[1] = "Effect"
      names(de)[5] = "[95% CI]"
      row.names(de)[1] = "Direct (c')"
      print_table(de, nsmalls=nsmall, file=file)
      cat("\n")
    }
    for(i in 1:length(meds)) {
      if("x-m" %in% mod.path)
        Run(run.process.mod.xm(i, eff.tag="(Conditional Effects [a] of X on M)"))
      if("m-y" %in% mod.path)
        Run(run.process.mod.my(i, eff.tag="(Conditional Effects [b] of M on Y)"))
      conditional = res$conditional
      simple.slopes = res$simple.slopes
      Run(run.process.med(eff.tag="(Conditional Indirect Effects [ab] of X through M on Y)"))
    }
  }

  if(length(meds)>0)
    Print("
    <<italic Note>>. The results based on bootstrapping or other random processes
    are <<italic unlikely>> identical to other statistical software (e.g., SPSS).
    To make results reproducible, you need to set a <<bold seed>> (any number).
    Please see the help page for details: <<bold help(PROCESS)>>
    Ignore this note if you have already set a seed. :)
    \n
    ")

  invisible(list(
    process.id=pid,
    process.type=ptype,
    model.m=model.m,
    model.y=model.y,
    results=RES
  ))
}


#' Tidy report of lavaan model.
#'
#' @param lavaan Model object fitted by \code{\link[lavaan:lavaan-class]{lavaan}}.
#' @param ci Method for estimating standard error (SE) and
#' 95\% confidence interval (CI).
#'
#' Default is \code{"raw"} (the standard approach of \code{lavaan}).
#' Other options:
#' \describe{
#'   \item{\code{"boot"}}{Percentile Bootstrap}
#'   \item{\code{"bc.boot"}}{Bias-Corrected Percentile Bootstrap}
#'   \item{\code{"bca.boot"}}{Bias-Corrected and Accelerated (BCa) Percentile Bootstrap}
#' }
#' @param nsim Number of simulation samples (bootstrap resampling)
#' for estimating SE and 95\% CI.
#' In formal analyses, \strong{\code{nsim=1000} (or larger)} is strongly suggested.
#' @param seed Random seed for obtaining reproducible results. Default is \code{NULL}.
#' @param digits,nsmall Number of decimal places of output. Default is \code{3}.
#' @param print Print results. Default is \code{TRUE}.
#' @param covariance Print (co)variances. Default is \code{FALSE}.
#' @param file File name of MS Word (\code{.doc}).
#'
#' @return
#' Invisibly return a list of results:
#' \describe{
#'   \item{\code{fit}}{Model fit indices.}
#'   \item{\code{measure}}{Latent variable measures.}
#'   \item{\code{regression}}{Regression paths.}
#'   \item{\code{covariance}}{Variances and/or covariances.}
#'   \item{\code{effect}}{Defined effect estimates.}
#' }
#'
#' @seealso
#' \code{\link{PROCESS}}, \code{\link{CFA}}
#'
#' @examples
#' ## Simple Mediation:
#' ## Solar.R (X) => Ozone (M) => Temp (Y)
#'
#' # PROCESS(airquality, y="Temp", x="Solar.R",
#' #         meds="Ozone", ci="boot", nsim=1000, seed=1)
#'
#' model = "
#' Ozone ~ a*Solar.R
#' Temp ~ c.*Solar.R + b*Ozone
#' Indirect := a*b
#' Direct := c.
#' Total := c. + a*b
#' "
#' lv = lavaan::sem(model=model, data=airquality)
#' lavaan::summary(lv, fit.measure=TRUE, ci=TRUE, nd=3)  # raw output
#' lavaan_summary(lv)
#' # lavaan_summary(lv, ci="boot", nsim=1000, seed=1)
#'
#'
#' ## Serial Multiple Mediation:
#' ## Solar.R (X) => Ozone (M1) => Wind(M2) => Temp (Y)
#'
#' # PROCESS(airquality, y="Temp", x="Solar.R",
#' #         meds=c("Ozone", "Wind"),
#' #         med.type="serial", ci="boot", nsim=1000, seed=1)
#'
#' model0 = "
#' Ozone ~ a1*Solar.R
#' Wind ~ a2*Solar.R + d12*Ozone
#' Temp ~ c.*Solar.R + b1*Ozone + b2*Wind
#' Indirect_All := a1*b1 + a2*b2 + a1*d12*b2
#' Ind_X_M1_Y := a1*b1
#' Ind_X_M2_Y := a2*b2
#' Ind_X_M1_M2_Y := a1*d12*b2
#' Direct := c.
#' Total := c. + a1*b1 + a2*b2 + a1*d12*b2
#' "
#' lv0 = lavaan::sem(model=model0, data=airquality)
#' lavaan::summary(lv0, fit.measure=TRUE, ci=TRUE, nd=3)  # raw output
#' lavaan_summary(lv0)
#' # lavaan_summary(lv0, ci="boot", nsim=1000, seed=1)
#'
#' model1 = "
#' Ozone ~ a1*Solar.R
#' Wind ~ d12*Ozone
#' Temp ~ c.*Solar.R + b1*Ozone + b2*Wind
#' Indirect_All := a1*b1 + a1*d12*b2
#' Ind_X_M1_Y := a1*b1
#' Ind_X_M1_M2_Y := a1*d12*b2
#' Direct := c.
#' Total := c. + a1*b1 + a1*d12*b2
#' "
#' lv1 = lavaan::sem(model=model1, data=airquality)
#' lavaan::summary(lv1, fit.measure=TRUE, ci=TRUE, nd=3)  # raw output
#' lavaan_summary(lv1)
#' # lavaan_summary(lv1, ci="boot", nsim=1000, seed=1)
#'
#' @export
lavaan_summary = function(lavaan,
                          ci=c("raw", "boot", "bc.boot", "bca.boot"),
                          nsim=100,
                          seed=NULL,
                          digits=3, nsmall=digits,
                          print=TRUE,
                          covariance=FALSE,
                          file=NULL) {
  FIT = lavaan::fitMeasures(lavaan)

  try({
    pe.error = TRUE
    pe = lavaan::parameterEstimates(lavaan, standardized=TRUE)
    pe.error = FALSE
  }, silent=TRUE)
  if(pe.error) {
    pe = lavaan::parameterEstimates(lavaan, standardized=FALSE)
    pe$std.all = NA
  }
  if("label" %notin% names(pe)) pe$label = ""

  if(length(ci)>1) ci = "raw"
  CI = switch(
    ci,
    "raw"="Raw (Standard)",
    "boot"="Percentile Bootstrap",
    "bc.boot"="Bias-Corrected Percentile Bootstrap",
    "bca.boot"="Bias-Corrected and Accelerated (BCa) Percentile Bootstrap")
  if(ci!="raw") {
    set.seed(seed)
    lv.boot = lavaan::bootstrapLavaan(
      lavaan, type="nonparametric",
      FUN=function(...) { lavaan::coef(..., type="user") },
      R=nsim)
    lv.boot = as.data.frame(lv.boot)
    pe$se = apply(lv.boot, 2, sd)
    bootci = apply(lv.boot, 2, boot_ci, type=ci)
    pe$ci.lower = bootci[1,]
    pe$ci.upper = bootci[2,]
  }

  extract_lavaan_table = function(lav.df, op, ci.raw) {
    RES = data.frame(Estimate=lav.df$est,
                     S.E.=lav.df$se)
    RES$z = RES$Estimate / RES$S.E.
    RES$pval = p.z(RES$z)
    if(ci.raw) {
      RES$LLCI = lav.df$ci.lower
      RES$ULCI = lav.df$ci.upper
    } else {
      RES$BootLLCI = lav.df$ci.lower
      RES$BootULCI = lav.df$ci.upper
    }
    RES$Beta = lav.df$std.all
    if(nrow(RES)>0) {
      if(op==":=") {
        row.names(RES) = paste(" ", lav.df$label)
      } else {
        row.names(RES) = paste(
          " ", lav.df$lhs, op, lav.df$rhs,
          str_replace_all("(" %^% lav.df$label %^% ")", "\\(\\)", ""))
      }
    }
    return(RES)
  }

  MES = extract_lavaan_table(pe[pe$op=="=~",], "=~", ci=="raw")
  REG = extract_lavaan_table(pe[pe$op=="~",], "<-", ci=="raw")
  COV = extract_lavaan_table(pe[pe$op=="~~",], "~~", ci=="raw")
  EFF = extract_lavaan_table(pe[pe$op==":=",], ":=", ci=="raw")

  ALL = data.frame()
  if(ci=="raw")
    NUL = data.frame(Estimate=NA, S.E.=NA, z=NA, pval=NA, LLCI=NA, ULCI=NA, Beta=NA)
  else
    NUL = data.frame(Estimate=NA, S.E.=NA, z=NA, pval=NA, BootLLCI=NA, BootULCI=NA, Beta=NA)
  if(nrow(MES)>0) {
    row.names(NUL) = "Latent Variables:"
    ALL = rbind(ALL, NUL, MES)
  }
  if(nrow(REG)>0) {
    row.names(NUL) = "Regression Paths:"
    ALL = rbind(ALL, NUL, REG)
  }
  if(nrow(COV)>0 & covariance) {
    row.names(NUL) = "(Co)variances:"
    ALL = rbind(ALL, NUL, COV)
  }
  if(nrow(EFF)>0) {
    row.names(NUL) = "Defined Effects:"
    ALL = rbind(ALL, NUL, EFF)
  }

  if(print) {
    cat("\n")
    Print("
    <<cyan Fit Measures (lavaan):>>
    {p(chi2=FIT['chisq'], df=FIT['df'], n=FIT['ntotal'], nsmall=nsmall)}
    \u03c7\u00b2/<<italic df>> = {FIT['chisq']/FIT['df']:.{nsmall}}{ifelse(FIT['df']==0, ' <<red (saturated model)>>', '')}
    AIC = {FIT['aic']:.{nsmall}} <<white (Akaike Information Criterion)>>
    BIC = {FIT['bic']:.{nsmall}} <<white (Bayesian Information Criterion)>>
    CFI = {FIT['cfi']:.{nsmall}} <<white (Comparative Fit Index)>>
    TLI = {FIT['tli']:.{nsmall}} <<white (Tucker-Lewis Index; Non-Normed Fit Index, NNFI)>>
    NFI = {FIT['nfi']:.{nsmall}} <<white (Normed Fit Index)>>
    IFI = {FIT['ifi']:.{nsmall}} <<white (Incremental Fit Index)>>
    GFI = {FIT['gfi']:.{nsmall}} <<white (Goodness-of-Fit Index)>>
    AGFI = {FIT['agfi']:.{nsmall}} <<white (Adjusted Goodness-of-Fit Index)>>
    RMSEA = {FIT['rmsea']:.{nsmall}}, 90% CI [{FIT['rmsea.ci.lower']:.{nsmall}}, {FIT['rmsea.ci.upper']:.{nsmall}}] <<white (Root Mean Square Error of Approximation)>>
    SRMR = {FIT['srmr']:.{nsmall}} <<white (Standardized Root Mean Square Residual)>>
    ")
    cat("\n")
    print_table(ALL, row.names=TRUE, nsmalls=nsmall,
                title="<<cyan Model Estimates (lavaan):>>",
                note=Glue("<<italic Note>>. {CI} Confidence Interval (CI) and SE."))
    cat("\n")
  }

  if(!is.null(file)) {
    print_table(
      ALL, nsmalls=nsmall, row.names=TRUE,
      title="<b>Table. Model Estimates.</b>",
      note="<i>Note</i>. * <i>p</i> < .05. ** <i>p</i> < .01. *** <i>p</i> < .001.",
      file=file,
      file.align.text=c(
        "left", "right", "right", "right", "right", "left", "right", "right", "right"
      ))
  }

  invisible(list(fit=FIT,
                 measure=MES,
                 regression=REG,
                 covariance=COV,
                 effect=EFF))
}


## Model 4 and 6
process_lav = function(data, y, x, meds, covs, clusters,
                       med.type, cov.path,
                       ci, nsim, seed,
                       nsmall=3,
                       file=NULL,
                       print=TRUE) {
  if(length(clusters)>=1)
    stop("Multilevel serial mediation is not supported currently.", call.=TRUE)

  if(length(clusters)==0)
    clusters = NULL
  if(length(clusters)>1)
    stop("As limited by the \"lavaan\" package, only one cluster is allowed.", call.=TRUE)

  CI = switch(
    ci,
    "mcmc"="<<red Reset to:>> Percentile Bootstrap",
    "boot"="Percentile Bootstrap",
    "bc.boot"="Bias-Corrected Percentile Bootstrap",
    "bca.boot"="Bias-Corrected and Accelerated (BCa) Percentile Bootstrap")

  # ?lavaan::`lavaan-class`

  model = lav_med_modeler(y, x, meds, covs, med.type, cov.path)
  # cat(model)

  lv = lavaan::sem(model=model, data=data, cluster=clusters)
  # lavaan::summary(lv, header=FALSE,
  #                 fit.measure=TRUE,
  #                 ci=TRUE,
  #                 standardized=FALSE,
  #                 rsquare=FALSE, nd=3)
  # lvb = lavaan::sem(model=model, data=data, se="bootstrap", bootstrap=100)
  # lavaan::fitMeasures(lv)

  cat(crayon::white("Running", nsim, "simulations (lavaan model)...\n"))

  MED = lavaan_summary(lv, ci=ci, nsim=nsim, seed=seed, print=FALSE)$effect

  # cat("\015")
  # cat(rep_char(" ", 30))
  # cat("\015")

  if(print) {
    MED.print = MED
    MED.print$Beta = NULL
    MED.print$CI = cc_ci(MED[["BootLLCI"]], MED[["BootULCI"]], nsmall)
    names(MED.print)[length(MED.print)] = "[Boot 95% CI]"
    MED.print$Beta = MED$Beta
    print_table(
      dplyr::select(MED.print, !c("BootLLCI", "BootULCI")),
      row.names=TRUE,
      nsmalls=nsmall,
      line=is.null(file),
      file=file,
      title=Glue("
      <<blue <<underline LAVAAN Syntax:>>>>\n<<italic {model}>>"),
      note=Glue("
      {CI} Confidence Interval
      <<white (SE and CI are estimated based on {nsim} Bootstrap samples.)>>"))
    cat("\n")
  }

  return(list(lavaan.syntax=model,
              lavaan.mediation=MED))
}


extract_med = function(med, ci, nsmall=3, direct=TRUE, total=TRUE) {
  boot = ifelse(grepl("boot", ci), TRUE, FALSE)
  # MED = rbind(
  #   c(med$d.avg, sd(med$d.avg.sims), med$d.avg.ci, med$d.avg.p),
  #   c(med$z.avg, sd(med$z.avg.sims), med$z.avg.ci, med$z.avg.p),
  #   c(med$tau.coef, sd(med$tau.sims), med$tau.ci, med$tau.p))
  MED = rbind(
    c(med$d.avg, sd(med$d.avg.sims),
      boot_ci(med$d.avg.sims, ci, med$d.avg),
      med$d.avg.p),
    c(med$z.avg, sd(med$z.avg.sims),
      boot_ci(med$z.avg.sims, ci, med$z.avg),
      med$z.avg.p),
    c(med$tau.coef, sd(med$tau.sims),
      boot_ci(med$tau.sims, ci, med$tau.coef),
      med$tau.p))
  MED = as.data.frame(MED)
  row.names(MED) = c("Indirect (ab)", "Direct (c')", "Total (c)")
  names(MED) = c("Effect", "S.E.", "LLCI", "ULCI", "p")
  MED$z = MED$Effect / MED$S.E.
  MED$pval = p.z(MED$z)
  MED$CI = cc_ci(MED[[3]], MED[[4]], nsmall)
  names(MED)[8] = ifelse(boot, "[Boot 95% CI]", "[MCMC 95% CI]")
  if(direct==FALSE) total = FALSE
  if(direct==FALSE) MED = MED[which(row.names(MED)!="Direct (c')"),]
  if(total==FALSE) MED = MED[which(row.names(MED)!="Total (c)"),]
  return(MED)
}


process_med = function(model.m,
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
  if(inherits(model.m, "lmerModLmerTest")) class(model.m) = "lmerMod"
  if(inherits(model.y, "lmerModLmerTest")) class(model.y) = "lmerMod"
  boot = ifelse(grepl("boot", ci), TRUE, FALSE)
  CI = switch(
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
      med = mediation::mediate(
        model.m=model.m,
        model.y=model.y,
        treat=x,
        mediator=medi,
        boot=boot,
        boot.ci.type="perc",  # "bca", "perc"
        sims=nsim)
      MED = extract_med(med, ci, nsmall, direct, total)
    } else {
      # moderated mediation
      COV.list = list()
      MED = data.frame()
      # DRC = data.frame()
      simple.slopes = simple.slopes[1:ncol(conditional)]
      for(i in 1:nrow(conditional)) {
        cond.list = list()
        for(j in 1:ncol(conditional)) {
          a = conditional[i, j]
          names(a) = names(conditional)[j]
          cond.list = c(cond.list, a)
        }
        COV.list = c(COV.list, list(cond.list))
      }
      cat(crayon::white("Running", nsim, "*", length(COV.list), "simulations...\n"))
      for(COV in COV.list) {
        set.seed(seed)
        med = mediation::mediate(
          model.m=model.m,
          model.y=model.y,
          treat=x,
          mediator=medi,
          covariates=COV,
          boot=boot,
          boot.ci.type="perc",  # "bca", "perc"
          sims=nsim)
        MEDi = extract_med(med, ci, nsmall, direct=FALSE, total=FALSE)
        # MEDi = cbind(data.frame(Path=row.names(MEDi)), MEDi)
        # names(MEDi)[1] = " "
        row.names(MEDi) = NULL
        MED = rbind(MED, MEDi)
        # MED = rbind(MED, MEDi[1,])
        # DRC = rbind(DRC, MEDi[2,])
      }
      # MED = cbind(rbind(simple.slopes, simple.slopes),
      #             rbind(MED, DRC))
      MED = cbind(simple.slopes, MED)
    }
  })

  # cat("\015")
  # cat(rep_char(" ", 30))
  # cat("\015")

  if(print) {
    if(eff.tag!="") eff.tag = "\n" %^% eff.tag
    print_table(
      dplyr::select(MED, !c("LLCI", "ULCI", "p")),
      row.names=is.null(conditional),
      nsmalls=nsmall,
      line=is.null(file),
      title=Glue("
      <<blue <<underline Indirect Path:>> \"{x}\" (X) ==> \"{medi}\" (M) ==> \"{y}\" (Y)>>{eff.tag}"),
      note=Glue("
      {CI} Confidence Interval
      <<white ({ifelse(boot, 'SE', 'Effect, SE,')} and CI are estimated based on {nsim} {ifelse(boot, 'Bootstrap samples', 'Monte Carlo samples')}.)>>"))
    cat("\n")
  }

  invisible(list(mediation=MED))
}


process_mod = function(model0,
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
  data.c = data.c
  suppressWarnings({
    simple.slopes = interactions::sim_slopes(
      model=model0,
      pred=!!x,
      modx=!!mod1,
      mod2=!!mod2,
      modx.values=mod1.val,
      mod2.values=mod2.val,
      johnson_neyman=TRUE)
  })
  mod1 = attributes(simple.slopes)[["modx"]]
  mod2 = attributes(simple.slopes)[["mod2"]]
  mod1.vals = attributes(simple.slopes)[["modx.values"]]
  mod2.vals = attributes(simple.slopes)[["mod2.values"]]
  res.sl = simple.slopes[["slopes"]]
  res.jn = simple.slopes[["jn"]]
  if(inherits(res.sl, "data.frame"))
    res.sl = list(res.sl)

  # MOD = coefficients(summary(model))
  # MOD = as.data.frame(MOD)[which(grepl(":", row.names(MOD))),]
  # row.names(MOD) = str_replace_all(row.names(MOD), ":", " * ")
  MOD = interaction_test(model, data=data.c, data.name="data.c")
  if(nrow(MOD)==2)
    if(row.names(MOD)[2]=="(All Interactions)")
      MOD = MOD[1,]

  RES = data.frame()
  RES0 = data.frame()
  for(i in 1:length(res.sl)) {
    res = res.sl[i]
    mod2.val = mod2.vals[i]
    names(mod2.val) = NULL
    if(!inherits(res, "data.frame"))
      res = as.data.frame(res) %>%
        mutate(across(c(-1), as.numeric))
    names(res)[2:3] = c("Effect", "S.E.")
    names(res)[4:5] = c("LLCI", "ULCI")
    names(res)[6] = str_sub(names(res)[6], 1, 1)
    names(res)[7] = "pval"
    if(is.null(mod2)) {
      res0 = res = cbind(data.frame(Mod1=mod1.vals), res[-1])
      if(c("- 1 SD", "Mean", "+ 1 SD") %allin% names(mod1.vals))
        res[[1]] = str_trim(formatF(res[[1]], nsmall)) %^%
          c(" (- SD)", " (Mean)", " (+ SD)")
      names(res0)[1] = mod1
      names(res)[1] = "\"" %^% mod1 %^% "\""
    } else {
      res0 = res = cbind(data.frame(Mod2=mod2.val), res)
      if(c("- 1 SD", "Mean", "+ 1 SD") %allin% names(mod2.vals))
        res[[1]] = str_trim(formatF(res[[1]], nsmall)) %^%
          c(" (- SD)", " (Mean)", " (+ SD)")[i]
      if(c("- 1 SD", "Mean", "+ 1 SD") %allin% names(mod1.vals))
        res[[2]] = str_trim(formatF(res[[2]], nsmall)) %^%
          c(" (- SD)", " (Mean)", " (+ SD)")
      names(res0)[1] = mod2
      names(res0)[2] = mod1
      names(res)[1] = "\"" %^% mod2 %^% "\""
      names(res)[2] = "\"" %^% mod1 %^% "\""
    }
    row.names(res) = row.names(res0) = NULL
    RES = rbind(RES, res)
    RES0 = rbind(RES0, res0)
  }
  RES$`[95% CI]` = cc_ci(RES[["LLCI"]], RES[["ULCI"]], nsmall)
  RES[[1]] = format(str_trim(formatF(RES[[1]], nsmall)),
                    width=nchar(names(RES)[1]))
  names(RES)[1] = format(names(RES)[1], width=max(nchar(RES[[1]])))
  if(!is.null(mod2)) {
    RES[[2]] = format(str_trim(formatF(RES[[2]], nsmall)),
                      width=nchar(names(RES)[2]))
    names(RES)[2] = format(names(RES)[2], width=max(nchar(RES[[2]])))
  }

  if(!is.null(mod2)) {
    term = row.names(MOD)[1]
    MOD.MOD = do.call(rbind, lapply(
      simple.slopes[["mods"]],
      function(model) {
        dt = model.frame(model)
        coef = interaction_test(model, data=dt, data.name="dt")
        coef = coef[term,]
        coef = cbind(data.frame(Interaction=row.names(coef)), coef)
        row.names(coef) = NULL
        return(coef)
      }))
    MOD.MOD = cbind(RES[1], MOD.MOD)
  } else {
    MOD.MOD = NULL
  }

  if(is.null(mod2))
    orders = order(RES0[[1]])
  else
    orders = order(RES0[[1]], RES0[[2]])
  RES0 = RES0[orders,]
  RES = RES[orders,]
  if(!is.null(mod2))
    MOD.MOD = MOD.MOD[orders,][!duplicated(MOD.MOD[[1]]),]

  if(print) {
    if(eff.tag!="") eff.tag="\n" %^% eff.tag
    print_table(
      MOD, row.names=TRUE, nsmalls=c(2, 0, 0, 0),
      line=is.null(file),
      title=Glue("Interaction Effect{ifelse(is.null(mod2), '', 's')} on \"{y}\" ({y.label})"))
    cat("\n")
    if(!is.null(mod2) & mod.type=="3-way") {
      print_table(
        MOD.MOD, row.names=FALSE, nsmalls=c(0, 0, 2, 0, 0, 0),
        line=is.null(file),
        title=Glue("Conditional Interaction Effects on \"{y}\" ({y.label})"))
      cat("\n")
    }
    print_table(
      dplyr::select(RES, !c("LLCI", "ULCI")),
      row.names=FALSE, nsmalls=nsmall,
      line=is.null(file),
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
# print.johnson_neyman = function(x, raw=FALSE, ...) {
#   if(raw) {
#     interactions:::print.johnson_neyman(x, ...)
#   } else {
#     atts = attributes(x)
#     if(atts$inside==FALSE)
#       inout = crayon::inverse("OUTSIDE")
#     else
#       inout = crayon::inverse("INSIDE")
#     b_format = jtools::num_print(x$bounds, atts$digits)
#     m_range = jtools::num_print(atts$modrange, atts$digits)
#     alpha = gsub("0\\.", "\\.", as.character(atts$alpha))
#     pmsg = paste("p <", alpha)
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


#' Tidy report of mediation analysis.
#'
#' @description
#' Tidy report of mediation analysis,
#' which is performed using the \code{\link[mediation]{mediation}} package.
#'
#' @param model Mediation model built using \code{\link[mediation:mediate]{mediation::mediate()}}.
#' @param digits,nsmall Number of decimal places of output. Default is \code{3}.
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
#' \dontrun{
#'
#' library(mediation)
#' # ?mediation::mediate
#'
#' ## Example 1: OLS Regression
#' ## Bias-corrected and accelerated (BCa) bootstrap confidence intervals
#'
#' ## Hypothesis: Solar radiation -> Ozone -> Daily temperature
#' lm.m = lm(Ozone ~ Solar.R + Month + Wind, data=airquality)
#' lm.y = lm(Temp ~ Ozone + Solar.R + Month + Wind, data=airquality)
#' set.seed(123)  # set a random seed for reproduction
#' med = mediate(lm.m, lm.y,
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
#' data = lmerTest::carrots  # long-format data
#' data = na.omit(data)  # omit missing values
#' lmm.m = lme4::lmer(Sweetness ~ Crisp + Gender + Age + (1 | Consumer), data=data)
#' lmm.y = lme4::lmer(Preference ~ Sweetness + Crisp + Gender + Age + (1 | Consumer), data=data)
#' set.seed(123)  # set a random seed for reproduction
#' med.lmm = mediate(lmm.m, lmm.y,
#'                   treat="Crisp", mediator="Sweetness",
#'                   sims=1000)
#' med_summary(med.lmm)
#' }
#'
#' @export
med_summary = function(model, digits=3, nsmall=digits, file=NULL) {
  # for raw function, see:
  # edit(mediation::mediate)
  # edit(mediation:::print.summary.mediate)
  # edit(mediation:::pval)
  x = model
  clp = 100 * x$conf.level
  if(x$boot) {
    ci.type = sprintf("%s Bootstrap Confidence Interval",
                      ifelse(x$boot.ci.type=="perc",
                             "Percentile",
                             "Bias-Corrected and Accelerated (BCa)"))
  } else {
    ci.type = sprintf("%s Confidence Interval",
                      ifelse(inherits(x, "mediate.tsls"),
                             "Two-Stage Least Squares",
                             "Monte Carlo (Quasi-Bayesian)"))
  }

  cat("\n")
  Print("Mediation Analysis:")
  X = x[["treat"]]
  M = x[["mediator"]]
  Y = names(model.frame(x[["model.y"]]))[1]
  Print("{X} ==> {M} ==> {Y}")

  if(!is.null(x$covariates)) {
    cat("\n")
    Print("Conditional on ...")
    conditional = data.frame(Value=unlist(x$covariates))
    conditional$Value = paste("=", conditional$Value)
    print(conditional)
    cat("\n")
  }

  smat = rbind(
    c(x$d.avg, sd(x$d.avg.sims), x$d.avg.ci, x$d.avg.p),
    c(x$z.avg, sd(x$z.avg.sims), x$z.avg.ci, x$z.avg.p),
    c(x$tau.coef, sd(x$tau.sims), x$tau.ci, x$tau.p))
  smat = as.data.frame(smat)
  row.names(smat) = c("Indirect", "Direct", "Total")
  smat$CI = cc_ci(smat[[3]], smat[[4]], nsmall)
  names(smat) = c("Effect",
                  "S.E.",
                  ifelse(x$boot, "Boot LLCI", "LLCI"),
                  ifelse(x$boot, "Boot ULCI", "ULCI"),
                  "pval",
                  ifelse(x$boot, "[Boot 95% CI]", "[MCMC 95% CI]"))
  print_table(smat[c(1, 2, 6, 5)], nsmalls=nsmall)
  Print(ci.type)
  Print("Sample Size: {x$nobs}")
  Print("Simulations: {x$sims} ({ifelse(x$boot, 'Bootstrap', 'Monte Carlo')})")
  cat("\n")

  if(!is.null(file)) {
    smat.new = smat
    smat.new.names = names(smat.new)
    smat.new.names[5] = "<i>p</i>"
    smat.new[[6]] = str_replace_all(smat.new[[6]], "-", "\u2013")
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
#' p1 = ccf_plot(chicken ~ egg, data=lmtest::ChickEgg)
#'
#' # a more colorful plot
#' p2 = ccf_plot(chicken ~ egg, data=lmtest::ChickEgg, alpha.ns=0.3,
#'               pos.color="#CD201F",
#'               neg.color="#21759B",
#'               ci.color="black")
#'
#' @seealso \code{\link{granger_test}}
#'
#' @export
ccf_plot = function(formula, data,
                    lag.max=30, sig.level=0.05,
                    xbreaks=seq(-100, 100, 10),
                    ybreaks=seq(-1, 1, 0.2),
                    ylim=NULL, alpha.ns=1,
                    pos.color="black",
                    neg.color="black",
                    ci.color="blue",
                    title=NULL, subtitle=NULL,
                    xlab="Lag", ylab="Cross-Correlation") {
  lag = acf = direc = sig = NULL
  pos.color1 = see::social_colors("red")
  neg.color1 = see::social_colors("blue grey")

  x = as.character(formula)[3]
  y = as.character(formula)[2]
  data = as.data.frame(data)
  if(is.null(title)) title = Glue("{x} \u2192 {y}")

  cc = stats::ccf(x=data[[x]], y=data[[y]], lag.max=lag.max, plot=FALSE, na.action=na.omit)
  ccdata = with(cc, data.frame(lag, acf))
  n = cc$n.used
  rsig = psych::t2r(qt(sig.level/2, n, lower.tail=F), n-2)
  ccdata$sig = as.factor(ifelse(abs(ccdata$acf)<rsig, 0, 1))
  ccdata$direc = as.factor(ifelse(ccdata$acf<0, 0, 1))

  p = ggplot(ccdata, aes(x=lag, y=acf, color=direc, alpha=sig)) +
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
#' Granger causality test examines whether
#' the lagged values of a predictor
#' have an incremental role in predicting (i.e., help to predict)
#' an outcome when controlling for the lagged values of the outcome.
#'
#' Granger causality does not necessarily constitute a true causal effect.
#'
#' @inheritParams ccf_plot
#' @param lags Time lags. Default is \code{1:5}.
#' @param test.reverse Whether to test reverse causality. Default is \code{TRUE}.
#' @param file File name of MS Word (\code{.doc}).
#'
#' @return A data frame of results.
#'
#' @examples
#' granger_test(chicken ~ egg, data=lmtest::ChickEgg)
#' granger_test(chicken ~ egg, data=lmtest::ChickEgg, lags=1:10, file="Granger.doc")
#' unlink("Granger.doc")  # delete file for code check
#'
#' @seealso
#' \code{\link{ccf_plot}},
#' \code{\link{granger_causality}}
#'
#' @export
granger_test = function(formula, data, lags=1:5,
                        test.reverse=TRUE,
                        file=NULL) {
  installed("lmtest")
  res = data.frame(Lag=lags, D1="", D2="", D12="")
  names(res)[2:4] = c("Hypothesized Direction",
                      "Reverse Direction",
                      "Hypothesized (vs. Reverse)")

  if(test.reverse) {
    formula.rev = as.formula(paste(formula[3], formula[1], formula[2]))
    formulas = list(formula, formula.rev)
  } else {
    formulas = list(formula)
  }

  Print("
  \n
  <<cyan Granger Causality Test (Bivariate)>>

  Hypothesized direction:
  <<blue {formula[2]} ~ {formula[2]}[1:Lags] + <<green {formula[3]}[1:Lags]>>>>
  ")

  for(f in formulas) {
    rev = FALSE
    if(test.reverse & f!=formulas[[1]]) {
      rev = TRUE
      Print("
      \n
      Reverse direction:
      <<blue {formula[3]} ~ {formula[3]}[1:Lags] + <<green {formula[2]}[1:Lags]>>>>
      ")
    }
    for(lag in lags) {
      gt = lmtest::grangertest(formula=f, data=data, order=lag, na.action=na.omit)
      Fval = gt[2, "F"]
      df1 = -gt[2, "Df"]
      df2 = gt[1, "Res.Df"]
      sig = str_trim(sig.trans(p.f(Fval, df1, df2)))
      result = bruceR::p(f=Fval, df1=df1, df2=df2)
      result.simple = formatF(Fval, 2) %^% ifelse(sig=="", "", "<sup>" %^% sig %^% "</sup>")
      Print("Lags = {lag}:\t{result}")
      res[which(res$Lag==lag), ifelse(rev, 3, 2)] = p.plain(f=Fval, df1=df1, df2=df2)
      res[which(res$Lag==lag), 4] = ifelse(
        rev,
        res[[which(res$Lag==lag), 4]] %^% " (vs. " %^% result.simple %^% ")",
        result.simple)
    }
  }

  cat("\n")
  if(!is.null(file)) {
    RES = res
    RES[[2]] = str_replace(str_replace(
      RES[[2]], "p", "<i>p</i>"), "F", "<i>F</i>")
    RES[[3]] = str_replace(str_replace(
      RES[[3]], "p", "<i>p</i>"), "F", "<i>F</i>")
    if(test.reverse==FALSE) RES = RES[1:2]
    print_table(RES, row.names=FALSE, digits=0,
                file.align.head="left",
                file.align.text="left",
                title="<b>Table. Granger Causality Test (Bivariate).</b>",
                note="<i>Note</i>. * <i>p</i> < .05. ** <i>p</i> < .01. *** <i>p</i> < .001.",
                file=file)
  }

  invisible(res[1:3])
}


vargranger = function(varmodel, var.y, var.x) {
  vms = varmodel[["varresult"]]
  vars = names(vms)
  if(length(var.x)==1) {
    if(var.x=="ALL")
      dropped.var = paste(paste0("^", vars[which(vars!=var.y)]), collapse="|")
    else
      dropped.var = var.x
  } else {
    dropped.var = var.x = paste(var.x, collapse="|")
  }
  vm.raw = vms[[var.y]]
  vm = lm(vm.raw[["terms"]], data=vm.raw[["model"]])
  lags = names(vm[["coefficients"]])
  dropped.vars = lags[which(grepl(dropped.var, lags))]
  dropped = paste(dropped.vars, collapse=" - ")
  aov = anova(update(vm, as.formula(paste("~ . -", dropped))), vm)
  df1 = aov[2, "Df"]
  df2 = aov[2, "Res.Df"]
  chi2 = aov[2, "F"] * df1
  p.chisq = p.chi2(chi2, df1)
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
#' Granger causality test (based on VAR model) examines whether
#' the lagged values of a predictor (or predictors)
#' help to predict an outcome when controlling for
#' the lagged values of the outcome itself.
#'
#' Granger causality does not necessarily constitute a true causal effect.
#'
#' @param varmodel VAR model fitted using the \code{\link[vars:VAR]{vars::VAR()}} function.
#' @param var.y,var.x [Optional] Default is \code{NULL} (all variables).
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
#'
#'   # R package "vars" should be installed
#'   library(vars)
#'   data(Canada)
#'   VARselect(Canada)
#'   vm = VAR(Canada, p=3)
#'   model_summary(vm)
#'   granger_causality(vm)
#' }
#'
#' @export
granger_causality = function(varmodel, var.y=NULL, var.x=NULL,
                             test=c("F", "Chisq"),
                             file=NULL,
                             check.dropped=FALSE) {
  vars = names(varmodel[["varresult"]])
  if(is.null(var.y)) var.y = vars
  if(is.null(var.x)) {
    res = data.frame(Equation=rep(var.y, each=length(vars)+1),
                     Excluded=c(vars, "ALL"))
    res = res[which(res$Equation!=res$Excluded), ]
  } else {
    if(length(var.x)==1)
      res = expand.grid(Equation=var.y,
                        Excluded=var.x)
    else
      res = expand.grid(
        Equation = var.y,
        Excluded = unique(c(
          var.x,
          paste(
            str_subset(var.x, "\\|", negate=TRUE),
            collapse="|")
        )))
    res$Equation = as.character(res$Equation)
    res$Excluded = as.character(res$Excluded)
    res = res[which(res$Excluded!=""), ]
  }

  res = do.call("rbind", lapply(1:nrow(res), function(i) {
    vargranger(varmodel, res[i, "Equation"], res[i, "Excluded"])
  }))

  res$Causality = res$Equation %^% " <= " %^% res$Excluded
  nchars = max(nchar(res$Causality))
  res.check = rbind(data.frame(Dropped="Dropped variables (lags)"),
                    res["Dropped"])
  res.check$Dropped = paste(" ", format(res.check$Dropped))
  names(res.check) = " "
  row.names(res.check) = c(" ", res$Causality)
  if(check.dropped) print(res.check)
  res$Dropped = NULL

  result = data.frame()
  for(var in var.y)
    result = rbind(result,
                   data.frame(
                     Equation=NA, Excluded=NA,
                     `F`=NA, df1=NA, df2=NA, p.F=NA, sig.F="",
                     `Chisq`=NA, df=NA, p.Chisq=NA, sig.Chisq="",
                     Causality=rep_char("-", nchars)),
                   res[which(res$Equation==var),])
  result$`F` = formatF(result$`F`, 2)
  result$`Chisq` = formatF(result$`Chisq`, 2)
  result$p.F = p.trans(result$p.F)
  result$p.Chisq = p.trans(result$p.Chisq)
  result$` ` = "  "
  names(result)[6] = "p"
  names(result)[7] = "  "
  names(result)[10] = " p"
  names(result)[11] = "   "
  names(result)[12] = "    "

  test.which = c()
  test.text = c()
  align.which = c("left")
  col.which = c("")
  if("F" %in% test) {
    test.which = c(test.which, 3:7)
    test.text = c(test.text, "<<italic F>> test")
    align.which = c(align.which, c("right", "right", "right", "right", "left"))
    col.which = c(col.which, c("<i>F</i>", "<i>df</i><sub>1</sub>", "<i>df</i><sub>2</sub>", "<i>p</i>", " "))
  }
  if("Chisq" %in% test) {
    test.which = c(test.which, 8:11)
    test.text = c(test.text, "Wald \u03c7\u00b2 test")
    align.which = c(align.which, c("right", "right", "right", "left"))
    col.which = c(col.which, c("\u03c7<sup>2</sup>", "<i>df</i>", " <i>p</i>", "  "))
  }

  cat("\n")
  Print("
  <<cyan Granger Causality Test (Multivariate)>>

  {paste(test.text, collapse=' and ')} based on VAR({varmodel$p}) model:
  ")
  print_table(result[c(12:13, test.which)],
              nsmalls=0,
              row.names=FALSE)
  cat("\n")

  if(!is.null(file)) {
    result[[12]] = result[[12]] %>%
      str_replace_all("<=", "\u2190") %>%
      str_replace_all("^-+$", "")
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

