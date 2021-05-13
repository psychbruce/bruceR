#### PROCESS Macro ####


# sim_slopes=interactions::sim_slopes
# mediate=mediation::mediate


PROCESS=function(data, x, y, cov,
                 mediators=NULL,
                 moderators=NULL,
                 multilevel=NULL,
                 bootstrap=TRUE,
                 simulation=1000,
                 seed=1) {
  Print("Coming soon...")
}




#### Indirect Effect: Model-Based (using the "mediation" package) ####


#' Tidy report of mediation analysis (to R Console or MS Word).
#'
#' @description
#' Tidy report of mediation analysis,
#' which is performed using the \code{\link[mediation]{mediation}} package.
#'
#' @param model Mediation model built using \code{\link[mediation:mediate]{mediation::mediate()}}.
#' @param digits Number of decimal places of output. Default is \code{3}.
#' @param nsmall The same as \code{digits}.
#' @param file File name of MS Word (\code{.doc}).
#' @param print.avg Just set as \code{TRUE} for a concise output.
#' For details, see the "Value" section in \code{\link[mediation:mediate]{mediation::mediate()}}.
#'
#' @return Invisibly return a data frame containing the results.
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
med_summary=function(model, digits=nsmall, nsmall=3,
                     file=NULL, print.avg=TRUE) {
  # for raw function, see:
  # edit(mediation:::print.summary.mediate)
  x=model
  clp=100*x$conf.level
  cat("\n")
  cat(sprintf("Mediation Analysis %s\n\n",
              ifelse(inherits(x, "mediate.tsls"),
                     "using Two-Stage Least Squares", "")))
  if(x$boot) {
    cat(sprintf("Nonparametric Bootstrap Confidence Intervals with the %s Method\n\n",
                ifelse(x$boot.ci.type=="perc", "Percentile", "BCa")))
  } else {
    cat(sprintf("%s Confidence Intervals\n\n",
                ifelse(inherits(x, "mediate.tsls"), "Two-Stage Least Squares", "Quasi-Bayesian")))
  }

  if(!is.null(x$covariates)) {
    Print("Conditional on ...")
    conditional=data.frame(Value=unlist(x$covariates))
    conditional$Value=paste("=", conditional$Value)
    print(conditional)
    cat("\n")
  }
  Print("Model Hypothesis:")
  X=x[["treat"]]
  M=x[["mediator"]]
  Y=names(stats::model.frame(x[["model.y"]]))[1]
  Print("{X} ==> {M} ==> {Y}")

  if(print.avg) {
    smat=rbind(
      c(x$d.avg, x$d.avg.ci, x$d.avg.p),
      c(x$z.avg, x$z.avg.ci, x$z.avg.p),
      c(x$tau.coef, x$tau.ci, x$tau.p))
    rownames(smat)=c(
      "Indirect Effect",
      "Direct Effect",
      "Total Effect")
  } else {
    smat=rbind(
      c(x$d0, x$d0.ci, x$d0.p),
      c(x$d1, x$d1.ci, x$d1.p),
      c(x$z0, x$z0.ci, x$z0.p),
      c(x$z1, x$z1.ci, x$z1.p),
      c(x$d.avg, x$d.avg.ci, x$d.avg.p),
      c(x$z.avg, x$z.avg.ci, x$z.avg.p),
      c(x$tau.coef, x$tau.ci, x$tau.p))
    rownames(smat)=c(
      "Indirect Effect (control)", "Indirect Effect (treated)",
      "Direct Effect (control)", "Direct Effect (treated)",
      "Indirect Effect (average)", "Direct Effect (average)",
      "Total Effect")
  }
  smat=as.data.frame(smat)
  names(smat)=c("Estimate", "LLCI", "ULCI", "pval")
  smat$Estimate.CI=paste0(
    formatF(smat$Estimate, nsmall), " [",
    formatF(smat$LLCI, nsmall), ", ",
    formatF(smat$ULCI, nsmall), "]")
  names(smat)[5]=paste0("Estimate [",
                        ifelse(x$boot, "Bootstrap ", ""),
                        clp, "% CI]")
  print_table(smat[c(5,4)], nsmalls=0)
  cat("\n")

  Print("Sample Size: {x$nobs}")
  Print("Simulations: {x$sims} ({ifelse(x$boot, 'Bootstrap', 'Monte Carlo')})")
  cat("\n")

  if(!is.null(file)) {
    smat.new=smat
    smat.new.names=names(smat.new)
    smat.new.names[4]="<i>p</i>"
    smat.new[[5]]=stringr::str_replace_all(smat.new[[5]], "-", "\u2013")
    print_table(smat.new[c(5,4)], nsmalls=0,
                col.names=c(smat.new.names[c(5,4)], " "),
                file=file,
                file.align.head=c("left", "left", "right", "left"),
                file.align.text=c("left", "left", "right", "left"),
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

