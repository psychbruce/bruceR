#### Indirect Effect: Model-Based (using the "mediation" package) ####


#' Tidy report of mediation analysis.
#'
#' @description
#' Tidy report of mediation analysis,
#' which is performed using the \code{\link[mediation]{mediation}} package.
#'
#' @param model Mediation model built using \code{\link[mediation:mediate]{mediation::mediate()}}.
#' @param digits Number of decimal places of output. Default is \code{3}.
#' @param nsmall The same as \code{digits}.
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
med_summary=function(model, digits=nsmall, nsmall=3, print.avg=TRUE) {
  # for raw function, see:
  # edit(mediation:::print.summary.mediate)
  x <- model
  clp <- 100 * x$conf.level
  cat("\n")
  cat(sprintf("Mediation Analysis %s\n\n",
              ifelse(inherits(x, "mediate.tsls"),
                     "using Two-Stage Least Squares", "")))
  if (x$boot) {
    cat(sprintf("Nonparametric Bootstrap Confidence Intervals with the %s Method\n\n",
                ifelse(x$boot.ci.type=="perc", "Percentile", "BCa")))
  } else {
    cat(sprintf("%s Confidence Intervals\n\n",
                ifelse(inherits(x, "mediate.tsls"), "Two-Stage Least Squares", "Quasi-Bayesian")))
  }

  if (!is.null(x$covariates)) {
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

  if (print.avg) {
    smat <- rbind(
      c(x$d.avg, x$d.avg.ci, x$d.avg.p),
      c(x$z.avg, x$z.avg.ci, x$z.avg.p),
      c(x$tau.coef, x$tau.ci, x$tau.p))
    rownames(smat) <- c(
      "Indirect Effect",
      "Direct Effect",
      "Total Effect")
  } else {
    smat <- rbind(
      c(x$d0, x$d0.ci, x$d0.p),
      c(x$d1, x$d1.ci, x$d1.p),
      c(x$z0, x$z0.ci, x$z0.p),
      c(x$z1, x$z1.ci, x$z1.p),
      c(x$d.avg, x$d.avg.ci, x$d.avg.p),
      c(x$z.avg, x$z.avg.ci, x$z.avg.p),
      c(x$tau.coef, x$tau.ci, x$tau.p))
    rownames(smat) <- c(
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


#' Granger causality test.
#'
#' @description
#' Granger test of predictive causality (between two time-series variables)
#' using the \code{\link[lmtest:grangertest]{lmtest::grangertest()}} function.
#'
#' @details
#' The Granger causality test can examine whether
#' the lagged values of a predictor
#' have any incremental role in predicting an outcome
#' if controlling for
#' the lagged values of the outcome itself.
#' It compares the two models by using an \emph{F} test.
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
#' @seealso \code{\link{ccf_plot}}
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

  Print("<<bold <<underline Granger Test of Predictive Causality>>>>")

  Print("\n\n\n<<bold Hypothesized direction:>>")
  Print("<<blue {formula[2]} ~ {formula[2]}[1:Lags] + <<green {formula[3]}[1:Lags]>>>>")

  for(f in formulas) {
    if(test.reverse & f!=formulas[[1]]) {
      Print("\n\n\n<<bold Reverse direction:>>")
      Print("<<blue {formula[3]} ~ {formula[3]}[1:Lags] + <<green {formula[2]}[1:Lags]>>>>")
    }
    for(lag in lags) {
      gt=lmtest::grangertest(formula=f, data=data, order=lag, na.action=stats::na.omit)
      result=bruceR::p(f=gt[2,"F"], df1=-gt[2,"Df"], df2=gt[1,"Res.Df"])
      Print("Lags = {lag}:\t{result}")
    }
  }
}

