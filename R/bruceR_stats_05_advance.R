#### Indirect Effect: Model-Based (using the "mediation" package) ####


#' Tidy report of mediation analysis based on the \code{mediation} package.
#'
#' @description
#' Tidy report of mediation analysis based on the \code{\link[mediation]{mediation}} package.
#'
#' @param model Mediation model built with the \code{mediation} package (see \code{\link[mediation]{mediate}}).
#' @param digits Number of decimal places of output. Default is \code{3}.
#' @param nsmall The same as \code{digits}.
#' @param print.avg Just set as \code{TRUE} for a concise output.
#' For details, see the "Value" section in \code{\link[mediation]{mediate}}.
#'
#' @examples
#' library(mediation)
#' ?mediation::mediate
#'
#' ## Example 1: OLS Regression
#' # Data and correlation matrix
#' Corr(airquality)
#' # Hypothesis: Solar radiation -> Ozone -> Daily temperature
#' lm.m=lm(Ozone ~ Solar.R + Month + Wind, data=airquality)
#' lm.y=lm(Temp ~ Ozone + Solar.R + Month + Wind, data=airquality)
#' # Model summary
#' model_summary(list(lm.m, lm.y))
#' check_collinearity(lm.m)
#' check_collinearity(lm.y)
#' # Mediation analysis
#' set.seed(123)  # set a random seed for reproduction
#' med=mediate(lm.m, lm.y,
#'             treat="Solar.R", mediator="Ozone",
#'             sims=1000, boot=TRUE, boot.ci.type="bca")
#' # Bias-corrected and accelerated (BCa) bootstrap confidence intervals
#' med_summary(med)
#'
#' ## Example 2: Multilevel Linear Model (Linear Mixed Model)
#' # Data and correlation matrix
#' library(lmerTest)
#' ?carrots  # long-format data
#' data=na.omit(carrots)  # omit missing values
#' setDT(data)  # set as data.table
#' Corr(data[,.(Preference, Crisp, Sweetness)])
#' # Hypothesis: Crips -> Sweetness -> Preference (for carrots)
#' # (models must be fit using "lme4::lmer" rather than "lmerTest::lmer")
#' lmm.m=lme4::lmer(Sweetness ~ Crisp + Gender + Age + (1 | Consumer), data=data)
#' lmm.y=lme4::lmer(Preference ~ Sweetness + Crisp + Gender + Age + (1 | Consumer), data=data)
#' # Model summary
#' model_summary(list(lmm.m, lmm.y))
#' check_collinearity(lmm.m)
#' check_collinearity(lmm.y)
#' # Mediation analysis
#' set.seed(123)  # set a random seed for reproduction
#' med.lmm=mediate(lmm.m, lmm.y,
#'                 treat="Crisp", mediator="Sweetness",
#'                 sims=1000)
#' # Monte Carlo simulation (quasi-Bayesian approximation)
#' # (bootstrap method is not applicable to "lmer" models)
#' med_summary(med.lmm)
#'
#' @importFrom stats model.frame
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
  Y=names(model.frame(x[["model.y"]]))[1]
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
  colnames(smat) <- c("Estimate",
                      paste0(clp, "% LLCI"),
                      paste0(clp, "% ULCI"),
                      "pval")
  print_table(smat, nsmalls=nsmall)
  cat("\n")

  Print("Sample Size: {x$nobs}")
  Print("Simulations: {x$sims} ({ifelse(x$boot, 'Bootstrap', 'Monte Carlo')})")
  cat("\n")
  invisible(x)
}


#### Indirect Effect: Sobel Test & MCMC ####


#' Mediation analysis based on \emph{b} and \emph{SE} with Sobel test and Monte Carlo simulation.
#'
#' @description
#' Estimating indirect effect from regression coefficients and standard errors (\emph{SE}) by using Sobel test and Monte Carlo simulation.
#'
#' Total effect (\strong{c}) = Direct effect (\strong{c'}) + Indirect effect (\strong{a*b})
#'
#' @param a Path \strong{a} (X -> Mediator).
#' @param SEa \emph{SE} of path \strong{a}.
#' @param b Path \strong{b} (Mediator -> Y).
#' @param SEb \emph{SE} of path \strong{b}.
#' @param direct [optional] Path \strong{c'} (X -> Y \strong{direct} effect, with M also included in model).
#' @param total [optional] Path \strong{c} (X -> Y \strong{total} effect, without M).
#' @param cov_ab Covariance between \strong{a} and \strong{b}.
#'
#' See \href{http://www.quantpsy.org/medmc/medmc.htm}{Selig & Preacher (2008)}:
#'
#' \emph{If you use SEM, path analysis, multilevel modeling, or some other multivariate method to obtain both a and b from a single model, then cov(a,b) can be found in the asymptotic covariance matrix of the parameter estimates.
#' If you use regression to obtain a and b in separate steps, then cov(a,b) = 0.}
#' @param seed Random seed.
#' @param rep Number of repetitions for Monte Carlo simulation. Default is 50,000. More than 1,000 are recommended.
#' @param nsmall Number of decimal places of output. Default is 3.
#'
#' @references
#' Sobel, M. E. (1982). Asymptotic confidence intervals for indirect effects in Structural Equation Models. \emph{Sociological Methodology, 13,} 290-312.
#'
#' Selig, J. P., & Preacher, K. J. (2008). Monte Carlo method for assessing mediation: An interactive tool for creating confidence intervals for indirect effects. \url{http://www.quantpsy.org/medmc/medmc.htm}
#'
#' @examples
#' med_mc(a=1.50, SEa=0.50, b=2.00, SEb=0.80)
#' med_mc(a=1.50, SEa=0.50, b=2.00, SEb=0.80, total=4.50)
#' med_mc(a=1.50, SEa=0.50, b=2.00, SEb=0.80, direct=1.50)
#'
#' @export
med_mc=function(a, SEa, b, SEb, direct=NULL, total=NULL,
                cov_ab=0, seed=NULL, rep=50000, nsmall=3) {
  indirect=a*b
  if(is.null(total)) {
    if(is.null(direct)) {
      # Print("No input for 'direct' or 'total' effect.")
    } else {
      total=direct+indirect
    }
  } else {
    if(is.null(direct)) {
      direct=total-indirect
    } else {
      total=direct+indirect  # priority: direct > total
      warning("Total effect is replaced by the sum of direct and indirect effects.")
    }
  }

  ## Direct and Indirect Effects ##
  if(is.null(total)==FALSE) {
    effect=data.frame(total, direct, indirect,
                      ratioTotal=indirect/total,
                      ratioRelative=abs(indirect/direct))
    names(effect)=c("Total", "Direct", "Indirect", "Ratio.Total", "Ratio.Relative")
    Print("Direct and Indirect Effects:")
    print_table(effect, row.names=FALSE, nsmalls=nsmall)
    Print("<<blue Total = Direct + Indirect
    Ratio.Total = Indirect / Total
    Ratio.Relative = Indirect / Direct
    \n>>")
  }

  ## Indirect Effect: Sobel Test & MCMAM ##
  sobel=sobel(a, SEa, b, SEb)
  mcmam=mcmam(a, SEa, b, SEb, cov_ab=cov_ab, seed=seed, rep=rep)
  mediation=rbind(sobel, mcmam)
  names(mediation)=c("a", "b", "a*b", "SE(a*b)", "z", "pval", "[95% ", "  CI]", "sig")
  Print("Test for Indirect Effect (a*b):")
  print_table(mediation, nsmalls=nsmall)
}


sobel=function(a, SEa, b, SEb) {
  ab=a*b
  SEab=sqrt(a^2*SEb^2 + b^2*SEa^2) # Sobel (1982) first-order solution
  # SEab=sqrt(a^2*SEb^2 + b^2*SEa^2 - SEa^2*SEb^2) # Goodman (1960) unbiased solution
  # SEab=sqrt(a^2*SEb^2 + b^2*SEa^2 + SEa^2*SEb^2) # Aroian (1944) second-order exact solution
  z=ab/SEab
  p=p.z(z)
  abLLCI=ab-1.96*SEab
  abULCI=ab+1.96*SEab
  sig=sig.trans(p)
  out=data.frame(a, b, ab, SEab, z, p, abLLCI, abULCI, sig)
  row.names(out)="Sobel test"
  return(out)
}


mcmam=function(a, SEa, b, SEb, cov_ab=0, seed=NULL, rep=50000, conf=0.95) {
  # http://www.quantpsy.org/medmc/medmc.htm
  if(!is.null(seed)) set.seed(seed)
  acov=matrix(c(
    SEa^2, cov_ab,
    cov_ab, SEb^2
  ), 2, 2)
  mcmc=MASS::mvrnorm(rep, c(a, b), acov, empirical=FALSE)
  abMC=mcmc[,1]*mcmc[,2]
  ab=mean(abMC)
  SEab=stats::sd(abMC)
  # z=ab/SEab
  # p=p.z(z)
  abLLCI=as.numeric(stats::quantile(abMC, (1-conf)/2))  # 0.025
  abULCI=as.numeric(stats::quantile(abMC, 1-(1-conf)/2))  # 0.975
  sig=ifelse(abLLCI>0 | abULCI<0, "yes", "no")
  out=data.frame(a, b, ab, SEab, z=NA, p=NA, abLLCI, abULCI, sig)
  row.names(out)="Monte Carlo"
  return(out)
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
#' A \code{gg} object, which you can further modify using \code{ggplot2} syntax
#' and save using the \code{ggsave} function.
#'
#' @examples
#' (d=lmtest::ChickEgg)  # time-series data
#' p1=ccf_plot(chicken ~ egg, data=d)
#' p2=ccf_plot(chicken ~ egg, data=d, alpha.ns=0.3,
#'             pos.color=see::social_colors("red"),
#'             neg.color=see::social_colors("blue grey"),
#'             ci.color="black")
#' ggsave(plot=p2, filename="CCF.png", width=8, height=6, dpi=500)
#'
#' @seealso \code{\link{granger_test}}
#'
#' @import ggplot2
#' @importFrom stats ccf qt
#' @importFrom dplyr mutate
#' @importFrom psych t2r
#' @export
ccf_plot=function(formula, data,
                  lag.max=30, sig.level=0.05,
                  xbreaks=seq(-30, 30, 10),
                  ybreaks=seq(-1, 1, 0.2),
                  ylim=NULL, alpha.ns=1,
                  pos.color="black", neg.color="black", ci.color="blue",
                  title=NULL, subtitle=NULL,
                  xlab="Lag", ylab="Cross-Correlation") {
  x=as.character(formula)[3]
  y=as.character(formula)[2]
  data=as.data.frame(data)
  if(is.null(title)) title=Glue("{x} \u2192 {y}")

  cc=stats::ccf(x=data[[x]], y=data[[y]], lag.max=lag.max, plot=FALSE)
  ccdata=with(cc, data.frame(`lag`, `acf`))
  n=cc$n.used
  rsig=t2r(qt(sig.level/2, n, lower.tail=F), n-2)
  ccdata=mutate(ccdata,
                `sig`=as.factor(ifelse(abs(acf)<rsig, 0, 1)),
                `direc`=as.factor(ifelse(acf<0, 0, 1)))

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
#' using the \code{lmtest::\link[lmtest]{grangertest}} function.
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
#' @param na.action What to do with missing values. Default is \code{na.omit}.
#'
#' @examples
#' (d=lmtest::ChickEgg)  # time-series data
#' granger_test(chicken ~ egg, data=d)
#' granger_test(chicken ~ egg, data=d, lags=1:10, test.reverse=TRUE)
#'
#' @seealso \code{\link{ccf_plot}}
#'
#' @importFrom stats na.omit as.formula
#' @importFrom lmtest grangertest
#' @export
granger_test=function(formula, data, lags=1:5,
                      test.reverse=FALSE, na.action=na.omit) {
  Print("<<bold <<underline Granger Test of Predictive Causality>>>>")

  Print("\n\n\n<<bold Hypothesized direction:>>")
  Print("<<blue {formula[2]} ~ {formula[2]}[1:Lags] + <<green {formula[3]}[1:Lags]>>>>")
  for(lag in lags) {
    gt=lmtest::grangertest(formula=formula, data=data, order=lag, na.action=na.action)
    result=bruceR::p(f=gt[2,"F"], df1=-gt[2,"Df"], df2=gt[1,"Res.Df"])
    Print("Lags = {lag}: {result}")
  }

  if(test.reverse) {
    Print("\n\n\n<<bold Reverse direction:>>")
    Print("<<blue {formula[3]} ~ {formula[3]}[1:Lags] + <<green {formula[2]}[1:Lags]>>>>")
    formula.rev=as.formula(paste(formula[3], formula[1], formula[2]))
    for(lag in lags) {
      gt=lmtest::grangertest(formula=formula.rev, data=data, order=lag, na.action=na.action)
      result=bruceR::p(f=gt[2,"F"], df1=-gt[2,"Df"], df2=gt[1,"Res.Df"])
      Print("Lags = {lag}: {result}")
    }
  }
}

