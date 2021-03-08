#### Significance Test and Report ####


#' Compute \emph{p} value.
#'
#' @param z,t,f,r,chi2 \emph{z}, \emph{t}, \emph{F}, \emph{r}, \eqn{\chi}^2 value.
#' @param n,df,df1,df2 Sample size or degree of freedom.
#'
#' @examples
#' p.z(1.96)
#' p.t(2, 100)
#' p.f(4, 1, 100)
#' p.r(0.2, 100)
#' p.chi2(3.84, 1)
#'
#' p(z=1.96)
#' p(t=2, df=100)
#' p(f=4, df1=1, df2=100)
#' p(r=0.2, n=100)
#' p(chi2=3.84, df=1)
#'
#' @export
p=function(z=NULL, t=NULL, f=NULL, r=NULL, chi2=NULL,
           n=NULL, df=NULL, df1=NULL, df2=NULL) {
  if(!is.null(z)) {p=p.z(z); pstat=Glue("<<italic z>> = {z:.2}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(t)) {p=p.t(t, df); pstat=Glue("<<italic t>>({df}) = {t:.2}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(f)) {p=p.f(f, df1, df2); pstat=Glue("<<italic F>>({df1}, {df2}) = {f:.2}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(r)) {p=p.r(r, n); pstat=Glue("<<italic r>>({n-2}) = {r:.2}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(chi2)) {p=p.chi2(chi2, df); pstat=Glue("\u03c7\u00b2({df}) = {chi2:.2}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  return(pstat)
}

#' @describeIn p Two-tailed \emph{p} value of \emph{z}.
#' @importFrom stats pnorm
#' @export
p.z=function(z) pnorm(abs(z), lower.tail=FALSE)*2

#' @describeIn p Two-tailed \emph{p} value of \emph{t}.
#' @importFrom stats pt
#' @export
p.t=function(t, df) pt(abs(t), df, lower.tail=FALSE)*2

#' @describeIn p One-tailed \emph{p} value of \emph{F}. (Note: \emph{F} test is one-tailed only.)
#' @importFrom stats pf
#' @export
p.f=function(f, df1, df2) pf(f, df1, df2, lower.tail=FALSE)

#' @describeIn p Two-tailed \emph{p} value of \emph{r}.
#' @export
p.r=function(r, n) p.t(r/sqrt((1-r^2)/(n-2)), n-2)

#' @describeIn p One-tailed \emph{p} value of \eqn{\chi}^2. (Note: \eqn{\chi}^2 test is one-tailed only.)
#' @importFrom stats pchisq
#' @export
p.chi2=function(chi2, df) pchisq(chi2, df, lower.tail=FALSE)


#' Transform p values.
#'
#' @param p \emph{p} value.
#' @param nsmall.p Number of decimal places of \emph{p} value. Default is \code{3}.
#'
#' @examples
#' p.trans(c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0.0000001, 1e-50)) %>% data.table(p=.)
#'
#' @seealso \code{\link{p.trans2}}
#'
#' @export
p.trans=function(p, nsmall.p=3) {
  mapply(function(p, nsmall.p) {
    ifelse(is.na(p) | p > 1 | p < 0, "",
           ifelse(p < 10^-nsmall.p, gsub("0(?=\\.)", "", Glue("<{10^-nsmall.p:.{nsmall.p}}"), perl=T),
                  gsub("0(?=\\.)", " ", Glue("{p:.{nsmall.p}}"), perl=T)))
  }, p, nsmall.p)
}


#' Transform p values.
#'
#' @inheritParams p.trans
#' @param p.min Minimum of \emph{p}. Default is \code{1e-99}.
#'
#' @examples
#' p.trans2(c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0.0000001, 1e-50)) %>% data.table(p=.)
#'
#' @seealso \code{\link{p.trans}}
#'
#' @export
p.trans2=function(p, nsmall.p=3, p.min=1e-99) {
  ifelse(is.na(p) | p > 1 | p < 0, "",
         ifelse(p < p.min, paste("<", p.min),
                ifelse(p < 10^-nsmall.p, paste("=", format(p, digits=1, scientific=T)),
                       paste("=", format(p, digits=0, nsmall=nsmall.p, scientific=F)))))
}


#' Transform p value to significance code.
#'
#' @inheritParams p.trans
#'
#' @examples
#' sig.trans(c(1, 0.09, 0.049, 0.009, 0.001, 0.0001, 1e-50)) %>% data.table(sig=.)
#'
#' @export
sig.trans=function(p) {
  ifelse(is.na(p) | p > 1 | p < 0, "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "** ",
                       ifelse(p < .05, "*  ",
                              ifelse(p < .10, ".  ", "   ")))))
}



#### Basic Statistics ####

#' Descriptive statistics.
#'
#' @param data Data frame or a numeric vector.
#' @param nsmall Number of decimal places of output. Default is \code{2}.
#' @param plot \code{TRUE} or \code{FALSE} (default).
#' Visualize the descriptive statistics using \code{GGally::\link[GGally]{ggpairs}}.
#' @param smooth \code{"none"} (default), \code{"lm"}, or \code{"loess"}.
#' Add fitting lines to scatter plots (if any).
#' @param save.file \code{NULL} (default, plot in RStudio) or a file name (\code{"xxx.png"}).
#' @param width Width (in "inch") of the saved plot. Default is \code{8}.
#' @param height Height (in "inch") of the saved plot. Default is \code{6}.
#' @param dpi DPI (dots per inch) of the saved plot. Default is \code{500}.
#'
#' @examples
#' d=as.data.table(bfi)
#' d[,`:=`(
#'   gender=as.factor(gender),
#'   education=as.factor(education),
#'   E=MEAN(d, "E", 1:5, rev=c(1,2), likert=1:6),
#'   O=MEAN(d, "O", 1:5, rev=c(2,5), likert=1:6)
#' )]
#' Describe(bfi[c("age", "gender", "education")])
#' Describe(d[,.(age, gender, education, E, O)], plot=TRUE)
#'
#' # Describe(airquality, plot=TRUE, smooth="lm",
#' #          save.file="Descriptive Statistics.png",
#' #          width=10, height=8, dpi=500)
#'
#' @import ggplot2
#' @importFrom psych describe
## @importFrom GGally ggpairs wrap
#' @export
Describe=function(data, nsmall=2, plot=FALSE, smooth="none",
                  save.file=NULL, width=8, height=6, dpi=500) {
  Print("Descriptive statistics:")

  if(is.numeric(data)) data=data.frame(X=data)
  desc=as.data.frame(describe(data, fast=FALSE))
  desc$vars = desc$trimmed = desc$mad = desc$range = desc$se = NULL
  names(desc)=c("N", "Mean", "SD", "Median", "Min", "Max", "Skewness", "Kurtosis")
  desc$`|`="|"
  desc$Missing=NA
  desc=desc[c("N", "Missing", "Mean", "SD", "|",
              "Median", "Min", "Max", "Skewness", "Kurtosis")]

  if(length(as.data.frame(data))==1)
    row.names(desc)=" "  # as.character(sys.call())[2]
  missing=nrow(data)-desc$N
  if(max(missing)==0) {
    desc$Missing=NULL
    print_table(desc, nsmalls=c(0, rep(nsmall, 8)))
  } else {
    desc$Missing=ifelse(missing==0, NA, missing)
    names(desc)[2]="(NA)"
    print_table(desc, nsmalls=c(0, 0, rep(nsmall, 8)))
  }

  p=NULL
  if(plot) {
    smooth=switch(smooth,
                  "none"="points",
                  "lm"="smooth",
                  "loess"="smooth_loess")
    p=GGally::ggpairs(data,
              lower=list(continuous=GGally::wrap(smooth, size=1, shape=16, alpha=0.3)),
              upper=list(continuous=GGally::wrap("cor", color="black"))) +
      theme_bruce(panel.bg="grey95")
    if(is.null(save.file)) {
      print(p)
    } else {
      ggsave(plot=p, filename=save.file, width=width, height=height, dpi=dpi)
      path=ifelse(grepl(":", save.file), save.file, paste0(getwd(), '/', save.file))
      Print("\n\n\n<<green \u2714>> Plot saved to <<blue '{path}'>>")
    }
  }

  invisible(list(desc=desc, plot=p))
}


#' Frequency statistics with histogram and density plot.
#'
#' @param var Vector or variable.
#' @param label [optional] A vector re-defining the labels of values.
#' @param sort \code{""} (default, sorted by raw order), \code{"-"} (decreasing order), or \code{"+"} (increasing order).
#' @param nsmall Number of decimal places of output. Default is 1.
#'
#' @examples
#' Freq(bfi$education)
#' Freq(bfi$gender, label=c("Male", "Female"))
#' Freq(bfi$age)
#'
#' @export
Freq=function(var, label=NULL, sort="", nsmall=1) {
  Print("Frequency table:")
  tableVar=table(var)
  N.na=sum(is.na(var))
  N=sum(tableVar)+N.na
  if(is.null(label)) label=names(tableVar)
  output=cbind(matrix(tableVar,
                      dimnames=list(label, "N")),
               matrix(round(tableVar/N*100, nsmall),
                      dimnames=list(label, "%")))
  if(N.na) output=rbind(output,
                        matrix(c(N.na, round(N.na/N*100, nsmall)),
                               ncol=2, dimnames=list("NA")))
  if(sort=="")
    print_table(output, nsmalls=c(0, nsmall))
  else if(sort=="-")
    print_table(output[order(output[,"N"], decreasing=TRUE),], nsmalls=c(0, nsmall))
  else if(sort=="+")
    print_table(output[order(output[,"N"], decreasing=FALSE),], nsmalls=c(0, nsmall))
  Print("Total <<italic N>> = {formatN(N)}")
  if(N.na>0) Print("Valid <<italic N>> = {formatN(N-N.na)}")

  invisible(output)
}


#' Correlation analysis with test and plot.
#'
#' @inheritParams Describe
#' @param method \code{"pearson"} (default), \code{"spearman"}, or \code{"kendall"}.
#' @param p.adjust Adjustment of \emph{p} values for multiple tests: \code{"none", "fdr", "holm", "bonferroni", ...}
#' For details, see \code{stats::\link[stats]{p.adjust}}.
#' @param plot \code{TRUE} (default) or \code{FALSE}, plot the correlation matrix.
#' @param plot.range Range of correlation coefficients for plot. Default is \code{c(-1, 1)}.
#' @param plot.color Color gradient for plot. Default is \code{c("#B52127", "white", "#2171B5")}.
#' You may also set it to, e.g., \code{c("red", "white", "blue")}.
#'
#' @examples
#' Corr(airquality)
#' Corr(airquality, p.adjust="bonferroni")
#' # Corr(airquality, save.file="Air-Corr.png")
#'
#' Corr(bfi[c("gender", "age", "education")])
#' # Corr(bfi, save.file="BFI-Corr.png", width=9, height=9)
#'
#' @importFrom psych corr.test cor.plot
#' @importFrom grDevices colorRampPalette png dev.off
#' @export
Corr=function(data, method="pearson", nsmall=2,
              p.adjust="none",
              plot=TRUE, plot.range=c(-1, 1),
              plot.color=c("#B52127", "white", "#2171B5"),
              save.file=NULL, width=8, height=6, dpi=500) {
  data=as.data.frame(data)
  exclude.vars=c()
  for(var in names(data)) {
    if(class(data[[var]]) %in% c("character", "factor")) {
      data[var]=NULL
      exclude.vars=c(exclude.vars, var)
    }
  }

  cor=cor0=corr.test(data, method=method, adjust=p.adjust)
  # print(cor, digits=nsmall, short=!CI)

  Print("Correlation matrix ({capitalize(method)}'s <<italic r>>):")
  cor$r[cor$r==1]=NA
  print_table(cor$r, nsmalls=nsmall)

  Print("\n\n\nSig. (2-tailed):")
  cor$p=gsub("=", " ", gsub(" ", "", p.trans2(cor$p)))
  for(i in 1:nrow(cor$p)) cor$p[i,i]=""
  print_table(cor$p)

  if(p.adjust!="none") {
    Print("<<blue P-values above the diagonal are adjusted for multiple tests ({capitalize(p.adjust)} method).>>")
  }

  if("matrix" %in% class(cor$n)) {
    Print("\n\n\nSample size:")
    print_table(cor$n, nsmalls=0)
  } else {
    Print("\n\n\nSample size: <<italic N>> = {cor$n}")
  }

  Print("\n\n\n95% CI for <<italic r>>:")
  cor$ci=cor$ci[c(2,1,3,4)]
  names(cor$ci)=c("r", "[95% ", "  CI]", "pval")
  print_table(cor$ci, nsmalls=nsmall)

  if(length(exclude.vars)>0) {
    cat("\n")
    Print("<<red !! Excluded non-numeric variable(s):>>")
    cat(paste(exclude.vars, collapse=", "))
    cat("\n")
  }

  cor=cor0
  if(plot) {
    if(!is.null(save.file)) {
      png(filename=save.file, width=width, height=height, units="in", res=dpi)
    }
    cor.plot(r=cor$r, adjust="none", numbers=TRUE, zlim=plot.range,
             diag=FALSE, xlas=2, n=201,
             pval=cor$p, stars=TRUE,
             alpha=1,
             gr=colorRampPalette(plot.color),
             main="Correlation Matrix")
    if(!is.null(save.file)) {
      dev.off()
      path=ifelse(grepl(":", save.file), save.file, paste0(getwd(), '/', save.file))
      Print("\n\n\n<<green \u2714>> Plot saved to <<blue '{path}'>>")
    }
  }

  invisible(cor)
}


#' Test the difference between two correlations (independent / nonindependent).
#'
#' @param r1,r2 Correlation coefficients (Pearson's \emph{r}).
#' @param n,n1,n2 Sample sizes.
#' @param rcov [Optional] Only for nonindependent \emph{r}s:
#'
#' \code{r1} is r(X,Y),
#'
#' \code{r2} is r(X,Z),
#'
#' then, as Y and Z are also correlated,
#'
#' we should also consider \code{rcov}: r(Y,Z)
#'
#' @return
#' Invisibly return the \emph{p} value.
#'
#' @examples
#' # two independent rs (X~Y vs. Z~W)
#' cor_diff(r1=0.20, n1=100, r2=0.45, n2=100)
#'
#' # two nonindependent rs (X~Y vs. X~Z, with Y and Z also correlated [rcov])
#' cor_diff(r1=0.20, r2=0.45, n=100, rcov=0.80)
#'
#' @export
cor_diff=function(r1, n1, r2, n2, n=NULL, rcov=NULL) {
  if(is.null(rcov)) {
    # independent rs
    z1=atanh(r1)
    z2=atanh(r2)
    zdiff=(z1-z2)/sqrt(1/(n1-3)+1/(n2-3))
    p=p.z(zdiff)
    Print("
    <<italic r>>1 = {formatF(r1)} (<<italic N>> = {formatN(n1)})
    <<italic r>>2 = {formatF(r2)} (<<italic N>> = {formatN(n2)})
    Difference of correlation: {p(z=zdiff)}
    ")
  } else {
    # nonindependent rs
    R=(1-r1^2-r2^2-rcov^2)+2*r1*r2*rcov
    tdiff=(r1-r2)*sqrt((n-1)*(1+rcov)/(2*R*(n-1)/(n-3)+(r1+r2)^2*(1-rcov)^3/4))
    p=p.t(tdiff, n-3)
    Print("
    <<italic r>>1 = {formatF(r1)}
    <<italic r>>2 = {formatF(r2)}
    (<<italic N>> = {formatN(n1)}, <<italic r>>_cov = {formatF(rcov)})
    Difference of correlation: {p(t=tdiff, df=n-3)}
    ")
  }
  invisible(p)
}

