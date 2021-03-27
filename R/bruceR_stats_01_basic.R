#### Significance Test and Report ####


#' Compute \emph{p} value.
#'
#' @param z,t,f,r,chi2 \emph{z}, \emph{t}, \emph{F}, \emph{r}, \eqn{\chi}^2 value.
#' @param n,df,df1,df2 Sample size or degree of freedom.
#'
#' @return \emph{p} value statistics.
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


## Transform \emph{p} value.
##
## @param p \emph{p} value.
## @param nsmall.p Number of decimal places of \emph{p} value. Default is \code{3}.
##
## @return A character string of transformed \emph{p} value.
##
## @examples
## p.trans(c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0.0000001, 1e-50)) %>% data.table(p=.)
##
## @seealso \code{\link{p.trans2}}
##
## @export
p.trans=function(p, nsmall.p=3) {
  mapply(function(p, nsmall.p) {
    ifelse(is.na(p) | p > 1 | p < 0, "",
           ifelse(p < 10^-nsmall.p, gsub("0(?=\\.)", "", Glue("<{10^-nsmall.p:.{nsmall.p}}"), perl=T),
                  gsub("0(?=\\.)", " ", Glue("{p:.{nsmall.p}}"), perl=T)))
  }, p, nsmall.p)
}


## Transform \emph{p} value.
##
## @inheritParams p.trans
## @param p.min Minimum of \emph{p}. Default is \code{1e-99}.
##
## @return A character string of transformed \emph{p} value.
##
## @examples
## p.trans2(c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0.0000001, 1e-50)) %>% data.table(p=.)
##
## @seealso \code{\link{p.trans}}
##
## @export
p.trans2=function(p, nsmall.p=3, p.min=1e-99) {
  ifelse(is.na(p) | p > 1 | p < 0, "",
         ifelse(p < p.min, paste("<", p.min),
                ifelse(p < 10^-nsmall.p, paste("=", format(p, digits=1, scientific=T)),
                       paste("=", format(p, digits=0, nsmall=nsmall.p, scientific=F)))))
}


## Transform \emph{p} value to significance code.
##
## @inheritParams p.trans
##
## @return A character string of significance code.
##
## @examples
## sig.trans(c(1, 0.09, 0.049, 0.009, 0.001, 0.0001, 1e-50)) %>% data.table(sig=.)
##
## @export
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
#' @param data Data frame or numeric vector.
#' @param nsmall Number of decimal places of output. Default is \code{2}.
#' @param plot \code{TRUE} or \code{FALSE} (default).
#' Visualize the descriptive statistics using \code{GGally::\link[GGally]{ggpairs}}.
#' @param all.as.numeric \code{TRUE} (default) or \code{FALSE}.
#' Transform all variables into numeric (continuous).
#' @param upper.triangle \code{TRUE} or \code{FALSE} (default).
#' Add (scatter) plots to upper triangle (time consuming when sample size is large).
#' @param upper.smooth \code{"none"} (default), \code{"lm"}, or \code{"loess"}.
#' Add fitting lines to scatter plots (if any).
#' @param save.file \code{NULL} (default, plot in RStudio) or a file name (\code{"xxx.png"}).
#' @param width Width (in "inch") of the saved plot. Default is \code{8}.
#' @param height Height (in "inch") of the saved plot. Default is \code{6}.
#' @param dpi DPI (dots per inch) of the saved plot. Default is \code{500}.
#'
#' @return
#' Invisibly return a list consisting of
#' (1) a data frame of descriptive statistics and
#' (2) a \code{ggplot2} object if users set \code{plot=TRUE}.
#'
#' @examples
#' \donttest{set.seed(1)
#' Describe(rnorm(1000000), plot=TRUE)
#'
#' Describe(airquality)
#' Describe(airquality, plot=TRUE, upper.triangle=TRUE, upper.smooth="lm")
#'
#' ?psych::bfi
#' Describe(bfi[c("age", "gender", "education")])
#'
#' d=as.data.table(psych::bfi)
#' d[,`:=`(
#'   gender=as.factor(gender),
#'   education=as.factor(education),
#'   E=MEAN(d, "E", 1:5, rev=c(1,2), likert=1:6),
#'   A=MEAN(d, "A", 1:5, rev=1, likert=1:6),
#'   C=MEAN(d, "C", 1:5, rev=c(4,5), likert=1:6),
#'   N=MEAN(d, "N", 1:5, likert=1:6),
#'   O=MEAN(d, "O", 1:5, rev=c(2,5), likert=1:6)
#' )]
#' Describe(d[,.(age, gender, education)], plot=TRUE, all.as.numeric=FALSE)
#' Describe(d[,.(age, gender, education, E, A, C, N, O)], plot=TRUE)
#' }
#' @seealso \link{Corr}
#'
#' @import ggplot2
#' @export
Describe=function(data, nsmall=2,
                  all.as.numeric=TRUE,
                  plot=FALSE,
                  upper.triangle=FALSE, upper.smooth="none",
                  save.file=NULL, width=8, height=6, dpi=500) {
  Print("Descriptive statistics:")

  if(is.numeric(data)) data=data.frame(X=data)
  desc=as.data.frame(psych::describe(data, fast=FALSE))
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

  data.new=as.data.frame(data)
  vars.not.numeric=c()
  if(all.as.numeric) {
    for(var in names(data.new)) {
      if(!is.numeric(data.new[[var]])) {
        data.new[[var]]=as.numeric(data.new[[var]])
        vars.not.numeric=c(vars.not.numeric, var)
      }
    }
  }
  if(length(vars.not.numeric)>0)
    Print("\n\n\n<<yellow NOTE: `{paste(vars.not.numeric, collapse='`, `')}` transformed to numeric.>>")

  p=NULL
  if(plot) {
    if(upper.triangle) {
      smooth=switch(upper.smooth,
                    "none"="points",
                    "lm"="smooth",
                    "loess"="smooth_loess")
      upper=list(continuous=GGally::wrap(
        smooth, size=1, shape=16, alpha=0.3))
    } else {
      upper="blank"
    }
    p=GGally::ggpairs(
      data.new, switch="both", axisLabels="none",
      upper=upper,
      lower=list(continuous=GGally::wrap(
        "cor", digits=nsmall,
        use="pairwise.complete.obs",
        size=4, color="black"))
    ) + theme_bruce() + theme(strip.text=element_text(size=12, color="black"))
    if(is.null(save.file)) {
      print(p)
    } else {
      cowplot::ggsave2(plot=p, filename=save.file,
                       width=width, height=height, dpi=dpi)
      file=stringr::str_split(save.file, "/", simplify=TRUE)
      path=paste0(getwd(), '/', file[length(file)])
      Print("\n\n\n<<green \u2714>> Plot saved to <<blue '{path}'>>")
    }
  }

  invisible(list(desc=desc, plot=p))
}


#' Frequency statistics.
#'
#' @param var Vector or variable.
#' @param label [optional] A vector re-defining the labels of values.
#' @param sort \code{""} (default, sorted by raw order), \code{"-"} (decreasing order), or \code{"+"} (increasing order).
#' @param nsmall Number of decimal places of output. Default is \code{1}.
#'
#' @return A data frame of frequency statistics.
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


#' Correlation analysis.
#'
#' @inheritParams Describe
#' @param data Data frame.
#' @param method \code{"pearson"} (default), \code{"spearman"}, or \code{"kendall"}.
#' @param p.adjust Adjustment of \emph{p} values for multiple tests:
#' \code{"none"}, \code{"fdr"}, \code{"holm"}, \code{"bonferroni"}, ...
#' For details, see \code{stats::\link[stats]{p.adjust}}.
#' @param plot \code{TRUE} (default) or \code{FALSE}. Plot the correlation matrix.
#' @param plot.range Range of correlation coefficients for plot. Default is \code{c(-1, 1)}.
#' @param plot.palette Color gradient for plot. Default is \code{c("#B52127", "white", "#2171B5")}.
#' You may also set it to, e.g., \code{c("red", "white", "blue")}.
#' @param plot.color.levels Default is \code{201}.
#'
#' @return Invisibly return the correlation results obtained from \code{psych::corr.test}.
#'
#' @examples
#' Corr(airquality)
#' Corr(airquality, p.adjust="bonferroni")
#'
#' d=as.data.table(psych::bfi)
#' d[,`:=`(
#'   gender=as.factor(gender),
#'   education=as.factor(education),
#'   E=MEAN(d, "E", 1:5, rev=c(1,2), likert=1:6),
#'   A=MEAN(d, "A", 1:5, rev=1, likert=1:6),
#'   C=MEAN(d, "C", 1:5, rev=c(4,5), likert=1:6),
#'   N=MEAN(d, "N", 1:5, likert=1:6),
#'   O=MEAN(d, "O", 1:5, rev=c(2,5), likert=1:6)
#' )]
#' Corr(d[,.(age, gender, education, E, A, C, N, O)])
#'
#' @seealso \link{Describe}
#'
#' @importFrom stats p.adjust
#' @export
Corr=function(data, method="pearson", nsmall=2,
              p.adjust="none", all.as.numeric=TRUE,
              plot=TRUE, plot.range=c(-1, 1),
              plot.palette=NULL, plot.color.levels=201,
              save.file=NULL, width=8, height=6, dpi=500) {
  data.new=as.data.frame(data)
  vars.not.numeric=c()
  if(all.as.numeric) {
    for(var in names(data.new)) {
      if(!is.numeric(data.new[[var]])) {
        data.new[[var]]=as.numeric(data.new[[var]])
        vars.not.numeric=c(vars.not.numeric, var)
      }
    }
  }

  cor=cor0=psych::corr.test(data.new, method=method,
                            adjust=p.adjust,
                            minlength=20)

  # Print("Correlation matrix ({capitalize(method)}'s <<italic r>>):")
  # for(i in 1:nrow(cor$r)) cor$r[i,i]=NA
  # print_table(cor$r, nsmalls=nsmall)
  #
  # Print("\n\n\n<<italic p>> values (2-tailed):")
  # cor$p=gsub("=", " ", gsub(" ", "", p.trans2(cor$p)))
  # for(i in 1:nrow(cor$p)) cor$p[i,i]=""
  # print_table(cor$p)
  # if(p.adjust!="none")
  #   Print("<<blue <<italic p>> values above the diagonal are adjusted for multiple tests ({capitalize(p.adjust)} method).>>")
  #
  # if("matrix" %in% class(cor$n)) {
  #   Print("\n\n\nSample size:")
  #   print_table(cor$n, nsmalls=0)
  # } else {
  #   Print("\n\n\nSample size: <<italic N>> = {cor$n}")
  # }

  if(plot) {
    Print("\n\n\nCorrelation matrix is displayed in plot.")
    if(p.adjust!="none")
      Print("<<blue <<italic p>> values ABOVE the diagonal are adjusted using the \"{p.adjust}\" method.>>")
  } else {
    message("To see the correlation matrix, please set `plot=TRUE`!")
  }

  if("matrix" %in% class(cor$n))
    Ns=cor$n[lower.tri(cor$n)]
  else
    Ns=cor$n

  Print("\n\n\n{capitalize(method)}'s <<italic r>> and 95% confidence intervals:")
  COR=cor$ci["r"]
  if(p.adjust=="none") {
    COR$`[95% CI]`=paste0(
      "[", formatF(cor$ci$lower, nsmall), ", ",
      formatF(cor$ci$upper, nsmall), "]")
    COR$pval=cor$ci$p
  } else {
    Print("<<blue <<italic p>> values and 95% CIs are adjusted using the \"{p.adjust}\" method.>>")
    COR$`[95% CI]`=paste0(
      "[", formatF(cor$ci.adj$lower, nsmall), ", ",
      formatF(cor$ci.adj$upper, nsmall), "]")
    COR$pval=p.adjust(cor$ci$p, method=p.adjust)
  }
  COR$N=Ns
  print_table(COR, nsmalls=c(nsmall, 0, 0, 0))

  if(length(vars.not.numeric)>0)
      Print("<<yellow NOTE: `{paste(vars.not.numeric, collapse='`, `')}` transformed to numeric.>>")

  cor=cor0
  if(plot) {
    if(is.null(plot.palette))
      plot.palette=c("#B52127", "white", "#2171B5")
    if(!is.null(save.file)) {
      grDevices::png(filename=save.file, width=width, height=height, units="in", res=dpi)
    }
    cor_plot(r=cor$r, adjust="none", nsmall=nsmall,
             numbers=TRUE, zlim=plot.range,
             diag=FALSE, xlas=2, n=plot.color.levels,
             pval=cor$p, stars=TRUE,
             alpha=1, gr=grDevices::colorRampPalette(plot.palette),
             main="Correlation Matrix")
    if(!is.null(save.file)) {
      grDevices::dev.off()
      file=stringr::str_split(save.file, "/", simplify=TRUE)
      path=paste0(getwd(), '/', file[length(file)])
      Print("\n\n\n<<green \u2714>> Plot saved to <<blue '{path}'>>")
    }
  }

  invisible(cor)
}



## modified `psych::cor.plot()`
## see comment lines
cor_plot <- function (r, numbers = TRUE, colors = TRUE, n = 51, main = NULL,
  zlim = c(-1, 1), show.legend = TRUE, labels = NULL, n.legend = 10,
  select = NULL, pval = NULL, cuts = c(0.001, 0.01), scale = TRUE,
  cex, MAR, upper = TRUE, diag = TRUE,
  symmetric = TRUE, stars = FALSE, adjust = "holm", xaxis = 1,
  xlas = 0, ylas = 2, gr = NULL, alpha = 0.75, min.length = NULL,
  nsmall=2,  # added in bruceR
  ...)
{
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  if (missing(MAR))
    # MAR <- 5
    MAR <- 4
  if (!is.matrix(r) & (!is.data.frame(r))) {
    if ((length(class(r)) > 1) & (inherits(r, "psych"))) {
      switch(class(r)[2], omega = {
        r <- r$schmid$sl
        nff <- ncol(r)
        r <- r[, 1:(nff - 3)]
        if (is.null(main)) {
          main <- "Omega plot"
        }
      }, cor.ci = {
        pval <- 2 * (1 - r$ptci)
        r <- r$rho
      }, fa = {
        r <- r$loadings
        if (is.null(main)) {
          main <- "Factor Loadings plot"
        }
      }, pc = {
        r <- r$loadings
        if (is.null(main)) {
          main <- "PCA Loadings plot"
        }
      }, principal = {
        r <- r$loadings
        if (is.null(main)) {
          main <- "PCA Loadings plot"
        }
      })
    }
  }
  else {
    if (symmetric & !psych::isCorrelation(r) & (nrow(r) != ncol(r))) {
      cp <- psych::corr.test(r, adjust = adjust)
      r <- cp$r
      pval <- cp$p
      if (is.null(main)) {
        main <- "Correlation plot"
      }
    }
  }
  R <- r <- as.matrix(r)
  if (!is.null(select))
    r <- r[select, select]
  if (min(dim(r)) < 2) {
    stop("You need at least two dimensions to make a meaningful plot")
  }
  if (is.null(n)) {
    n <- dim(r)[2]
  }
  nf <- dim(r)[2]
  nvar <- dim(r)[1]
  if (!upper)
    r[col(r) > row(r)] <- NA
  if (!diag)
    r[col(r) == row(r)] <- NA
  if (nf == nvar)
    r <- t(r)
  if (missing(pval) | is.null(pval)) {
    pval <- matrix(rep(1, nvar * nf), nvar)
  }
  else {
    if (length(pval) != nvar * nf) {
      pr = matrix(0, nvar, nf)
      pr[row(pr) > col(pr)] <- pval
      pr <- pr + t(pr)
      diag(pr) <- 0
      pval <- pr
    }
    if (!stars) {
      pval <- psych::con2cat(pval, cuts = cuts)
      pval <- (length(cuts) + 1 - pval)/length(cuts)
    }
    pval <- t(pval)
  }
  if (is.null(labels)) {
    if (is.null(rownames(r)))
      rownames(r) <- paste("V", 1:nvar)
    if (is.null(colnames(r)))
      colnames(r) <- paste("V", 1:nf)
  }
  else {
    rownames(r) <- colnames(r) <- labels
  }
  if (!is.null(min.length)) {
    rownames(r) <- abbreviate(rownames(r), minlength = min.length)
    colnames(r) <- abbreviate(colnames(r), minlength = min.length)
  }
  max.len <- max(nchar(rownames(r)))/6
  if (is.null(zlim)) {
    zlim <- range(r)
  }
  if (colors) {
    if (missing(gr)) {
      gr <- grDevices::colorRampPalette(c("red", "white", "blue"))
    }
    if (max(r, na.rm = TRUE) > 1) {
      maxr <- max(r)
      n1 <- n * (zlim[2] - zlim[1])/(maxr - zlim[1])
      colramp <- rep(NA, n)
      n1 <- ceiling(n1)
      colramp[1:(n1 + 1)] <- gr(n1 + 1)
      colramp[(n1 + 1):n] <- colramp[n1 + 1]
      zlim[2] <- maxr
    }
    else {
      colramp <- gr(n)
    }
  }
  else {
    colramp <- grDevices::grey((n:0)/n)
  }
  colramp <- grDevices::adjustcolor(colramp, alpha.f = alpha)
  if (nvar != nf) {
    r <- t(r)
  }
  ord1 <- seq(nvar, 1, -1)
  if (nf == nvar) {
    r <- r[, ord1]
    pval <- pval[, ord1]
  }
  else {
    r <- r[, ord1]
    pval <- t(pval[ord1, ])
  }
  # graphics::par(mar = c(MAR + max.len, MAR + max.len, 4, 0.5))
  graphics::par(mar = c(MAR + max.len, MAR + max.len, 2.5, 0.5))
  if (show.legend) {
    graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(0.9, 0.1),
      heights = c(1, 1))
  }
  graphics::image(r, col = colramp, axes = FALSE, main = main, zlim = zlim)
  graphics::box()
  at1 <- (0:(nf - 1))/(nf - 1)
  at2 <- (0:(nvar - 1))/(nvar - 1)
  lab1 <- rownames(r)
  lab2 <- colnames(r)
  if (xaxis == 3) {
    line <- -0.5
    tick <- FALSE
  }
  else {
    line <- NA
    tick <- TRUE
  }
  if (max.len > 0.5) {
    graphics::axis(2, at = at2, labels = lab2, las = ylas, ...)
    graphics::axis(xaxis, at = at1, labels = lab1, las = xlas, line = line,
      tick = tick, ...)
  }
  else {
    graphics::axis(2, at = at2, labels = lab2, las = ylas, ...)
    graphics::axis(xaxis, at = at1, labels = lab1, las = xlas, line = line,
      tick = tick, ...)
  }
  if (numbers) {
    rx <- rep(at1, ncol(r))
    ry <- rep(at2, each = nrow(r))
    # rv <- round(r, 2)  # modified in bruceR
    rv <- formatF(r, nsmall)  # modified in bruceR
    if (stars) {
      symp <- stats::symnum(pval, corr = FALSE, cutpoints = c(0,
        0.001, 0.01, 0.05, 1), symbols = c("***", "**",
        "*", " "), legend = FALSE)
      rv[!is.na(rv)] <- paste0(rv[!is.na(rv)], symp[!is.na(rv)])
      rv <- gsub("NA.*", "", rv)  # modified in bruceR
      if (missing(cex))
        cex = 9/max(nrow(r), ncol(r))
      graphics::text(rx, ry, rv, cex = cex, ...)
    }
    else {
      if (missing(cex))
        cex = 9/max(nrow(r), ncol(r))
      if (scale) {
        graphics::text(rx, ry, rv, cex = pval * cex, ...)
      }
      else {
        graphics::text(rx, ry, rv, cex = cex, ...)
      }
    }
  }
  if (show.legend) {
    leg <- matrix(seq(from = zlim[1], to = zlim[2], by = (zlim[2] -
      zlim[1])/n), nrow = 1)
    # graphics::par(mar = c(MAR, 0, 4, 3))
    graphics::par(mar = c(MAR, 0, 2.5, 3))
    graphics::image(leg, col = colramp, axes = FALSE, zlim = zlim)
    at2 <- seq(0, 1, 1/n.legend)
    labels = seq(zlim[1], zlim[2], (zlim[2] - zlim[1])/(length(at2) -
      1))
    graphics::axis(4, at = at2, labels = labels, las = 2, ...)
  }
  invisible(R)
}


#' Test the difference between two correlations.
#'
#' @param r1,r2 Correlation coefficients (Pearson's \emph{r}).
#' @param n,n1,n2 Sample sizes.
#' @param rcov [optional] Only for nonindependent \emph{r}s:
#'
#' \code{r1} is r(X,Y),
#'
#' \code{r2} is r(X,Z),
#'
#' then, as Y and Z are also correlated,
#'
#' we should also consider \code{rcov}: r(Y,Z)
#'
#' @return Invisibly return the \emph{p} value.
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
    (<<italic N>> = {formatN(n)}, <<italic r>>_cov = {formatF(rcov)})
    Difference of correlation: {p(t=tdiff, df=n-3)}
    ")
  }
  invisible(p)
}

