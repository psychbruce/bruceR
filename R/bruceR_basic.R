##  Install Package:  'Ctrl + Shift + B'
##  Check Package:    'Ctrl + Shift + E'
##  Test Package:     'Ctrl + Shift + T'


#' bruceR: BRoadly Useful Collections and Extensions of R functions
#'
#' Packing many useful R packages and functions into one package.
#' Making data analyses and statistics more elegant.
#'
#' @section Main Functions in \code{bruceR}:
#' \code{bruceR} includes functions for 1) basic analyses, 2) multivariate computing (e.g., scale mean),
#' 3) questionnaire analyses (e.g., reliability, EFA, CFA), 4) advanced outputs for LM, GLM, HLM (LMM), and GLMM,
#' 5) nice themes for \code{ggplot2}, and 6) drawing standard China maps.
#'
#' \describe{
#'   \item{\strong{Basic Functions}}{
#'       \code{\link{set.wd}}
#'
#'       \code{\link{set.seeds}}
#'
#'       \code{\link{Print}} (see also \code{\link{Glue}})
#'
#'       \code{\link{print_table}}
#'
#'       \code{\link{p}}
#'
#'       \code{\link{Describe}} (descriptive statistics)
#'
#'       \code{\link{Freq}} (frequency statistics with plot)
#'
#'       \code{\link{Corr}} (correlation analysis with plot)
#'   }
#'   \item{\strong{Multivariate Computing}}{
#'       \code{\link{COUNT}}
#'
#'       \code{\link{SUM}}
#'
#'       \code{\link{MEAN}}
#'
#'       \code{\link{STD}}
#'
#'       \code{\link{CONSEC}}
#'
#'       \code{\link{RECODE}}
#'
#'       \code{\link{RESCALE}}
#'
#'       \code{\link{LOOKUP}}
#'   }
#'   \item{\strong{Reliability, EFA, & CFA}}{
#'       \code{\link{Alpha}}
#'
#'       \code{\link{EFA}}
#'
#'       \code{\link{CFA}}
#'   }
#'   \item{\strong{MANOVA, Simple Effects, & Multiple Comparisons}}{
#'       \emph{(Coming soon...)}
#'   }
#'   \item{\strong{Linear Models}}{
#'       \code{\link{regress}} (do many kinds of regression analyses in one function)
#'
#'       \code{\link{grand_mean_center}}
#'
#'       \code{\link{group_mean_center}}
#'
#'       \code{\link{GLM_summary}}
#'
#'       \code{\link{HLM_summary}}
#'   }
#'   \item{\strong{Mediation & Moderation}}{
#'       \code{\link{med}}
#'
#'       \code{\link{mod_med}}
#'
#'       \code{\link{simple_slope}}
#'   }
#'   \item{\strong{Plotting with \code{ggplot2}}}{
#'       \code{\link{theme_bruce}}
#'
#'       \code{\link{drawChinaMap}}
#'   }
#' }
#'
#' @note
#' \emph{Please always use \href{https://www.rstudio.com/products/rstudio/download/preview/}{RStudio} as an \href{https://en.wikipedia.org/wiki/Integrated_development_environment}{IDE} instead of using the raw R.}
#'
#' @author \href{https://www.zhihu.com/people/psychbruce/}{Han-Wu-Shuang (Bruce) Bao} <E-mail: \email{baohws@@psych.ac.cn} or \email{psychbruce@@qq.com}>
#'
#' @docType package
#' @name bruceR-package
#' @aliases bruceR
NULL


.onAttach=function(libname, pkgname) {
  # suppressPackageStartupMessages({
  #   library(rio)
  #   library(glue)
  #   library(dplyr)
  #   library(stringr)
  #   library(data.table)
  #   library(psych)
  #   library(ggplot2)
  # })

  # packageStartupMessage("Welcome to my package")

  # <<magenta {rep_char('=', 10)} {Sys.Date()} {rep_char('=', 10)}>>
  # <<magenta {rep_char('=', 10)} User Guide {rep_char('=', 10)}>>

  pkgs=.packages(all.available=T)
  pkgs.bruceR=c("tidyverse", "ggstatsplot", "MBESS", "MuMIn",
                "jtools", "summarytools", "texreg", "semPlot")
  for(pkg in pkgs.bruceR) {
    if(pkg %notin% pkgs) {
      Print("Package '<<red {pkg}>>' will be installed...")
      install.packages(pkg)
    }
  }
  rm(pkg, pkgs, pkgs.bruceR)

  Print("
  <<bold <<blue
  <<magenta {rep_char('=', 10)} bruceR (version 0.1.0) {rep_char('=', 10)}>>
  The most commonly used R packages are also loaded:
  <<green 'rio', 'glue', 'dplyr', 'stringr', 'data.table', 'psych', 'ggplot2'>>

  For an overview and introduction, please type:  ?bruceR
  Run some examples:  example(\"regress\")

  <<silver * Check updates of 'bruceR' on <<underline https://github.com/psychbruce/bruceR>>>>
  >>>>
  ")
}


#### Basic Functions ####


#' A simple extension of \code{\%in\%}
#' @inheritParams base::`%in%`
#' @seealso \code{\link[base]{match}} (\code{\%in\%})
#' @export
`%notin%`=function(x, table) {
  match(x, table, nomatch = 0) == 0
}


#' A simple extension of \code{\%in\%}
#' @inheritParams base::`%in%`
#' @seealso \code{\link[base]{match}} (\code{\%in\%})
#' @export
`%allin%`=function(x, table) {
  all(x %in% table)
}


#' A simple extension of \code{\%in\%}
#' @inheritParams base::`%in%`
#' @seealso \code{\link[base]{match}} (\code{\%in\%})
#' @export
`%anyin%`=function(x, table) {
  any(x %in% table)
}


#' A simple extension of \code{\%in\%}
#' @param pattern a character string containing \strong{regular expressions} to be matched in the given character vector.
#' @param vector a character vector.
#' @seealso \code{\link[base]{match}} (\code{\%in\%})
#' @export
`%partin%`=function(pattern, vector) {
  any(grepl(pattern, vector, perl=TRUE))
}


#' Set working directory to the path of \strong{current} script
#'
#' @import rstudioapi
#' @param dir A character string. If \code{NULL} (default), set working directory to the path of the current R script.
#' @examples
#' set.wd()  # set working directory to the path of the current R script
#' set.wd("E:/")  # "\" is not allowed, you should use "/"
#' set.wd("../")  # set working directory to the parent directory
#' @export
set.wd=function(dir=NULL) {
  if(is.null(dir)) dir=dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(dir)
  path=getwd()
  Print("<<green \u2714>> Setting working directory to <<blue '{path}'>>")
}


#' Set random seeds with a specific version of R
#'
#' The new versions of R (>= 3.6.0) have changed the mechanism of generating random numbers.
#' To have an exact replication of previous results that were based on random numbers, the version of R should be specified.
#' By default, it sets the version to "3.5.0". Note that versions earlier than 3.6.0 will all generate the same result.
#'
#' @examples
#' set.seeds(1, version="3.5.0")
#' sample(1:10)  # 3  4  5  7  2  8  9  6 10  1
#' @export
set.seeds=function(seed, version="3.5.0") {
  suppressWarnings(RNGversion(version))
  set.seed(seed)
}


#' Check package dependencies
#' @param pkg package name
#' @examples
#' pkg.depend("sjstats")
#' pkg.depend("MuMIn")
#' @export
pkg_depend=function(pkg) {
  default.pkgs=c("base", "boot", "class", "cluster", "codetools", "compiler",
                 "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth",
                 "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet", "parallel",
                 "rpart", "spatial", "splines", "stats", "stats4", "survival",
                 "tclck", "tools", "translation", "utils")
  pkgs=unlist(tools::package_dependencies(pkg, recursive=TRUE))
  pkgs=sort(setdiff(union(pkg, pkgs), default.pkgs))
  packages=data.frame(Package=pkgs, Description=mapply(packageDescription, pkgs, fields="Title"))
  View(packages, pkg)
  invisible(packages)
}


#' Print a string with fruitful formats in a concise manner
#'
#' Be tired of \code{print()} and \code{cat()}? Try this function! Just type \code{example("Print")} in the console and see its power.
#'
#' This function is based on \code{\link[glue]{glue}} and \code{\link[glue]{glue_col}}.
#' See more details in their help pages.
#' @import glue
#' @importFrom crayon reset bold italic underline strikethrough black silver white red green blue yellow magenta cyan
#' @param ... A string in which expressions enclosed by \code{"{ }"} will be evaluated as R code.
#' Long strings are broken by line and concatenated together. Leading whitespace and blank lines from the first and last lines are automatically trimmed.
#' @return A string.
#' @examples
#' name="Bruce"
#' Print("My name is <<underline <<bold {name}>>>>.
#'        <<bold <<blue Pi = {pi:.15}.>>>>
#'        <<italic <<green 1 + 1 = {1 + 1}.>>>>
#'        sqrt({x}) = <<red {sqrt(x):.3}>>", x=10)
#' @export
Print=function(...) {
  tryCatch({
    output=glue(..., .transformer=sprintf_transformer, .envir=parent.frame())
    output_color=glue_col( gsub("<<", "{", gsub(">>", "}", output)) )
    print(output_color)
  }, error=function(e) {
    warning(e)
    print(...)
  })
}


#' @describeIn Print Glue strings
#' @export
Glue=function(...) {
  output=glue(..., .transformer=sprintf_transformer, .envir=parent.frame())
  output_color=glue_col( gsub("<<", "{", gsub(">>", "}", output)) )
  return(output_color)
}


sprintf_transformer=function(text, envir) {
  text=glue(text, .envir=envir)
  m=regexpr(":.+$", text)
  if(m!=-1) {
    format=substring(regmatches(text, m), 2)
    regmatches(text, m)=""
    res=eval(parse(text=text, keep.source=FALSE), envir)
    do.call(sprintf, list(glue("%{format}f"), res))
  } else {
    eval(parse(text=text, keep.source=FALSE), envir)
  }
}


#' Repeat a character for many times and paste
#' @export
rep_char=function(char, rep.times) {
  paste(rep(char, rep.times), collapse="")
}


#' Print three-line table
#' @param x a matrix, data.frame, data.table, or a model (\code{lm, glm, lmer, glmer, ...}).
#' @examples
#' model=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
#' print_table(model)
#' @export
print_table=function(x, row.names=TRUE, nsmalls=3,
                     style=c("line", "data")) {
  ## Preprocess data.frame ##
  linechar1="\u2501" # top-and-down '=' [bug in some computers!]
  linechar2="\u2500" # in-table '-'
  linechar1=linechar2
  if(length(style)==2) style="line"
  if(!class(x) %anyin% c("matrix", "data.frame", "data.table")) {
    coef.table=coef(summary(x))
    if(!is.null(coef.table)) x=coef.table
  }
  x=as.data.frame(x)
  sig=NULL
  if(length(nsmalls)==1) nsmalls=rep(nsmalls, length(x))
  for(j in 1:length(x)) {
    if(grepl("Pr\\(|pval", names(x)[j])) {
      sig=formatF(sig.trans(x[,j]), 0) # formatF will make * left-aligned
      names(x)[j]="p"
      x[,j]=p.trans(x[,j])
    } else {
      x[,j]=formatF(x[,j], nsmalls[j])
    }
    if(grepl("S\\.E\\.|Std\\. Error|^se$", names(x)[j])) {
      x[,j]=paste0("(", x[,j], ")") # add ( ) to S.E.
      x[grepl("\\.", x[,j])==FALSE, j]="" # remove ( ) from blank S.E.
      if(grepl("S\\.E\\.", names(x)[j])==FALSE) names(x)[j]="S.E."
    }
    if(grepl("\\[", names(x)[j])) x[,j]=paste0("[", x[,j], ",")
    if(grepl("\\]", names(x)[j])) x[,j]=paste0(x[,j], "]")
    if(grepl("^[Ee]stimate$", names(x)[j])) names(x)[j]="Coef."
    names(x)[j]=gsub(" value|val$", "", names(x)[j])
  }
  if(!is.null(sig)) {x=cbind(x, ` `=sig); x$` `=as.character(x$` `)}

  ## Compute length to generate line-chars ##
  title.length=nchar(names(x))
  vars.length=c() # bug: vars.length=apply(apply(x, 2, nchar), 2, max)
  for(j in 1:length(x)) vars.length[j]=max(nchar(x[,j]))

  ## Generate a row with 'linechar2' ##
  n.lines=apply(rbind(title.length, vars.length), 2, max)+1
  n.lines.rn=max(nchar(row.names(x)))+1
  n.lines.table=n.lines.rn+sum(n.lines)+ifelse(style=="line", 0, length(x))
  line.row=data.frame()
  for(j in 1:length(x)) line.row[1,j]=rep_char(linechar2, n.lines[j])
  names(line.row)=names(x)
  row.names(line.row)[1]=rep_char(linechar2, n.lines.rn)

  ## Rbind and deal with 'row.names' (T or F) ##
  x=rbind(line.row, x)
  if(row.names==F & style=="line")
    n.lines.table=n.lines.table-n.lines.rn
  if(row.names==F & style=="data")
    n.lines.table=n.lines.table-n.lines.rn-1
  table.line=rep_char(linechar1, n.lines.table)
  if(row.names==F & style=="data")
    table.line=paste0(" ", table.line)

  ## Output ##
  cat(table.line); cat("\n")
  if(style=="line") {
    if(row.names==T) cat(rep_char(" ", n.lines.rn))
    for(j in 1:length(x)) cat(sprintf(glue("% {n.lines[j]}s"), names(x)[j]))
    cat("\n")
    for(i in 1:nrow(x)) {
      if(row.names==T) cat(sprintf(glue("%-{n.lines.rn}s"), row.names(x[i,])))
      for(j in 1:length(x)) cat(sprintf(glue("% {n.lines[j]}s"), x[i,j]))
      cat("\n")
    }
  } else if(style=="data") {
    print(x, row.names=row.names)
  }
  cat(table.line); cat("\n")
}

# Good-looking tabs !!!
# Print("\u2500\u2501\u2502\u2503\u2504\u2505\u2506\u2507\u2508\u2509")


#' Format "1234" to "1,234"
#' @param x any R object, typically numeric.
#' @return Formatted R object.
#' @export
formatN=function(x, mark=",") {
  format(x, big.mark=mark)
}


#' Format numeric values
#' @param x any R object, typically numeric.
#' @return Formatted R object.
#' @export
formatF=function(x, nsmall=3) {
  format(x, digits=0, nsmall=nsmall, scientific=F)
}


#' A simple extension of \code{rgb()}
#' @param r,g,b an integar \emph{[0, 255]} indicating red, green, and blue
#' @param alpha a value \emph{[0, 1]} indicating color opacity (transparency); if not specified, an opaque colour will be generated
#' @return \code{"#rrggbb"} or \code{"#rrggbbaa"}
#' @examples
#' RGB(255, 0, 0)  # red: "#FF0000"
#' RGB(255, 0, 0, )  # red with 80\% opacity: "#FF0000CC"
#' @export
RGB=function(r, g, b, alpha) {
  rgb(r/255, g/255, b/255, alpha)
}


#' Timer
#' @param t0 Time at the beginning, typically: \code{t0 = Sys.time()}
#' @param unit A value from \code{c("auto", "secs", "mins", "hours", "days", "weeks")}. Default is \code{"secs"}.
#' @return A string.
#' @examples
#' t0=Sys.time()
#' dtime(t0)
#' @export
dtime=function(t0, unit="secs") {
  # secs, mins, hours
  format(difftime(Sys.time(), t0, units=unit), digits=0)
}




#### Excel-Style Functions ####


#' Randomly sampling (like Excel's function \code{RANDBETWEEN})
#' @param range numeric vector, or character vector.
#' @param n sample size.
#' @param seed random seed.
#' @examples
#' RANDBETWEEN(1:10, n=1000000) %>% Freq()
#' RANDBETWEEN(LETTERS, n=1000000) %>% Freq()
#' @export
RANDBETWEEN=function(range, n=1, seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  # floor(runif(n=n, min=bottom, max=up+1))
  sample(range, n, replace=TRUE)
}




#### Basic Statistics ####


#' Compute \emph{p} value
#' @examples
#' ## example("p")
#'
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
#' @export
p.z=function(z) pnorm(abs(z), lower.tail=FALSE)*2

#' @describeIn p Two-tailed \emph{p} value of \emph{t}.
#' @export
p.t=function(t, df) pt(abs(t), df, lower.tail=FALSE)*2

#' @describeIn p One-tailed \emph{p} value of \emph{F}. (Note: \emph{F} test is one-tailed only.)
#' @export
p.f=function(f, df1, df2) pf(f, df1, df2, lower.tail=FALSE)

#' @describeIn p Two-tailed \emph{p} value of \emph{r}.
#' @export
p.r=function(r, n) p.t(r/sqrt((1-r^2)/(n-2)), n-2)

#' @describeIn p One-tailed \emph{p} value of \eqn{\chi}^2. (Note: \eqn{\chi}^2 test is one-tailed only.)
#' @export
p.chi2=function(chi2, df) pchisq(chi2, df, lower.tail=FALSE)


# p.trans=function(p, nsmall.p=5) {
#   ifelse(p<2e-16, "< 2e-16",
#          ifelse(p<10^-nsmall.p, format(p, digits=2, scientific=T),
#                 format(p, digits=0, nsmall=nsmall.p, scientific=F)))
# }

# p.trans=function(p, nsmall.p=3) {
#   ifelse(is.na(p), "",
#          ifelse(p < 1e-10, sprintf(glue("% {nsmall.p+3}s"), "<1e-10"),
#                 ifelse(p < 10^-nsmall.p, sprintf(glue("% {nsmall.p+3}.0e"), p),
#                        sprintf(glue("% {nsmall.p+3}.{nsmall.p}f"), p))))
# }

p.trans=function(p, nsmall.p=3) {
  mapply(function(p, nsmall.p) {
    ifelse(is.na(p) | p > 1 | p < 0, "",
           ifelse(p < 10^-nsmall.p, gsub("0(?=\\.)", "", Glue("<{10^-nsmall.p:.{nsmall.p}}"), perl=T),
                  gsub("0(?=\\.)", " ", Glue("{p:.{nsmall.p}}"), perl=T)))
  }, p, nsmall.p)
}

# p.trans(c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0.0000001, 1e-100)) %>% as.data.frame()

p.trans2=function(p, nsmall.p=3) {
  ifelse(is.na(p) | p > 1 | p < 0, "",
         ifelse(p < 1e-10, "< 1e-10",
                ifelse(p < 10^-nsmall.p, paste("=", format(p, digits=1, scientific=T)),
                       paste("=", format(p, digits=0, nsmall=nsmall.p, scientific=F)))))
}


sig.trans=function(p) {
  ifelse(is.na(p) | p > 1 | p < 0, "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*",
                              ifelse(p < .10, ".", "")))))
}


#' Compute 95\% confidence interval (CI) of a variable
#' @param var a variable (e.g., data$var) or a numeric vector
#' @export
CI=function(var, nsmall=2) {
  Print("Mean = {mean(var):.{nsmall}}, 95% CI [{quantile(var, 0.025):.{nsmall}}, {quantile(var, 0.975):.{nsmall}}]")
}


#' Describe data
#' @export
Describe=function(data, nsmall=2) {
  Print("Descriptive statistics:")
  desc=psych::describe(data, fast=FALSE)
  desc$vars = desc$trimmed = desc$mad = desc$range = desc$se = NULL
  names(desc)=c("N", "Mean", "SD", "Median", "Min", "Max", "Skewness", "Kurtosis")
  if(length(as.data.frame(data))==1) row.names(desc)=as.character(sys.call())[2]
  print_table(desc, nsmall=c(0, rep(nsmall, 7)))
  invisible(desc)
}


#' Frequency statistics with histogram and density plot
#' @import ggplot2
#' @examples
#' Freq(psych::bfi$education)
#' Freq(psych::bfi$gender, label=c("Male", "Female"))
#' Freq(psych::bfi$age, plot=T)
#' @export
Freq=function(var, label=NULL, sort="",
              nsmall=1,
              plot=FALSE, bins=18, fill.color="#FDF6E3") {
  Print("Frequency table:")
  tableVar=table(var)
  N.na=sum(is.na(var))
  N=sum(tableVar)+N.na
  if(is.null(label)) label=names(tableVar)
  output=cbind(matrix(tableVar,
                      dimnames=list(label, "N")),
               matrix(round(tableVar/N*100, nsmall),
                      dimnames=list(label, "%")))
  if(N.na) output=rbind(output, matrix(c(N.na, round(N.na/N*100, nsmall)),
                                       ncol=2, dimnames=list("NA")))
  if(sort=="")
    print_table(output, nsmall=c(0, nsmall))
  else if(sort=="-")
    print_table(output[order(output[,"N"], decreasing=T),], nsmall=c(0, nsmall))
  else if(sort=="+")
    print_table(output[order(output[,"N"], decreasing=F),], nsmall=c(0, nsmall))
  Print("Total <<italic N>> = {formatN(N)}")
  if(N.na>0) Print("Valid <<italic N>> = {formatN(N-N.na)}")
  if(plot) {
    # hist(var, xlab=deparse(substitute(var)),
    #      main=paste("Histogram of", deparse(substitute(var))))
    p=ggplot(NULL, aes(x=var)) +
      geom_histogram(aes(y=..density..),
                     bins=min(length(unique(var)), bins),
                     color="black", fill=fill.color) +
      # geom_density(color=F, fill=fill.color, alpha=0.4) +
      # geom_line(stat="density") +
      labs(x=deparse(substitute(var)), y="Density") +
      theme_bruce(grid.y=F)
    print(p)
  }
  invisible(output)
}

## @rdname Freq
## @export
## freq=Freq


#' Correlation analysis with test and plot
#' @importFrom psych corr.test cor.plot
#' @export
Corr=function(data, method="pearson", digits=4, short=FALSE, plot=TRUE) {
  cor=corr.test(data, adjust="none", method=method)
  print(cor, digits=digits, short=short)
  if(plot) cor.plot(cor$r, adjust="none", numbers=TRUE, diag=FALSE, pval=cor$p, stars=TRUE)
  # partial correlation:
  # print(corpcor::cor2pcor(cor(data)), digits=4)
  invisible(cor)
}

## @rdname Corr
## @export
## corr=Corr


