#### Data Manipulation ####


#' Create, modify, and delete variables.
#'
#' Enhanced functions to create, modify, and/or delete variables.
#' The functions \strong{combine} the advantages of
#' \code{\link[base:within]{within}} (base),
#' \code{\link[dplyr:mutate]{mutate}} (dplyr),
#' \code{\link[dplyr:transmute]{transmute}} (dplyr), and
#' \code{\link[data.table:data.table]{:=}} (data.table).
#' See examples below for the usage and convenience.
#'
#' @param data A \code{\link[data.table:data.table]{data.table}}
#' (preferred).
#' @param expr R expression(s) enclosed in \code{{...}} to compute variables.
#'
#' Passing to \code{\link[data.table:data.table]{data.table}}:
#' \code{DT[ , `:=`(expr), ]}
#'
#' Execute each line of expression in \code{{...}} \emph{one by one},
#' such that newly created variables are available immediately.
#' This is an advantage of \code{\link[dplyr:mutate]{mutate}} and
#' has been implemented here for \code{\link[data.table:data.table]{data.table}}.
#' @param when [Optional] Compute for which rows or rows meeting what condition(s)?
#'
#' Passing to \code{\link[data.table:data.table]{data.table}}:
#' \code{DT[when, , ]}
#' @param by [Optional] Compute by what group(s)?
#'
#' Passing to \code{\link[data.table:data.table]{data.table}}:
#' \code{DT[ , , by]}
#' @param drop Drop existing variables and return only new variables?
#' Default is \code{FALSE}, which returns all variables.
#'
#' @return
#' \code{add()} returns a new
#' \code{\link[data.table:data.table]{data.table}},
#' with the raw data unchanged.
#'
#' \code{added()} returns nothing and has already changed the raw data.
#'
#' @examples
#' ## ====== Usage 1: add() ====== ##
#'
#' d = as.data.table(within.1)
#' d$XYZ = 1:8
#' d
#'
#' # add() does not change the raw data:
#' add(d, {B = 1; C = 2})
#' d
#'
#' # new data should be assigned to an object:
#' d = d %>% add({
#'   ID = str_extract(ID, "\\d")  # modify a variable
#'   XYZ = NULL                   # delete a variable
#'   A = .mean("A", 1:4)          # create a new variable
#'   B = A * 4    # new variable is immediately available
#'   C = 1        # never need ,/; at the end of any line
#' })
#' d
#'
#'
#' ## ====== Usage 2: added() ====== ##
#'
#' d = as.data.table(within.1)
#' d$XYZ = 1:8
#' d
#'
#' # added() has already changed the raw data:
#' added(d, {B = 1; C = 2})
#' d
#'
#' # raw data has already become the new data:
#' added(d, {
#'   ID = str_extract(ID, "\\d")
#'   XYZ = NULL
#'   A = .mean("A", 1:4)
#'   B = A * 4
#'   C = 1
#' })
#' d
#'
#'
#' ## ====== Using `when` and `by` ====== ##
#'
#' d = as.data.table(between.2)
#' d
#'
#' added(d, {SCORE2 = SCORE - mean(SCORE)},
#'       A == 1 & B %in% 1:2,  # `when`: for what conditions
#'       by=B)                 # `by`: by what groups
#' d
#' na.omit(d)
#'
#'
#' ## ====== Return Only New Variables ====== ##
#'
#' newvars = add(within.1, {
#'   ID = str_extract(ID, "\\d")
#'   A = .mean("A", 1:4)
#' }, drop=TRUE)
#' newvars
#'
#'
#' ## ====== Better Than `base::within()` ====== ##
#'
#' d = as.data.table(within.1)
#'
#' # wrong order: C B A
#' within(d, {
#'   A = 4
#'   B = A + 1
#'   C = 6
#' })
#'
#' # correct order: A B C
#' add(d, {
#'   A = 4
#'   B = A + 1
#'   C = 6
#' })
#'
#' @describeIn
#' add Return the \emph{new data}.
#'
#' You need to assign the new data to an object:
#'
#' \preformatted{data = add(data, {...})}
#'
#' @export
add = function(data, expr, when, by, drop=FALSE) {
  data = as.data.table(data)
  exprs = as.character(substitute(expr))
  when = deparse(substitute(when))
  by = deparse(substitute(by))
  if(exprs[1]!="{")
    stop("Please use { } for expressions.\nSee: help(add)", call.=FALSE)
  for(e in exprs[-1]) {
    es = str_split(e, " \\= | <- ", n=2, simplify=TRUE)[1,]
    if(str_detect(es[2], "^\\.(mean|sum)\\(.*\\)$"))
      e = paste(es[1], "=", eval(parse(text=es[2])))
    else if(str_detect(es[2], "\\.(mean|sum)\\("))
      stop("Please use ONLY `.mean()` or `.sum()` for one line, without any other expression.", call.=FALSE)
    eval(parse(text=glue("data[{when}, `:=`({e}), {by}][]")))
  }
  if(drop) {
    dn = names(data)
    dn.new = str_split(exprs[-1], " \\= | <- ", n=2, simplify=TRUE)[,1]
    dn.drop = base::setdiff(dn, dn.new)
    data[, (dn.drop) := NULL][]
  }
  return(data)
}


#' @describeIn
#' add Return nothing and \emph{change the raw data immediately}.
#'
#' NO need to assign the new data:
#'
#' \preformatted{added(data, {...})}
#'
#' @export
added = function(data, expr, when, by, drop=FALSE) {
  if(!is.data.table(data))
    stop("Data must be a `data.table`!\nSee: help(added)", call.=FALSE)
  exprs = as.character(substitute(expr))
  when = deparse(substitute(when))
  by = deparse(substitute(by))
  if(exprs[1]!="{")
    stop("Please use { } for expressions.\nSee: help(add)", call.=FALSE)
  for(e in exprs[-1]) {
    es = str_split(e, " \\= | <- ", n=2, simplify=TRUE)[1,]
    if(str_detect(es[2], "^\\.(mean|sum)\\(.*\\)$"))
      e = paste(es[1], "=", eval(parse(text=es[2])))
    else if(str_detect(es[2], "\\.(mean|sum)\\("))
      stop("Please use ONLY `.mean()` or `.sum()` for one line, without any other expression.", call.=FALSE)
    eval(parse(text=glue("data[{when}, `:=`({e}), {by}][]")))
  }
  if(drop) {
    dn = names(data)
    dn.new = str_split(exprs[-1], " \\= | <- ", n=2, simplify=TRUE)[,1]
    dn.drop = base::setdiff(dn, dn.new)
    data[, (dn.drop) := NULL][]
  }
  Print("<<green \u221a>> Raw data has already been changed. Please check.")
  invisible(data)
}


#' Recode a variable.
#'
#' A wrapper of \code{\link[car:recode]{car::recode()}}.
#'
#' @param var Variable (numeric, character, or factor).
#' @param recodes A character string definine the rule of recoding. e.g., \code{"lo:1=0; c(2,3)=1; 4=2; 5:hi=3; else=999"}
#'
#' @return A vector of recoded variable.
#'
#' @examples
#' d = data.table(var=c(NA, 0, 1, 2, 3, 4, 5, 6))
#' added(d, {
#'   var.new = RECODE(var, "lo:1=0; c(2,3)=1; 4=2; 5:hi=3; else=999")
#' })
#' d
#'
#' @export
RECODE = function(var, recodes) {
  car::recode(var, recodes)
}


#' Rescale a variable (e.g., from 5-point to 7-point).
#'
#' @param var Variable (numeric).
#' @param from Numeric vector, the range of old scale (e.g., \code{1:5}).
#' If not defined, it will compute the range of \code{var}.
#' @param to Numeric vector, the range of new scale (e.g., \code{1:7}).
#'
#' @return A vector of rescaled variable.
#'
#' @examples
#' d = data.table(var=rep(1:5, 2))
#' added(d, {
#'   var1 = RESCALE(var, to=1:7)
#'   var2 = RESCALE(var, from=1:5, to=1:7)
#' })
#' d  # var1 is equal to var2
#'
#' @export
RESCALE = function(var, from=range(var, na.rm=T), to) {
  (var - median(from)) / (max(from) - median(from)) * (max(to) - median(to)) + median(to)
}


#' Min-max scaling (min-max normalization).
#'
#' This function resembles \code{\link[bruceR:RESCALE]{RESCALE()}}
#' and it is just equivalent to \code{RESCALE(var, to=0:1)}.
#'
#' @param v Variable (numeric vector).
#' @param min Minimum value (default is 0).
#' @param max Maximum value (default is 1).
#'
#' @return A vector of rescaled variable.
#'
#' @examples
#' scaler(1:5)
#' # the same: RESCALE(1:5, to=0:1)
#'
#' @export
scaler = function(v, min=0, max=1) {
  min + (v - min(v, na.rm=T)) * (max - min) / (max(v, na.rm=T) - min(v, na.rm=T))
}


#### Significance Test and Report ####


#' Compute \emph{p} value.
#'
#' @param z,t,f,r,chi2 \emph{z}, \emph{t}, \emph{F}, \emph{r}, \eqn{\chi}^2 value.
#' @param n,df,df1,df2 Sample size or degree of freedom.
#' @param digits,nsmall Number of decimal places of output. Default is \code{2}.
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
p = function(z=NULL, t=NULL, f=NULL, r=NULL, chi2=NULL,
             n=NULL, df=NULL, df1=NULL, df2=NULL, digits=2, nsmall=digits) {
  if(!is.null(z)) {p = p.z(z); pstat = Glue("<<italic z>> = {z:.{nsmall}}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(t)) {p = p.t(t, df); pstat = Glue("<<italic t>>({df}) = {t:.{nsmall}}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(f)) {p = p.f(f, df1, df2); pstat = Glue("<<italic F>>({df1}, {df2}) = {f:.{nsmall}}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(r)) {p = p.r(r, n); pstat = Glue("<<italic r>>({n-2}) = {r:.{nsmall}}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(chi2)) {p = p.chi2(chi2, df); pstat = Glue("\u03c7\u00b2({df}{ifelse(is.null(n), '', ', <<italic N>> = ' %^% n)}) = {chi2:.{nsmall}}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  return(pstat)
}

p.plain = function(z=NULL, t=NULL, f=NULL, r=NULL, chi2=NULL,
                   n=NULL, df=NULL, df1=NULL, df2=NULL, digits=2, nsmall=digits) {
  if(!is.null(z)) {p = p.z(z); pstat = Glue("z = {z:.{nsmall}}, p {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(t)) {p = p.t(t, df); pstat = Glue("t({df}) = {t:.{nsmall}}, p {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(f)) {p = p.f(f, df1, df2); pstat = Glue("F({df1}, {df2}) = {f:.{nsmall}}, p {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(r)) {p = p.r(r, n); pstat = Glue("r({n-2}) = {r:.{nsmall}}, p {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(chi2)) {p = p.chi2(chi2, df); pstat = Glue("\u03c7\u00b2({df}{ifelse(is.null(n), '', ', N = ' %^% n)}) = {chi2:.{nsmall}}, p {p.trans2(p)} {sig.trans(p)}")}
  return(pstat)
}

#' @describeIn p Two-tailed \emph{p} value of \emph{z}.
#' @export
p.z = function(z) pnorm(abs(z), lower.tail=FALSE)*2

#' @describeIn p Two-tailed \emph{p} value of \emph{t}.
#' @export
p.t = function(t, df) pt(abs(t), df, lower.tail=FALSE)*2

#' @describeIn p One-tailed \emph{p} value of \emph{F}. (Note: \emph{F} test is one-tailed only.)
#' @export
p.f = function(f, df1, df2) pf(f, df1, df2, lower.tail=FALSE)

#' @describeIn p Two-tailed \emph{p} value of \emph{r}.
#' @export
p.r = function(r, n) p.t(r/sqrt((1-r^2)/(n-2)), n-2)

#' @describeIn p One-tailed \emph{p} value of \eqn{\chi}^2. (Note: \eqn{\chi}^2 test is one-tailed only.)
#' @export
p.chi2 = function(chi2, df) ifelse(df==0, 1, pchisq(chi2, df, lower.tail=FALSE))


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
p.trans = function(p, nsmall.p=3) {
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
p.trans2 = function(p, nsmall.p=3, p.min=1e-99) {
  ifelse(is.na(p) | p > 1 | p < 0, "",
         ifelse(p < p.min, paste("<", p.min),
                ifelse(p < 10^-nsmall.p, paste("=", format(p, digits=1, scientific=TRUE)),
                       paste("=", formatF(p, nsmall=nsmall.p)))))
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
sig.trans = function(p) {
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
#' @param all.as.numeric \code{TRUE} (default) or \code{FALSE}.
#' Transform all variables into numeric (continuous).
#' @param digits,nsmall Number of decimal places of output. Default is \code{2}.
#' @param file File name of MS Word (\code{.doc}).
#' @param plot \code{TRUE} or \code{FALSE} (default).
#' Visualize the descriptive statistics using \code{\link[GGally:ggpairs]{GGally::ggpairs()}}.
#' @param upper.triangle \code{TRUE} or \code{FALSE} (default).
#' Add (scatter) plots to upper triangle (time consuming when sample size is large).
#' @param upper.smooth \code{"none"} (default), \code{"lm"}, or \code{"loess"}.
#' Add fitting lines to scatter plots (if any).
#' @param plot.file \code{NULL} (default, plot in RStudio) or a file name (\code{"xxx.png"}).
#' @param plot.width Width (in "inch") of the saved plot. Default is \code{8}.
#' @param plot.height Height (in "inch") of the saved plot. Default is \code{6}.
#' @param plot.dpi DPI (dots per inch) of the saved plot. Default is \code{500}.
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
#' # ?psych::bfi
#' Describe(psych::bfi[c("age", "gender", "education")])
#'
#' d = as.data.table(psych::bfi)
#' added(d, {
#'   gender = as.factor(gender)
#'   education = as.factor(education)
#'   E = .mean("E", 1:5, rev=c(1,2), range=1:6)
#'   A = .mean("A", 1:5, rev=1, range=1:6)
#'   C = .mean("C", 1:5, rev=c(4,5), range=1:6)
#'   N = .mean("N", 1:5, range=1:6)
#'   O = .mean("O", 1:5, rev=c(2,5), range=1:6)
#' })
#' Describe(d[, .(age, gender, education)], plot=TRUE, all.as.numeric=FALSE)
#' Describe(d[, .(age, gender, education, E, A, C, N, O)], plot=TRUE)
#' }
#' @seealso \code{\link{Corr}}
#'
#' @export
Describe = function(data,
                    all.as.numeric=TRUE,
                    digits=2, nsmall=digits,
                    file=NULL,
                    plot=FALSE,
                    upper.triangle=FALSE, upper.smooth="none",
                    plot.file=NULL, plot.width=8, plot.height=6, plot.dpi=500) {
  if(is.numeric(data)) data = data.frame(X=data)
  desc = as.data.frame(psych::describe(data, fast=FALSE))
  desc$vars = desc$trimmed = desc$mad = desc$range = desc$se = NULL
  names(desc) = c("N", "Mean", "SD", "Median", "Min", "Max", "Skewness", "Kurtosis")
  desc$`|` = "|"
  desc$Missing = NA
  desc = desc[c("N", "Missing", "Mean", "SD", "|",
                "Median", "Min", "Max", "Skewness", "Kurtosis")]

  if(length(as.data.frame(data))==1)
    row.names(desc) = " "  # as.character(sys.call())[2]
  missing = nrow(data) - desc$N
  if(max(missing)==0) {
    desc$Missing = NULL
    nsmalls = c(0, rep(nsmall, 8))
  } else {
    desc$Missing = ifelse(missing==0, NA, missing)
    names(desc)[2] = "(NA)"
    nsmalls = c(0, 0, rep(nsmall, 8))
  }
  print_table(desc, nsmalls=nsmalls, file=file,
              title="Descriptive Statistics:")

  data.new = as.data.frame(data)
  vars.not.numeric = c()
  if(all.as.numeric) {
    for(var in names(data.new)) {
      if(!is.numeric(data.new[[var]])) {
        data.new[[var]] = as.numeric(data.new[[var]])
        vars.not.numeric = c(vars.not.numeric, var)
      }
    }
  }
  if(length(vars.not.numeric)>0)
    Print("\n\n\n<<yellow NOTE: `{paste(vars.not.numeric, collapse='`, `')}` transformed to numeric.>>")

  p = NULL
  if(plot) {
    # if(!pacman::p_isinstalled("GGally"))
    #   stop("Package `GGally` needs to be installed for plotting.\nRun this code: install.packages(\"GGally\")", call.=FALSE)
    if(upper.triangle) {
      smooth = switch(upper.smooth,
                      "none"="points",
                      "lm"="smooth",
                      "loess"="smooth_loess")
      upper = list(continuous=GGally::wrap(
        smooth, size=1, shape=16, alpha=0.3))
    } else {
      upper = "blank"
    }
    p = GGally::ggpairs(
      data.new, switch="both", axisLabels="none",
      upper=upper,
      lower=list(continuous=GGally::wrap(
        "cor", digits=nsmall,
        use="pairwise.complete.obs",
        size=4, color="black"))) +
      theme_bruce() +
      theme(strip.text=element_text(size=12, color="black"))
    if(is.null(plot.file)) {
      print(p)
    } else {
      cowplot::ggsave2(plot=p, filename=plot.file,
                       width=plot.width, height=plot.height, dpi=plot.dpi)
      plot.file = str_split(plot.file, "/", simplify=TRUE)
      plot.path = paste0(getwd(), '/', plot.file[length(plot.file)])
      Print("\n\n\n<<green \u221a>> Plot saved to <<blue '{plot.path}'>>")
    }
  }

  invisible(list(desc=desc, plot=p))
}


#' Frequency statistics.
#'
#' @param x A vector of values (or a data frame).
#' @param varname [Optional] Variable name, if \code{x} is a data frame.
#' @param labels [Optional] A vector re-defining the labels of values.
#' @param sort \code{""} (default, sorted by the order of variable values/labels),
#' \code{"-"} (decreasing by N), or \code{"+"} (increasing by N).
#' @param digits,nsmall Number of decimal places of output. Default is \code{1}.
#' @param file File name of MS Word (\code{.doc}).
#'
#' @return A data frame of frequency statistics.
#'
#' @examples
#' data = psych::bfi
#'
#' ## Input `data$variable`
#' Freq(data$education)
#' Freq(data$gender, labels=c("Male", "Female"))
#' Freq(data$age)
#'
#' ## Input one data frame and one variable name
#' Freq(data, "education")
#' Freq(data, "gender", labels=c("Male", "Female"))
#' Freq(data, "age")
#'
#' @export
Freq = function(x, varname, labels, sort="",
                digits=1, nsmall=digits, file=NULL) {
  if(inherits(x, "data.frame")) {
    if(missing(varname))
      stop("Please also specify `varname` (variable name). See help page: help(Freq)", call.=FALSE)
    if(length(varname)>1)
      stop("Please specify only ONE variable name.", call.=FALSE)
    var = as.data.frame(x)[[varname]]
  } else {
    var = x
  }
  table.var = table(var)
  N.na = sum(is.na(var))
  N = sum(table.var) + N.na
  if(missing(labels)) labels = names(table.var)
  output = cbind(matrix(table.var,
                        dimnames=list(labels, "N")),
                 matrix(round(table.var/N*100, nsmall),
                        dimnames=list(labels, "%")))
  if(N.na) output = rbind(output,
                          matrix(c(N.na, round(N.na/N*100, nsmall)),
                                 ncol=2, dimnames=list("(NA)")))
  if(sort=="-")
    output = output[order(output[,"N"], decreasing=TRUE),]
  if(sort=="+")
    output = output[order(output[,"N"], decreasing=FALSE),]

  if(is.null(file)) {
    note = Glue("Total <<italic N>> = {formatN(N)}")
    if(N.na>0) note = note %^% "\n" %^%
        Glue("Valid <<italic N>> = {formatN(N-N.na)}")
  } else {
    note = "Total <i>N</i> = " %^% formatN(N)
    if(N.na>0) note = note %^% "</p>\n<p>" %^%
        "Valid <i>N</i> = " %^% formatN(N-N.na)
  }
  print_table(output, nsmalls=c(0, nsmall), file=file,
              title="Frequency Statistics:", note=note)

  invisible(output)
}


#' Correlation analysis.
#'
#' @inheritParams Describe
#' @param data Data frame.
#' @param method \code{"pearson"} (default), \code{"spearman"}, or \code{"kendall"}.
#' @param p.adjust Adjustment of \emph{p} values for multiple tests:
#' \code{"none"}, \code{"fdr"}, \code{"holm"}, \code{"bonferroni"}, ...
#' For details, see \code{\link[stats:p.adjust]{stats::p.adjust()}}.
#' @param digits,nsmall Number of decimal places of output. Default is \code{2}.
#' @param file File name of MS Word (\code{.doc}).
#' @param plot \code{TRUE} (default) or \code{FALSE}. Plot the correlation matrix.
#' @param plot.range Range of correlation coefficients for plot. Default is \code{c(-1, 1)}.
#' @param plot.palette Color gradient for plot. Default is \code{c("#B52127", "white", "#2171B5")}.
#' You may also set it to, e.g., \code{c("red", "white", "blue")}.
#' @param plot.color.levels Default is \code{201}.
#'
#' @return
#' Invisibly return the correlation results obtained from
#' \code{\link[psych:corr.test]{psych::corr.test()}}.
#'
#' @examples
#' Corr(airquality)
#' Corr(airquality, p.adjust="bonferroni")
#'
#' d = as.data.table(psych::bfi)
#' added(d, {
#'   gender = as.factor(gender)
#'   education = as.factor(education)
#'   E = .mean("E", 1:5, rev=c(1,2), range=1:6)
#'   A = .mean("A", 1:5, rev=1, range=1:6)
#'   C = .mean("C", 1:5, rev=c(4,5), range=1:6)
#'   N = .mean("N", 1:5, range=1:6)
#'   O = .mean("O", 1:5, rev=c(2,5), range=1:6)
#' })
#' Corr(d[, .(age, gender, education, E, A, C, N, O)])
#'
#' @seealso \code{\link{Describe}}
#'
#' @export
Corr = function(data,
                method="pearson",
                p.adjust="none",
                all.as.numeric=TRUE,
                digits=2, nsmall=digits,
                file=NULL,
                plot=TRUE, plot.range=c(-1, 1),
                plot.palette=NULL, plot.color.levels=201,
                plot.file=NULL, plot.width=8, plot.height=6, plot.dpi=500) {
  data.new = as.data.frame(data)
  vars.not.numeric = c()
  if(all.as.numeric) {
    for(var in names(data.new)) {
      if(!is.numeric(data.new[[var]])) {
        data.new[[var]] = as.numeric(data.new[[var]])
        vars.not.numeric = c(vars.not.numeric, var)
      }
    }
  }

  cor = psych::corr.test(data.new, method=method,
                         adjust=p.adjust,
                         minlength=20)
  COR = cor$ci["r"]
  if(p.adjust=="none") {
    COR$`[95% CI]` = cc_ci(cor$ci$lower, cor$ci$upper, nsmall)
    COR$pval = cor$ci$p
  } else {
    COR$`[95% CI]` = cc_ci(cor$ci.adj$lower, cor$ci.adj$upper, nsmall)
    COR$pval = p.adjust(cor$ci$p, method=p.adjust)
  }
  if(inherits(cor$n, "matrix"))
    Ns = cor$n[lower.tri(cor$n)]
  else
    Ns = cor$n
  COR$r = formatF(COR$r, nsmall)
  COR$N = Ns

  if(length(vars.not.numeric)>0) {
    Print("<<yellow NOTE: `{paste(vars.not.numeric, collapse='`, `')}` transformed to numeric.>>")
    cat("\n")
  }

  if(plot) {
    if(is.null(plot.palette))
      plot.palette = c("#B52127", "white", "#2171B5")
    if(!is.null(plot.file)) {
      grDevices::png(filename=plot.file, width=plot.width, height=plot.height, units="in", res=plot.dpi)
    }
    try({
      plot.error = TRUE
      cor_plot(r=cor$r, adjust="none", nsmall=nsmall,
               numbers=TRUE, zlim=plot.range,
               diag=FALSE, xlas=2, n=plot.color.levels,
               pval=cor$p, stars=TRUE,
               alpha=1, gr=grDevices::colorRampPalette(plot.palette),
               main="Correlation Matrix")
      plot.error = FALSE
    }, silent=TRUE)
    if(plot.error) {
      warning = Glue("
        Plot is NOT successfully displayed in the RStudio `Plots` Pane.
        Please check if the `Plots` Pane of your RStudio is too small.
        You should enlarge the `Plots` Pane (and/or clear all plots).")
      warning(warning, call.=TRUE)
      cat("\n")
    } else {
      Print("Correlation matrix is displayed in the RStudio `Plots` Pane.")
      if(p.adjust!="none")
        Print("<<blue <<italic p>> values ABOVE the diagonal are adjusted using the \"{p.adjust}\" method.>>")
      cat("\n")
    }
    if(!is.null(plot.file)) {
      grDevices::dev.off()
      plot.file = str_split(plot.file, "/", simplify=TRUE)
      plot.path = paste0(getwd(), '/', plot.file[length(plot.file)])
      Print("<<green \u221a>> Plot saved to <<blue '{plot.path}'>>")
      cat("\n")
    }
  }

  if(is.null(file)) {
    Print("{capitalize(method)}'s <<italic r>> and 95% confidence intervals:")
    if(p.adjust!="none")
      Print("<<blue <<italic p>> values and 95% CIs are adjusted using the \"{p.adjust}\" method.>>")
    print_table(COR, nsmalls=0)
    cat("\n")
  } else {
    Print("Descriptive Statistics and Correlation Matrix:")
    cor.mat = matrix(formatF(cor$r, nsmall),
                     nrow=nrow(cor$r),
                     dimnames=list(rownames(cor$r),
                                   colnames(cor$r)))
    cor.sig = sig.trans(cor$p)
    if(p.adjust=="none") {
      for(i in 1:nrow(cor.mat))
        for(j in i:ncol(cor.mat))
          cor.mat[i,j] = NA
    } else {
      for(i in 1:nrow(cor.mat))
        cor.mat[i,i] = NA
    }
    for(i in 1:nrow(cor.mat)) {
      for(j in 1:ncol(cor.mat)) {
        cor.mat[i,j] = str_replace_all(cor.mat[i,j], "0\\.", ".")
        if(!is.na(cor.mat[i,j])) {
          if(as.numeric(cor.mat[i,j])>0)
            cor.mat[i,j] = paste0("&ensp;", str_trim(cor.mat[i,j]))
          if(grepl("\\*", cor.sig[i,j]))
            cor.mat[i,j] = paste0(cor.mat[i,j], "<sup>", str_replace_all(cor.sig[i,j], "\\s", "&ensp;"), "</sup>")
          else
            cor.mat[i,j] = paste0(cor.mat[i,j], "<sup>&ensp;&ensp;&ensp;</sup>")
        }
      }
    }
    for(i in 1:nrow(cor.mat))
      cor.mat[i,i] = "&ensp;&nbsp;\u2014"
    des.cor = cbind(Variable=1:nrow(cor.mat) %^% ". " %^% row.names(cor.mat),
                    Describe(data, file="NOPRINT")$desc[c("Mean", "SD")],
                    cor.mat)
    names(des.cor) = c("Variable", "<i>M</i>", "<i>SD</i>", 1:ncol(cor.mat))
    if(p.adjust=="none") des.cor[ncol(des.cor)] = NULL
    COR.new = COR
    COR.new$Pairs = row.names(COR)
    COR.new$`r [95% CI]` = paste(COR$r, COR$`[95% CI]`) %>%
      str_replace_all("-", "\u2013") %>%
      str_replace_all("0\\.", ".") %>%
      str_replace_all("^ \\.", "&ensp;.")
    COR.new$p = p.trans(COR$pval)
    COR.new = COR.new[c("Pairs", "r [95% CI]", "p", "N")]
    names(COR.new) = c("Pairs", "<i>r</i> [95% CI]", "<i>p</i>", "<i>N</i>")
    cor.ci = paste0(
      "<p><br/><br/></p>",
      "<p><b>Table 2. Correlations and 95% Confidence Intervals.</b></p>",
      df_to_html(
        COR.new,
        align.head=c("left", "center", "center", "center"),
        align.text=c("left", "left", "right", "right"))$TABLE,
      "<p><i>Note</i>.",
      ifelse(p.adjust=="none", "</p>",
             " <i>p</i> values and 95% CIs are adjusted using the \"" %^% p.adjust %^% "\" method.</p>")
    )
    print_table(
      des.cor, nsmalls=nsmall, row.names=FALSE,
      title="<b>Table 1. Descriptive Statistics and Correlation Matrix.</b>",
      note=paste0(
        "<i>Note</i>. ",
        ifelse(p.adjust=="none", "",
               "<i>p</i> values above the diagonal are adjusted using the \"" %^% p.adjust %^% "\" method.</p>\n<p>"),
        "* <i>p</i> < .05. ** <i>p</i> < .01. *** <i>p</i> < .001."),
      append=cor.ci,
      file=file,
      file.align.head=c("left", "center", "center",
                        rep("center' style='width:49px", times=ncol(des.cor)-3)),
      file.align.text=c("left", "right", "right",
                        rep("left", times=ncol(des.cor)-3))
    )
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
    stop("You need at least two dimensions to make a meaningful plot.", call.=TRUE)
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
cor_diff = function(r1, n1, r2, n2, n=NULL, rcov=NULL) {
  if(is.null(rcov)) {
    # independent rs
    z1 = atanh(r1)
    z2 = atanh(r2)
    zdiff = (z1-z2) / sqrt(1/(n1-3) + 1/(n2-3))
    p = p.z(zdiff)
    Print("
    <<italic r>>1 = {formatF(r1)} (<<italic N>> = {formatN(n1)})
    <<italic r>>2 = {formatF(r2)} (<<italic N>> = {formatN(n2)})
    Difference of correlation: {p(z=zdiff)}
    ")
  } else {
    # nonindependent rs
    R = (1-r1^2-r2^2-rcov^2) + 2*r1*r2*rcov
    tdiff = (r1-r2) * sqrt((n-1)*(1+rcov) / (2*R*(n-1)/(n-3) + (r1+r2)^2*(1-rcov)^3/4))
    p = p.t(tdiff, n-3)
    Print("
    <<italic r>>1 = {formatF(r1)}
    <<italic r>>2 = {formatF(r2)}
    (<<italic N>> = {formatN(n)}, <<italic r>>_cov = {formatF(rcov)})
    Difference of correlation: {p(t=tdiff, df=n-3)}
    ")
  }
  invisible(p)
}


#### T-Tests ####

#' One-sample, independent-samples, and paired-samples t-test.
#'
#' @description
#' One-sample, independent-samples, and paired-samples \emph{t}-test,
#' with both Frequentist and Bayesian approaches.
#' The output includes descriptives, \emph{t} statistics,
#' mean difference with 95\% CI, Cohen's \emph{d} with 95\% CI,
#' and Bayes factor (BF10; \code{BayesFactor} package needs to be installed).
#' It also tests the assumption of homogeneity of variance
#' and allows users to determine whether variances are equal or not.
#'
#' Users can simultaneously test multiple dependent and/or independent variables.
#' The results of one pair of Y-X would be summarized in one row in the output.
#' Key results can be saved in APA format to MS Word.
#'
#' @details
#' Note that the point estimate of Cohen's \emph{d} is computed using
#' the common method "Cohen's \emph{d} = mean difference / (pooled) standard deviation", which is
#' consistent with results from other R packages (e.g., \code{effectsize}) and software (e.g., \code{jamovi}).
#' The 95\% CI of Cohen's \emph{d} is estimated based on the 95\% CI of mean difference
#' (i.e., also divided by the pooled standard deviation).
#'
#' However, different packages and software diverge greatly on the estimate of the 95\% CI of Cohen's \emph{d}.
#' R packages such as \code{psych} and \code{effectsize}, R software \code{jamovi},
#' and several online statistical tools for estimating effect sizes
#' indeed produce surprisingly inconsistent results on the 95\% CI of Cohen's \emph{d}.
#'
#' See an illustration of this issue in the section "Examples".
#'
#' @param data Data frame (wide-format only, i.e., one case in one row).
#' @param y Dependent variable(s).
#' Multiple variables should be included in a character vector \code{c()}.
#'
#' For paired-samples \emph{t}-test, the number of variables should be 2, 4, 6, etc.
#' @param x Independent variable(s).
#' Multiple variables should be included in a character vector \code{c()}.
#'
#' Only necessary for independent-samples \emph{t}-test.
#' @param paired For paired-samples \emph{t}-test, set it as \code{TRUE}. Default is \code{FALSE}.
#' @param paired.d.type Type of Cohen's \emph{d} for paired-samples \emph{t}-test (see Lakens, 2013).
#'
#' Default is \code{"dz"}. Options include:
#' \describe{
#'   \item{\code{"dz"} (\emph{d} for standardized difference)}{
#'     Cohen's \eqn{d_{z} = \frac{M_{diff}}{SD_{diff}}}
#'   }
#'   \item{\code{"dav"} (\emph{d} for average standard deviation)}{
#'     Cohen's \eqn{d_{av} = \frac{M_{diff}}{ \frac{SD_{1} + SD_{2}}{2} }}
#'   }
#'   \item{\code{"drm"} (\emph{d} for repeated measures, corrected for correlation)}{
#'     Cohen's \eqn{d_{rm} = \frac{M_{diff} \times \sqrt{2(1 - r_{1,2})}}{
#'       \sqrt{SD_{1}^2 + SD_{2}^2 - 2 \times r_{1,2} \times SD_{1} \times SD_{2}} }}
#'   }
#' }
#' @param var.equal If Levene's test indicates a violation of the homogeneity of variance,
#' then you should better set this argument as \code{FALSE}. Default is \code{TRUE}.
#' @param mean.diff Whether to display results of mean difference and its 95\% CI. Default is \code{TRUE}.
#' @param test.value The true value of the mean (or difference in means for a two-samples test). Default is \code{0}.
#' @param test.sided Any of \code{"="} (two-sided, the default), \code{"<"} (one-sided), or \code{">"} (one-sided).
#' @param factor.rev Whether to reverse the levels of factor (X)
#' such that the test compares higher vs. lower level. Default is \code{TRUE}.
#' @param bayes.prior Prior scale in Bayesian \emph{t}-test. Default is 0.707.
#' See details in \code{\link[BayesFactor:ttestBF]{BayesFactor::ttestBF()}}.
#' @param digits,nsmall Number of decimal places of output. Default is \code{2}.
#' @param file File name of MS Word (\code{.doc}).
#'
#' @examples
#' ## Demo data ##
#' d1 = between.3
#' d1$Y1 = d1$SCORE  # shorter name for convenience
#' d1$Y2 = rnorm(32)  # random variable
#' d1$B = factor(d1$B, levels=1:2, labels=c("Low", "High"))
#' d1$C = factor(d1$C, levels=1:2, labels=c("M", "F"))
#' d2 = within.1
#'
#' ## One-sample t-test ##
#' TTEST(d1, "SCORE")
#' TTEST(d1, "SCORE", test.value=5)
#'
#' ## Independent-samples t-test ##
#' TTEST(d1, "SCORE", x="A")
#' TTEST(d1, "SCORE", x="A", var.equal=FALSE)
#' TTEST(d1, y="Y1", x=c("A", "B", "C"))
#' TTEST(d1, y=c("Y1", "Y2"), x=c("A", "B", "C"),
#'       mean.diff=FALSE,  # remove to save space
#'       file="t-result.doc")
#' unlink("t-result.doc")  # delete file for code check
#'
#' ## Paired-samples t-test ##
#' TTEST(d2, y=c("A1", "A2"), paired=TRUE)
#' TTEST(d2, y=c("A1", "A2", "A3", "A4"), paired=TRUE)
#'
#'
#' \dontrun{
#'
#'   ## Illustration for the issue stated in "Details"
#'
#'   # Inconsistency in the 95% CI of Cohen's d between R packages:
#'   # In this example, the true point estimate of Cohen's d = 3.00
#'   # and its 95% CI should be equal to 95% CI of mean difference.
#'
#'   data = data.frame(X=rep(1:2, each=3), Y=1:6)
#'   data  # simple demo data
#'
#'   TTEST(data, y="Y", x="X")
#'   # d = 3.00 [0.73, 5.27] (estimated based on 95% CI of mean difference)
#'
#'   MANOVA(data, dv="Y", between="X") %>%
#'     EMMEANS("X")
#'   # d = 3.00 [0.73, 5.27] (the same as TTEST)
#'
#'   psych::cohen.d(x=data, group="X")
#'   # d = 3.67 [0.04, 7.35] (strange)
#'
#'   psych::d.ci(d=3.00, n1=3, n2=3)
#'   # d = 3.00 [-0.15, 6.12] (significance inconsistent with t-test)
#'
#'   # jamovi uses psych::d.ci() to compute 95% CI
#'   # so its results are also: 3.00 [-0.15, 6.12]
#'
#'   effectsize::cohens_d(Y ~ rev(X), data=data)
#'   # d = 3.00 [0.38, 5.50] (using the noncentrality parameter method)
#'
#'   effectsize::t_to_d(t=t.test(Y ~ rev(X), data=data, var.equal=TRUE)$statistic,
#'                      df_error=4)
#'   # d = 3.67 [0.47, 6.74] (merely an approximate estimate, often overestimated)
#'   # see ?effectsize::t_to_d
#'
#'   # https://www.psychometrica.de/effect_size.html
#'   # d = 3.00 [0.67, 5.33] (slightly different from TTEST)
#'
#'   # https://www.campbellcollaboration.org/escalc/
#'   # d = 3.00 [0.67, 5.33] (slightly different from TTEST)
#'
#'   # Conclusion:
#'   # TTEST() provides a reasonable estimate of Cohen's d and its 95% CI,
#'   # and effectsize::cohens_d() offers another method to compute the CI.
#' }
#'
#' @seealso \code{\link{MANOVA}}, \code{\link{EMMEANS}}
#'
#' @references
#' Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science:
#' A practical primer for \emph{t}-tests and ANOVAs. \emph{Frontiers in Psychology, 4}, Article 863.
#'
#' @export
TTEST = function(data, y, x=NULL,
                 paired=FALSE,
                 paired.d.type="dz",
                 var.equal=TRUE,
                 mean.diff=TRUE,
                 test.value=0,
                 test.sided=c("=", "<", ">"),
                 factor.rev=TRUE,
                 bayes.prior="medium",
                 digits=2, nsmall=digits,
                 file=NULL) {
  data = as.data.frame(data)

  if(paired) {
    if(!is.null(x))
      stop("For paired-samples t-test, x should not be used.", call.=TRUE)
    if(length(y)%%2==1)
      stop("For paired-samples t-test, y should be a vector of 2, 4, 6, ... variables.", call.=TRUE)
    type = "Paired-Samples t-test"
    mu = ifelse(factor.rev, "\u03bc2 - \u03bc1", "\u03bc1 - \u03bc2")
    y = split(y, rep(1:(length(y)/2), each=2))
  } else {
    if(is.null(x)) {
      type = "One-Sample t-test"
      mu = "\u03bc"
    } else {
      type = "Independent-Samples t-test"
      mu = ifelse(factor.rev, "\u03bc2 - \u03bc1", "\u03bc1 - \u03bc2")
      if(test.value!=0)
        message("Test value (mu) for Bayes test is reset to 0.")
    }
  }

  if(length(test.sided)>1) test.sided = "="
  if(test.sided %notin% c("=", "<", ">"))
    stop("test.sided should be one of \"=\", \"<\", or \">\"", call.=TRUE)
  hypo = switch(test.sided,
                `=`=Glue("two-sided ({mu} \u2260 {test.value})"),
                `<`=Glue("one-sided ({mu} < {test.value})"),
                `>`=Glue("one-sided ({mu} > {test.value})"))

  ## Info
  Print("
  \n
  <<cyan {type}>>

  Hypothesis: {hypo}
  \n
  ")

  ## Test
  nmsd = data.frame()
  lev = data.frame()
  res = data.frame()
  for(yi in y) {
    if(is.null(x)) {
      res.list = ttest(data, yi, NULL, paired, paired.d.type, var.equal,
                       test.value, test.sided, factor.rev, bayes.prior)
      res = rbind(res, res.list$res)
      nmsd = rbind(nmsd, res.list$nmsd)
      if(!is.null(res.list$levene))
        lev = rbind(lev, res.list$levene)
    } else {
      for(xi in x) {
        res.list = ttest(data, yi, xi, paired, paired.d.type, var.equal,
                         test.value, test.sided, factor.rev, bayes.prior)
        res = rbind(res, res.list$res)
        nmsd = rbind(nmsd, res.list$nmsd)
        if(!is.null(res.list$levene))
          lev = rbind(lev, res.list$levene)
      }
    }
  }

  ## Print (nmsd)
  nmsd$MeanSD = paste0(formatF(nmsd$Mean, nsmall), " (",
                       formatF(nmsd$S.D., nsmall), ")")
  names(nmsd)[length(nmsd)] = "Mean (S.D.)"
  nmsd$Mean = nmsd$S.D. = NULL
  Print("Descriptives:")
  print_table(nmsd, row.names=FALSE, nsmalls=0)
  cat("\n")

  ## Print (check)
  if(nrow(lev)>0) {
    Print("Levene\u2019s test for homogeneity of variance:")
    print_table(lev, nsmalls=c(2, 0, 0, 0))
    Print("<<italic Note>>: H0 = equal variance (homoscedasticity).
          If significant (violation of the assumption),
          then you should better set `var.equal=FALSE`.")
    cat("\n")
  }

  ## Print (t-test)
  RES = res[,1:3]  # t, df, pval
  RES$Diff = cc_m_ci(res$diff, res$llci, res$ulci, nsmall)
  RES$Cohen = cc_m_ci(res$Cohen_d, res$LLCI, res$ULCI, nsmall)
  RES$BF10 = sprintf(Glue("%.{nsmall}e"), res$BF10)
  if(!is.null(file)) {
    RES$Diff = str_replace_all(RES$Diff, "-", "\u2013")
    RES$Cohen = str_replace_all(RES$Cohen, "-", "\u2013")
    RES$BF10 = str_replace_all(RES$BF10, "-", "\u2013")
    names(RES) = c("<i>t</i>",
                 "<i>df</i>",
                 "<i>p</i>",
                 "Difference [95% CI]",
                 "Cohen\u2019s <i>d</i> [95% CI]",
                 "BF<sub>10</sub>")
  } else {
    names(RES)[4:5] = c("Difference [95% CI]",
                        "Cohen\u2019s d [95% CI]")
  }
  if(mean.diff==FALSE)
    RES[,4] = NULL
  print_table(
    RES,
    nsmalls=c(nsmall, ifelse(var.equal, 0, nsmall), 0, 0, 0, 0, 0),
    title=ifelse(is.null(file),
                 Glue("Results of t-test{ifelse(var.equal, '', ' (adjusted df)')}:"),
                 type),
    file=file,
    file.align.text=c("left",
                      "right", "right", "right", "left",
                      "right", "right", "right"))
  if(is.null(file)) cat("\n")

  invisible(res)
}


## one y and one x
ttest = function(data, y, x=NULL,
                 paired=FALSE,
                 paired.d.type="dz",
                 var.equal=TRUE,
                 test.value=0,
                 test.sided="=",
                 factor.rev=TRUE,
                 bayes.prior="medium") {
  ## Data
  data = na.omit(data[c(y, x)])
  alternative = switch(test.sided,
                       `=`="two.sided",
                       `<`="less",
                       `>`="greater")
  if(alternative=="less")
    nullInterval = c(-Inf, 0)
  else if(alternative=="greater")
    nullInterval = c(0, Inf)
  else
    nullInterval = NULL

  ## Prepare
  if(paired) {
    Y = "Paired"
    X = ""
    y1 = data[[y[1]]]
    y2 = data[[y[2]]]
    nmsd = data.frame(Variable=c(y[1], y[2]),
                      Mean=c(mean(y1), mean(y2)),
                      S.D.=c(sd(y1), sd(y2)),
                      N=nrow(data))
    if(factor.rev) {
      formula = as.formula(Glue("Pair({y[2]}, {y[1]}) ~ 1"))
      label = paste(y[2], "-", y[1])
    } else {
      formula = as.formula(Glue("Pair({y[1]}, {y[2]}) ~ 1"))
      label = paste(y[1], "-", y[2])
    }
    if(paired.d.type=="dz")
      sd.pooled = sd(y1-y2)
    else if(paired.d.type=="dav")
      sd.pooled = (sd(y1) + sd(y2)) / 2
    else if(paired.d.type=="drm")
      sd.pooled = sqrt(sd(y1)^2 + sd(y2)^2 - 2*cor(y1, y2)*sd(y1)*sd(y2)) / sqrt(2*(1 - cor(y1, y2)))
    else
      stop("`paired.d.type` must be one of \"dz\", \"dav\", \"drm\".", call.=FALSE)
    mu.BF = test.value
  } else {
    if(is.null(x)) {
      Y = y
      X = ""
      y1 = data[[y]]
      y2 = NULL
      formula = as.formula(Glue("{y} ~ 1"))
      nmsd = data.frame(Variable=y,
                        Mean=mean(y1),
                        S.D.=sd(y1),
                        N=nrow(data))
      label = paste(y, "-", test.value)
      sd.pooled = sd(y1)
      mu.BF = test.value
    } else {
      formula = as.formula(Glue("{y} ~ {x}"))
      data[[x]] = as.factor(data[[x]])
      if(nlevels(data[[x]])!=2)
        stop(Glue("Levels of the factor \"{x}\" should be 2."), call.=TRUE)
      nmsd = plyr::ddply(
        .data=data,
        .variables=plyr::as.quoted(x),
        .fun=summarise,
        bruceR.Mean=mean(!!sym(y)),
        bruceR.S.D.=sd(!!sym(y)),
        bruceR.N=length(!!sym(y)))
      names(nmsd) = c("Level", "Mean", "S.D.", "N")
      nmsd = cbind(data.frame(Variable=y,
                              Factor=x),
                   as.data.frame(nmsd))
      if(factor.rev) data[[x]] = forcats::fct_rev(data[[x]])
      label = paste(levels(data[[x]]), collapse=" - ")
      Y = y
      X = x %^% " "
      xlevels = levels(data[[x]])
      y1 = data[which(data[[x]]==xlevels[1]), y]
      y2 = data[which(data[[x]]==xlevels[2]), y]
      df1 = length(y1)-1
      df2 = length(y2)-1
      # https://github.com/jamovi/jmv/blob/master/R/ttestis.b.R
      if(var.equal)
        sd.pooled = sqrt((var(y1)*df1+var(y2)*df2) / (df1+df2))
      else
        sd.pooled = sqrt((var(y1)+var(y2)) / 2)
      mu.BF = 0
    }
  }

  ## Test
  res = stats::t.test(formula=formula,
                      data=data,
                      var.equal=var.equal,
                      mu=test.value,
                      alternative=alternative)
  t = res[["statistic"]]
  df = res[["parameter"]]
  pval = res[["p.value"]]
  est = res[["estimate"]]
  diff = ifelse(length(est)==1, est, est[1]-est[2])-test.value
  llci = res[["conf.int"]][1]-test.value
  ulci = res[["conf.int"]][2]-test.value
  Cohen_d = diff/sd.pooled
  LLCI = llci/sd.pooled
  ULCI = ulci/sd.pooled
  try({
    BF10 = NA
    Error = NA
    BF = BayesFactor::ttestBF(x=y1, y=y2,
                              paired=paired,
                              mu=mu.BF,
                              nullInterval=nullInterval,
                              rscale=bayes.prior)
    BF = BayesFactor::extractBF(BF)
    BF10 = BF$bf[1]
    Error = BF$error[1]
  }, silent=TRUE)
  if(is.na(BF10))
    message("`BayesFactor` package needs to be installed.\n")
  RES = data.frame(
    Y=Y,
    Comparison=X %^% "(" %^% label %^% ")",
    t=t,
    df=df,
    pval=pval,
    diff=diff,
    llci=llci,
    ulci=ulci,
    Cohen_d=Cohen_d,
    LLCI=LLCI,
    ULCI=ULCI,
    BF10=BF10,
    Error=Error)
  row.names(RES) = RES$Y %^% ": " %^% RES$Comparison

  ## Check
  if(is.null(x)) {
    levene = NULL
  } else {
    lev = car::leveneTest(formula, data, center=mean)
    levene = cbind(lev[1, "F value"],
                   lev[1, "Df"],
                   lev[2, "Df"],
                   lev[1, "Pr(>F)"]) %>% as.data.frame()
    names(levene) = c("Levene\u2019s F", "df1", "df2", "pval")
    row.names(levene) = row.names(RES)
  }

  return(list(nmsd=nmsd, levene=levene, res=RES[,-1:-2]))
}

