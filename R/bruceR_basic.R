#### Basic Functions ####


#' A simple extension of \code{\%in\%}
#' @param x Numeric or character vector.
#' @param vector Numeric or character vector.
#' @examples
#' data=data.table(ID=1:10, X=RANDBETWEEN(1:10, 10))
#' data
#' data[ID %notin% c(1, 3, 5, 7, 9)]
#' @seealso \code{\link[base]{match}} (\code{\%in\%})
#' @export
`%notin%`=function(x, vector) {
  match(x, vector, nomatch=0) == 0
}


#' A simple extension of \code{\%in\%}
#' @inheritParams %notin%
#' @examples
#' 1:2 %allin% 1:3  # TRUE
#' 3:4 %allin% 1:3  # FALSE
#' @seealso \code{\link[base]{match}} (\code{\%in\%}), \code{\link{\%anyin\%}}, \code{\link{\%nonein\%}}, \code{\link{\%partin\%}}
#' @export
`%allin%`=function(x, vector) {
  all(x %in% vector)
}


#' A simple extension of \code{\%in\%}
#' @inheritParams %notin%
#' @examples
#' 3:4 %anyin% 1:3  # TRUE
#' 4:5 %anyin% 1:3  # FALSE
#' @seealso \code{\link[base]{match}} (\code{\%in\%}), \code{\link{\%allin\%}}, \code{\link{\%nonein\%}}, \code{\link{\%partin\%}}
#' @export
`%anyin%`=function(x, vector) {
  any(x %in% vector)
}


#' A simple extension of \code{\%in\%}
#' @inheritParams %notin%
#' @examples
#' 3:4 %nonein% 1:3  # FALSE
#' 4:5 %nonein% 1:3  # TRUE
#' @seealso \code{\link[base]{match}} (\code{\%in\%}), \code{\link{\%allin\%}}, \code{\link{\%anyin\%}}, \code{\link{\%partin\%}}
#' @export
`%nonein%`=function(x, vector) {
  !any(x %in% vector)
}


#' A simple extension of \code{\%in\%}
#' @param pattern Character string containing \strong{regular expressions} to be matched.
#' @param vector Character vector.
#' @examples
#' "Bei" %partin% c("Beijing", "Shanghai")  # TRUE
#' "bei" %partin% c("Beijing", "Shanghai")  # FALSE
#' "[aeiou]ng" %partin% c("Beijing", "Shanghai")  # TRUE
#' @seealso \code{\link[base]{match}} (\code{\%in\%}), \code{\link{\%allin\%}}, \code{\link{\%anyin\%}}, \code{\link{\%nonein\%}}
#' @export
`%partin%`=function(pattern, vector) {
  any(grepl(pattern, vector, perl=TRUE))
}


#' Set working directory to the path of \strong{current} script
#'
#' @importFrom rstudioapi getSourceEditorContext
#' @param dir \code{NULL} (default) or a character string specifying the working directory.
#'
#' If \code{NULL}, set working directory to the path of \strong{the current R script}.
#' @examples
#' set.wd()  # set working directory to the path of the current R script
#' set.wd("D:/")  # "\" is not allowed, you should use "/"
#' set.wd("../")  # set working directory to the parent directory
#' @seealso \code{\link{setwd}}
#' @export
set.wd=function(dir=NULL) {
  if(is.null(dir)) dir=dirname(getSourceEditorContext()$path)
  setwd(dir)
  path=getwd()
  Print("<<green \u2714>> Set working directory to <<blue '{path}'>>")
}


#' Set random seeds with a specific version of R
#'
#' The new versions of R (>= 3.6.0; since April 2019) have changed the mechanism of generating random numbers.
#' To have an exact replication of previous results based on random numbers, the version of R should be specified.
#' By default, it sets the version to "3.5.0". All the versions earlier than 3.6.0 will generate the same result.
#'
#' @param seed Random seed.
#' @param version Character string specifying the R version. Default is "3.5.0".
#' @examples
#' set.seeds(1, version="3.5.0")
#' sample(1:10)  # 3  4  5  7  2  8  9  6 10  1
#'
#' set.seeds(1, version="3.6.0")
#' sample(1:10)  # 9  4  7  1  2  5  3 10  6  8
#' @seealso \code{\link{set.seed}}
#' @export
set.seeds=function(seed, version="3.5.0") {
  suppressWarnings(RNGversion(version))
  set.seed(seed)
}


#' Check dependencies of a package
#' @param pkg Package name.
#' @param exclude [optional] Package(s) and their dependencies excluded from the dependencies of \code{pkg}.
#' Useful if you want to see the unique contribution of \code{pkg}.
#' @examples
#' pkg_depend("MuMIn")
#'
#' pkg_depend("sjstats")
#' pkg_depend("sjstats", exclude="tidyverse")  # "partial dependencies" when "controlling for" other packages
#'
#' pkg_depend("dplyr", exclude="tidyverse")  # no unique contribution
#' pkg_depend("lme4", exclude=c("tidyverse", "ggstatsplot"))  # no unique contribution
#' @export
pkg_depend=function(pkg, exclude=NULL) {
  default.pkgs=c("base", "boot", "class", "cluster", "codetools", "compiler",
                 "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth",
                 "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet", "parallel",
                 "rpart", "spatial", "splines", "stats", "stats4", "survival",
                 "tcltk", "tools", "translations", "utils")
  exclude.pkgs=default.pkgs
  for(ex in exclude)
    exclude.pkgs=union(exclude.pkgs, unlist(tools::package_dependencies(ex, recursive=TRUE)))
  pkgs=unlist(tools::package_dependencies(pkg, recursive=TRUE))
  pkgs=sort(setdiff(union(pkg, pkgs), exclude.pkgs))
  if(length(pkgs)==0) {
    Print("<<blue Package <<green '{pkg}'>> is already a dependency of your excluded package(s).>>")
  } else {
    packages=data.frame(Package=pkgs, Description=mapply(packageDescription, pkgs, fields="Title"))
    View(packages, pkg)
    invisible(packages)
  }
}


#' Check update of \code{bruceR} package
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_text
#' @examples
#' library(bruceR)
#' check_update()
#' @export
check_update=function() {
  # user.ver=sessionInfo()[["otherPkgs"]][["bruceR"]][["Version"]]
  user.ver=as.character(packageVersion("bruceR"))
  curr.ver=read_html("https://github.com/psychbruce/bruceR") %>% html_node("h2+ h3 code") %>% html_text()
  Print("
  <<blue
  Your installed version of bruceR: <<green {user.ver}>>
  The latest version on GitHub.com: <<green {curr.ver}>>
  <<red {ifelse(user.ver==curr.ver, 'No need to update.', 'You can update.')}>>
  >>
  ")
}


#' Paste and print texts with rich formats and colors
#'
#' Be tired of \code{print()} and \code{cat()}? Try \code{Print()}!
#' Run \strong{\code{example("Print")}} and see its power.
#'
#' See more details in help pages of \code{glue::\link[glue]{glue}} and \code{glue::\link[glue]{glue_col}}.
#' @import glue
#' @importFrom crayon reset bold italic underline strikethrough black silver white red green blue yellow magenta cyan
#' @param ... Character strings enclosed by \code{"{ }"} will be evaluated as R codes.
#'
#' Character strings enclosed by \code{"<< >>"} will be printed as formatted and colored texts.
#'
#' Long strings are broken by line and concatenated together.
#'
#' Leading whitespace and blank lines from the first and last lines are automatically trimmed.
#' @examples
#' name="Bruce"
#' Print("My name is <<underline <<bold {name}>>>>.
#'        <<bold <<blue Pi = {pi:.15}.>>>>
#'        <<italic <<green 1 + 1 = {1 + 1}.>>>>
#'        sqrt({x}) = <<red {sqrt(x):.3}>>", x=10)
#' @describeIn Print Paste and print strings.
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


#' @describeIn Print Paste strings.
#' @import glue
#' @importFrom crayon reset bold italic underline strikethrough black silver white red green blue yellow magenta cyan
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


#' Repeat a character string for many times and paste them up
#' @param char Character string.
#' @param rep.times Times for repeat.
#' @examples
#' rep_char("a", 5)
#' @export
rep_char=function(char, rep.times) {
  paste(rep(char, rep.times), collapse="")
}


#' Print three-line table
#' @param x Matrix, data.frame, data.table, or any model object (e.g., \code{lm, glm, lmer, glmer, ...}).
#' @param row.names \code{TRUE} (default) or \code{FALSE}. Whether to print row names.
#' @param nsmalls A number or numeric vector specifying the number of decimal places of output. Default is 3.
#' @examples
#' model=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
#' print_table(model)
#' @export
print_table=function(x, row.names=TRUE, nsmalls=3) {
  ## Preprocess data.frame ##
  linechar1="\u2501"  # top-and-down '=' [bug in some computers!]
  linechar2="\u2500"  # in-table '-'
  linechar1=linechar2
  if(class(x) %nonein% c("matrix", "data.frame", "data.table")) {
    coef.table=coef(summary(x))
    if(is.null(coef.table)==FALSE) x=coef.table
  }
  x=as.data.frame(x)
  sig=NULL
  if(length(nsmalls)==1) nsmalls=rep(nsmalls, length(x))
  for(j in 1:length(x)) {
    if(grepl("Pr\\(|pval", names(x)[j])) {
      sig=formatF(sig.trans(x[,j]), 0)  # formatF will make * left-aligned
      names(x)[j]="p"
      x[,j]=p.trans(x[,j])
    } else {
      x[,j]=formatF(x[,j], nsmalls[j])
    }
    if(grepl("S\\.E\\.|Std\\. Error|^se$", names(x)[j])) {
      x[,j]=paste0("(", x[,j], ")")  # add ( ) to S.E.
      x[grepl("\\.", x[,j])==FALSE, j]=""  # remove ( ) from blank S.E.
      if(grepl("S\\.E\\.", names(x)[j])==FALSE) names(x)[j]="S.E."
    }
    if(grepl("\\[", names(x)[j])) x[,j]=paste0("[", x[,j], ",")
    if(grepl("\\]", names(x)[j])) x[,j]=paste0(x[,j], "]")
    if(grepl("^[Ee]stimate$", names(x)[j])) names(x)[j]="Coef."
    names(x)[j]=gsub(" value|val$", "", names(x)[j])
  }
  if(is.null(sig)==FALSE & "sig" %notin% names(x)) {
    x=cbind(x, ` `=sig)
    x$` `=as.character(x$` `)
  }

  ## Compute length to generate line-chars ##
  title.length=nchar(names(x))
  vars.length=c()  # bug: vars.length=apply(apply(x, 2, nchar), 2, max)
  for(j in 1:length(x)) vars.length[j]=max(nchar(x[,j]))

  ## Generate a row with 'linechar2' ##
  n.lines=apply(rbind(title.length, vars.length), 2, max)+1
  n.lines.rn=max(nchar(row.names(x)))+1
  n.lines.table=n.lines.rn+sum(n.lines)
  line.row=data.frame()
  for(j in 1:length(x))
    line.row[1,j]=rep_char(linechar2, n.lines[j])
  names(line.row)=names(x)
  row.names(line.row)[1]=rep_char(linechar2, n.lines.rn)

  ## Row-bind and deal with 'row.names' (T or F) ##
  x=rbind(line.row, x)
  if(row.names==FALSE)
    n.lines.table=n.lines.table-n.lines.rn
  table.line=rep_char(linechar1, n.lines.table)

  ## Output ##
  Print(table.line)
  if(row.names==TRUE)
    cat(rep_char(" ", n.lines.rn))
  for(j in 1:length(x))
    cat(sprintf(glue("% {n.lines[j]}s"), names(x)[j]))
  cat("\n")
  for(i in 1:nrow(x)) {
    if(row.names==TRUE)
      cat(sprintf(glue("%-{n.lines.rn}s"), row.names(x[i,])))
    for(j in 1:length(x))
      cat(sprintf(glue("% {n.lines[j]}s"), ifelse(is.na(x[i,j]) | grepl("NA$", x[i,j]), "", x[i,j])))
    cat("\n")
  }
  Print(table.line)
}

# Good-looking tabs !!!
# Print("\u2500\u2501\u2502\u2503\u2504\u2505\u2506\u2507\u2508\u2509")


#' Format "1234" to "1,234"
#' @param x Any R object, typically numeric.
#' @param mark Usually \code{","}.
#' @return Formatted character string.
#' @examples
#' formatN(1234)
#' @export
formatN=function(x, mark=",") {
  format(x, big.mark=mark)
}


#' Format numeric values
#' @param x Any R object, typically numeric.
#' @param nsmall Number of decimal places of output. Default is 3.
#' @return Formatted character string.
#' @examples
#' formatF(pi)
#' @export
formatF=function(x, nsmall=3) {
  format(x, digits=0, nsmall=nsmall, scientific=F)
}


#' A simple extension of \code{rgb()}
#' @param r,g,b Red, Green, Blue: 0~255.
#' @param alpha Color opacity (transparency): 0~1.
#' If not specified, an opaque colour will be generated.
#' @return \code{"#rrggbb"} or \code{"#rrggbbaa"}
#' @examples
#' RGB(255, 0, 0)  # red: "#FF0000"
#' RGB(255, 0, 0, 0.8)  # red with 80\% opacity: "#FF0000CC"
#' @export
RGB=function(r, g, b, alpha) {
  rgb(r/255, g/255, b/255, alpha)
}


#' Timer (compute time difference)
#' @param t0 Time at the beginning.
#' @param unit Options: \code{"auto", "secs", "mins", "hours", "days", "weeks"}. Default is \code{"secs"}.
#' @param nsmall Number of decimal places of output. Default is 0.
#' @examples
#' t0=Sys.time()
#' dtime(t0)
#' @export
dtime=function(t0, unit="secs", nsmall=0) {
  dt=difftime(Sys.time(), t0, units=unit)
  format(dt, digits=nsmall)
}




#### Excel-Style Functions ####


#' Randomly sampling (like Excel's function \code{RANDBETWEEN})
#' @param range Numeric or character vector.
#' @param n Sample size for sampling.
#' @param seed Random seed.
#' @examples
#' RANDBETWEEN(1:10, n=1000000) %>% Freq()
#' RANDBETWEEN(LETTERS, n=1000000) %>% Freq()
#' @export
RANDBETWEEN=function(range, n=1, seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  # floor(runif(n=n, min=bottom, max=up+1))
  sample(range, n, replace=TRUE)
}


#' Search, match, and look up values (like Excel's functions \code{INDEX + MATCH})
#'
#' In Excel, we can use \code{VLOOKUP}, \code{HLOOKUP}, \code{XLOOKUP} (a new function released in 2019), or the combination of \code{INDEX} and \code{MATCH} to search, match, and look up values.
#' Here I provide a similar function.
#'
#' If multiple values were simultaneously matched, a warning message would be printed.
#' @import data.table
#' @importFrom dplyr left_join
#' @param data \code{data.frame} or \code{data.table}.
#' @param vars Character or character vector, specifying the variable(s) to be searched in \code{data}.
#' @param data.ref Reference data containing both the reference variable(s) and the lookup variable(s).
#' @param vars.ref Character or character vector (with the \strong{same length and order} as \code{vars}),
#' specifying the reference variable(s) to be matched in \code{data.ref}.
#' @param vars.lookup Character or character vector, specifying the variable(s) to be looked up and returned from \code{data.ref}.
#' @param return What to return. Default (\code{"new.data"}) is to return a \code{data.frame} or \code{data.table} with the lookup values added.
#' You may also set it to \code{"new.var"} or \code{"new.value"}.
#' @seealso
#' \code{dplyr::\link[dplyr]{left_join}}
#'
#' \href{https://support.office.com/en-us/article/XLOOKUP-function-B7FD680E-6D10-43E6-84F9-88EAE8BF5929}{XLOOKUP: Excel's new function, the VLOOKUP "slayer" (August 2019)}
#'
#' \href{https://www.excel-university.com/xlookup/}{XLOOKUP: Excel University}
#' @examples
#' ref=data.table(City=rep(c("A", "B", "C"), each=5),
#'                Year=rep(2013:2017, times=3),
#'                GDP=RANDBETWEEN(1000:2000, n=15, seed=1),
#'                PM2.5=RANDBETWEEN(10:300, n=15, seed=1))
#' ref
#'
#' data=data.table(sub=1:5,
#'                 city=c("A", "A", "B", "C", "C"),
#'                 year=c(2013, 2014, 2015, 2016, 2017))
#' data
#'
#' # LOOKUP(data, "city", ref, "City", "GDP")  # return with a warning
#' LOOKUP(data, c("city", "year"), ref, c("City", "Year"), "GDP")
#' LOOKUP(data, c("city", "year"), ref, c("City", "Year"), c("GDP", "PM2.5"))
#' @export
LOOKUP=function(data, vars,
                data.ref, vars.ref,
                vars.lookup,
                return=c("new.data", "new.var", "new.value")) {
  by=vars.ref
  names(by)=vars
  data.ref=as.data.frame(data.ref)
  data.new=left_join(data,
                     data.ref[c(vars.ref, vars.lookup)],
                     by=by)
  if(nrow(data.new)>nrow(data)) {
    data.ref=unique(data.ref, by=vars.ref)
    data.new=left_join(data,
                       data.ref[c(vars.ref, vars.lookup)],
                       by=by)
    warning("More than one values were matched, only the first value in 'data.ref' was returned. Please check your reference data!")
  }
  if(length(return)==3) return="new.data"
  if(return=="new.value" & length(vars.lookup)>=2) return="new.var"
  if(return=="new.data") {
    if(is.data.table(data)) data.new=as.data.table(data.new)
    return(data.new)
  } else if(return=="new.var") {
    return(data.new[vars.lookup])
  } else if(return=="new.value") {
    return(data.new[[vars.lookup]])
  }
}




#### Basic Statistics ####


#' Compute \emph{p} value
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
#' @param var Variable (e.g., \code{data$var}) or numeric vector.
#' @param nsmall Number of decimal places of output. Default is 3.
#' @param empirical \code{TRUE} (default, return 2.5\% and 97.5\% percentiles) or \code{FALSE} (basd on \emph{t} distribution).
#' If \code{TRUE}, directly extract CI from data (instead of computing CI based on normal or \emph{t} distribution).
#' @examples
#' CI(bfi$age)
#' CI(bfi$age, emp=F)  # NOTE: "emp" can partially match "empirical"
#' @export
CI=function(var, nsmall=2, empirical=TRUE) {
  M=mean(var)
  N=length(var)
  SE=sd(var)/sqrt(N)
  LLCI=ifelse(empirical, quantile(var, 0.025), M + qt(0.025,N)*SE)
  ULCI=ifelse(empirical, quantile(var, 0.975), M + qt(0.975,N)*SE)
  Print("Mean = {M:.{nsmall}}, 95% CI [{LLCI:.{nsmall}}, {ULCI:.{nsmall}}]{ifelse(empirical, ' (empirical)', '')}")
}


#' Descriptive statistics
#' @importFrom GGally ggpairs wrap
#' @param data \code{data.frame} or \code{data.table}.
#' @param nsmall Number of decimal places of output. Default is 2.
#' @param plot \code{TRUE} or \code{FALSE} (default), visualize the descriptive statistics with \code{GGally::\link[GGally]{ggpairs}}.
#' @param smooth \code{FALSE} (default), \code{"lm"}, or \code{"loess"}, add fitted lines to scatter plots (if any).
#' @param save.file \code{NULL} (default, plot in RStudio) or a file name (\code{"xxx.png"}).
#' @param save.size Size (in "inch") of the saved plot. Default is \code{"8:6"}.
#' @param save.dpi DPI (dots per inch) of the saved plot. Default is \code{500}.
#' @examples
#' d=bfi
#' d$gender=as.factor(d$gender)
#' d$education=as.factor(d$education)
#' Describe(d[c("age", "gender", "education")])
#' Describe(d[c("age", "gender", "education")], plot=T)
#'
#' Describe(airquality, plot=T, smooth="lm",
#'          save.file="Desc.png", save.size="10:8", save.dpi=1000)
#' @export
Describe=function(data, nsmall=2, plot=FALSE, smooth=FALSE,
                  save.file=NULL, save.size="8:6", save.dpi=500) {
  Print("Descriptive statistics:")
  desc=psych::describe(data, fast=FALSE)
  desc$vars = desc$trimmed = desc$mad = desc$range = desc$se = NULL
  names(desc)=c("N", "Mean", "SD", "Median", "Min", "Max", "Skewness", "Kurtosis")
  desc$Missing=nrow(data)-desc$N
  desc$Missing=ifelse(desc$Missing==0, NA, desc$Missing)
  if(length(as.data.frame(data))==1)
    row.names(desc)=as.character(sys.call())[2]
  print_table(desc, nsmalls=c(0, rep(nsmall, 7), 0))

  if(plot) {
    smooth=ifelse(smooth==FALSE, "points",
                  ifelse(smooth=="lm", "smooth",
                         ifelse(smooth=="loess", "smooth_loess")))
    p=ggpairs(data,
              lower=list(continuous=wrap(smooth, size=1, shape=16, alpha=0.3)),
              upper=list(continuous=wrap("cor", color="black"))) +
      theme_bruce(panel.bg="grey95")
    if(is.null(save.file)) {
      print(p)
    } else {
      size=as.numeric(strsplit(save.size, ":")[[1]])
      ggsave(save.file, p, width=size[1], height=size[2], dpi=save.dpi)
      path=ifelse(grepl(":", save.file), save.file, paste0(getwd(), '/', save.file))
      Print("\n\n\n<<green \u2714>> Plot saved to <<blue '{path}'>>")
    }
  }

  invisible(list(desc, plot=p))
}


#' Frequency statistics with histogram and density plot
#' @import ggplot2
#' @param var Vector or variable.
#' @param label [optional] A vector re-defining the labels of values.
#' @param sort \code{""} (default, sorted by raw order), \code{"-"} (decreasing order), or \code{"+"} (increasing order).
#' @param nsmall Number of decimal places of output. Default is 1.
#' @param plot \code{TRUE} or \code{FALSE} (default), draw a histogram plot.
#' @param bins Number of bars in histogram plot. Default is \code{18}.
#' @param fill.color Color filled in histogram bar. Default is \code{"#FDF6E3"}.
#' @examples
#' Freq(bfi$education)
#' Freq(bfi$gender, label=c("Male", "Female"))
#' Freq(bfi$age, plot=T)
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
      theme_bruce()
    print(p)
  }
  invisible(output)
}

## @rdname Freq
## @export
## freq=Freq


#' Correlation analysis with test and plot
#' @importFrom psych corr.test cor.plot
#' @importFrom Hmisc capitalize
#' @inheritParams Describe
#' @param data \code{data.frame} or \code{data.table}.
#' @param method \code{"pearson"} (default), \code{"spearman"}, or \code{"kendall"}.
#' @param adjust Adjustment for multiple tests: \code{"none", "holm", "bonferroni", "fdr", ...}. (See \code{\link[stats]{p.adjust}} for details.)
#' @param CI \code{TRUE} (default) or \code{FALSE}, output confidence intervals of correlations.
#' @param nsmall Number of decimal places of output. Default is 4.
#' @param plot \code{TRUE} (default) or \code{FALSE}, plot the correlation matrix.
#' @param plot.range Range of correlation coefficients for plot. Default is \code{c(-1, 1)}.
#' @param plot.color Color gradient for plot. Default is \code{c("#B52127", "white", "#2171B5")}.
#' You may also set it to, e.g., \code{c("red", "white", "blue")}.
#' @examples
#' Corr(airquality)
#' Corr(airquality, adjust="bonferroni")
#' Corr(airquality, save.file="Air-Corr.png")
#'
#' Corr(bfi[c("gender", "age", "education")])
#' Corr(bfi, CI=FALSE, save.file="BFI-Corr.png", save.size="9:9")
#' @export
Corr=function(data, method="pearson",
              adjust="none", CI=TRUE, nsmall=4,
              plot=TRUE, plot.range=c(-1, 1),
              plot.color=c("#B52127", "white", "#2171B5"),
              save.file=NULL, save.size="8:6", save.dpi=500) {
  cor=cor0=corr.test(data, method=method, adjust=adjust)
  # print(cor, digits=nsmall, short=!CI)
  Print("Correlation matrix ({capitalize(method)}'s <<italic r>>):")
  cor$r[cor$r==1]=NA
  print_table(cor$r, nsmalls=nsmall)
  Print("\n\n\nSig. (2-tailed):")
  cor$p=p.trans2(cor$p) %>% gsub(" ", "", .) %>% gsub("=", " ", .)
  for(i in 1:nrow(cor$p)) cor$p[i,i]=""
  print_table(cor$p)
  if(adjust!="none") Print("<<blue P-values above the diagonal are adjusted for multiple tests ({capitalize(adjust)}).>>")
  if(class(cor$n)=="matrix") {
    Print("\n\n\nSample size:")
    print_table(cor$n, nsmalls=0)
  }
  if(class(cor$n)=="numeric") {
    Print("\n\n\nSample size: <<italic N>> = {cor$n}")
  }
  if(CI) {
    Print("\n\n\n95% CI for <<italic r>>:")
    cor$ci=cor$ci[c(2,1,3,4)]
    names(cor$ci)=c("r", "[95% ", "  CI]", "pval")
    print_table(cor$ci, nsmalls=nsmall)
  }

  cor=cor0
  if(plot) {
    if(!is.null(save.file)) {
      size=as.numeric(strsplit(save.size, ":")[[1]])
      png(file=save.file, width=size[1], height=size[2], units="in", res=save.dpi)
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

  # partial correlation:
  # print(corpcor::cor2pcor(cor(data)), digits=4)

  invisible(cor)
}


#' Test the difference between two correlations (independent / nonindependent)
#' @param r1,r2 Correlation coefficients (Pearson's \emph{r}).
#' @param n1,n2 Sample sizes.
#' @param rcov [Optional] Only for nonindependent \emph{r}s:
#'
#' \code{r1} is r(X,Y),
#'
#' \code{r2} is r(X,Z),
#'
#' then, as Y and Z are also correlated,
#'
#' we should also consider \code{rcov}: r(Y,Z)
#' @return Invisibly return the \emph{p} value.
#' @examples
#' # two independent rs (X~Y vs. Z~W)
#' Corr_diff(0.20, 100, 0.45, 100)
#'
#' # two nonindependent rs (X~Y vs. X~Z, with Y and Z also correlated [rcov])
#' Corr_diff(0.20, 100, 0.45, 100, rcov=0.80)
#' @export
Corr_diff=function(r1, n1, r2, n2, rcov=NULL) {
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
    if(n1!=n2) stop("'n1' should be equal to 'n2'!")
    n=n1
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
