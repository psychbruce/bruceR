#### Basic Functions ####


#' A simple extension of \code{\%in\%}.
#'
#' @param x Numeric or character vector.
#' @param vector Numeric or character vector.
#'
#' @examples
#' data=data.table(ID=1:10, X=RANDBETWEEN(1:10, 10))
#' data
#' data[ID %notin% c(1, 3, 5, 7, 9)]
#'
#' @seealso \code{\link[base]{match}} (\code{\%in\%})
#'
#' @export
`%notin%`=function(x, vector) {
  match(x, vector, nomatch=0) == 0
}


#' A simple extension of \code{\%in\%}.
#'
#' @inheritParams %notin%
#'
#' @examples
#' 1:2 %allin% 1:3  # TRUE
#' 3:4 %allin% 1:3  # FALSE
#'
#' @seealso \code{\link[base]{match}} (\code{\%in\%}), \code{\link{\%anyin\%}}, \code{\link{\%nonein\%}}, \code{\link{\%partin\%}}
#'
#' @export
`%allin%`=function(x, vector) {
  all(x %in% vector)
}


#' A simple extension of \code{\%in\%}.
#'
#' @inheritParams %notin%
#'
#' @examples
#' 3:4 %anyin% 1:3  # TRUE
#' 4:5 %anyin% 1:3  # FALSE
#'
#' @seealso \code{\link[base]{match}} (\code{\%in\%}), \code{\link{\%allin\%}}, \code{\link{\%nonein\%}}, \code{\link{\%partin\%}}
#'
#' @export
`%anyin%`=function(x, vector) {
  any(x %in% vector)
}


#' A simple extension of \code{\%in\%}.
#'
#' @inheritParams %notin%
#'
#' @examples
#' 3:4 %nonein% 1:3  # FALSE
#' 4:5 %nonein% 1:3  # TRUE
#'
#' @seealso \code{\link[base]{match}} (\code{\%in\%}), \code{\link{\%allin\%}}, \code{\link{\%anyin\%}}, \code{\link{\%partin\%}}
#'
#' @export
`%nonein%`=function(x, vector) {
  !any(x %in% vector)
}


#' A simple extension of \code{\%in\%}.
#'
#' @param pattern Character string containing \strong{regular expressions} to be matched.
#' @param vector Character vector.
#'
#' @examples
#' "Bei" %partin% c("Beijing", "Shanghai")  # TRUE
#' "bei" %partin% c("Beijing", "Shanghai")  # FALSE
#' "[aeiou]ng" %partin% c("Beijing", "Shanghai")  # TRUE
#'
#' @seealso \code{\link[base]{match}} (\code{\%in\%}), \code{\link{\%allin\%}}, \code{\link{\%anyin\%}}, \code{\link{\%nonein\%}}
#'
#' @export
`%partin%`=function(pattern, vector) {
  any(grepl(pattern, vector, perl=TRUE))
}


#' Set working directory to where the \strong{current} script is.
#'
#' @param dir \code{NULL} (default) or a character string specifying the working directory.
#' If \code{NULL}, set working directory to the path of \strong{the current R script}.
#'
#' @examples
#' # set.wd()  # set working directory to the path of the current R script
#' # set.wd("D:/")  # "\" is not allowed, you should use "/"
#' # set.wd("../")  # set working directory to the parent directory
#'
#' @seealso \code{\link{setwd}}
#'
#' @export
set.wd=function(dir=NULL) {
  if(is.null(dir)) dir=dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(dir)
  path=getwd()
  Print("<<green \u2714>> Set working directory to <<blue '{path}'>>")
}


#' Check dependencies of R packages.
#'
#' @param pkgs Package(s).
#' @param excludes [optional] Package(s) and their dependencies excluded from the dependencies of \code{pkgs}.
#' Useful if you want to see the unique dependencies of \code{pkgs}.
#'
#' @examples
#' ## Not run:
#'
#' # pkg_depend("jmv")
#' # pkg_depend("dplyr", excludes="jmv")  # no unique dependencies
#'
#' # pkg_depend(pacman::p_depends("bruceR", local=TRUE)$Imports)
#'
#' ## End(Not run)
#'
#' @export
pkg_depend=function(pkgs, excludes=NULL) {
  default.pkgs=c("base", "boot", "class",
                 "cluster", "codetools", "compiler",
                 "datasets", "foreign", "graphics",
                 "grDevices", "grid", "KernSmooth",
                 "lattice", "MASS", "Matrix",
                 "methods", "mgcv", "nlme",
                 "nnet", "parallel", "rpart",
                 "spatial", "splines", "stats",
                 "stats4", "survival", "tcltk",
                 "tools", "translations", "utils")
  exclude.pkgs=default.pkgs
  for(ex in excludes)
    exclude.pkgs=union(exclude.pkgs, unlist(tools::package_dependencies(ex, recursive=TRUE)))
  for(pkg in pkgs)
    pkgs=union(pkgs, unlist(tools::package_dependencies(pkg, recursive=TRUE)))
  return(sort(setdiff(pkgs, exclude.pkgs)))
}


#' Install suggested R packages.
#'
#' It checks and installs R packages suggested by \code{bruceR} (default)
#' or any other package.
#'
#' @param by Suggested by which package? Default is \code{"bruceR"}.
#'
#' @examples
#' # pkg_depend("irr")
#' # pacman::p_delete(irr, lpSolve)
#' # pkg_install_suggested()
#'
#' @export
pkg_install_suggested=function(by="bruceR") {
  pkgs.depends=pacman::p_depends(by, character.only=TRUE, local=TRUE)
  pkgs.installed=pacman::p_library()
  pkgs.need.install=setdiff(union(pkgs.depends$Imports,
                                  pkgs.depends$Suggests),
                            pkgs.installed)
  if(length(pkgs.need.install)>0)
    utils::install.packages(pkgs.need.install)
  else
    Print("<<green All packages suggested by `{by}` have been installed!>>")
}


#' Print texts with rich formats and colors.
#'
#' @describeIn Print Paste and print strings.
#'
#' @description
#' Be frustrated with \code{print()} and \code{cat()}? Try \code{Print()}!
#' Run examples to see what it can do.
#'
#' @details
#' See more details in \code{glue::\link[glue]{glue}} and \code{glue::\link[glue]{glue_col}}.
#'
#' @param ... Character strings enclosed by \code{"{ }"} will be evaluated as R codes.
#'
#' Character strings enclosed by \code{"<< >>"} will be printed as formatted and colored texts.
#'
#' Long strings are broken by line and concatenated together.
#'
#' Leading whitespace and blank lines from the first and last lines are automatically trimmed.
#'
#' @examples
#' name="Bruce"
#' Print("My name is <<underline <<bold {name}>>>>.
#'        <<bold <<blue Pi = {pi:.15}.>>>>
#'        <<italic <<green 1 + 1 = {1 + 1}.>>>>
#'        sqrt({x}) = <<red {sqrt(x):.3}>>", x=10)
#'
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
#'
#' @importFrom glue glue glue_col
#' @importFrom crayon bold italic underline reset blurred inverse hidden strikethrough
#' @importFrom crayon black white silver red green blue yellow cyan magenta
#' @importFrom crayon bgBlack bgWhite bgRed bgGreen bgBlue bgYellow bgCyan bgMagenta
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


#' Repeat a character string for many times and paste them up.
#'
#' @param char Character string.
#' @param rep.times Times for repeat.
#'
#' @examples
#' rep_char("a", 5)
#'
#' @export
rep_char=function(char, rep.times) {
  paste(rep(char, rep.times), collapse="")
}


## Capitalize the first letter of a string.
capitalize=function(string) {
  capped=grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1)=toupper(substr(string[capped], 1, 1))
  return(string)
}


#' Print a three-line table.
#'
#' @param x Matrix, data.frame (or data.table), or any model object (e.g., \code{lm, glm, lmer, glmer, ...}).
#' @param row.names Whether to print row names. Default is \code{TRUE}.
#' @param nsmalls A number or numeric vector specifying the number of decimal places of output. Default is \code{3}.
#'
#' @examples
#' model=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
#' print_table(model)
#'
#' @importFrom stats coef
#' @export
print_table=function(x, row.names=TRUE, nsmalls=3) {
  ## Preprocess data.frame ##

  # Good-looking tabs !!!
  # Print("\u2500\u2501\u2502\u2503\u2504\u2505\u2506\u2507\u2508\u2509")
  linechar1="\u2501"  # top-and-down '=' [bug in some computers!]
  linechar2="\u2500"  # in-table '-'
  linechar1=linechar2

  if(class(x) %nonein% c("matrix", "data.frame", "data.table")) {
    coef.table=coef(summary(x))
    if(!is.null(coef.table)) x=coef.table
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
    # if(grepl("^[Ee]stimate$", names(x)[j])) names(x)[j]="Coef."
    names(x)[j]=gsub(" value|val$", "", names(x)[j])
  }
  if(is.null(sig)==FALSE & "sig" %notin% names(x)) {
    p.pos=which(names(x)=="p")
    nvars=length(names(x))
    if(p.pos<nvars)
      x=cbind(x[1:p.pos], ` `=sig, x[(p.pos+1):nvars])
    else
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


#' Format "1234" to "1,234".
#'
#' @param x A number or numeric vector.
#' @param mark Usually \code{","}.
#'
#' @return Formatted character string.
#'
#' @examples
#' formatN(1234)
#'
#' @seealso \code{\link{formatF}}
#'
#' @export
formatN=function(x, mark=",") {
  format(x, big.mark=mark)
}


#' Format numeric values.
#'
#' @param x A number or numeric vector.
#' @param nsmall Number of decimal places of output. Default is \code{3}.
#'
#' @return Formatted character string.
#'
#' @examples
#' formatF(pi, 20)
#'
#' @seealso \code{\link{formatN}}
#'
#' @export
formatF=function(x, nsmall=3) {
  format(x, digits=0, nsmall=nsmall, scientific=F)
}


#' A simple extension of \code{rgb()}.
#'
#' @param r,g,b Red, Green, Blue: 0~255.
#' @param alpha Color transparency (opacity): 0~1.
#' If not specified, an opaque color will be generated.
#'
#' @return \code{"#rrggbb"} or \code{"#rrggbbaa"}
#'
#' @examples
#' RGB(255, 0, 0)  # red: "#FF0000"
#' RGB(255, 0, 0, 0.8)  # red with 80\% opacity: "#FF0000CC"
#'
#' @export
RGB=function(r, g, b, alpha) {
  grDevices::rgb(r/255, g/255, b/255, alpha)
}


#' Timer (compute time difference).
#'
#' @param t0 Time at the beginning.
#' @param unit Options: \code{"auto", "secs", "mins", "hours", "days", "weeks"}. Default is \code{"secs"}.
#' @param nsmall Number of decimal places of output. Default is \code{0}.
#'
#' @examples
#' t0=Sys.time()
#' dtime(t0)
#'
#' @export
dtime=function(t0, unit="secs", nsmall=0) {
  dt=difftime(Sys.time(), t0, units=unit)
  format(dt, digits=nsmall)
}




#### Excel-Style Functions ####


#' Random sampling (like Excel's function \code{RANDBETWEEN}).
#'
#' @param range Numeric or character vector.
#' @param n Sample size for sampling. Default is \code{1}.
#' @param seed Random seed.
#'
#' @examples
#' RANDBETWEEN(1:10, n=1000000) %>% Freq()
#' RANDBETWEEN(LETTERS, n=1000000) %>% Freq()
#'
#' @export
RANDBETWEEN=function(range, n=1, seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  # floor(runif(n=n, min=bottom, max=up+1))
  sample(range, n, replace=TRUE)
}


#' Search, match, and look up values (like Excel's functions \code{INDEX + MATCH}).
#'
#' In Excel, we can use \code{VLOOKUP}, \code{HLOOKUP}, \code{XLOOKUP} (a new function released in 2019),
#' or the combination of \code{INDEX} and \code{MATCH} to search, match, and look up values.
#' Here I provide a similar function.
#'
#' If multiple values were simultaneously matched, a warning message would be printed.
#'
#' @param data Main data.
#' @param vars Character or character vector, specifying the variable(s) to be searched in \code{data}.
#' @param data.ref Reference data containing both the reference variable(s) and the lookup variable(s).
#' @param vars.ref Character or character vector (with the \strong{same length and order} as \code{vars}),
#' specifying the reference variable(s) to be matched in \code{data.ref}.
#' @param vars.lookup Character or character vector, specifying the variable(s) to be looked up and returned from \code{data.ref}.
#' @param return What to return. Default (\code{"new.data"}) is to return a \code{data.frame} or \code{data.table} with the lookup values added.
#' You may also set it to \code{"new.var"} or \code{"new.value"}.
#'
#' @seealso
#' \code{dplyr::left_join}
#'
#' \href{https://support.office.com/en-us/article/XLOOKUP-function-B7FD680E-6D10-43E6-84F9-88EAE8BF5929}{XLOOKUP: Excel's new function, the VLOOKUP "slayer" (August 2019)}
#'
#' \href{https://www.excel-university.com/xlookup/}{XLOOKUP: Excel University}
#'
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
#'
#' @export
LOOKUP=function(data, vars,
                data.ref, vars.ref,
                vars.lookup,
                return=c("new.data", "new.var", "new.value")) {
  by=vars.ref
  names(by)=vars
  data.ref=as.data.frame(data.ref)
  data.new=dplyr::left_join(data,
                            data.ref[c(vars.ref, vars.lookup)],
                            by=by)
  if(nrow(data.new)>nrow(data))
    warning("More than one values are matched!")
  if(length(return)==3) return="new.data"
  if(return=="new.value" & length(vars.lookup)>=2) return="new.var"
  if(return=="new.data") {
    return(data.new)
  } else if(return=="new.var") {
    return(data.new[vars.lookup])
  } else if(return=="new.value") {
    return(data.new[[vars.lookup]])
  }
}

