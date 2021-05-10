#### Basic Functions ####


`%>%`=dplyr::`%>%`


#' Paste strings together.
#'
#' Paste strings together. A wrapper of \code{paste0()}.
#' Why \code{\%^\%}? Because typing \code{\%} and \code{^} is pretty easy by
#' pressing \strong{Shift + 5 + 6 + 5}.
#'
#' @param x,y Any objects, usually a numeric or character string or vector.
#'
#' @return A character string/vector of the pasted values.
#'
#' @examples
#' "He" %^% "llo"
#' "X" %^% 1:10
#' "Q" %^% 1:5 %^% letters[1:5]
#'
#' @export
`%^%`=function(x, y) {
  paste0(x, y)
}


#' The opposite of \code{\%in\%}.
#'
#' @param x Numeric or character vector.
#' @param vector Numeric or character vector.
#'
#' @return A vector of \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' data=data.table(ID=1:10, X=RANDBETWEEN(1:10, 10))
#' data
#' data[ID %notin% c(1, 3, 5, 7, 9)]
#'
#' @seealso \code{\link[base:match]{\%in\%}}
#'
#' @export
`%notin%`=function(x, vector) {
  match(x, vector, nomatch=0) == 0
}


#' A simple extension of \code{\%in\%}.
#'
#' @inheritParams %notin%
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' 1:2 %allin% 1:3  # TRUE
#' 3:4 %allin% 1:3  # FALSE
#'
#' @seealso \code{\link[base:match]{\%in\%}},
#' \code{\link{\%anyin\%}},
#' \code{\link{\%nonein\%}},
#' \code{\link{\%partin\%}}
#'
#' @export
`%allin%`=function(x, vector) {
  all(x %in% vector)
}


#' A simple extension of \code{\%in\%}.
#'
#' @inheritParams %notin%
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' 3:4 %anyin% 1:3  # TRUE
#' 4:5 %anyin% 1:3  # FALSE
#'
#' @seealso \code{\link[base:match]{\%in\%}},
#' \code{\link{\%allin\%}},
#' \code{\link{\%nonein\%}},
#' \code{\link{\%partin\%}}
#'
#' @export
`%anyin%`=function(x, vector) {
  any(x %in% vector)
}


#' A simple extension of \code{\%in\%}.
#'
#' @inheritParams %notin%
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' 3:4 %nonein% 1:3  # FALSE
#' 4:5 %nonein% 1:3  # TRUE
#'
#' @seealso \code{\link[base:match]{\%in\%}},
#' \code{\link{\%allin\%}},
#' \code{\link{\%anyin\%}},
#' \code{\link{\%partin\%}}
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
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' "Bei" %partin% c("Beijing", "Shanghai")  # TRUE
#' "bei" %partin% c("Beijing", "Shanghai")  # FALSE
#' "[aeiou]ng" %partin% c("Beijing", "Shanghai")  # TRUE
#'
#' @seealso \code{\link[base:match]{\%in\%}},
#' \code{\link{\%allin\%}},
#' \code{\link{\%anyin\%}},
#' \code{\link{\%nonein\%}}
#'
#' @export
`%partin%`=function(pattern, vector) {
  any(grepl(pattern, vector, perl=TRUE))
}


#' Set working directory to where the current file is.
#'
#' Set working directory to the path of the currently opened file.
#' You can use this function in both \strong{.R/.Rmd files and R Console}.
#' \href{https://www.rstudio.com/products/rstudio/download/preview/}{RStudio}
#' (version >= 1.2) is required for running this function.
#'
#' @param path \code{NULL} (default) or a specific path.
#' Default is to extract the path of the currently opened file
#' (usually .R or .Rmd) using the \code{rstudioapi::getSourceEditorContext} function.
#' @param directly \code{TRUE} (default) or \code{FALSE}.
#' Default is to directly execute \code{setwd("...")} within the function (recommended).
#' Otherwise, it will send code \code{setwd("...")} to the R Console
#' and then execute it (not recommended due to a delay of execution).
#' @param ask \code{TRUE} or \code{FALSE} (default).
#' If \code{TRUE}, you can select a folder with the prompt of a dialog.
#'
#' @return Invisibly return the path.
#'
#' @examples
#' \dontrun{
#' # RStudio (version >= 1.2) is required for running this function.
#' set.wd()  # set working directory to the path of the currently opened file
#' set.wd("~/")  # set working directory to the home directory
#' set.wd("../")  # set working directory to the parent directory
#' set.wd(ask=TRUE)  # select a folder with the prompt of a dialog
#' }
#'
#' @seealso \code{\link[base:getwd]{setwd}}
#'
#' @export
set.wd=function(path=NULL, directly=TRUE, ask=FALSE) {
  if(rstudioapi::isAvailable()==FALSE)
    stop("RStudio is required for running this function!\n\n",
         "Please download and install the latest version of RStudio:\n",
         "https://rstudio.com/products/rstudio/download/preview/")
  is.windows=ifelse(Sys.info()[["sysname"]]=="Windows", TRUE, FALSE)
  if(is.null(path)) {
    tryCatch({
      if(ask) {
        # RStudio version >= 1.1.287
        if(is.windows)
          path=iconv(rstudioapi::selectDirectory(), from="UTF-8", to="GBK")
        else
          path=rstudioapi::selectDirectory()
      } else {
        # # RStudio version >= 1.4.843
        # if(is.windows)
        #   file.path=iconv(rstudioapi::documentPath(), from="UTF-8", to="GBK")
        # else
        #   file.path=rstudioapi::documentPath()

        # RStudio version >= 0.99.1111
        path=dirname(rstudioapi::getSourceEditorContext()$path)
      }
    }, error=function(e) {
      # Error: Function documentPath not found in RStudio
      message("Your RStudio version is: ", rstudioapi::getVersion(), "\n")
      message("Please update RStudio to the latest version:\n",
              "https://rstudio.com/products/rstudio/download/preview/\n")
    })
  }
  if(length(path)>0) {
    if(directly) {
      eval(parse(text=paste0("setwd(\"", path, "\")")))
      Print("<<green \u2714>> Set working directory to <<blue \"{getwd()}\">>")
    } else {
      rstudioapi::sendToConsole(paste0("setwd(\"", path, "\")"), execute=TRUE)
    }
  }
  invisible(path)
}


#' Check dependencies of R packages.
#'
#' @param pkgs Package(s).
#' @param excludes [optional] Package(s) and their dependencies excluded from the dependencies of \code{pkgs}.
#' Useful if you want to see the unique dependencies of \code{pkgs}.
#'
#' @return A character vector of package names.
#'
## @examples
## pkg_depend("jmv")
## pkg_depend("dplyr", excludes="jmv")  # no unique dependencies
## pkg_depend(pacman::p_depends("bruceR", local=TRUE)$Imports)
##
#' @seealso \code{\link{pkg_install_suggested}}
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
#' @return No return value.
#'
## @examples
## pkg_install_suggested()
##
#' @seealso \code{\link{pkg_depend}}
#'
#' @export
pkg_install_suggested=function(by="bruceR") {
  if(by=="bruceR") {
    pkgs.suggests=c(
      "devtools", "tidyverse", "ggstatsplot",
      "sampling", "irr", "correlation", "forecast",
      "JSmediation", "processR", "lavaan", "metafor",
      "AER", "TOSTER", "BEST", "BayesFactor",
      "pwr", "simr", "r2mlm", "multilevel",
      "caret", "party", "randomForest", "jiebaR",
      "sjstats", "reghelper", "summarytools", "apaTables",
      "ggrepel", "ggsignif", "ggridges", "ggthemes", "ggExtra",
      "corrplot", "showtext"
    )
  } else {
    pkgs.suggests=pacman::p_depends(by, character.only=TRUE, local=TRUE)$Suggests
  }
  pkgs.installed=pacman::p_library()
  pkgs.need.install=setdiff(pkgs.suggests, pkgs.installed)
  if(length(pkgs.need.install)>0)
    utils::install.packages(pkgs.need.install)
  else
    Print("<<green All packages suggested by `{by}` have been installed!>>")
}


#' Print strings with rich formats and colors.
#'
#' @describeIn Print Paste and print strings.
#'
#' @description
#' Be frustrated with \code{print()} and \code{cat()}? Try \code{Print()}!
#' Run examples to see what it can do.
#'
#' @details
#' See more details in \code{\link[glue:glue]{glue::glue()}} and \code{\link[glue:glue]{glue::glue_col()}}.
#'
#' @param ... Character strings enclosed by \code{"{ }"} will be evaluated as R code.
#'
#' Character strings enclosed by \code{"<< >>"} will be printed as formatted and colored text.
#'
#' Long strings are broken by line and concatenated together.
#'
#' Leading whitespace and blank lines from the first and last lines are automatically trimmed.
#'
#' @return Formatted text.
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


#' Run code parsed from text.
#'
#' @param text Character strings for running.
#' You can use \code{"{ }"} to insert any R object in the environment.
#'
#' @return No return value.
#'
#' @examples
#' Run("a=1")
#' a
#'
#' Run("a={a+1}")
#' a
#'
#' @export
Run=function(text) {
  eval(parse(text=Glue(text)), envir=parent.frame())
}


#' Repeat a character string for many times and paste them up.
#'
#' @param char Character string.
#' @param rep.times Times for repeat.
#'
#' @return Character string.
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


#' Print a three-line table (to R Console or MS Word).
#'
#' This basic function prints any data frame as a three-line table
#' to either R Console or Microsoft Word (.doc).
#' It has been used in many other functions in \code{bruceR}.
#' The implementation of Word output is using HTML code.
#' You can check the raw HTML code by opening the Word file with any text editor.
#'
#' @param x Matrix, data.frame (or data.table), or any model object (e.g., \code{lm, glm, lmer, glmer, ...}).
#' @param row.names Whether to print row names. Default is \code{TRUE}.
#' @param nsmalls A number or numeric vector specifying the number of decimal places of output. Default is \code{3}.
#' @param title Title text, which will be inserted in <p></p> (HTML code).
#' @param note Note text, which will be inserted in <p></p> (HTML code).
#' @param append Other contents appended in the end (HTML code).
#' @param file File name of MS Word (\code{.doc}).
#' @param file.align.head,file.align.text Alignment of table head or table text:
#' \code{"left"}, \code{"right"}, \code{"center"}.
#' Either one value of them OR a character vector of mixed values
#' with the same length as the table columns.
#' Default alignment (if set as \code{"auto"}):
#' left, right, right, ..., right.
#'
#' @return Invisibly return a list of data frame and HTML code.
#'
#' @seealso \code{\link{Describe}}, \code{\link{Freq}}, \code{\link{Corr}}
#'
#' @examples
#' print_table(airquality, file="airquality.doc")
#' unlink("airquality.doc")  # delete file for test
#'
#' model=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
#' print_table(model)
#' print_table(model, file="model.doc")
#' unlink("model.doc")  # delete file for test
#'
#' @importFrom stats coef
#' @export
print_table=function(x, row.names=TRUE, nsmalls=3,
                     title="", note="", append="",
                     file=NULL,
                     file.align.head="auto",
                     file.align.text="auto") {
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
    # if(grepl("\\[", names(x)[j])) x[,j]=paste0("[", x[,j], ",")
    # if(grepl("\\]", names(x)[j])) x[,j]=paste0(x[,j], "]")
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
  xr=rbind(line.row, x)
  if(row.names==FALSE)
    n.lines.table=n.lines.table-n.lines.rn
  table.line=rep_char(linechar1, n.lines.table)

  ## Output ##
  if(is.null(file)) {
    if(title!="") Print(title)
    Print(table.line)
    if(row.names==TRUE)
      cat(rep_char(" ", n.lines.rn))
    for(j in 1:length(xr))
      cat(sprintf(glue("% {n.lines[j]}s"), names(xr)[j]))
    cat("\n")
    for(i in 1:nrow(xr)) {
      if(row.names==TRUE)
        cat(sprintf(glue("%-{n.lines.rn}s"), row.names(xr[i,])))
      for(j in 1:length(xr))
        cat(sprintf(glue("% {n.lines[j]}s"), ifelse(is.na(xr[i,j]) | grepl("NA$", xr[i,j]), "", xr[i,j])))
      cat("\n")
    }
    Print(table.line)
    if(note!="") Print(note)
  }
  if(row.names==TRUE) {
    x=cbind(rn=row.names(x), x)
    names(x)[1]=""
  }
  html=df_to_html(x, title=title, note=note, append=append,
                  file=file,
                  align.head=file.align.head,
                  align.text=file.align.text)

  invisible(list(df=x, html=html))
}


df_to_html=function(df, title="", note="", append="",
                    replace.minus=TRUE,
                    file=NULL,
                    align.head="auto",
                    align.text="auto") {
  if(!is.null(file))
    file=stringr::str_replace(file, "\\.docx$", ".doc")

  TITLE=title
  TNOTE=note
  APPEND=append

  if(length(align.head)==1) {
    if(align.head=="auto")
      align.head=c("left", rep("right", times=ncol(df)-1))
    else
      align.head=rep(align.head, times=ncol(df))
  }
  if(length(align.text)==1) {
    if(align.text=="auto")
      align.text=c("left", rep("right", times=ncol(df)-1))
    else
      align.text=rep(align.text, times=ncol(df))
  }

  df=as.data.frame(df)
  for(j in 1:ncol(df)) {
    df[[j]]="<td align='" %^% align.text[j] %^% "'>" %^%
      stringr::str_trim(stringr::str_replace_all(df[[j]], "^\\s*-", "\u2013")) %^% "</td>"
  }

  THEAD="<tr> " %^%
    paste("<th align='" %^%
            align.head %^%
            "'>" %^% names(df) %^% "</th>",
          collapse=" ") %^% " </tr>"

  TBODY="<tr> " %^%
    paste(apply(df, 1, function(...) paste(..., collapse=" ")),
          collapse=" </tr>\n<tr> ") %^% " </tr>"
  if(replace.minus)
  TBODY=stringr::str_replace_all(TBODY, ">\\s*NA\\s*<", "><")
  TBODY=stringr::str_replace_all(TBODY, "\\s+</td>", "")

  TABLE=paste0("
<table>
<thead>
", THEAD, "
</thead>
<tbody>
", TBODY, "
</tbody>
</table>
")

  HTML=paste0("<!DOCTYPE html>
<html>

<head>
<meta charset='utf-8'>
<title></title>
<style>
", ifelse(
  grepl("\\.doc$", file),
  "body {font-size: 10.5pt; font-family: Times New Roman;}",
  ""
), "
p {margin: 0px;}
table {border-collapse: collapse; border-spacing: 0px; color: #000000;
       border-top: 2px solid #000000; border-bottom: 2px solid #000000;}
table thead th {border-bottom: 1px solid #000000;}
table th, table td {padding-left: 5px; padding-right: 5px; height: 19px;}
</style>
</head>

<body>
<p>", TITLE, "</p>", TABLE, "<p>", TNOTE, "</p>", APPEND, "
</body>

</html>")

  if(!is.null(file)) {
    # sink(file)
    # cat(HTML)
    # sink()
    f=file(file, "w", encoding="UTF-8")
    cat(HTML, file=f)
    close(f)
    Print("<<green \u2714>> Table saved to <<blue '{paste0(getwd(), '/', file)}'>>")
    cat("\n")
  }

  invisible(list(HTML=HTML, TABLE=TABLE))
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
#' @return \code{"#rrggbb"} or \code{"#rrggbbaa"}.
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
#' @return A character string of time difference.
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
#' @return Numeric or character vector (the same class as \code{range}).
#'
#' @examples
#' RANDBETWEEN(1:10, n=1000) %>% Freq()
#' RANDBETWEEN(LETTERS, n=1000) %>% Freq()
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
#' @param vars Character (vector), specifying the variable(s) to be searched in \code{data}.
#' @param data.ref Reference data containing both the reference variable(s) and the lookup variable(s).
#' @param vars.ref Character (vector), with the \strong{same length and order} as \code{vars},
#' specifying the reference variable(s) to be matched in \code{data.ref}.
#' @param vars.lookup Character (vector), specifying the variable(s) to be looked up and returned from \code{data.ref}.
#' @param return What to return. Default (\code{"new.data"}) is to return a data frame with the lookup values added.
#' You may also set it to \code{"new.var"} or \code{"new.value"}.
#'
#' @return New data object, new variable, or new value (see the parameter \code{return}).
#'
#' @seealso
#' \code{dplyr::left_join}
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

