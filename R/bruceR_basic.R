#### Pipeline Functions ####


# `%>%`=dplyr::`%>%`


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
#' data=data.table(ID=1:10, X=sample(1:10, 10))
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



#### Basic Functions ####


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
#' @param by Suggested by which package?
#'
#' @return No return value.
#'
#' @examples
#' \dontrun{
#'
#' pkg_install_suggested()  # install all packages suggested by me
#' }
#'
#' @seealso \code{\link{pkg_depend}}
#'
#' @export
pkg_install_suggested=function(by) {
  if(missing(by)) {
    pkgs.suggests="
    rstudioapi, devtools, pacman,
    tidyverse, ggstatsplot, jmv,
    dplyr, tidyr, stringr, forcats, data.table,
    rio, haven, foreign, readxl, openxlsx, clipr,
    tibble, plyr, glue, crayon,
    emmeans, effectsize, performance,
    pwr, simr, MASS, sampling, careless,
    irr, correlation, corpcor, corrplot,
    afex, car, psych, lmtest, nnet,
    lme4, lmerTest, multilevel, r2mlm, MuMIn,
    metafor, meta, metaSEM, metapower,
    mediation, interactions, JSmediation,
    lavaan, lavaanPlot, semPlot, processR,
    jtools, reghelper, summarytools, texreg,
    sjstats, sjPlot, apaTables,
    forecast, vars, pls, plm, AER,
    TOSTER, BEST, BayesFactor, brms,
    mlr, caret, party, randomForest, e1071, varImp,
    downloader, rvest, RCurl, RSelenium, mailR, jiebaR,
    ggplot2, ggtext, cowplot, see,
    ggrepel, ggeffects, ggsignif, ggridges, ggthemes,
    ggbreak, ggplotify, ggExtra, GGally, wordcloud2,
    patchwork, showtext"
    cat("\n")
    Print(pkgs.suggests)
    cat("\n")
    yesno=utils::menu(title="All these packages would be installed. Do you want to install them?",
                      choices=c("Yes", "No"))
    if(yesno==1) {
      pkgs.suggests=pkgs.suggests %>%
        str_remove_all("\\s") %>%
        str_split(",", simplify=TRUE) %>%
        as.character()
    } else {
      return(invisible())
    }
  } else {
    pkgs.suggests=pacman::p_depends(by, character.only=TRUE, local=TRUE)$Suggests
  }
  pkgs.installed=pacman::p_library()
  pkgs.need.install=base::setdiff(pkgs.suggests, pkgs.installed)
  if(length(pkgs.need.install)>0) {
    utils::install.packages(pkgs.need.install)
  } else {
    if(missing(by))
      Print("<<green Done!>>")
    else
      Print("<<green All packages suggested by `{by}` have been installed!>>")
  }
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
#' Possible formats/colors that can be used in \code{"<< >>"} include:
#'
#' (1) bold, italic, underline, reset, blurred, inverse, hidden, strikethrough;
#'
#' (2) black, white, silver, red, green, blue, yellow, cyan, magenta;
#'
#' (3) bgBlack, bgWhite, bgRed, bgGreen, bgBlue, bgYellow, bgCyan, bgMagenta.
#'
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
    output=glue::glue(..., .transformer=sprintf_transformer, .envir=parent.frame())
    output_color=glue::glue_col( gsub("<<", "{", gsub(">>", "}", output)) )
    print(output_color)
  }, error=function(e) {
    warning(e)
    print(...)
  })
}


#' @describeIn Print Paste strings.
#'
#' @export
Glue=function(...) {
  output=glue::glue(..., .transformer=sprintf_transformer, .envir=parent.frame())
  output_color=glue::glue_col( gsub("<<", "{", gsub(">>", "}", output)) )
  return(output_color)
}


sprintf_transformer=function(text, envir) {
  text=glue::glue(text, .envir=envir)
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
#' @param ... Character string(s) to run.
#' You can use \code{"{ }"} to insert any R object in the environment.
#' @param silent Suppress error/warning messages. Default is \code{FALSE}.
#'
#' @return Invisibly return the running expression(s).
#'
#' @examples
#' Run("a=1", "b=2")
#' Run("print({a+b})")
#'
#' @export
Run=function(..., silent=FALSE) {
  text=glue::glue(..., .sep="\n", .envir=parent.frame())
  if(silent) {
    suppressWarnings({
      eval(parse(text=text), envir=parent.frame())
    })
  } else {
    eval(parse(text=text), envir=parent.frame())
  }
  invisible(text)
}


#' Split up a string (with separators) into a character vector.
#'
#' Split up a string (with separators) into a character vector (whitespace around separator is trimmed).
#'
#' @param x Character string.
#' @param sep Pattern for separation.
#'
#' Default is \code{"auto"}, including 5 common separators: , ; | \\n \\t.
#'
#' @return Character vector.
#'
#' @examples
#' cc("a,b,c,d,e")
#'
#' cc(" a , b , c , d , e ")
#'
#' cc("1, 2, 3, 4, 5")
#'
#' cc("A 1 , B 2 ; C 3 | D 4 \t E 5")
#'
#' cc("
#' American
#' British
#' Chinese
#' ")
#'
#' @export
cc=function(x, sep="auto") {
  as.character(
    stringr::str_split(
      stringr::str_trim(x),
      ifelse(sep=="auto", "\\s*[,;\\|\\n\\t]\\s*", sep),
      simplify=TRUE)
  )
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
  paste(rep(char, times=rep.times), collapse="")
}


## Capitalize the first letter of a string.
capitalize=function(string) {
  capped=grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1)=toupper(substr(string[capped], 1, 1))
  return(string)
}


#' Print a three-line table (to R Console and Microsoft Word).
#'
#' This basic function prints any data frame as a three-line table
#' to either R Console or Microsoft Word (.doc).
#' It has been used in many other functions of \code{bruceR} (see below).
#'
#' @param x Matrix, data.frame (or data.table), or any model object (e.g., \code{lm, glm, lmer, glmer, ...}).
#' @param digits,nsmalls Numeric vector specifying the number of decimal places of output. Default is \code{3}.
#' @param nspaces Number of whitespaces between columns. Default is \code{1}.
#' @param row.names,col.names Print row/column names. Default is \code{TRUE} (column names are always printed).
#' To modify the names, you can use a character vector with the same length as the raw names.
#' @param title Title text, which will be inserted in <p></p> (HTML code).
#' @param note Note text, which will be inserted in <p></p> (HTML code).
#' @param append Other contents, which will be appended in the end (HTML code).
#' @param line Lines looks like true line (\code{TRUE}) or \code{=== --- ===} (\code{FALSE}).
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
#' @seealso
#' These functions have implemented MS Word file output using this function:
#' \itemize{
#'   \item \code{\link{Describe}}
#'   \item \code{\link{Freq}}
#'   \item \code{\link{Corr}}
#'   \item \code{\link{EFA}} / \code{\link{PCA}}
#'   \item \code{\link{CFA}}
#'   \item \code{\link{TTEST}}
#'   \item \code{\link{MANOVA}}
#'   \item \code{\link{model_summary}}
#'   \item \code{\link{med_summary}}
#'   \item \code{\link{lavaan_summary}}
#'   \item \code{\link{PROCESS}}
#'   \item \code{\link{granger_test}}
#'   \item \code{\link{granger_causality}}
#' }
#'
#' @examples
#' print_table(data.frame(x=1))
#'
#' print_table(airquality, file="airquality.doc")
#' unlink("airquality.doc")  # delete file for code check
#'
#' model=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
#' print_table(model)
#' print_table(model, file="model.doc")
#' unlink("model.doc")  # delete file for code check
#'
#' @export
print_table=function(x, digits=3, nsmalls=digits,
                     nspaces=1,
                     row.names=TRUE,
                     col.names=TRUE,
                     title="", note="", append="",
                     line=TRUE,
                     file=NULL,
                     file.align.head="auto",
                     file.align.text="auto") {
  ## Preprocess data.frame ##
  if(!inherits(x, c("matrix", "data.frame", "data.table"))) {
    coef.table=coef(summary(x))
    if(!is.null(coef.table)) x=coef.table
  }
  x=as.data.frame(x)
  sig=NULL
  if(length(nsmalls)==1) nsmalls=rep(nsmalls, length(x))
  for(j in 1:length(x)) {
    if(inherits(x[,j], "factor"))
      x[,j]=as.character(x[,j])
    if(grepl("Pr\\(|pval|p.value|<i>p</i>", names(x)[j])) {
      sig=formatF(sig.trans(x[,j]), 0)  # formatF will make * left-aligned
      if(grepl("<i>p</i>", names(x)[j])==FALSE)
        names(x)[j]="p"
      x[,j]=p.trans(x[,j])
    } else {
      x[,j]=formatF(x[,j], nsmalls[j])
    }
    if(grepl("^S\\.E\\.$|^Std\\. Error$|^se$|^SE$|^BootSE$", names(x)[j])) {
      x[,j]=paste0("(", x[,j], ")")  # add ( ) to S.E.
      x[grepl("\\d", x[,j])==FALSE, j]=""  # remove ( ) from blank S.E.
      if(grepl("S\\.E\\.", names(x)[j])==FALSE) names(x)[j]="S.E."
    }
    if(grepl("^S\\.D\\.$|^Std\\. Deviation$", names(x)[j])) {
      x[,j]=paste0("(", x[,j], ")")  # add ( ) to S.D.
      x[grepl("\\d", x[,j])==FALSE, j]=""  # remove ( ) from blank S.E.
      if(grepl("S\\.D\\.", names(x)[j])==FALSE) names(x)[j]="S.D."
    }
    # if(grepl("\\[", names(x)[j])) x[,j]=paste0("[", x[,j], ",")
    # if(grepl("\\]", names(x)[j])) x[,j]=paste0(x[,j], "]")
    # if(grepl("^[Ee]stimate$", names(x)[j])) names(x)[j]="Coef."
    names(x)[j]=gsub(" value$|val$", "", names(x)[j])
  }
  if(is.null(sig)==FALSE & "sig" %notin% names(x)) {
    p.pos=which(names(x) %in% c("p", "<i>p</i>"))
    nvars=length(names(x))
    if(p.pos<nvars)
      x=cbind(x[1:p.pos], ` `=sig, x[(p.pos+1):nvars])
    else
      x=cbind(x, ` `=sig)
    x$` `=as.character(x$` `)
  }

  if(inherits(row.names, "character")) {
    row.names(x)=row.names
    row.names=TRUE
  }
  if(inherits(col.names, "character")) {
    names(x)=col.names
    col.names=TRUE
  }

  ## Compute length to generate line-chars ##
  linechar=ifelse(line, "\u2500", "-")
  title.length=nchar(names(x), type="width")
  vars.length=c()  # bug: vars.length=apply(apply(x, 2, nchar), 2, max)
  for(j in 1:length(x)) vars.length[j]=max(nchar(x[,j], type="width"))
  n.lines=apply(rbind(title.length, vars.length), 2, max)+nspaces
  n.lines.rn=max(nchar(row.names(x), type="width"))+nspaces
  if(row.names)
    table.line=rep_char(linechar, sum(n.lines)+n.lines.rn)
  else
    table.line=rep_char(linechar, sum(n.lines))

  ## Output ##
  if(is.null(file)) {
    if(title!="") Print(title)
    Print(table.line)
    if(row.names)
      cat(rep_char(" ", n.lines.rn))
    for(j in 1:length(x)) {
      name.j=names(x)[j]
      cat(rep_char(" ", n.lines[j]-nchar(name.j, type="width")) %^% name.j)
    }
    cat("\n")
    Print(table.line)
    for(i in 1:nrow(x)) {
      if(row.names) {
        row.name.i=row.names(x)[i]
        cat(row.name.i %^% rep_char(" ", n.lines.rn-nchar(row.name.i, type="width")))
      }
      for(j in 1:length(x)) {
        # cat(sprintf(glue("% {n.lines[j]}s"), ifelse(is.na(xr[i,j]) | grepl("NA$", xr[i,j]), "", xr[i,j])))
        x.ij=ifelse(is.na(x[i,j]) | grepl("NA$", x[i,j]), "", x[i,j])
        cat(rep_char(" ", n.lines[j]-nchar(x.ij, type="width")) %^% x.ij)
      }
      cat("\n")
    }
    Print(table.line)
    if(note!="") Print(note)
  }
  if(row.names) {
    x=cbind(rn=row.names(x), x)
    names(x)[1]=""
  }
  if(!is.null(file)) {
    html=df_to_html(x, title=title, note=note, append=append,
                    file=file,
                    align.head=file.align.head,
                    align.text=file.align.text)
  } else {
    html=NULL
  }

  invisible(list(df=x, html=html))
}


df_to_html=function(df, title="", note="", append="",
                    file=NULL,
                    align.head="auto",
                    align.text="auto") {
  if(!is.null(file)) {
    if(file=="NOPRINT") {
      file=NULL
    } else {
      file=str_replace(file, "\\.docx$", ".doc")
      if(str_detect(file, "\\.doc$")==FALSE)
        file=paste0(file, ".doc")
    }
  }

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
      str_trim(str_replace_all(df[[j]], "^\\s*-{1}", "\u2013")) %^% "</td>"
  }

  THEAD="<tr> " %^%
    paste("<th align='" %^%
            align.head %^%
            "'>" %^% names(df) %^% "</th>",
          collapse=" ") %^% " </tr>"

  TBODY="<tr> " %^%
    paste(apply(df, 1, function(...) paste(..., collapse=" ")),
          collapse=" </tr>\n<tr> ") %^% " </tr>"
  TBODY=TBODY %>%
    str_replace_all(">\\s*NA\\s*<", "><") %>%
    str_replace_all("\\s+</td>", "</td>") %>%
    str_replace_all("\\[\\s+", "[") %>%
    str_replace_all("\\,\\s+", ", ") %>%
    str_replace_all("<\\.001", "< .001")

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
  "body, pre {font-size: 10.5pt; font-family: Times New Roman;}",
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
    if(file!="NOPRINT") {
      # sink(file)
      # cat(HTML)
      # sink()
      f=file(file, "w", encoding="UTF-8")
      cat(HTML, file=f)
      close(f)
      Print("<<green \u221a>> Table saved to <<bold \"{paste0(getwd(), '/', file)}\">>")
      cat("\n")
    }
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
#' @seealso \code{\link[base:format]{format}}, \code{\link{formatF}}
#'
#' @export
formatN=function(x, mark=",") {
  format(x, big.mark=mark)
}


#' Format numeric values.
#'
#' @param x A number or numeric vector.
#' @param digits,nsmall Number of decimal places of output. Default is \code{3}.
#'
#' @return Formatted character string.
#'
#' @examples
#' formatF(pi, 20)
#'
#' @seealso \code{\link[base:format]{format}}, \code{\link{formatN}}
#'
#' @export
formatF=function(x, digits=3, nsmall=digits) {
  # format(x, digits=0, nsmall=nsmall, scientific=FALSE)
  if(inherits(x, "character")) {
    xf=sprintf(paste0("%-", max(nchar(x), na.rm=TRUE), "s"), x)  # left adjustment
  } else {
    x=sprintf(paste0("%.", nsmall, "f"), x)
    xf=sprintf(paste0("%", max(nchar(x), na.rm=TRUE), "s"), x)
  }
  return(xf)
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
#' @param digits,nsmall Number of decimal places of output. Default is \code{0}.
#'
#' @return A character string of time difference.
#'
#' @examples
#' t0=Sys.time()
#' dtime(t0)
#'
#' @export
dtime=function(t0, unit="secs", digits=0, nsmall=digits) {
  dt=difftime(Sys.time(), t0, units=unit)
  format(dt, digits=1, nsmall=nsmall)
}




#### File I/O ####


#' Set working directory to the path of currently opened file.
#'
#' Set working directory to the path of currently opened file (usually an R script).
#' You can use this function in both \strong{.R/.Rmd files and R Console}.
#' \href{https://www.rstudio.com/products/rstudio/download/preview/}{RStudio}
#' (version >= 1.2) is required for running this function.
#'
#' @param path \code{NULL} (default) or a specific path.
#' Default is to extract the path of the currently opened file
#' (usually .R or .Rmd) using the \code{rstudioapi::getSourceEditorContext} function.
#' @param ask \code{TRUE} or \code{FALSE} (default).
#' If \code{TRUE}, you can select a folder with the prompt of a dialog.
#'
#' @return Invisibly return the path.
#'
#' @examples
#' \dontrun{
#'
#'   # RStudio (version >= 1.2) is required for running this function.
#'   set.wd()  # set working directory to the path of the currently opened file
#'   set.wd("~/")  # set working directory to the home path
#'   set.wd("../")  # set working directory to the parent path
#'   set.wd(ask=TRUE)  # select a folder with the prompt of a dialog
#' }
#'
#' @seealso \code{\link[base:getwd]{setwd}}
#'
#' @describeIn set.wd Main function
#' @aliases set_wd
#' @export
set.wd=function(path=NULL, ask=FALSE) {
  # if(rstudioapi::isAvailable()==FALSE)
  #   stop("[RStudio] is required for running this function!\n",
  #        "Please download and install the latest version of RStudio:\n",
  #        "https://www.rstudio.com/products/rstudio/download/", call.=TRUE)
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
    Run("setwd(\"{path}\")")
    Print("<<green \u221a>> Set working directory to <<bold \"{getwd()}\">>")
    # rstudioapi::sendToConsole(paste0("setwd(\"", path, "\")"), execute=TRUE)
  }
  invisible(path)
}


#' @describeIn set.wd The alias of \code{set.wd} (the same)
#' @export
set_wd=set.wd


file_ext=function(filename) {
  filename=str_trim(filename)
  pos=regexpr("\\.([[:alnum:]]+)$", filename)
  ifelse(pos>-1L, tolower(substring(filename, pos+1L)), "")
}


#' Import data from a file (TXT, CSV, Excel, SPSS, Stata, ...) or clipboard.
#'
#' @description
#' Import data from a file, with format automatically judged from file extension.
#' This function is inspired by \code{\link[rio:import]{rio::import()}}
#' and has several modifications.
#' Its purpose is to avoid using lots of \code{read_xxx()} functions in your code
#' and to provide one tidy function for data import.
#'
#' It supports many file formats and uses corresponding R functions:
#'
#' \itemize{
#'   \item Plain text (.txt, .csv, .csv2, .tsv, .psv), using \code{\link[data.table:fread]{data.table::fread()}}
#'   \item Excel (.xls, .xlsx), using \code{\link[readxl:read_excel]{readxl::read_excel()}}
#'   \item SPSS (.sav), using \code{\link[foreign:read.spss]{foreign::read.spss()}};
#'   if failed, using \code{\link[haven:read_spss]{haven::read_sav()}} instead
#'   \item Stata (.dta), using \code{\link[foreign:read.dta]{foreign::read.dta()}};
#'   if failed, using \code{\link[haven:read_dta]{haven::read_dta()}} instead
#'   \item R objects (.rda, .rdata, .Rdata), using \code{\link[base:load]{base::load()}}
#'   \item R serialized objects (.rds), using \code{\link[base:readRDS]{base::readRDS()}}
#'   \item Clipboard (on Windows and Mac OS), using \code{\link[clipr:read_clip_tbl]{clipr::read_clip_tbl()}}
#'   \item Other formats, using \code{\link[rio:import]{rio::import()}}
#' }
#'
#' @param file File name (with extension).
#' If unspecified, then data will be imported from clipboard.
#' @param sheet [Only for Excel] Excel sheet name (or sheet number).
#' Default is the first sheet.
#' Ignored if the sheet is specified via \code{range}.
#' @param range [Only for Excel] Excel cell range. Default are all cells in a sheet.
#' You may specify it as \code{range="A1:E100"} or \code{range="Sheet1!A1:E100"}.
#' @param encoding File encoding. Default is \code{NULL}.
#' Alternatives can be \code{"UTF-8"}, \code{"GBK"}, \code{"CP936"}, etc.
#'
#' If you find messy code for Chinese text in the imported data,
#' it is usually effective to set \code{encoding="UTF-8"}.
#' @param header Does the first row contain column names (\code{TRUE} or \code{FALSE})? Default is \code{"auto"}.
#' @param setclass,as Class of the imported data. Default is \code{"data.frame"}.
#' Ignored if the data file is R object (.rds, .rda, .rdata, .Rdata).
#'
#' Alternatives can be:
#' \itemize{
#'   \item data.frame: \code{"data.frame"}, \code{"df"}, \code{"DF"}
#'   \item data.table: \code{"data.table"}, \code{"dt"}, \code{"DT"}
#'   \item tbl_df: \code{"tibble"}, \code{"tbl_df"}, \code{"tbl"}
#' }
#'
#' @return A data object (default class is \code{data.frame}).
#'
#' @examples
#' \dontrun{
#'
#'   # Import data from system clipboard
#'   data=import()  # read from clipboard (on Windows and Mac OS)
#'
#'   # If you have an Excel file named "mydata.xlsx"
#'   export(airquality, file="mydata.xlsx")
#'
#'   # Import data from a file
#'   data=import("mydata.xlsx")  # default: data.frame
#'   data=import("mydata.xlsx", as="data.table")
#' }
#'
#' @seealso \code{\link{export}}
#'
#' @export
import=function(file,
                sheet=NULL, range=NULL,
                encoding=NULL, header="auto",
                setclass=as, as="data.frame") {
  ## initialize
  if(missing(file)) {
    file="clipboard"
    fmt="clipboard"
  } else {
    if(file.exists(file)==FALSE)
      stop("No such file. Did you forget adding the path or file extension?", call.=FALSE)
    fmt=file_ext(file)  # file format extracted from file extension
  }

  ## import data
  if(fmt=="") {
    stop("File has no extension.", call.=FALSE)
  } else if(fmt=="clipboard") {
    if(header=="auto") header=TRUE
    x=clipr::read_clip()
    if(is.null(x) | (is.character(x) & length(x)==1 & x[1]==""))
      stop("The system clipboard is empty. You may first copy something.", call.=FALSE)
    else
      data=clipr::read_clip_tbl(x=x, header=header)
  } else if(fmt %in% c("rds")) {
    data=readRDS(file=file)
  } else if(fmt %in% c("rda", "rdata")) {
    envir=new.env()
    load(file=file, envir=envir)
    if(length(ls(envir))>1)
      warning("RData file contains multiple objects. Returning the first object.", call.=FALSE)
    data=get(ls(envir)[1], envir)
  } else if(fmt %in% c("txt", "csv", "csv2", "tsv", "psv")) {
    if(is.null(encoding)) encoding="unknown"
    data=data.table::fread(input=file,
                           sep="auto",
                           encoding=encoding,
                           header=header)
  } else if(fmt %in% c("xls", "xlsx")) {
    if(header=="auto") header=TRUE
    data=readxl::read_excel(path=file,
                            sheet=sheet,
                            range=range,
                            col_names=header)
  } else if(fmt %in% c("sav")) {
    try({
      error=TRUE
      data=foreign::read.spss(
        file=file,
        reencode=ifelse(is.null(encoding), NA, encoding),
        to.data.frame=TRUE,
        use.value.labels=FALSE)
      error=FALSE
    }, silent=TRUE)
    if(error) {
      message("[Retry] Using `haven::read_sav()` to import the data...")
      data=haven::read_sav(file=file, encoding=encoding)
    }
  } else if(fmt %in% c("dta")) {
    try({
      error=TRUE
      data=foreign::read.dta(file=file, convert.factors=FALSE)
      error=FALSE
    }, silent=TRUE)
    if(error) {
      message("[Retry] Using `haven::read_dta()` to import the data...")
      data=haven::read_dta(file=file, encoding=encoding)
    }
  } else {
    data=rio::import(file=file)
  }

  ## report data
  if(is.data.frame(data))
    Print("<<green \u221a>> Successfully imported: {nrow(data)} obs. of {ncol(data)} variables")
  else
    Print("<<green \u221a>> Successfully imported: {length(data)} values of class `{class(data)[1]}`")

  ## return data
  if(is.null(setclass) | fmt %in% c("rds", "rda", "rdata")) {
    return(data)
  } else if(setclass %in% c("data.frame", "df", "DF")) {
    return(base::as.data.frame(data))
  } else if(setclass %in% c("data.table", "dt", "DT")) {
    return(data.table::as.data.table(data))
  } else if(setclass %in% c("tibble", "tbl_df", "tbl")) {
    return(tibble::as_tibble(data))
  } else {
    return(data)
  }
}


#' Export data to a file (TXT, CSV, Excel, SPSS, Stata, ...) or clipboard.
#'
#' @description
#' Export data to a file, with format automatically judged from file extension.
#' This function is inspired by \code{\link[rio:export]{rio::export()}}
#' and has several modifications.
#' Its purpose is to avoid using lots of \code{write_xxx()} functions in your code
#' and to provide one tidy function for data export.
#'
#' It supports many file formats and uses corresponding R functions:
#'
#' \itemize{
#'   \item Plain text (.txt, .csv, .csv2, .tsv, .psv), using \code{\link[data.table:fwrite]{data.table::fwrite()}};
#'   if the \code{encoding} argument is specified, using \code{\link[utils:write.table]{utils::write.table()}} instead
#'   \item Excel (.xls, .xlsx), using \code{\link[openxlsx:write.xlsx]{openxlsx::write.xlsx()}}
#'   \item SPSS (.sav), using \code{\link[haven:read_spss]{haven::write_sav()}}
#'   \item Stata (.dta), using \code{\link[haven:read_dta]{haven::write_dta()}}
#'   \item R objects (.rda, .rdata, .Rdata), using \code{\link[base:save]{base::save()}}
#'   \item R serialized objects (.rds), using \code{\link[base:readRDS]{base::saveRDS()}}
#'   \item Clipboard (on Windows and Mac OS), using \code{\link[clipr:write_clip]{clipr::write_clip()}}
#'   \item Other formats, using \code{\link[rio:export]{rio::export()}}
#' }
#'
#' @param x Any R object, usually a data frame (\code{data.frame}, \code{data.table}, \code{tbl_df}).
#' Multiple R objects should be included in a \emph{named} \code{list} (see examples).
#'
#' If you want to save R objects other than a data frame (e.g., model results),
#' you'd better specify \code{file} with extensions .rda, .rdata, or .Rdata.
#' @param file File name (with extension).
#' If unspecified, then data will be exported to clipboard.
#' @param sheet [Only for Excel] Excel sheet name(s).
#' Default is Sheet1, Sheet2, ...
#' You may specify multiple sheet names in a character vector
#' \code{c()} with the \emph{same length} as \code{x} (see examples).
#' @param encoding File encoding. Default is \code{NULL}.
#' Alternatives can be \code{"UTF-8"}, \code{"GBK"}, \code{"CP936"}, etc.
#'
#' If you find messy code for Chinese text in the exported data (often in CSV when opened with Excel),
#' it is usually effective to set \code{encoding="GBK"} or \code{encoding="CP936"}.
#' @param header Does the first row contain column names (\code{TRUE} or \code{FALSE})? Default is \code{"auto"}.
#' @param overwrite Overwrite the existing file (if any)? Default is \code{TRUE}.
#'
#' @return No return value.
#'
#' @examples
#' \dontrun{
#'
#'   export(airquality)  # paste to clipboard
#'   export(airquality, file="mydata.csv")
#'   export(airquality, file="mydata.sav")
#'
#'   export(list(airquality, npk), file="mydata.xlsx")  # Sheet1, Sheet2
#'   export(list(air=airquality, npk=npk), file="mydata.xlsx")  # a named list
#'   export(list(airquality, npk), sheet=c("air", "npk"), file="mydata.xlsx")
#'
#'   export(list(a=1, b=npk, c="character"), file="abc.Rdata")  # .rda, .rdata
#'   d=import("abc.Rdata")  # load only the first object and rename it to `d`
#'   load("abc.Rdata")  # load all objects with original names to environment
#'
#'   export(lm(yield ~ N*P*K, data=npk), file="lm_npk.Rdata")
#'   model=import("lm_npk.Rdata")
#'   load("lm_npk.Rdata")  # because x is unnamed, the object has a name "List1"
#'
#'   export(list(m1=lm(yield ~ N*P*K, data=npk)), file="lm_npk.Rdata")
#'   model=import("lm_npk.Rdata")
#'   load("lm_npk.Rdata")  # because x is named, the object has a name "m1"
#' }
#'
#' @seealso \code{\link{import}}, \code{\link{print_table}}
#'
#' @export
export=function(x, file, sheet=NULL,
                encoding=NULL, header="auto",
                overwrite=TRUE) {
  ## initialize
  if(missing(file)) {
    file="clipboard"
    fmt="clipboard"
  } else {
    if(file.exists(file)==TRUE) {
      if(overwrite)
        message("Overwrite file \"", file, "\" ...")
      else
        stop("File \"", file, "\" existed!", call.=FALSE)
    }
    fmt=file_ext(file)  # file format extracted from file extension
  }

  ## export data
  if(fmt=="") {
    stop("File has no extension.", call.=FALSE)
  } else if(fmt=="clipboard") {
    if(header=="auto") header=TRUE
    suppressWarnings({
      clipr::write_clip(content=x, sep="\t",
                        row.names=FALSE,
                        col.names=header)
    })
  } else if(fmt %in% c("rds")) {
    saveRDS(object=x, file=file)
  } else if(fmt %in% c("rda", "rdata")) {
    if(is.data.frame(x)) {
      save(x, file=file)
    } else if(is.list(x)) {
      if(inherits(x, "list")==FALSE) x=list(x)
      if(is.null(names(x)))
        names(x)=paste0("List", 1:length(x))
      envir=as.environment(x)
      save(list=names(x), file=file, envir=envir)
    } else if(is.environment(x)) {
      save(list=ls(x), file=file, envir=x)
    } else if(is.character(x)) {
      save(list=x, file=file)
    } else {
      stop("`x` must be a data.frame, list, or environment.", call.=FALSE)
    }
  } else if(fmt %in% c("txt", "csv", "csv2", "tsv", "psv")) {
    sep=switch(fmt,
               txt="\t",
               csv=",",
               csv2=";",
               tsv="\t",
               psv="|")
    dec=ifelse(fmt=="csv2", ",", ".")
    if(header=="auto") header=TRUE
    if(is.null(encoding)) {
      data.table::fwrite(x=x, file=file,
                         sep=sep, dec=dec,
                         row.names=FALSE,
                         col.names=header)
    } else {
      utils::write.table(x=x, file=file,
                         sep=sep, dec=dec,
                         row.names=FALSE,
                         col.names=header,
                         quote=FALSE, na="",
                         fileEncoding=encoding)
    }
  } else if(fmt %in% c("xls", "xlsx")) {
    if(inherits(x, "list")==FALSE) x=list(x)  # one element
    if(header=="auto") header=TRUE
    if(is.null(sheet)) {
      if(is.null(names(x)))
        names(x)=paste0("Sheet", 1:length(x))
      openxlsx::write.xlsx(x=x, file=file,
                           overwrite=overwrite,
                           rowNames=FALSE,
                           colNames=header)
    } else {
      sheet=as.character(sheet)
      if(length(x)==length(sheet)) {
        n=length(x)
        if(file.exists(file)) {
          wb=openxlsx::loadWorkbook(file=file)
          sheets=openxlsx::getSheetNames(file=file)
          for(i in 1:n) {
            if(sheet[i] %in% sheets)
              openxlsx::removeWorksheet(wb, sheet=sheet[i])
            openxlsx::addWorksheet(wb, sheetName=sheet[i])
            openxlsx::writeData(wb, sheet=sheet[i], x=x[[i]],
                                rowNames=FALSE,
                                colNames=header)
          }
          openxlsx::saveWorkbook(wb, file=file, overwrite=TRUE)
        } else {
          names(x)=sheet
          openxlsx::write.xlsx(x=x, file=file,
                               overwrite=overwrite,
                               rowNames=FALSE,
                               colNames=header)
        }
      } else {
        stop("Length of sheet should be equal to length of x!", call.=FALSE)
      }
    }
  } else if(fmt %in% c("sav")) {
    x=restore_labelled(x)
    haven::write_sav(data=x, path=file)
  } else if(fmt %in% c("dta")) {
    x=restore_labelled(x)
    haven::write_dta(data=x, path=file)
  } else {
    rio::export(x=x, file=file)
  }

  ## report status
  if(fmt=="clipboard")
    Print("<<green \u221a>> Successfully paste to clipboard")
  else
    Print("<<green \u221a>> Successfully saved to <<bold \"{paste0(getwd(), '/', file)}\">>")
}


restore_labelled=function(x) {
  # restore labelled variable classes
  x[]=lapply(x, function(v) {
    if(is.factor(v)) {
      haven::labelled(
        x=as.numeric(v),
        labels=stats::setNames(seq_along(levels(v)), levels(v)),
        label=attr(v, "label", exact=TRUE))
    } else if(!is.null(attr(v, "labels", exact=TRUE)) | !is.null(attr(v, "label", exact=TRUE))) {
      haven::labelled(
        x=v,
        labels=attr(v, "labels", exact=TRUE),
        label=attr(v, "label", exact=TRUE))
    } else {
      v
    }
  })
  x
}




#### Excel-Style Functions ####


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
#' @return New data object, new variable, or new value (see the argument \code{return}).
#'
#' @seealso
#' \code{\link[dplyr:mutate-joins]{dplyr::left_join()}}
#'
#' \href{https://www.excel-university.com/xlookup/}{XLOOKUP: Excel University}
#'
#' @examples
#' ref=data.table(City=rep(c("A", "B", "C"), each=5),
#'                Year=rep(2013:2017, times=3),
#'                GDP=sample(1000:2000, 15),
#'                PM2.5=sample(10:300, 15))
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
  data.new=left_join(data,
                     data.ref[c(vars.ref, vars.lookup)],
                     by=by)
  if(nrow(data.new)>nrow(data))
    warning("More than one values are matched!", call.=TRUE)
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

