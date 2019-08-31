#### Variable Computing ####


#' Recode a variable
## @inheritParams car::recode
#' @param var A variable (numeric vector, character vector, or factor).
#' @param recodes A character string of recode specifications: e.g., \code{"lo:1=0; c(2,3)=1; 4=2; 5:hi=3; else=999"}.
#' @examples
#' d=data.table(var=c(NA, 0, 1, 2, 3, 4, 5, 6))
#' d
#'
#' d[,":="(var.recoded=RECODE(var, "lo:1=0; c(2,3)=1; 4=2; 5:hi=3; else=999"))]
#' d
#' @export
RECODE=function(var, recodes) {
  car::recode(var, recodes)
}


#' Rescale likert scales (e.g., from 5-point to 7-point)
#' @param var A variable (numeric vector).
#' @param from A numeric vector indicating the range of old scale (e.g., \code{1:5}).
#' If not set, it will automatically take the range of \code{var}.
#' @param to A numeric vector indicating the range of new scale (e.g., \code{1:7}).
#' @examples
#' d=data.table(var=rep(1:5, 2))
#' d
#'
#' d[,":="(var1=RESCALE(var, to=1:7))]
#' d[,":="(var2=RESCALE(var, from=1:5, to=1:7))]
#' @export
RESCALE=function(var, from=range(var), to) {
  (var-median(from)) / (max(from)-median(from)) * (max(to)-median(to)) + median(to)
}


#' Search, match, and look up values
#' @import data.table
#' @importFrom dplyr left_join
#' @param data \code{data.frame} or \code{data.table}.
#' @param vars Character or character vector, specifying the variable(s) to be searched in \code{data}.
#' @param data.ref Reference data containing both the reference variable(s) and the lookup variable(s).
#' @param vars.ref Character or character vector (with the \strong{same length and order} as \code{vars}),
#' specifying the reference variable(s) to be matched in \code{data.ref}.
#' @param vars.lookup Character or character vector, specifying the variable(s) to be looked up and returned from \code{data.ref}.
#' @return A new \code{data.frame} or \code{data.table}, with the lookup values added.
#' If multiple values were simultaneously matched, a warning message would be printed.
#' Then you may check if there was anything wrong in \code{data.ref}, and/or re-define \code{vars} and \code{vars.ref}.
#' @examples
#' dict=data.table(City=rep(c("A", "B", "C"), each=5),
#'                 Year=rep(2013:2017, times=3),
#'                 GDP=RANDBETWEEN(1000:2000, n=15, seed=1),
#'                 PM2.5=RANDBETWEEN(10:300, n=15, seed=1))
#' dict
#'
#' data=data.table(sub=1:5,
#'                 city=c("A", "A", "B", "C", "C"),
#'                 year=c(2013, 2014, 2015, 2016, 2017))
#' data
#'
#' LOOKUP(data, "city", dict, "City", "GDP")  # return with a warning
#' LOOKUP(data, c("city", "year"), dict, c("City", "Year"), "GDP")
#' LOOKUP(data, c("city", "year"), dict, c("City", "Year"), c("GDP", "PM2.5"))
#' @export
LOOKUP=function(data, vars,
                data.ref, vars.ref,
                vars.lookup) {
  by=vars.ref
  names(by)=vars
  data.ref=as.data.table(data.ref)
  data.new=left_join(data,
                     as.data.frame(data.ref)[c(vars.ref, vars.lookup)],
                     by=by)
  if(nrow(data.new)>nrow(data)) {
    data.ref=unique(data.ref, by=vars.ref)
    data.new=left_join(data,
                       as.data.frame(data.ref)[c(vars.ref, vars.lookup)],
                       by=by)
    warning("More than one values were matched, only the first value in 'data.ref' was returned. Please check your reference data!")
  }
  if(is.data.table(data)) data.new=as.data.table(data.new)
  return(data.new)
}


#' Compute multiple variables in an elegant manner
#' @param data A \code{data.frame} or \code{data.table}.
#' @param var [optional 1] The common part across a series of variables (e.g., \code{"RSES"}, the Rosenberg Self-Esteem Scale).
#' @param items [optional 1] The unique part across a series of variables (e.g., \code{1:10}).
#' @param vars [optional 2] A character vector specifying the variable list (e.g., \code{c("x1", "x2", "x3")}).
#' @param varrange [optional 3] A character specifying the range of variables with the same order as in the data (e.g., \code{"Age:Edu"}).
#' @examples
#' ## From now on, please use 'data.table' instead of 'data.frame'.
#' ## Believe me.
#'
#' ## Run the examples:
#' ## example("MEAN")
#'
#' d=data.table(x1=1:5,
#'              x4=c(2,2,5,4,5),
#'              x3=c(3,2,NA,NA,5),
#'              x2=c(4,4,NA,2,5),
#'              x5=c(5,4,1,4,5))
#' d
#' # I deliberately set this order to
#' # show you the difference between "vars" and "varrange".
#'
#' d[,":="(na=COUNT(d, "x", 1:5, value=NA),
#'         n.2=COUNT(d, "x", 1:5, value=2),
#'         sum=SUM(d, "x", 1:5),
#'         m1=MEAN(d, "x", 1:5),
#'         m2=MEAN(d, vars=c("x1", "x4")),
#'         m3=MEAN(d, varrange="x1:x2", rev="x2", likert=1:5),
#'         cons1=CONSEC(d, "x", 1:5),
#'         cons2=CONSEC(d, varrange="x1:x5")
#'         )]
#' d  # It has been already changed.
#'
## print_table(d, row.names=F, nsmalls=1)
## # A nice style of output throughout the 'bruceR' package.
##
#' ## NOTE: ":=" is indeed a special function in the 'data.table' package.
#' ## See a similar function "mutate()" in the 'dplyr' package: ?dplyr::mutate
#' ## For data.table, you need NOT to re-assign the tranformed data object,
#' ## because it can automatically update the variables in situ!
#' @name %%COMPUTE%%
#' @aliases COUNT SUM MEAN STD CONSEC
NULL


convert2vars=function(data,
                      var=NULL, items=NULL,
                      vars=NULL,
                      varrange=NULL,
                      rev=NULL) {
  if(!is.null(varrange)) {
    dn=names(data)
    varrange=strsplit(varrange, ":")[[1]]
    vars=dn[which(dn==varrange[1]):which(dn==varrange[2])]
  }
  if(is.null(vars)) vars=paste0(var, items)
  if(is.character(rev)) rev=which(vars %in% rev)
  vars=paste(deparse(substitute(data)), vars, sep="$")
  return(list(vars=vars, rev=rev))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes \strong{Count} a certain value across multiple variables
#' @export
COUNT=function(data, var=NULL, items=NULL, vars=NULL, varrange=NULL,
               value=NA) {
  Count=function(...) sum(c(...), na.rm=TRUE)
  v.r=convert2vars(data, var, items, vars, varrange)
  vars=v.r$vars
  if(is.na(value))
    varlist=paste0("is.na(", vars, ")")
  else
    varlist=paste0(vars, "==", value)
  eval(parse(text=paste0("mapply(Count, ", paste(varlist, collapse=", "), ")")))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes Compute \strong{sum} across multiple variables
#' @export
SUM=function(data, var=NULL, items=NULL, vars=NULL, varrange=NULL,
             rev=NULL, likert=NULL,
             na.rm=TRUE) {
  Sum=function(...) sum(..., na.rm=na.rm)
  v.r=convert2vars(data, var, items, vars, varrange, rev)
  vars=v.r$vars
  rev=v.r$rev
  pre=rep("", length(vars))
  pre[rev]=ifelse(is.null(likert), "", paste0(min(likert)+max(likert), "-"))
  varlist=paste0(pre, vars)
  eval(parse(text=paste0("mapply(Sum, ", paste(varlist, collapse=", "), ")")))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes Compute \strong{mean} across multiple variables
#' @export
MEAN=function(data, var=NULL, items=NULL, vars=NULL, varrange=NULL,
              rev=NULL, likert=NULL,
              na.rm=TRUE) {
  Mean=function(...) mean(c(...), na.rm=na.rm)
  v.r=convert2vars(data, var, items, vars, varrange, rev)
  vars=v.r$vars
  rev=v.r$rev
  pre=rep("", length(vars))
  pre[rev]=ifelse(is.null(likert), "", paste0(min(likert)+max(likert), "-"))
  varlist=paste0(pre, vars)
  eval(parse(text=paste0("mapply(Mean, ", paste(varlist, collapse=", "), ")")))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes Compute \strong{standard deviation} across multiple variables
#' @export
STD=function(data, var=NULL, items=NULL, vars=NULL, varrange=NULL,
             rev=NULL, likert=NULL,
             na.rm=TRUE) {
  Std=function(...) sd(c(...), na.rm=na.rm)
  v.r=convert2vars(data, var, items, vars, varrange, rev)
  vars=v.r$vars
  rev=v.r$rev
  pre=rep("", length(vars))
  pre[rev]=ifelse(is.null(likert), "", paste0(min(likert)+max(likert), "-"))
  varlist=paste0(pre, vars)
  eval(parse(text=paste0("mapply(Std, ", paste(varlist, collapse=", "), ")")))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes Compute \strong{consecutive identical digits} across multiple variables (especially useful in detecting careless responding)
#' @import stringr
#' @export
CONSEC=function(data, var=NULL, items=NULL,
                vars=NULL,
                varrange=NULL,
                values=0:9) {
  Conseq=function(string, number=values) {
    # Consecutive Identical Digits
    pattern=paste(paste0(number, "{2,}"), collapse="|")
    ifelse(grepl(pattern, string), max(nchar(str_extract_all(string=string, pattern=pattern, simplify=TRUE))), 0)
  }
  v.r=convert2vars(data, var, items, vars, varrange)
  vars=v.r$vars
  varlist=vars
  eval(parse(text=paste0("mapply(Conseq, paste0(", paste(varlist, collapse=", "), "))")))
}




#### Reliability, EFA, and CFA ####


#' Reliability analysis (Cronbach's \eqn{\alpha} and corrected item-total correlation)
## @import jmv
#' @inheritParams %%COMPUTE%%
#' @export
Alpha=function(data, var, items, vars=NULL, rev=NULL) {
  if(is.null(vars)) vars=paste0(var, items)
  if(!is.null(rev)) rev=paste0(var, rev)
  jmv::reliability(data, vars=eval(vars), revItems=eval(rev),
                   meanScale=TRUE, sdScale=TRUE,
                   itemRestCor=TRUE, alphaItems=TRUE)
}


#' Exploratory factor analysis (EFA)
## @import jmv
#' @inheritParams %%COMPUTE%%
#' @param vartext e.g., \code{"X[1:5] + Y[c(1,3)] + Z"}
#' @param method \code{"eigen"} (default), \code{"parallel"}, or \code{"fixed"}, the way to determine the number of factors
#' @param extraction \code{"pa"} (default), \code{"ml"}, or \code{"minres"},
#' using respectively "prinicipal axis", "maximum likelihood", or "minimum residual" as the factor extraction method
#' @param rotation \code{"varimax"} (default), \code{"oblimin"}, or \code{"none"}, the rotation to use in estimation
#' @param nFactors An integer (default is 1) fixing the number of factors. Only relevant when \code{method="fixed"}.
#' @param hideLoadings A number (0~1, default is 0.3) for hiding factor loadings below this value.
#' @examples
#' EFA(bfi, "E[1:5] + A[1:5] + C[1:5] + N[1:5] + O[1:5]", method="fixed", nFactors=5)
#' @seealso \code{\link[jmv]{efa}}
#' @note It does not have the extraction method "Principal Components". You may still use SPSS.
#' @export
EFA=function(data, vartext,
             method="eigen", extraction="pa", rotation="varimax",
             nFactors=1, hideLoadings=0.3) {
  jmv::efa(data, vars=eval(expand_vars(vartext)),
           nFactorMethod=method, # "eigen", "parallel", "fixed"
           extraction=extraction, # "pa", "ml", "minres"
           rotation=rotation, # "none", "varimax", "quartimax", "promax", "oblimin", "simplimax"
           minEigen=1,
           nFactors=nFactors,
           hideLoadings=hideLoadings, sortLoadings=TRUE,
           screePlot=TRUE, eigen=TRUE,
           factorCor=TRUE, factorSummary=TRUE, modelFit=TRUE,
           kmo=TRUE, bartlett=TRUE)
}


## Expand multiple variables with complex string formats
## Input: "X[1:5] + Y[c(1,3)] + Z"
## Output:
expand_vars=function(vartext) {
  vartexts=gsub(" ", "", strsplit(vartext, "\\+")[[1]])
  vars=c()
  for(vartext.i in vartexts) {
    if(grepl("\\[|\\]", vartext.i)==TRUE) {
      vars.i=eval(parse(text=paste0("paste0('", gsub("\\]", ")", gsub("\\[", "',", vartext.i)))))
    } else {
      vars.i=vartext.i
    }
    vars=c(vars, vars.i)
  }
  return(vars)
}


## CFA model formula transformation
modelCFA.trans=function(style=c("jmv", "lavaan"),
                        model, highorder="") {
  # model: free style input
  model=gsub("^\\s+|\\s+$", "", model)
  model=strsplit(gsub(" ", "", strsplit(model, "(;|\\n)+")[[1]]), "=~")
  # jmv style
  model.jmv=list()
  for(i in 1:length(model)) {
    var=model[[i]][[2]]
    vars=expand_vars(var)
    model.jmv[[i]]=list(label=model[[i]][[1]],
                        vars=vars)
  }
  # lavaan style
  model.lav=c()
  for(i in 1:length(model.jmv)) {
    model.i=paste(model.jmv[[i]]$label, paste(model.jmv[[i]]$vars, collapse=" + "), sep=" =~ ")
    model.lav=c(model.lav, model.i)
  }
  model.lav=paste(model.lav, collapse="\n")
  # high-order CFA (only for lavaan)
  factors=sapply(model.jmv, function(x) x$label)
  if(highorder!="")
    model.lav=paste(model.lav,
                    paste(highorder, "=~",
                          paste(factors, collapse=" + ")),
                    paste(highorder, "~~", highorder),
                    sep="\n")
  # output
  if(style=="jmv") return(model.jmv)
  if(style=="lavaan") return(model.lav)
}


#' Confirmatory factor analysis (CFA)
## @import jmv
#' @import lavaan
#' @import semPlot
#' @inheritParams %%COMPUTE%%
#' @param model Model formula.
#' @param highorder Optional. High-order factor. Default is \code{""}.
#' @examples
#' data.cfa=lavaan::HolzingerSwineford1939
#' CFA(data.cfa, "Visual =~ x[1:3]; Textual =~ x[c(4,5,6)]; Speed =~ x7 + x8 + x9", plot=T)
#' CFA(data.cfa, model="
#'     Visual =~ x[1:3]
#'     Textual =~ x[c(4,5,6)]
#'     Speed =~ x7 + x8 + x9
#'     ", highorder="Ability", plot=T)
#'
#' data.bfi=psych::bfi
#' data.bfi=data.bfi[complete.cases(data.bfi),]
#' CFA(data.bfi, "E =~ E[1:5]; A =~ A[1:5]; C =~ C[1:5]; N =~ N[1:5]; O =~ O[1:5]", plot=T)
#' @export
CFA=function(data, model="A =~ a[1:5]; B =~ b[c(1,3,5)]; C =~ c1 + c2 + c3",
             highorder="", orthogonal=FALSE, missing="listwise",
             style=c("jmv", "lavaan"), CI=FALSE, MI=FALSE, plot=FALSE) {
  model.jmv=modelCFA.trans("jmv", model)
  model.lav=modelCFA.trans("lavaan", model, highorder)
  if(orthogonal | highorder!="") style="lavaan"
  if(plot & "lavaan" %notin% style) style=append(style, "lavaan")
  cat("# Latent variable definitions\n")
  cat(model.lav, "\n\n")

  results=list()

  # jmv style
  if("jmv" %in% style) {
    fit.jmv=jmv::cfa(data=data, factors=model.jmv,
                     resCov=NULL,
                     constrain="facVar", # or "facInd"
                     # 'facVar' fixes the factor variances to 1
                     # 'facInd' fixes each factor to the scale of its first indicator
                     ci=CI, mi=MI, # modification indices
                     stdEst=TRUE, resCovEst=TRUE,
                     # pathDiagram=plot,
                     fitMeasures=c("cfi", "tli", "rmsea", "srmr", "aic", "bic"),
                     miss=missing) # fiml (default), listwise
    # cat(r$modelSyntax)
    cat("#### jamovi style output ####\n")
    print(fit.jmv)
    results=c(results, fit.jmv=fit.jmv)
  }

  # lavaan style
  if("lavaan" %in% style) {
    fit.lav=lavaan::cfa(data=data, model=model.lav,
                        std.lv=TRUE,
                        # TRUE: fixing the factor residual variances to 1
                        # FALSE: fixing the factor loading of the first indicator to 1
                        orthogonal=orthogonal,
                        missing=missing) # fiml, listwise (default)
    cat("#### lavaan style output ####\n\n")
    summary(fit.lav, fit.measures=TRUE, standard=TRUE)
    if(MI) print(modificationIndices(fit.lav))
    if(plot) semPlot::semPaths(fit.lav, "std", curveAdjacent=TRUE,
                               style="lisrel", nDigits=2, edge.label.cex=1)
    results=c(results, fit.lavaan=fit.lav)
  }

  invisible(results)
}


