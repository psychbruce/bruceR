#### Multivariate Computation ####


#' Recode a variable.
#'
#' A wrapper of \code{\link[car:recode]{car::recode()}}.
#'
#' @param var Variable (numeric, character, or factor).
#' @param recodes Character string: e.g., \code{"lo:1=0; c(2,3)=1; 4=2; 5:hi=3; else=999"}.
#'
#' @return A vector of recoded variable.
#'
#' @examples
#' d=data.table(var=c(NA, 0, 1, 2, 3, 4, 5, 6))
#' d
#'
#' d[,":="(var.recoded=RECODE(var, "lo:1=0; c(2,3)=1; 4=2; 5:hi=3; else=999"))]
#' d
#'
#' @export
RECODE=function(var, recodes) {
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
#' d=data.table(var=rep(1:5, 2))
#' d[,":="(var1=RESCALE(var, to=1:7),
#'         var2=RESCALE(var, from=1:5, to=1:7))]
#' d  # var1 is equal to var2
#'
#' @export
RESCALE=function(var, from=range(var, na.rm=T), to) {
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
scaler=function(v, min=0, max=1) {
  min + (v - min(v, na.rm=T)) * (max - min) / (max(v, na.rm=T) - min(v, na.rm=T))
}


#' Multivariate computation.
#'
#' @description
#' Easily compute multivariate sum, mean, and other scores.
#' Reverse scoring can also be easily implemented without saving extra variables.
#' \code{\link{Alpha}} function uses a similar method to deal with reverse scoring.
#'
#' Three options to specify variables:
#' \enumerate{
#'   \item \strong{\code{var + items}}: use the common and unique parts of variable names.
#'   \item \strong{\code{vars}}: directly define a character vector of variables.
#'   \item \strong{\code{varrange}}: use the starting and stopping positions of variables.
#' }
#'
#' @param data Data frame.
#' @param var \strong{[Option 1]}
#' The common part across the variables. e.g., \code{"RSES"}
#' @param items \strong{[Option 1]}
#' The unique part across the variables. e.g., \code{1:10}
#' @param vars \strong{[Option 2]}
#' A character vector specifying the variables. e.g., \code{c("X1", "X2", "X3", "X4", "X5")}
#' @param varrange \strong{[Option 3]}
#' A character string specifying the positions ("starting:stopping") of variables. e.g., \code{"A1:E5"}
#' @param value [Only for \code{COUNT}] The value to be counted.
#' @param rev [Optional] Variables that need to be reversed. It can be
#' (1) a character vector specifying the reverse-scoring variables (recommended), or
#' (2) a numeric vector specifying the item number of reverse-scoring variables (not recommended).
#' @param likert [Optional] Range of likert scale (e.g., \code{1:5}, \code{c(1, 5)}).
#' If not provided, it will be automatically estimated from the given data (BUT you should use this carefully).
#' @param na.rm Ignore missing values. Default is \code{TRUE}.
#' @param values [Only for \code{CONSEC}] Values to be counted as consecutive identical values. Default is all numbers (\code{0:9}).
#'
#' @return A vector of computed values.
#'
#' @examples
#' d=data.table(x1=1:5,
#'              x4=c(2,2,5,4,5),
#'              x3=c(3,2,NA,NA,5),
#'              x2=c(4,4,NA,2,5),
#'              x5=c(5,4,1,4,5))
#' d
#' ## I deliberately set this order to show you
#' ## the difference between "vars" and "varrange".
#'
#' d[,`:=`(
#'   na=COUNT(d, "x", 1:5, value=NA),
#'   n.2=COUNT(d, "x", 1:5, value=2),
#'   sum=SUM(d, "x", 1:5),
#'   m1=MEAN(d, "x", 1:5),
#'   m2=MEAN(d, vars=c("x1", "x4")),
#'   m3=MEAN(d, varrange="x1:x2", rev="x2", likert=1:5),
#'   cons1=CONSEC(d, "x", 1:5),
#'   cons2=CONSEC(d, varrange="x1:x5")
#' )]
#' d
#'
#' data=as.data.table(psych::bfi)
#' data[,`:=`(
#'   E=MEAN(d, "E", 1:5, rev=c(1,2), likert=1:6),
#'   O=MEAN(d, "O", 1:5, rev=c(2,5), likert=1:6)
#' )]
#' data
#'
#' @name %%COMPUTE%%
## @aliases COUNT SUM MEAN STD CONSEC
NULL


convert2vars=function(data,
                      var=NULL, items=NULL,
                      vars=NULL,
                      varrange=NULL,
                      rev=NULL) {
  if(!is.null(varrange)) {
    dn=names(data)
    varrange=gsub(" ", "", strsplit(varrange, ":")[[1]])
    vars=dn[which(dn==varrange[1]):which(dn==varrange[2])]
  }
  if(is.null(vars)) vars=paste0(var, items)
  if(is.numeric(rev)) rev=paste0(var, rev)  # bug fixed on 2019-09-28
  if(is.character(rev)) rev=which(vars %in% rev)
  vars.raw=vars
  # vars=paste(deparse(substitute(data)), vars, sep="$")
  vars=paste0(deparse(substitute(data)), "$`", vars, "`")
  return(list(vars.raw=vars.raw, vars=vars, rev=rev))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes \strong{Count} a certain value across multiple variables.
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


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes Compute \strong{mode} across multiple variables.
#' @export
MODE=function(data, var=NULL, items=NULL, vars=NULL, varrange=NULL) {
  getmode=function(v) {
    uniqv=unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  Mode=function(...) getmode(c(...))
  varlist=convert2vars(data, var, items, vars, varrange)$vars
  eval(parse(text=paste0("mapply(Mode, ", paste(varlist, collapse=", "), ")")))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes Compute \strong{sum} across multiple variables.
#' @export
SUM=function(data, var=NULL, items=NULL, vars=NULL, varrange=NULL,
             rev=NULL, likert=NULL,
             na.rm=TRUE) {
  Sum=function(...) sum(..., na.rm=na.rm)
  v.r=convert2vars(data, var, items, vars, varrange, rev)
  vars=v.r$vars
  rev=v.r$rev
  if(!is.null(rev) & is.null(likert)) {
    ranges=apply(as.data.frame(data)[,v.r$vars.raw], 2, range)
    likert=c(min(ranges[1,], na.rm=TRUE), max(ranges[2,], na.rm=TRUE))
    warning("The range of likert scale was automatically estimated from the given data. If you are not sure about this, please specify the `likert` argument. See ?SUM", call.=TRUE)
  }
  pre=rep("", length(vars))
  pre[rev]=ifelse(is.null(likert), "", paste0(sum(range(likert)), "-"))
  varlist=paste0(pre, vars)
  eval(parse(text=paste0("mapply(Sum, ", paste(varlist, collapse=", "), ")")))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes Compute \strong{mean} across multiple variables.
#' @export
MEAN=function(data, var=NULL, items=NULL, vars=NULL, varrange=NULL,
              rev=NULL, likert=NULL,
              na.rm=TRUE) {
  Mean=function(...) mean(c(...), na.rm=na.rm)
  v.r=convert2vars(data, var, items, vars, varrange, rev)
  vars=v.r$vars
  rev=v.r$rev
  if(!is.null(rev) & is.null(likert)) {
    ranges=apply(as.data.frame(data)[,v.r$vars.raw], 2, range)
    likert=c(min(ranges[1,], na.rm=TRUE), max(ranges[2,], na.rm=TRUE))
    warning("The range of likert scale was automatically estimated from the given data. If you are not sure about this, please specify the `likert` argument. See ?MEAN", call.=TRUE)
  }
  pre=rep("", length(vars))
  pre[rev]=ifelse(is.null(likert), "", paste0(sum(range(likert)), "-"))
  varlist=paste0(pre, vars)
  eval(parse(text=paste0("mapply(Mean, ", paste(varlist, collapse=", "), ")")))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes Compute \strong{standard deviation} across multiple variables.
#' @export
STD=function(data, var=NULL, items=NULL, vars=NULL, varrange=NULL,
             rev=NULL, likert=NULL,
             na.rm=TRUE) {
  Std=function(...) sd(c(...), na.rm=na.rm)
  v.r=convert2vars(data, var, items, vars, varrange, rev)
  vars=v.r$vars
  rev=v.r$rev
  if(!is.null(rev) & is.null(likert)) {
    ranges=apply(as.data.frame(data)[,v.r$vars.raw], 2, range)
    likert=c(min(ranges[1,], na.rm=TRUE), max(ranges[2,], na.rm=TRUE))
    warning("The range of likert scale was automatically estimated from the given data. If you are not sure about this, please specify the `likert` argument. See ?STD", call.=TRUE)
  }
  pre=rep("", length(vars))
  pre[rev]=ifelse(is.null(likert), "", paste0(sum(range(likert)), "-"))
  varlist=paste0(pre, vars)
  eval(parse(text=paste0("mapply(Std, ", paste(varlist, collapse=", "), ")")))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes Compute \strong{consecutive identical digits} across multiple variables (especially useful in detecting careless responding).
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


#' Reliability analysis (Cronbach's \eqn{\alpha} and McDonald's \eqn{\omega}).
#'
#' @description
#' An extension of \code{\link[psych:alpha]{psych::alpha()}} and \code{\link[psych:omega]{psych::omega()}},
#' reporting (1) scale statistics
#' (Cronbach's \eqn{\alpha} and McDonald's \eqn{\omega}) and
#' (2) item statistics
#' (item-rest correlation [i.e., corrected item-total correlation]
#' and Cronbach's \eqn{\alpha} if item deleted).
#'
#' Three options to specify variables:
#' \enumerate{
#'   \item \strong{\code{var + items}}: use the common and unique parts of variable names.
#'   \item \strong{\code{vars}}: directly define a character vector of variables.
#'   \item \strong{\code{varrange}}: use the starting and stopping positions of variables.
#' }
#'
#' @inheritParams %%COMPUTE%%
#'
#' @return
#' A list of results obtained from
#' \code{\link[psych:alpha]{psych::alpha()}} and \code{\link[psych:omega]{psych::omega()}}.
#'
#' @examples
#' # ?psych::bfi
#' data=psych::bfi
#' Alpha(data, "E", 1:5)  # "E1" & "E2" should be reversed
#' Alpha(data, "E", 1:5, rev=1:2)  # correct
#' Alpha(data, "E", 1:5, rev=c("E1", "E2"))  # also correct
#' Alpha(data, vars=c("E1", "E2", "E3", "E4", "E5"), rev=c("E1", "E2"))
#' Alpha(data, varrange="E1:E5", rev=c("E1", "E2"))
#'
#' # using dplyr::select()
#' data %>% select(E1, E2, E3, E4, E5) %>%
#'   Alpha(vars=names(.), rev=c("E1", "E2"))
#'
#' @seealso
#' \code{\link{MEAN}}
#'
#' @export
Alpha=function(data, var, items, vars=NULL, varrange=NULL, rev=NULL,
               digits=3, nsmall=digits) {
  if(!is.null(varrange)) {
    dn=names(data)
    varrange=gsub(" ", "", strsplit(varrange, ":")[[1]])
    vars=dn[which(dn==varrange[1]):which(dn==varrange[2])]
  }
  if(is.null(vars)) vars=paste0(var, items)
  if(is.numeric(rev)) rev=paste0(var, rev)
  n.total=nrow(data)
  data=na.omit(as.data.frame(data)[vars])
  n.valid=nrow(data)
  for(v in vars) {
    data[[v]]=as.numeric(data[[v]])
    if(v %in% rev) {
      data[[v]]=min(data[[v]])+max(data[[v]])-data[[v]]
      vr=paste(v, "(rev)")
      Run("data=dplyr::rename(data, `{vr}`={v})")
    }
  }

  suppressMessages({
    suppressWarnings({
      alpha=psych::alpha(data, delete=FALSE, warnings=FALSE)
      omega=psych::omega(data, nfactors=1, flip=FALSE)
      loadings=psych::principal(data, nfactors=1, scores=FALSE)$loadings

      items=cbind(
        alpha$item.stats[c("mean", "sd", "r.drop")],
        alpha$alpha.drop[c("raw_alpha")])
      names(items)=c("Mean", "S.D.",
                     "Item-Rest Cor.",
                     "Cronbach's \u03b1")
      items.need.rev=vars[loadings<0]
    })
  })

  Print("
  \n
  <<cyan Reliability Analysis>>

  Summary:
  Total Items: {length(vars)}
  Scale Range: {min(data)} ~ {max(data)}
  Total Cases: {n.total}
  Valid Cases: {n.valid} ({100*n.valid/n.total:.1}%)

  Scale Statistics:
  <<italic Mean>> = {alpha$total$mean:.{nsmall}}
  <<italic S.D.>> = {alpha$total$sd:.{nsmall}}
  Cronbach's \u03b1 = {alpha$total$raw_alpha:.{nsmall}}
  McDonald's \u03c9 = {omega$omega.tot:.{nsmall}}
  ")
  # Cronbach's \u03b1: {alpha$total$raw_alpha:.{nsmall}} (based on raw scores)
  # Cronbach's \u03b1: {alpha$total$std.alpha:.{nsmall}} (based on standardized items)

  if(alpha$total$raw_alpha<0.5 | length(items.need.rev)>0) {
    cat("\n")
    if(alpha$total$raw_alpha<0.5)
      Print("<<yellow Warning: Scale reliability is low. You may check item codings.>>")
    if(length(items.need.rev)==1)
      Print("<<yellow Item {items.need.rev} correlates negatively with the scale and may be reversed.>>")
    if(length(items.need.rev)>1)
      Print("<<yellow Items {paste(items.need.rev, collapse=', ')} correlate negatively with the scale and may be reversed.>>")
    if(length(items.need.rev)>0)
      Print("<<yellow You can specify this argument: rev=c(\"{paste(items.need.rev, collapse='\", \"')}\")>>")
  }

  cat("\n")
  print_table(items, nsmalls=nsmall,
              title="Item Statistics (Cronbach's \u03b1 If Item Deleted):",
              note="Item-Rest Cor. = Corrected Item-Total Correlation")
  cat("\n")

  # rel=jmv::reliability(data, vars=eval(vars), revItems=eval(rev),
  #                      meanScale=TRUE, sdScale=TRUE,
  #                      alphaScale=TRUE, omegaScale=TRUE,
  #                      itemRestCor=TRUE, alphaItems=TRUE, omegaItems=TRUE)
  # rel$items$setTitle("Item Reliability Statistics (if item is dropped)")

  invisible(list(alpha=alpha, omega=omega))
}


#' Exploratory factor analysis (EFA).
#'
#' An extension of \code{\link[jmv:efa]{jmv::efa()}}.
#'
#' @inheritParams %%COMPUTE%%
#'
#' @param vartext Character string specifying the model (e.g., \code{"X[1:5] + Y[c(1,3)] + Z"}).
#' @param method \code{"eigen"} (default), \code{"parallel"}, or \code{"fixed"}, the way to determine the number of factors.
#' @param extraction \code{"pa"} (default), \code{"ml"}, or \code{"minres"},
#' using "principal axis", "maximum likelihood", or "minimum residual" as the factor extraction method, respectively.
#' @param rotation \code{"varimax"} (default), \code{"oblimin"}, or \code{"none"}, the rotation method.
#' @param nFactors An integer (default is 1) fixing the number of factors.
#' Only relevant when \code{method="fixed"}.
#' @param hideLoadings A number (0~1, default is 0.3) for hiding factor loadings below this value.
#'
#' @return No return value.
#'
#' @note It does not have the extraction method "Principal Components". You may still use SPSS.
#'
#' @seealso
#' \code{\link[jmv:efa]{jmv::efa()}}
#'
#' @examples
#' \donttest{EFA(psych::bfi, "E[1:5] + A[1:5] + C[1:5] + N[1:5] + O[1:5]", method="fixed", nFactors=5)
#' }
#' @export
EFA=function(data, vartext,
             method="eigen", extraction="pa", rotation="varimax",
             nFactors=1, hideLoadings=0.3) {
  jmv::efa(data, vars=eval(expand_vars(vartext)),
           nFactorMethod=method,  # "eigen", "parallel", "fixed"
           extraction=extraction,  # "pa", "ml", "minres"
           rotation=rotation,  # "none", "varimax", "quartimax", "promax", "oblimin", "simplimax"
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


#' Confirmatory factor analysis (CFA).
#'
#' An extension of \code{\link[lavaan:cfa]{lavaan::cfa()}}.
#'
#' @inheritParams %%COMPUTE%%
#' @param model Model formula. See examples.
#' @param highorder High-order factor. Default is \code{""}.
#' @param orthogonal Default is \code{FALSE}. If \code{TRUE}, all covariances among latent variables are set to zero.
#' @param missing Default is \code{"listwise"}. Alternative is \code{"fiml"} ("Full Information Maximum Likelihood").
## @param CI \code{TRUE} or \code{FALSE} (default), provide confidence intervals for the model estimates.
## @param MI \code{TRUE} or \code{FALSE} (default), provide modification indices for the parameters not included in the model.
#' @param digits,nsmall Number of decimal places of output. Default is \code{3}.
#' @param file File name of MS Word (\code{.doc}).
#'
#' @return
#' A list of results returned by \code{\link[lavaan:cfa]{lavaan::cfa()}}.
#'
#' @seealso
#' \code{\link{lavaan_summary}}
#'
#' @examples
#' \donttest{data.cfa=lavaan::HolzingerSwineford1939
#' CFA(data.cfa, "Visual =~ x[1:3]; Textual =~ x[c(4,5,6)]; Speed =~ x7 + x8 + x9")
#' CFA(data.cfa, model="
#'     Visual =~ x[1:3]
#'     Textual =~ x[c(4,5,6)]
#'     Speed =~ x7 + x8 + x9
#'     ", highorder="Ability")
#'
#' data.bfi=na.omit(psych::bfi)
#' CFA(data.bfi, "E =~ E[1:5]; A =~ A[1:5]; C =~ C[1:5]; N =~ N[1:5]; O =~ O[1:5]")
#' }
#' @export
CFA=function(data, model="A =~ a[1:5]; B =~ b[c(1,3,5)]; C =~ c1 + c2 + c3",
             highorder="", orthogonal=FALSE, missing="listwise",
             # CI=FALSE, MI=FALSE,
             digits=3, nsmall=digits,
             file=NULL) {
  # model.jmv=modelCFA.trans("jmv", model)
  model.lav=modelCFA.trans("lavaan", model, highorder)
  # if(orthogonal==TRUE | highorder!="") style="lavaan"

  cat("\n")
  Print("<<cyan Model Syntax (lavaan):>>")
  cat(model.lav)
  cat("\n")

  # # jmv style
  # if("jmv" %in% style) {
  #   fit.jmv=jmv::cfa(data=data, factors=model.jmv,
  #                    resCov=NULL,
  #                    constrain="facVar", # or "facInd"
  #                    # 'facVar' fixes the factor variances to 1
  #                    # 'facInd' fixes each factor to the scale of its first indicator
  #                    ci=CI, mi=MI, # modification indices
  #                    stdEst=TRUE, resCovEst=TRUE,
  #                    # pathDiagram=plot,
  #                    fitMeasures=c("cfi", "tli", "rmsea", "srmr", "aic", "bic"),
  #                    miss=missing) # fiml (default), listwise
  #   cat("\n#### jamovi style output ####\n")
  #   print(fit.jmv)
  # }

  # lavaan style
  fit.lav=lavaan::cfa(model=model.lav,
                      data=data,
                      std.lv=TRUE,
                      # TRUE: fixing the factor residual variances to 1
                      # FALSE: fixing the factor loading of the first indicator to 1
                      orthogonal=orthogonal,
                      missing=missing) # fiml, listwise (default)
  # cat("\n#### lavaan output ####\n\n")
  lavaan_summary(fit.lav, ci="raw", nsmall=nsmall, file=file)
  # lavaan::summary(fit.lav,
  #                 fit.measures=TRUE,
  #                 standardized=TRUE,
  #                 ci=CI,
  #                 modindices=MI)
  # if(MI) print(lavaan::modificationIndices(fit.lav))
  # if(plot) semPlot::semPaths(fit.lav, "std", curveAdjacent=TRUE,
  #                            style="lisrel", nDigits=2, edge.label.cex=1)

  invisible(fit.lav)
}

