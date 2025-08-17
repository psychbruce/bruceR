#### Multivariate Computation ####


#' Multivariate computation.
#'
#' @description
#' Easily compute multivariate sum, mean, and other scores. Reverse scoring can also be easily implemented without saving extra variables.
#' [Alpha()] function uses a similar method to deal with reverse scoring.
#'
#' Three options to specify variables:
#' 1. **`var` + `items`**: common and unique parts of variable names (suggested).
#' 2. **`vars`**: a character vector of variable names (suggested).
#' 3. **`varrange`**: starting and stopping positions of variables (NOT suggested).
#'
#' @param data Data frame.
#' @param var **\[Option 1\]** Common part across variables: e.g., `"RSES"`, `"XX.{i}.pre"` (if `var` string has any placeholder in braces `{...}`, then `items` will be pasted into the braces, see examples)
#' @param items **\[Option 1\]** Unique part across variables: e.g., `1:10`, `c("a", "b", "c")`
#' @param vars **\[Option 2\]** Character vector specifying variables: e.g., `c("X1", "X2", "X3", "X4", "X5")`
#' @param varrange **\[Option 3\]** Character string specifying positions (`"start:stop"`) of variables: e.g., `"A1:E5"`
#' @param value \[Only for [COUNT()]\] The value to be counted.
#' @param rev \[Optional\] Variables that need to be reversed. It can be (1) a character vector specifying the reverse-scoring variables (recommended), or (2) a numeric vector specifying the item number of reverse-scoring variables (not recommended).
#' @param range,likert \[Optional\] Range of likert scale: e.g., `1:5`, `c(1, 5)`. If not provided, it will be automatically estimated from the given data (BUT you should use this carefully).
#' @param na.rm Ignore missing values. Defaults to `TRUE`.
#' @param values \[Only for [CONSEC()]\] Values to be counted as consecutive identical values. Defaults to all numbers (`0:9`).
#'
#' @return
#' A vector of computed values.
#'
#' @examples
#' d = data.table(
#'   x1 = 1:5,
#'   x4 = c(2,2,5,4,5),
#'   x3 = c(3,2,NA,NA,5),
#'   x2 = c(4,4,NA,2,5),
#'   x5 = c(5,4,1,4,5)
#' )
#' d
#' ## I deliberately set this order to show you
#' ## the difference between "vars" and "varrange".
#'
#' ## ====== Usage 1: data.table `:=` ====== ##
#' d[, `:=`(
#'   na = COUNT(d, "x", 1:5, value=NA),
#'   n.2 = COUNT(d, "x", 1:5, value=2),
#'   sum = SUM(d, "x", 1:5),
#'   m1 = MEAN(d, "x", 1:5),
#'   m2 = MEAN(d, vars=c("x1", "x4")),
#'   m3 = MEAN(d, varrange="x1:x2", rev="x2", range=1:5),
#'   cons1 = CONSEC(d, "x", 1:5),
#'   cons2 = CONSEC(d, varrange="x1:x5")
#' )]
#' d
#'
#' ## ====== Usage 2: `add()` & `added()` ====== ##
#' data = as.data.table(psych::bfi)
#' added(data, {
#'   gender = as.factor(gender)
#'   education = as.factor(education)
#'   E = .mean("E", 1:5, rev=c(1,2), range=1:6)
#'   A = .mean("A", 1:5, rev=1, range=1:6)
#'   C = .mean("C", 1:5, rev=c(4,5), range=1:6)
#'   N = .mean("N", 1:5, range=1:6)
#'   O = .mean("O", 1:5, rev=c(2,5), range=1:6)
#' }, drop=TRUE)
#' data
#'
#' ## ====== New Feature for `var` & `items` ====== ##
#' d = data.table(
#'   XX.1.pre = 1:5,
#'   XX.2.pre = 6:10,
#'   XX.3.pre = 11:15
#' )
#' add(d, { XX.mean = .mean("XX.{i}.pre", 1:3) })
#' add(d, { XX.mean = .mean("XX.{items}.pre", 1:3) })  # the same
#' add(d, { XX.mean = .mean("XX.{#$%^&}.pre", 1:3) })  # the same
#'
#' @name %%COMPUTE%%
NULL


paste_var_items = function(var, items) {
  if(grepl("\\{.*\\}", var)) {
    var.split = strsplit(var, "\\{.*\\}")[[1]]
    vars = paste0(var.split[1], items, var.split[2])
  } else {
    vars = paste0(var, items)
  }
  return(vars)
}


convert2vars = function(data,
                        var=NULL, items=NULL,
                        vars=NULL,
                        varrange=NULL,
                        rev=NULL) {
  if(!is.null(varrange)) {
    dn = names(data)
    varrange = gsub(" ", "", strsplit(varrange, ":")[[1]])
    vars = dn[which(dn==varrange[1]):which(dn==varrange[2])]
  }
  if(is.null(vars)) vars = paste_var_items(var, items)
  if(is.numeric(rev)) {
    if(is.null(var))
      stop("Argument `rev` must be character rather than numeric if you specify `vars` or `varrange`.", call.=FALSE)
    else
      rev = paste0(var, rev)
  }
  if(is.character(rev)) rev = which(vars %in% rev)
  vars.raw = vars
  vars = paste0(deparse(substitute(data)), "$`", vars, "`")
  return(list(vars.raw=vars.raw, vars=vars, rev=rev))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes
#' **Count** a certain value across variables.
#' @export
COUNT = function(data, var=NULL, items=NULL, vars=NULL, varrange=NULL,
                 value=NA) {
  Count = function(...) sum(c(...), na.rm=TRUE)
  v.r = convert2vars(data, var, items, vars, varrange)
  vars = v.r$vars
  if(is.na(value))
    varlist = paste0("is.na(", vars, ")")
  else
    varlist = paste0(vars, "==", value)
  expr = paste0("mapply(Count, ", paste(varlist, collapse=", "), ")")
  eval(parse(text=expr))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes
#' Compute **mode** across variables.
#' @export
MODE = function(data, var=NULL, items=NULL, vars=NULL, varrange=NULL) {
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  Mode = function(...) getmode(c(...))
  varlist = convert2vars(data, var, items, vars, varrange)$vars
  expr = paste0("mapply(Mode, ", paste(varlist, collapse=", "), ")")
  eval(parse(text=expr))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes
#' Compute **sum** across variables.
#' @export
SUM = function(data,
               var=NULL, items=NULL, vars=NULL, varrange=NULL,
               rev=NULL, range=likert, likert=NULL,
               na.rm=TRUE) {
  Sum = function(...) sum(..., na.rm=na.rm)
  v.r = convert2vars(data, var, items, vars, varrange, rev)
  vars = v.r$vars
  rev = v.r$rev
  if(!is.null(rev) & is.null(range)) {
    ranges = apply(as.data.frame(data)[,v.r$vars.raw], 2, function(...) range(..., na.rm=TRUE))
    range = c(min(ranges[1,], na.rm=TRUE), max(ranges[2,], na.rm=TRUE))
    warning("The range of likert scale was automatically estimated from the given data. If you are not sure about this, please specify the `range` argument. See ?SUM")
  }
  pre = rep("", length(vars))
  pre[rev] = ifelse(is.null(range), "", paste0(sum(range(range)), "-"))
  varlist = paste0(pre, vars)
  expr = paste0("mapply(Sum, ", paste(varlist, collapse=", "), ")")
  eval(parse(text=expr))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes
#' Tidy version of [SUM()], can only be used in [`add()/added()`][bruceR::add].
#' @export
.sum = function(var=NULL, items=NULL, vars=NULL, varrange=NULL,
                rev=NULL, range=likert, likert=NULL,
                na.rm=TRUE) {
  Sum = glue("function(...) sum(..., na.rm={na.rm})")
  if(!is.null(varrange))
    stop("Please specify either `vars` or `var` + `items`. To specify `varrange`, please use `SUM()`.", call.=FALSE)
  if(is.null(vars)) vars = paste_var_items(var, items)
  if(!is.null(rev) & is.null(range))
    stop("Please also specify the `range` argument!", call.=FALSE)
  if(is.numeric(rev)) {
    if(is.null(var))
      stop("Argument `rev` must be character rather than numeric if you specify `vars` or `varrange`.", call.=FALSE)
    else
      rev = paste0(var, rev)
  }
  if(is.character(rev)) rev = which(vars %in% rev)
  pre = rep("", length(vars))
  pre[rev] = ifelse(is.null(range), "", paste0(sum(range(range)), "-"))
  varlist = paste0(pre, vars)
  expr = paste0("mapply(", Sum, ", ", paste(varlist, collapse=", "), ")")
  return(glue(expr))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes
#' Compute **mean** across variables.
#' @export
MEAN = function(data,
                var=NULL, items=NULL, vars=NULL, varrange=NULL,
                rev=NULL, range=likert, likert=NULL,
                na.rm=TRUE) {
  Mean = function(...) mean(c(...), na.rm=na.rm)
  v.r = convert2vars(data, var, items, vars, varrange, rev)
  vars = v.r$vars
  rev = v.r$rev
  if(!is.null(rev) & is.null(range)) {
    ranges = apply(as.data.frame(data)[,v.r$vars.raw], 2, function(...) range(..., na.rm=TRUE))
    range = c(min(ranges[1,], na.rm=TRUE), max(ranges[2,], na.rm=TRUE))
    warning("The range of likert scale was automatically estimated from the given data. If you are not sure about this, please specify the `range` argument. See ?MEAN")
  }
  pre = rep("", length(vars))
  pre[rev] = ifelse(is.null(range), "", paste0(sum(range(range)), "-"))
  varlist = paste0(pre, vars)
  expr = paste0("mapply(Mean, ", paste(varlist, collapse=", "), ")")
  eval(parse(text=expr))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes
#' Tidy version of [MEAN()], can only be used in [`add()/added()`][bruceR::add].
#' @export
.mean = function(var=NULL, items=NULL, vars=NULL, varrange=NULL,
                 rev=NULL, range=likert, likert=NULL,
                 na.rm=TRUE) {
  Mean = glue("function(...) mean(c(...), na.rm={na.rm})")
  if(!is.null(varrange))
    stop("Please specify either `vars` or `var` + `items`. To specify `varrange`, please use `MEAN()`.", call.=FALSE)
  if(is.null(vars)) vars = paste_var_items(var, items)
  if(!is.null(rev) & is.null(range))
    stop("Please also specify the `range` argument!", call.=FALSE)
  if(is.numeric(rev)) {
    if(is.null(var))
      stop("Argument `rev` must be character rather than numeric if you specify `vars` or `varrange`.", call.=FALSE)
    else
      rev = paste0(var, rev)
  }
  if(is.character(rev)) rev = which(vars %in% rev)
  pre = rep("", length(vars))
  pre[rev] = ifelse(is.null(range), "", paste0(sum(range(range)), "-"))
  varlist = paste0(pre, vars)
  expr = paste0("mapply(", Mean, ", ", paste(varlist, collapse=", "), ")")
  return(glue(expr))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes
#' Compute **standard deviation** across variables.
#' @export
STD = function(data,
               var=NULL, items=NULL, vars=NULL, varrange=NULL,
               rev=NULL, range=likert, likert=NULL,
               na.rm=TRUE) {
  Std = function(...) sd(c(...), na.rm=na.rm)
  v.r = convert2vars(data, var, items, vars, varrange, rev)
  vars = v.r$vars
  rev = v.r$rev
  if(!is.null(rev) & is.null(range)) {
    ranges = apply(as.data.frame(data)[,v.r$vars.raw], 2, function(...) range(..., na.rm=TRUE))
    range = c(min(ranges[1,], na.rm=TRUE), max(ranges[2,], na.rm=TRUE))
    warning("The range of likert scale was automatically estimated from the given data. If you are not sure about this, please specify the `range` argument. See ?STD")
  }
  pre = rep("", length(vars))
  pre[rev] = ifelse(is.null(range), "", paste0(sum(range(range)), "-"))
  varlist = paste0(pre, vars)
  expr = paste0("mapply(Std, ", paste(varlist, collapse=", "), ")")
  eval(parse(text=expr))
}


#' @describeIn grapes-grapes-COMPUTE-grapes-grapes
#' Compute **consecutive identical digits** across variables (especially useful in detecting careless responding).
#' @export
CONSEC = function(data,
                  var=NULL, items=NULL, vars=NULL, varrange=NULL,
                  values=0:9) {
  Conseq = function(string, number=values) {
    # Consecutive Identical Digits
    pattern = paste(paste0(number, "{2,}"), collapse="|")
    ifelse(grepl(pattern, string), max(nchar(str_extract_all(string=string, pattern=pattern, simplify=TRUE))), 0)
  }
  v.r = convert2vars(data, var, items, vars, varrange)
  vars = v.r$vars
  varlist = vars
  expr = paste0("mapply(Conseq, paste0(", paste(varlist, collapse=", "), "))")
  eval(parse(text=expr))
}


#### Reliability, EFA, and CFA ####


#' Reliability analysis (Cronbach's \eqn{\alpha} and McDonald's \eqn{\omega}).
#'
#' @description
#' An extension of [psych::alpha()] and [psych::omega()], reporting (1) scale statistics (Cronbach's \eqn{\alpha} and McDonald's \eqn{\omega}) and (2) item statistics (item-rest correlation \[i.e., corrected item-total correlation\] and Cronbach's \eqn{\alpha} if item deleted).
#'
#' Three options to specify variables:
#' 1. **`var` + `items`**: common and unique parts of variable names (suggested).
#' 2. **`vars`**: a character vector of variable names (suggested).
#' 3. **`varrange`**: starting and stopping positions of variables (NOT suggested).
#'
#' @inheritParams %%COMPUTE%%
#' @param digits Number of decimal places of output. Defaults to `3`.
#'
#' @return
#' A list of results obtained from [psych::alpha()] and [psych::omega()].
#'
#' @seealso
#' [MEAN()]
#'
#' [EFA()]
#'
#' [CFA()]
#'
#' @examples
#' # ?psych::bfi
#' \donttest{data = psych::bfi
#' Alpha(data, "E", 1:5)   # "E1" & "E2" should be reversed
#' Alpha(data, "E", 1:5, rev=1:2)            # correct
#' Alpha(data, "E", 1:5, rev=cc("E1, E2"))   # also correct
#' Alpha(data, vars=cc("E1, E2, E3, E4, E5"), rev=cc("E1, E2"))
#' Alpha(data, varrange="E1:E5", rev=cc("E1, E2"))
#'
#' # using dplyr::select()
#' data %>% select(E1, E2, E3, E4, E5) %>%
#'   Alpha(vars=names(.), rev=cc("E1, E2"))
#' }
#' @export
Alpha = function(
    data, var, items, vars=NULL, varrange=NULL, rev=NULL,
    digits=3
) {
  if(!is.null(varrange)) {
    dn = names(data)
    varrange = gsub(" ", "", strsplit(varrange, ":")[[1]])
    vars = dn[which(dn==varrange[1]):which(dn==varrange[2])]
  }
  if(is.null(vars)) vars = paste_var_items(var, items)
  if(is.numeric(rev)) rev = paste0(var, rev)
  n.total = nrow(data)
  data = na.omit(as.data.frame(data)[vars])
  n.valid = nrow(data)
  for(v in vars) {
    data[[v]] = as.numeric(data[[v]])
    if(v %in% rev) {
      data[[v]] = min(data[[v]]) + max(data[[v]]) - data[[v]]
      vr = paste(v, "(rev)")
      Run("data = dplyr::rename(data, `{vr}`={v})")
    }
  }
  nitems = length(vars)

  suppressMessages({
    suppressWarnings({
      alpha = psych::alpha(data, delete=FALSE, warnings=FALSE)
      omega = psych::omega(data, nfactors=1, flip=FALSE)
      loadings = psych::principal(data, nfactors=1, scores=FALSE)$loadings

      items = cbind(
        alpha$item.stats[c("mean", "sd", "r.drop")],
        alpha$alpha.drop[c("raw_alpha")])
      names(items) = c("Mean", "S.D.",
                       "Item-Rest Cor.",
                       "Cronbach\u2019s \u03b1")
      items.need.rev = vars[loadings<0]
    })
  })

  Print("
  \n
  <<cyan Reliability Analysis>>

  Summary:
  Total Items: {nitems}
  Scale Range: {min(data)} ~ {max(data)}
  Total Cases: {n.total}
  Valid Cases: {n.valid} ({100*n.valid/n.total:.1}%)

  Scale Statistics:
  <<italic Mean>> = {alpha$total$mean:.{digits}}
  <<italic S.D.>> = {alpha$total$sd:.{digits}}
  Cronbach\u2019s \u03b1 = {alpha$total$raw_alpha:.{digits}}
  McDonald\u2019s \u03c9 = {omega$omega.tot:.{digits}}
  ")
  # Cronbach's \u03b1: {alpha$total$raw_alpha:.{digits}} (based on raw scores)
  # Cronbach's \u03b1: {alpha$total$std.alpha:.{digits}} (based on standardized items)

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
  print_table(items, digits=digits,
              title="Item Statistics (Cronbach\u2019s \u03b1 If Item Deleted):",
              note="Item-Rest Cor. = Corrected Item-Total Correlation")
  cat("\n")

  # rel = jmv::reliability(data, vars=eval(vars), revItems=eval(rev),
  #                        meanScale=TRUE, sdScale=TRUE,
  #                        alphaScale=TRUE, omegaScale=TRUE,
  #                        itemRestCor=TRUE, alphaItems=TRUE, omegaItems=TRUE)
  # rel$items$setTitle("Item Reliability Statistics (if item is dropped)")

  invisible(list(alpha=alpha, omega=omega))
}


#' Principal Component Analysis (PCA) and Exploratory Factor analysis (EFA).
#'
#' @description
#' An extension of [psych::principal()] and [psych::fa()], performing either Principal Component Analysis (PCA) or Exploratory Factor Analysis (EFA).
#'
#' Three options to specify variables:
#' 1. **`var` + `items`**: common and unique parts of variable names (suggested).
#' 2. **`vars`**: a character vector of variable names (suggested).
#' 3. **`varrange`**: starting and stopping positions of variables (NOT suggested).
#'
#' @inheritParams %%COMPUTE%%
#' @param method Extraction method.
#' - `"pca"`: Principal Component Analysis (default)
#' - `"pa"`: Principal Axis Factor Analysis
#' - `"ml"`: Maximum Likelihood Factor Analysis
#' - `"minres"`: Minimum Residual Factor Analysis
#' - `"uls"`: Unweighted Least Squares Factor Analysis
#' - `"ols"`: Ordinary Least Squares Factor Analysis
#' - `"wls"`: Weighted Least Squares Factor Analysis
#' - `"gls"`: Generalized Least Squares Factor Analysis
#' - `"alpha"`: Alpha Factor Analysis (Kaiser & Coffey, 1965)
#' @param rotation Rotation method.
#' - `"none"`: None (not suggested)
#' - `"varimax"`: Varimax (default)
#' - `"oblimin"`: Direct Oblimin
#' - `"promax"`: Promax
#' - `"quartimax"`: Quartimax
#' - `"equamax"`: Equamax
#' @param nfactors How to determine the number of factors/components?
#' - `"eigen"`: based on eigenvalue (> minimum eigenvalue) (default)
#' - `"parallel"`: based on parallel analysis
#' - any number >= `1`: user-defined fixed number
#' @param sort.loadings Sort factor/component loadings by size? Defaults to `TRUE`.
#' @param hide.loadings A number (0~1) for hiding absolute factor/component loadings below this value. Defaults to `0` (does not hide any loading).
#' @param plot.scree Display the scree plot? Defaults to `TRUE`.
#' @param kaiser Do the Kaiser normalization (as in SPSS)? Defaults to `TRUE`.
#' @param max.iter Maximum number of iterations for convergence. Defaults to `25` (the same as in SPSS).
#' @param min.eigen Minimum eigenvalue (used if `nfactors="eigen"`). Defaults to `1`.
#' @param digits Number of decimal places of output. Defaults to `3`.
#' @param file File name of MS Word (`".doc"`).
#' @param ... Arguments passed from [PCA()] to [EFA()].
#'
#' @note
#' Results based on the `varimax` rotation method are identical to SPSS. The other rotation methods may produce results slightly different from SPSS.
#'
#' @return
#' A list of results:
#' \describe{
#'   \item{`result`}{
#'     The R object returned from [psych::principal()] or [psych::fa()]
#'   }
#'   \item{`result.kaiser`}{
#'     The R object returned from [psych::kaiser()] (if any)
#'   }
#'   \item{`extraction.method`}{
#'     Extraction method
#'   }
#'   \item{`rotation.method`}{
#'     Rotation method
#'   }
#'   \item{`eigenvalues`}{
#'     A `data.frame` of eigenvalues and sum of squared (SS) loadings
#'   }
#'   \item{`loadings`}{
#'     A `data.frame` of factor/component loadings and communalities
#'   }
#'   \item{`scree.plot`}{
#'     A `ggplot` object of the scree plot
#'   }
#' }
#'
#' @describeIn EFA
#' Exploratory Factor Analysis
#'
#' @seealso
#' [MEAN()]
#'
#' [Alpha()]
#'
#' [CFA()]
#'
#' @examples
#' \donttest{data = psych::bfi
#' EFA(data, "E", 1:5)              # var + items
#' EFA(data, "E", 1:5, nfactors=2)  # var + items
#'
#' EFA(data, varrange="A1:O5",
#'     nfactors="parallel",
#'     hide.loadings=0.45)
#'
#' # the same as above:
#' # using dplyr::select() and dplyr::matches()
#' # to select variables whose names end with numbers
#' # (regexp: \\d matches all numbers, $ matches the end of a string)
#' data %>% select(matches("\\d$")) %>%
#'   EFA(vars=names(.),       # all selected variables
#'       method="pca",        # default
#'       rotation="varimax",  # default
#'       nfactors="parallel", # parallel analysis
#'       hide.loadings=0.45)  # hide loadings < 0.45
#' }
#' @export
EFA = function(
    data, var, items, vars=NULL, varrange=NULL, rev=NULL,
    method=c("pca", "pa", "ml", "minres", "uls", "ols", "wls", "gls", "alpha"),
    rotation=c("none", "varimax", "oblimin", "promax", "quartimax", "equamax"),
    nfactors=c("eigen", "parallel", "(any number >= 1)"),
    sort.loadings=TRUE,
    hide.loadings=0.00,
    plot.scree=TRUE,
    # plot.factor=TRUE,
    kaiser=TRUE,
    max.iter=25,
    min.eigen=1,
    digits=3,
    file=NULL
) {
  if(!is.null(varrange)) {
    dn = names(data)
    varrange = gsub(" ", "", strsplit(varrange, ":")[[1]])
    vars = dn[which(dn==varrange[1]):which(dn==varrange[2])]
  }
  if(is.null(vars)) vars = paste_var_items(var, items)
  if(is.numeric(rev)) rev = paste0(var, rev)
  n.total = nrow(data)
  data = na.omit(as.data.frame(data)[vars])
  n.valid = nrow(data)
  for(v in vars) {
    data[[v]] = as.numeric(data[[v]])
    if(v %in% rev) {
      data[[v]] = min(data[[v]])+max(data[[v]])-data[[v]]
      vr = paste(v, "(rev)")
      Run("data = dplyr::rename(data, `{vr}`={v})")
    }
  }
  nitems = length(vars)

  # determine number of factors
  eigen.value = eigen(cor(data), only.values=TRUE)$values
  eigen.parallel = NULL
  error.nfactors = "`nfactors` should be \"eigen\", \"parallel\", or an integer (>= 1)."
  if(length(nfactors)>1) nfactors = "eigen"
  if(is.numeric(nfactors)) {
    if(nfactors<1) stop(error.nfactors, call.=FALSE)
    nfactors = nfactors
  } else if(nfactors=="eigen") {
    nfactors = sum(eigen.value>min.eigen)
  } else if(nfactors=="parallel") {
    eigen.parallel = parallel_analysis(nrow(data), ncol(data), niter=20)
    nfactors = max(which(eigen.value<=eigen.parallel)[1]-1, 1)
    if(is.na(nfactors)) nfactors = length(eigen.value)
  } else {
    stop(error.nfactors, call.=FALSE)
  }

  # extraction method
  valid.methods = c("pca", "pa", "ml", "minres", "uls", "ols", "wls", "gls", "alpha")
  if(length(method)>1) method = "pca"
  if(method %notin% valid.methods)
    stop(Glue("
    EFA() has changed significantly since bruceR v0.8.0.
    `method` should be one of \"{paste(valid.methods, collapse='\", \"')}\".
    See: help(EFA)"), call.=FALSE)
  Method = switch(
    method,
    "pca"="Principal Component Analysis",
    "pa"="Principal Axis Factor Analysis",
    "ml"="Maximum Likelihood Factor Analysis",
    "minres"="Minimum Residual Factor Analysis",
    "uls"="Unweighted Least Squares Factor Analysis",
    "ols"="Ordinary Least Squares Factor Analysis",
    "wls"="Weighted Least Squares Factor Analysis",
    "gls"="Generalized Least Squares Factor Analysis",
    "alpha"="Alpha Factor Analysis (Kaiser & Coffey, 1965)")

  # rotation method
  valid.rotations = c("none", "varimax", "oblimin", "promax", "quartimax", "equamax")
  if(length(rotation)>1) rotation = "varimax"
  if(rotation %notin% valid.rotations)
    stop(Glue("
    EFA() has changed significantly since bruceR v0.8.0.
    `rotation` should be one of \"{paste(valid.rotations, collapse='\", \"')}\".
    See: help(EFA)"), call.=FALSE)
  Method.Rotation = switch(
    rotation,
    "none"="None",
    "varimax"="Varimax",
    "oblimin"="Oblimin",
    "promax"="Promax",
    "quartimax"="Quartimax",
    "equamax"="Equamax")
  if(rotation %in% c("none", "equamax")) kaiser = FALSE
  if(kaiser) Method.Rotation = paste(Method.Rotation, "(with Kaiser Normalization)")
  if(nfactors==1) Method.Rotation = "(Only one component was extracted. The solution was not rotated.)"

  # analyze
  suppressMessages({
    suppressWarnings({
      kmo = psych::KMO(data)$MSA
      btl = psych::cortest.bartlett(data, n=nrow(data))
      if(method=="pca") {
        efa = psych::principal(
          data, nfactors=nfactors, rotate=rotation)
      } else {
        efa = psych::fa(
          data, nfactors=nfactors, rotate=rotation,
          fm=method, max.iter=max.iter)
      }
      if(kaiser & nfactors>1) {
        Rotation = rotation
        if(rotation=="varimax") Rotation = "Varimax"  # GPArotation::Varimax
        if(rotation=="promax") Rotation = "Promax"  # psych::Promax
        efak = psych::kaiser(efa, rotate=Rotation)
        loadings = efak$loadings
      } else {
        efak = NULL
        loadings = efa$loadings
      }
      class(loadings) = "matrix"
    })
  })

  # print
  analysis = ifelse(method=="pca",
                    "Principal Component Analysis",
                    "Explanatory Factor Analysis")
  tag = ifelse(method=="pca", "Component", "Factor")
  Print("
  \n
  <<cyan {analysis}>>

  Summary:
  Total Items: {nitems}
  Scale Range: {min(data)} ~ {max(data)}
  Total Cases: {n.total}
  Valid Cases: {n.valid} ({100*n.valid/n.total:.1}%)

  Extraction Method:
  - {Method}
  Rotation Method:
  - {Method.Rotation}

  KMO and Bartlett's Test:
  - Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy: MSA = {kmo:.{digits}}
  - Bartlett's Test of Sphericity: Approx. {p(chi2=btl$chisq, df=btl$df)}
  ")

  # eigenvalues and SS loadings
  SS.loadings = apply(apply(loadings, 2, function(x) x^2), 2, sum)
  SS.loadings = c(SS.loadings, rep(NA, nitems-nfactors))
  eigen = data.frame(
    Eigenvalue = eigen.value,
    PropVar0 = 100*(eigen.value/nitems),
    CumuVar0 = cumsum(100*(eigen.value/nitems)),
    SS.Loading = SS.loadings,
    PropVar1 = 100*(SS.loadings/nitems),
    CumuVar1 = cumsum(100*(SS.loadings/nitems))
  )
  row.names(eigen) = paste(tag, 1:nitems)
  names(eigen) = c("Eigenvalue", "Variance %", "Cumulative %",
                   "SS Loading", "Variance %", "Cumulative %")
  cat("\n")
  print_table(eigen, digits=digits,
              title="Total Variance Explained:")

  # factor loadings
  loadings = as.data.frame(loadings)
  abs.loadings = abs(loadings)
  max = apply(abs.loadings, 1, max)
  which.max = apply(abs.loadings, 1, which.max)
  loadings$Communality = efa$communality
  # loadings$Uniqueness = efa$uniquenesses
  if(sort.loadings) loadings = loadings[order(which.max, -max), ]
  for(v in names(loadings)[1:nfactors]) {
    loadings[abs(loadings[[v]])<abs(hide.loadings), v] = NA
  }
  info1 = ifelse(rotation!="none" & nfactors>1, " (Rotated)", "")
  info2 = ifelse(sort.loadings, " (Sorted by Size)", "")
  loadings.info = info1%^%info2
  cat("\n")
  print_table(loadings, digits=digits,
              title=Glue("{tag} Loadings{loadings.info}:"))
  Print("
  Communality = Sum of Squared (SS) Factor Loadings
  (Uniqueness = 1 - Communality)
  \n
  ")
  if(!is.null(file))
    print_table(loadings, digits=digits, file=file,
                title=Glue("{tag} Loadings{loadings.info}:"),
                note=Glue("Extraction Method: {Method}.</p><p>Rotation Method: {Method.Rotation}."))

  # scree plot
  dp = data.frame(
    Type = "Data",
    Component = 1:length(eigen.value),
    Eigenvalue = eigen.value)
  if(!is.null(eigen.parallel)) {
    dp = rbind(dp, data.frame(
      Type = "Parallel (Simulation)",
      Component = 1:length(eigen.parallel),
      Eigenvalue = eigen.parallel))
  }
  Type = Component = Eigenvalue = NULL
  p = ggplot(dp, aes(x=Component, y=Eigenvalue, color=Type, fill=Type)) +
    geom_hline(yintercept=min.eigen, linetype=2) +
    geom_path(size=1) +
    geom_point(size=2.5, shape=21) +
    scale_y_continuous(limits=c(0, ceiling(max(eigen.value)))) +
    scale_color_manual(values=c("black", "grey50")) +
    scale_fill_manual(values=c("grey50", "grey90")) +
    labs(x=tag, title="Scree Plot") +
    theme_bruce() +
    theme(legend.position=c(0.85, 0.75))
  if(plot.scree) {
    try({
      plot.error = TRUE
      print(p)
      plot.error = FALSE
    }, silent=TRUE)
    if(plot.error) {
      warning = Glue("
        Plot is NOT successfully displayed in the RStudio `Plots` Pane.
        Please check if the `Plots` Pane of your RStudio is too small.
        You should enlarge the `Plots` Pane (and/or clear all plots).")
      warning(warning)
      cat("\n")
    }
  }

  # jmv::efa(data, vars=eval(expand_vars(vartext)),
  #          nFactorMethod=method,  # "eigen", "parallel", "fixed"
  #          extraction=extraction,  # "pa", "ml", "minres"
  #          rotation=rotation,  # "none", "varimax", "quartimax", "promax", "oblimin", "simplimax"
  #          minEigen=1,
  #          nFactors=nFactors,
  #          hideLoadings=hideLoadings, sortLoadings=TRUE,
  #          screePlot=TRUE, eigen=TRUE,
  #          factorCor=TRUE, factorSummary=TRUE, modelFit=TRUE,
  #          kmo=TRUE, bartlett=TRUE)

  invisible(list(
    result=efa,
    result.kaiser=efak,
    extraction.method=Method,
    rotation.method=Method.Rotation,
    eigenvalues=eigen,
    loadings=loadings,
    scree.plot=p))
}


#' @describeIn EFA
#' Principal Component Analysis - a wrapper of `EFA(..., method="pca")`
#' @export
PCA = function(..., method="pca") { EFA(..., method=method) }


parallel_analysis = function(nrow, ncol, niter=20) {
  sim.eigen = lapply(1:niter, function(x) {
    sim.data = matrix(rnorm(nrow*ncol), nrow=nrow, ncol=ncol)
    eigen(cor(sim.data), only.values=TRUE)$values
  })
  sim.eigen = t(matrix(unlist(sim.eigen), ncol=niter))
  sim.eigen.CI = apply(sim.eigen, 2, function(x) quantile(x, 0.95))
  return(sim.eigen.CI)
}


## Expand multiple variables with complex string formats
## Input: "X[1:5] + Y[c(1,3)] + Z"
## Output:
expand_vars = function(vartext) {
  vartexts = gsub(" ", "", strsplit(vartext, "\\+")[[1]])
  vars = c()
  for(vartext.i in vartexts) {
    if(grepl("\\[|\\]", vartext.i)==TRUE) {
      vars.i = eval(parse(text=paste0("paste0('", gsub("\\]", ")", gsub("\\[", "',", vartext.i)))))
    } else {
      vars.i = vartext.i
    }
    vars = c(vars, vars.i)
  }
  return(vars)
}


## CFA model formula transformation
modelCFA.trans = function(style=c("jmv", "lavaan"),
                          model, highorder="") {
  # model: free style input
  model = gsub("^\\s+|\\s+$", "", model)
  model = strsplit(gsub(" ", "", strsplit(model, "(;|\\n)+")[[1]]), "=~")
  # jmv style
  model.jmv = list()
  for(i in 1:length(model)) {
    var = model[[i]][[2]]
    vars = expand_vars(var)
    model.jmv[[i]] = list(label=model[[i]][[1]],
                          vars=vars)
  }
  # lavaan style
  model.lav = c()
  for(i in 1:length(model.jmv)) {
    model.i = paste(model.jmv[[i]]$label, paste(model.jmv[[i]]$vars, collapse=" + "), sep=" =~ ")
    model.lav = c(model.lav, model.i)
  }
  model.lav = paste(model.lav, collapse="\n")
  # high-order CFA (only for lavaan)
  factors = sapply(model.jmv, function(x) x$label)
  if(highorder!="")
    model.lav = paste(model.lav,
                      paste(highorder, "=~",
                            paste(factors, collapse=" + ")),
                      paste(highorder, "~~", highorder),
                      sep="\n")
  # output
  if(style=="jmv") return(model.jmv)
  if(style=="lavaan") return(model.lav)
}


#' Confirmatory Factor Analysis (CFA).
#'
#' An extension of [lavaan::cfa()].
#'
#' @inheritParams %%COMPUTE%%
#' @param model Model formula. See examples.
#' @param estimator The estimator to be used (for details, see [lavaan options][lavaan::lavOptions]).
#'
#' Defaults to `"ML"`. Can be one of the following:
#' - `"ML"`: Maximum Likelihood (can be extended to `"MLM"`, `"MLMV"`, `"MLMVS"`, `"MLF"`, or `"MLR"` for robust standard errors and robust test statistics)
#' - `"GLS"`: Generalized Least Squares
#' - `"WLS"`: Weighted Least Squares
#' - `"ULS"`: Unweighted Least Squares
#' - `"DWLS"`: Diagonally Weighted Least Squares
#' - `"DLS"`: Distributionally-weighted Least Squares
#' @param highorder High-order factor. Defaults to `""`.
#' @param orthogonal Defaults to `FALSE`. If `TRUE`, all covariances among latent variables are set to zero.
#' @param missing Defaults to `"listwise"`. Alternative is `"fiml"` ("Full Information Maximum Likelihood").
#' @param digits Number of decimal places of output. Defaults to `3`.
#' @param file File name of MS Word (`".doc"`).
#'
#' @return
#' A list of results returned by [lavaan::cfa()].
#'
#' @seealso
#' [Alpha()]
#'
#' [EFA()]
#'
#' [lavaan_summary()]
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
#' data.bfi = na.omit(psych::bfi)
#' CFA(data.bfi, "E =~ E[1:5]; A =~ A[1:5]; C =~ C[1:5]; N =~ N[1:5]; O =~ O[1:5]")
#' }
#' @export
CFA = function(
    data,
    model="A =~ a[1:5]; B =~ b[c(1,3,5)]; C =~ c1 + c2 + c3",
    estimator="ML",
    highorder="", orthogonal=FALSE, missing="listwise",
    digits=3,
    file=NULL
) {
  # model.jmv = modelCFA.trans("jmv", model)
  model.lav = modelCFA.trans("lavaan", model, highorder)
  # if(orthogonal==TRUE | highorder!="") style = "lavaan"

  cat("\n")
  Print("<<cyan Model Syntax (lavaan):>>")
  cat(model.lav)
  cat("\n")

  # lavaan style
  fit.lav = lavaan::cfa(
    model=model.lav,
    data=data,
    estimator=estimator,
    std.lv=TRUE,
    # TRUE: fixing the factor residual variances to 1
    # FALSE: fixing the factor loading of the first indicator to 1
    orthogonal=orthogonal,
    missing=missing  # fiml, listwise (default)
  )
  # cat("\n#### lavaan output ####\n\n")
  lavaan_summary(fit.lav, ci="raw", digits=digits, file=file)
  # lavaan::summary(fit.lav,
  #                 fit.measures=TRUE,
  #                 standardized=TRUE,
  #                 ci=CI,
  #                 modindices=MI)
  # if(MI) print(lavaan::modificationIndices(fit.lav))
  # if(plot) semPlot::semPaths(fit.lav, "std", curveAdjacent=TRUE,
  #                            style="lisrel", nDigits=2, edge.label.cex=1)

  Print("Estimator: {estimator}")
  cat("\n")

  invisible(fit.lav)
}

