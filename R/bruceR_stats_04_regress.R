#### Regression ####


#' Paste a formula into a string.
#'
#' @param formula R formula.
#'
#' @return A character string indicating the formula.
#'
#' @examples
#' formula_paste(y ~ x)
#' formula_paste(y ~ x + (1 | g))
#'
#' @export
formula_paste=function(formula) {
  paste(formula[2], formula[1], formula[3], collapse=" ")
}


#' Expand all interaction terms in a formula.
#'
#' @param formula R formula or a character string indicating the formula.
#' @param as.char Return character? Default is \code{FALSE}.
#'
#' @return A formula/character object including all expanded terms.
#'
#' @examples
#' formula_expand(y ~ a*b*c)
#' formula_expand("y ~ a*b*c")
#'
#' @export
formula_expand=function(formula, as.char=FALSE) {
  inter_expand=function(inter) paste(attr(terms.formula(as.formula(paste("~", inter))), "term.labels"), collapse=" + ")
  f=as.character(as.formula(formula))
  fx=f[3]
  fx.R=str_extract_all(fx, "\\([^\\)]+\\)", simplify=T)
  if(length(fx.R)>0) for(i in 1:length(fx.R)) if(grepl("\\*", fx.R[i])) fx.R[i]=paste0("(", inter_expand(str_remove_all(fx.R[i], "\\(|\\|.*")), " ", str_extract(fx.R[i], "\\|.*"))
  fx.F=str_remove_all(fx, "[\\+ ]*\\([^\\)]+\\)[\\+ ]*")
  fx.F=ifelse(fx.F=="", "", inter_expand(fx.F))
  fx=paste(fx.F, paste(fx.R, collapse=" + "), sep=ifelse(length(fx.R)==0 | fx.F=="", "", " + "))
  f=as.formula(paste(f[2], f[1], fx))
  if(as.char) f=formula_paste(f)
  return(f)
}


## Advanced %in% for factor variables (e.g., match "Sex" in "Sex1")
`%varin%`=function(x, vector) {
  any(grepl(paste0("^", x, "$"), vector))
}


## Find and return something from a list
find=function(vars, list) {
  n=0; var=group=c(); N=length(unlist(list))
  for(i in 1:length(list)) for(v in vars) if(v %varin% list[[i]]) {n=n+1; var=c(var, v); group=c(group, names(list[i]))}
  # for(i in 1:length(list)) for(lv in list[[i]]) if(lv %varin% vars) {n=n+1; var=c(var, lv); group=c(group, names(list[i]))}
  return(list(N=N, n=n, var=var, group=group))
}


#' Grand-mean centering.
#'
#' Compute grand-mean centered variables.
#' Usually used for GLM interaction-term predictors and HLM level-2 predictors.
#'
#' @param data Data object.
#' @param vars Variable(s) to be centered.
#' @param std Standardized or not. Default is \code{FALSE}.
#' @param add.suffix The suffix of the centered variable(s).
#' Default is \code{""}. You may set it to \code{"_c"}, \code{"_center"}, etc.
#'
#' @return A new data object containing the centered variable(s).
#'
#' @examples
#' d=data.table(a=1:5, b=6:10)
#'
#' d.c=grand_mean_center(d, "a")
#' d.c
#'
#' d.c=grand_mean_center(d, c("a", "b"), add.suffix="_center")
#' d.c
#'
#' @seealso \code{\link{group_mean_center}}
#'
#' @export
grand_mean_center=function(data, vars=names(data),
                           std=FALSE, add.suffix="") {
  data.c=as.data.frame(data)
  for(var in vars)
    if(inherits(data.c[[var]], c("numeric", "integer", "double", "logical")))
      data.c[paste0(var, add.suffix)]=as.numeric(scale(data.c[var], center=TRUE, scale=std))
  if(data.table::is.data.table(data))
    data.c=data.table::as.data.table(data.c)
  return(data.c)
}

#' Group-mean centering.
#'
#' Compute group-mean centered variables.
#' Usually used for HLM level-1 predictors.
#'
#' @inheritParams grand_mean_center
#' @param by Grouping variable.
#' @param add.group.mean The suffix of the variable name(s) of group means.
#' Default is \code{"_mean"} (see Examples).
#'
#' @return A new data object containing the centered variable(s).
#'
#' @examples
#' d=data.table(x=1:9, g=rep(1:3, each=3))
#'
#' d.c=group_mean_center(d, "x", by="g")
#' d.c
#'
#' d.c=group_mean_center(d, "x", by="g", add.suffix="_c")
#' d.c
#'
#' @seealso \code{\link{grand_mean_center}}
#'
#' @export
group_mean_center=function(data, vars=setdiff(names(data), by), by,
                           std=FALSE,
                           add.suffix="",
                           add.group.mean="_mean") {
  data.c=as.data.frame(data)
  grouplist=sort(unique(data.c[[by]]))
  for(var in vars) {
    for(group in grouplist) {
      if(inherits(data.c[[var]], c("numeric", "integer", "double", "logical"))) {
        dvar=data.c[which(data.c[by]==group), var]
        data.c[which(data.c[by]==group), paste0(var, add.group.mean)]=mean(dvar, na.rm=TRUE)
        data.c[which(data.c[by]==group), paste0(var, add.suffix)]=as.numeric(scale(dvar, center=TRUE, scale=std))
      }
    }
  }
  if(data.table::is.data.table(data))
    data.c=data.table::as.data.table(data.c)
  return(data.c)
}


#' Regression analysis.
#'
#' @inheritParams GLM_summary
#' @inheritParams HLM_summary
#' @param formula Model formula like \code{y ~ x1 + x2} (for \code{lm, glm}) or \code{y ~ x1 + x2 + (1 | group)} (for \code{lmer, glmer}).
#' @param data Data frame.
#' @param family [optional] The same as in \code{glm} and \code{glmer} (e.g., \code{family=binomial} will fit a logistic model).
#'
#' @return No return value.
#'
#' @examples
#' \dontrun{
#'
#'   ## lm
#'   regress(Temp ~ Month + Day + Wind + Solar.R, data=airquality, robust=TRUE)
#'
#'   ## glm
#'   regress(case ~ age + parity + education + spontaneous + induced,
#'           data=infert, family=binomial, robust="HC1", cluster="stratum")
#'
#'   ## lmer
#'   library(lmerTest)
#'   regress(Reaction ~ Days + (Days | Subject), data=sleepstudy)
#'   regress(Preference ~ Sweetness + Gender + Age + Frequency +
#'             (1 | Consumer), data=carrots)
#'
#'   ## glmer
#'   library(lmerTest)
#'   data.glmm=MASS::bacteria
#'   regress(y ~ trt + week + (1 | ID), data=data.glmm, family=binomial)
#'   regress(y ~ trt + week + hilo + (1 | ID), data=data.glmm, family=binomial)
#' }
#'
#' @export
regress=function(formula, data, family=NULL,
                 digits=3, nsmall=digits,
                 robust=FALSE, cluster=NULL,
                 level2.predictors="", vartypes=NULL,
                 test.rand=FALSE) {
  call=sys.call()[-1]  # get function call (argument list)
  if(!is.null(family))
    family.text=ifelse(!is.null(call$family),
                       deparse(call$family),
                       deparse(call[[3]]))
  y=as.character(formula)[2]
  x=as.character(formula)[3]
  # dots=list(...)
  if(grepl("\\|", x)==FALSE) {
    # lm & glm
    if(is.null(family)) {
      # model=lm(formula=formula, data=data)
      GLM_summary(model=NULL,
                  robust, cluster,
                  nsmall,
                  formula=formula, data=data)
    } else {
      # model=glm(formula=formula, data=data, family=family)
      GLM_summary(model=NULL,
                  robust, cluster,
                  nsmall,
                  formula=formula, data=data, family=family.text)
    }
  } else {
    # lmer & glmer
    if(is.null(family)) {
      # model=lmerTest::lmer(formula=formula, data=data)
      HLM_summary(model=NULL, level2.predictors, vartypes,
                  test.rand, nsmall,
                  formula=formula, data=data)
    } else {
      # model=lme4::glmer(formula=formula, data=data, family=family)
      HLM_summary(model=NULL,
                  test.rand=test.rand, nsmall=nsmall,
                  formula=formula, data=data, family=family.text)
    }
  }
}




#### Model Summary ####


#' Tidy report of regression models.
#'
#' Tidy report of regression models.
#' Most types of models are supported.
#' This function is an extension (and combination) of
#' \code{\link[texreg:screenreg]{texreg::screenreg()}},
#' \code{\link[texreg:htmlreg]{texreg::htmlreg()}},
#' \code{\link[MuMIn:std.coef]{MuMIn::std.coef()}},
#' \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM()}},
#' \code{\link[performance:r2_mcfadden]{performance::r2_mcfadden()}},
#' \code{\link[performance:r2_nagelkerke]{performance::r2_nagelkerke()}}.
#'
#' @param model.list A single model or a list of (various types of) models.
#' Most types of regression models are supported!
#' @param std Standardized coefficients? Default is \code{FALSE}.
#' Only applicable to linear models and linear mixed models.
#' Not applicable to generalized linear (mixed) models.
#' @param digits,nsmall Number of decimal places of output. Default is \code{3}.
#' @param file File name of MS Word (\code{.doc}).
#' @param zero Display "0" before "."? Default is \code{TRUE}.
#' @param modify.se Replace standard errors.
#' Useful if you need to replace raw SEs with robust SEs.
#' New SEs should be provided as a list of numeric vectors.
#' See usage in \code{\link[texreg:screenreg]{texreg::screenreg()}}.
#' @param modify.head Replace model names.
#' @param line Lines look like true line (\code{TRUE}) or \code{=== --- ===} (\code{FALSE}).
#' Only relevant to R Console output.
#' @param bold The \emph{p}-value threshold below which the coefficients will be formatted in bold.
#' @param ... Other arguments passed to
#' \code{\link[texreg:screenreg]{texreg::screenreg()}} or
#' \code{\link[texreg:htmlreg]{texreg::htmlreg()}}.
#'
#' @return Invisibly return the output (character string).
#'
#' @seealso
#' \code{\link{PROCESS}}
#'
#' \code{\link{GLM_summary}}
#'
#' \code{\link{HLM_summary}}
#'
#' \code{\link{med_summary}}
#'
#' \code{\link{lavaan_summary}}
#'
#' \code{\link{print_table}}
#'
#' @examples
#' \dontrun{
#'
#'   #### Example 1: Linear Model ####
#'   lm1=lm(Temp ~ Month + Day, data=airquality)
#'   lm2=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
#'   model_summary(lm1)
#'   model_summary(lm2)
#'   model_summary(list(lm1, lm2))
#'   model_summary(list(lm1, lm2), std=TRUE, digits=2)
#'   model_summary(list(lm1, lm2), file="OLS Models.doc")
#'   unlink("OLS Models.doc")  # delete file for code check
#'
#'   #### Example 2: Generalized Linear Model ####
#'   glm1=glm(case ~ age + parity,
#'            data=infert, family=binomial)
#'   glm2=glm(case ~ age + parity + education + spontaneous + induced,
#'            data=infert, family=binomial)
#'   model_summary(list(glm1, glm2))  # "std" is not applicable to glm
#'   model_summary(list(glm1, glm2), file="GLM Models.doc")
#'   unlink("GLM Models.doc")  # delete file for code check
#'
#'   #### Example 3: Linear Mixed Model ####
#'   library(lmerTest)
#'   hlm1=lmer(Reaction ~ (1 | Subject), data=sleepstudy)
#'   hlm2=lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
#'   hlm3=lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
#'   model_summary(list(hlm1, hlm2, hlm3))
#'   model_summary(list(hlm1, hlm2, hlm3), std=TRUE)
#'   model_summary(list(hlm1, hlm2, hlm3), file="HLM Models.doc")
#'   unlink("HLM Models.doc")  # delete file for code check
#'
#'   #### Example 4: Generalized Linear Mixed Model ####
#'   library(lmerTest)
#'   data.glmm=MASS::bacteria
#'   glmm1=glmer(y ~ trt + week + (1 | ID), data=data.glmm, family=binomial)
#'   glmm2=glmer(y ~ trt + week + hilo + (1 | ID), data=data.glmm, family=binomial)
#'   model_summary(list(glmm1, glmm2))  # "std" is not applicable to glmm
#'   model_summary(list(glmm1, glmm2), file="GLMM Models.doc")
#'   unlink("GLMM Models.doc")  # delete file for code check
#'
#'   #### Example 5: Multinomial Logistic Model ####
#'   library(nnet)
#'   d=airquality
#'   d$Month=as.factor(d$Month)  # Factor levels: 5, 6, 7, 8, 9
#'   mn1=multinom(Month ~ Temp, data=d, Hess=TRUE)
#'   mn2=multinom(Month ~ Temp + Wind + Ozone, data=d, Hess=TRUE)
#'   model_summary(mn1)
#'   model_summary(mn2)
#'   model_summary(mn2, file="Multinomial Logistic Model.doc")
#'   unlink("Multinomial Logistic Model.doc")  # delete file for code check
#' }
#'
#' @export
model_summary=function(model.list,
                       std=FALSE,
                       digits=3,
                       nsmall=digits,
                       file=NULL,
                       zero=ifelse(std, FALSE, TRUE),
                       modify.se=NULL,
                       modify.head=NULL,
                       line=TRUE,
                       bold=0,
                       ...) {
  if(inherits(model.list, "varest")) {
    model.list=model.list$varresult
    modify.head=names(model.list)
  }
  if(inherits(model.list, "list")==FALSE)
    model.list=list(model.list)
  if(is.null(file)) {
    sumreg=texreg::screenreg
  } else {
    sumreg=texreg::htmlreg
  }

  model_y=function(model) {
    if(any(class(model) %in% c("lmerMod", "lmerModLmerTest", "glmerMod")))
      y=model@call[["formula"]][[2]]
    else if(any(class(model) %in% c("lme")))
      y=model$call[["fixed"]][[2]]
    else
      y=model$call[["formula"]][[2]]
    if(is.null(y)) y=""
    return(y)
  }
  model_std_coef=function(model) {
    MuMIn::std.coef(model, partial.sd=FALSE)[,1]
  }
  model_std_s.e.=function(model) {
    MuMIn::std.coef(model, partial.sd=FALSE)[,2]
  }
  model_R2mcfadden=function(model) {
    ifelse(
      inherits(model, "glm"),
      1-model$deviance/model$null.deviance,
      NA)
  }
  model_R2nagelkerke=function(model) {
    ifelse(
      inherits(model, "glm"),
      as.numeric(performance::r2_nagelkerke(model)),
      NA)
  }
  model_R2m=function(model) {
    ifelse(
      inherits(model, c("lme", "lmerMod", "lmerModLmerTest", "glmerMod")),
      as.numeric(MuMIn::r.squaredGLMM(model)[1, "R2m"]),
      NA)
  }
  model_R2c=function(model) {
    ifelse(
      inherits(model, c("lme", "lmerMod", "lmerModLmerTest", "glmerMod")),
      as.numeric(MuMIn::r.squaredGLMM(model)[1, "R2c"]),
      NA)
  }

  if(is.null(modify.head)) {
    new.model.names=NULL
    try({
      if(any(unlist(lapply(model.list, inherits, "nnet")))) {
        multinom.y=as.character(lapply(model.list, model_y))[1]
        multinom.ref=model.list[[1]][["lab"]][1]
      } else {
        new.model.names=paste(paste0("(", 1:length(model.list), ")"),
                              as.character(lapply(model.list, model_y)))
        new.model.names=str_trim(new.model.names)
      }
    }, silent=TRUE)
  } else {
    new.model.names=modify.head
  }

  if(std) {
    new.coef=lapply(model.list, model_std_coef)
    new.s.e.=lapply(model.list, model_std_s.e.)
    omit="Intercept"
  } else {
    new.coef=0
    new.s.e.=0
    omit=NULL
  }

  if(!is.null(modify.se)) new.s.e.=modify.se

  suppressWarnings({
    new.R2.all=list()
    if(any(unlist(lapply(model.list, inherits, c("lme", "lmerMod", "lmerModLmerTest", "glmerMod"))))) {
      try({
        new.R2=list(
          R2m=as.numeric(lapply(model.list, model_R2m)),
          R2c=as.numeric(lapply(model.list, model_R2c))
        )
        names(new.R2)=c("Marginal R^2", "Conditional R^2")
        new.R2.all=c(new.R2.all, new.R2)
      }, silent=TRUE)
    }
    if(any(unlist(lapply(model.list, inherits, "glm")))) {
      new.R2=list(
        R2mcfadden=as.numeric(lapply(model.list, model_R2mcfadden)),
        R2nagelkerke=as.numeric(lapply(model.list, model_R2nagelkerke))
      )
      names(new.R2)=c("McFadden's R^2", "Nagelkerke's R^2")
      new.R2.all=c(new.R2.all, new.R2)
    }
    if(length(new.R2.all)==0)
      new.R2.all=NULL

    output=sumreg(
      model.list, file=NULL,
      leading.zero=zero, digits=nsmall, bold=bold,
      custom.model.names=new.model.names,
      override.coef=new.coef,
      override.se=new.s.e.,
      omit.coef=omit,
      custom.gof.rows=new.R2.all,
      margin=1,
      inline.css=FALSE,
      doctype=TRUE,
      html.tag=TRUE,
      head.tag=TRUE,
      body.tag=TRUE,
      caption.above=TRUE,
      caption=NULL,
      custom.note="",
      include.loglik=FALSE,
      include.deviance=FALSE,
      beside=TRUE,  # for 'multinom' models
      ...)
  })

  if(is.null(file)) {
    if(line) {
      output=output %>%
        str_replace_all(
          "[-=]{3,}",
          rep_char("\u2500", str_count(output, "=")/2))
    }
    cat("\n")
    Print("<<cyan Model Summary>>")
    cat(output)
    Print("<<italic Note>>. * <<italic p>> < .05, ** <<italic p>> < .01, *** <<italic p>> < .001.")
    cat("\n")
    if(length(model.list)==1) {
      try({
        suppressWarnings({
          check=performance::check_collinearity(model.list[[1]])
        })
        if(!is.null(check)) {
          print(check)
          cat("\n")
        }
      }, silent=TRUE)
    }
  } else {
    file=str_replace(file, "\\.docx$", ".doc")
    output=output %>%
      str_replace_all("&nbsp;", "") %>%
      str_replace_all("'", "\u2019") %>%
      str_replace_all("<td>-", "<td>\u2013") %>%
      str_replace_all("<td>(?=\\.|\\d\\.)", "<td>&ensp;") %>%
      str_replace_all("R\\^2|R<sup>2</sup>", "<i>R</i><sup>2</sup>") %>%
      str_replace(
        "<style>",
        "<style>\nbody {font-size: 10.5pt; font-family: Times New Roman;}\np {margin: 0px;}\nth, td {height: 19px;}") %>%
      str_replace(
        "<body>",
        paste0(
          "<body>\n<p><b>Table X. Regression Models",
          ifelse(any(unlist(lapply(model.list, class)) %in% "nnet"),
                 paste0(" (Reference Group: ", multinom.y, " = \u2018", multinom.ref, "\u2019)"),
                 ""),
          ".</b></p>")) %>%
      str_replace(
        "</body>",
        paste0(
          "<p><i>Note</i>. ",
          ifelse(std, "Standardized ", "Unstandardized "),
          "regression coefficients are displayed, with standard errors in parentheses.</p><p>",
          "* <i>p</i> < .05. ** <i>p</i> < .01. *** <i>p</i> < .001.</p>",
          "</body>"))
    # sink(file)
    # cat(output)
    # sink()
    f=file(file, "w", encoding="UTF-8")
    cat(output, file=f)
    close(f)
    Print("<<green \u2714>> Table saved to <<blue '{paste0(getwd(), '/', file)}'>>")
    cat("\n")
  }

  invisible(output)
}




#### GLM Functions ####


#' Tidy report of GLM (\code{lm} and \code{glm} models).
#'
#' @param model A model fitted by \code{lm} or \code{glm} function.
#' @param robust \strong{[Only for \code{lm} and \code{glm}]}
#' \code{FALSE} (default), \code{TRUE} (then the default is \code{"HC1"}),
#' \code{"HC0"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"}, \code{"HC4"}, \code{"HC4m"}, or \code{"HC5"}.
#' It will add a table with heteroskedasticity-robust standard errors (aka. Huber-White standard errors).
#' For details, see \code{?sandwich::vcovHC} and \code{?jtools::summ.lm}.
#'
#' *** \code{"HC1"} is the default of Stata, whereas \code{"HC3"} is the default suggested by the \code{sandwich} package.
#' @param cluster \strong{[Only for \code{lm} and \code{glm}]}
#' Cluster-robust standard errors are computed if cluster is set to the name of the input data's cluster variable or is a vector of clusters.
#' If you specify \code{cluster}, you may also specify the type of \code{robust}. If you do not specify \code{robust}, \code{"HC1"} will be set as default.
#' @param digits,nsmall Number of decimal places of output. Default is 3.
#' @param ... Other arguments. You may re-define \code{formula}, \code{data}, or \code{family}.
#'
#' @return No return value.
#'
#' @examples
#' \donttest{## Example 1: OLS regression
#' lm=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
#' GLM_summary(lm)
#' GLM_summary(lm, robust="HC1")
#' # Stata's default is "HC1"
#' # R package <sandwich>'s default is "HC3"
#'
#' ## Example 2: Logistic regression
#' glm=glm(case ~ age + parity + education + spontaneous + induced,
#'         data=infert, family=binomial)
#' GLM_summary(glm)
#' GLM_summary(glm, robust="HC1", cluster="stratum")
#' }
#' @seealso \code{\link{HLM_summary}}, \code{\link{regress}}
#'
#' @export
GLM_summary=function(model, robust=FALSE, cluster=NULL,
                     digits=3, nsmall=digits, ...) {
  dots=list(...)
  if(c("formula", "data") %allin% names(dots)) {
    # re-modeling
    formula=dots$formula
    data=dots$data
    if("family" %notin% names(dots))
      eval(parse(text=Glue("model=lm({formula_paste(formula)}, data=data)")))
    else
      eval(parse(text=Glue("model=glm({formula_paste(formula)}, data=data, family={dots$family})")))
  } else {
    formula=model[["call"]][["formula"]]
  }
  dv=formula[[2]]
  sumModel=summary(model)
  N=nrow(model$model)

  ## lm vs.glm ##
  if(class(model)[1]=="lm") {
    ## Print: Model Information ##
    Print("
    <<underline MODEL INFO:>>
    Model type: General Linear Model (GLM) (OLS Regression)
    Observations: <<italic N>> = {N}{ifelse('na.action' %in% names(model), Glue(' ({length(model$na.action)} missing cases deleted)'), '')}
    ")

    ## Print: Model Fit (R^2) ##
    Ftest=sumModel[["fstatistic"]]
    names(Ftest)=c("F", "df1", "df2")
    Print("\n\n
    <<underline MODEL FIT:>>
    {p(f={Ftest['F']}, df1={Ftest['df1']}, df2={Ftest['df2']})}
    <<italic R>>\u00b2 = {sumModel$r.squared:.5} (Adjusted <<italic R>>\u00b2 = {sumModel$adj.r.squared:.5})
    ")

    ## Print: ANOVA Table ##
    # cat("\n")
    # GLM_anova(model)

    ## Print: Fixed Effects ##
    FE=as.data.frame(sumModel[["coefficients"]])
    names(FE)=c("b", "S.E.", "t", "p")
    df=model[["df.residual"]]
    FE=cbind(
      FE,
      sig=sig.trans(FE$p),
      CI=paste0("[",
                formatF(FE[,1]+qt(0.025, df)*FE[,2], nsmall), ", ",
                formatF(FE[,1]+qt(0.975, df)*FE[,2], nsmall), "]"))
    if(length(model[["model"]])>2) {
      FE.vif=jtools::summ(model, vif=TRUE)
      FE=cbind(FE, VIF=FE.vif$coeftable[,"VIF"])
    } else {
      FE=cbind(FE, VIF=NA)
    }
    FE$VIF=formatF(FE$VIF, nsmall=nsmall)
    FE$VIF[1]=""
    FE$p=p.trans(FE$p)
    names(FE)[5:6]=c(" ", "[95% CI of b]")
    cat("\n")
    Print("<<underline FIXED EFFECTS:>>
           Outcome variable: {dv} (<<italic N>> = {N})")
    print_table(FE, nsmalls=c(nsmall, nsmall, 2, 0, 0, 0, 0))

    ## Print: Robust SE ##
    if(robust!=FALSE | !is.null(cluster)) {
      if(robust==TRUE) robust="HC1"
      if(!is.null(cluster) & !is.character(robust)) robust="HC1"
      summ.rob=jtools::summ(model, robust=robust, cluster=cluster)
      FE.rob=as.data.frame(summ.rob$coeftable)
      names(FE.rob)=c("b", "S.E.*", "t*", "p*")
      FE.rob=cbind(
        FE.rob,
        sig=sig.trans(FE.rob$`p*`),
        CI=paste0("[",
                  formatF(FE.rob[,1]+qt(0.025, df)*FE.rob[,2], nsmall), ", ",
                  formatF(FE.rob[,1]+qt(0.975, df)*FE.rob[,2], nsmall), "]"))
      FE.rob$`p*`=p.trans(FE.rob$`p*`)
      names(FE.rob)[5:6]=c(" ", "[95% CI of b]")
      cat("\n")
      Print("{ifelse(is.null(cluster), 'Heteroskedasticity', 'Cluster')}-robust standard errors:")
      print_table(FE.rob, nsmalls=c(nsmall, nsmall, 2, 0, 0, 0))
      Print("<<blue Robust S.E.: type = {robust}{ifelse(is.null(cluster), '', glue('; clustering variable = {paste(cluster, collapse=', ')}'))}.>>")
    }

    ## Print: Standardized Coefficients ##
    if(nrow(FE)>1) {
      FE.std=as.data.frame(MuMIn::std.coef(model, partial.sd=FALSE))[-1, 1:2]
      # FE.std=sjstats::std_beta(model)[2:3]
      # row.names(FE.std)=row.names(FE)[-1]
      names(FE.std)=c("Beta*", "S.E.*")
      FE.rp=jtools::summ(model, part.corr=TRUE)
      t=FE.std[,1]/FE.std[,2]  # FE$t[-1]
      p=p.t(t, df)
      FE.std=cbind(
        FE.std,
        sig=sig.trans(p),
        CI.std=paste0("[",
                      formatF(FE.std[,1]+qt(0.025, df)*FE.std[,2], nsmall), ", ",
                      formatF(FE.std[,1]+qt(0.975, df)*FE.std[,2], nsmall), "]"),
        r.partial=FE.rp$coeftable[-1, "partial.r"],
        r.part=FE.rp$coeftable[-1, "part.r"])
      cat("\n")
      Print("Standardized coefficients: {dv} (<<italic N>> = {N})")
      names(FE.std)[3:4]=c(" ", "[95% CI of Beta]")
      print_table(FE.std, nsmalls=nsmall)
    }
  } else if(class(model)[1]=="glm") {
    ## Print: Model Information ##
    Print("
    <<underline MODEL INFO:>>
    Model type: Generalized Linear Model (GLM)
    Family: {model$family$family}
    Link function: {model$family$link}
    Observations: <<italic N>> = {N}{ifelse('na.action' %in% names(model), Glue(' ({length(model$na.action)} missing cases deleted)'), '')}
    ")

    ## Print: Model Fit (Pseudo-R^2 and Information Criteria) ##
    # null.model=glm(as.formula(paste(dv, "~ 1")), data=model$data,
    #                family=model$family$family)
    # aov.glm=anova(null.model, model)
    aov.glm=anova(model, test="Chisq")
    Chi2=sum(aov.glm$Deviance, na.rm=TRUE)
    Df=sum(aov.glm$Df, na.rm=TRUE)
    # R2.glm=sjstats::r2(model)  # 'sjstats::r2()' is deprecated [2019-09].
    R2.glm=performance::r2_nagelkerke(model)
    Print("\n\n
    <<underline MODEL FIT:>>
    AIC = {AIC(model):.{nsmall}}
    BIC = {BIC(model):.{nsmall}}
    {p(chi2={Chi2}, df={Df})}
    {rep_char('\u2500', 7)} Pseudo-<<italic R>>\u00b2s {rep_char('\u2500', 7)}
    McFadden's <<italic R>>\u00b2    = {1 - model$deviance/model$null.deviance:.5}  <<blue (= 1 - logLik(model)/logLik(null.model))>>
    Nagelkerke's <<italic R>>\u00b2  = {R2.glm:.5}  <<blue (= Cragg-Uhler's <<italic R>>\u00b2, adjusts Cox & Snell's)>>
    ")
    # McFadden's adj. <<italic R>>\u00b2 = {1 - (logLik(model)-length(model$coefficients)+1)/logLik(null.model):.5}  <<blue (adjusted for number of predictors)>>
    # Cox & Snell's <<italic R>>\u00b2 = {R2.glm$CoxSnell:.5}  <<blue (problematic, not suggested)>>
    # Nagelkerke's <<italic R>>\u00b2  = {R2.glm$Nagelkerke:.5}  <<blue (= Cragg-Uhler's <<italic R>>\u00b2, adjusts Cox & Snell's)>>

    ## Print: Fixed Effects ##
    FE=as.data.frame(sumModel[["coefficients"]])
    names(FE)=c("b", "S.E.", "z", "p")
    b=FE[,1]
    se=FE[,2]
    FE=cbind(
      FE,
      sig=sig.trans(FE$p),
      CI=paste0("[",
                formatF(b-1.96*se, nsmall), ", ",
                formatF(b+1.96*se, nsmall), "]"),
      OR=exp(b))
    if(length(model[["model"]])>2) {
      FE.vif=jtools::summ(model, vif=TRUE)
      FE=cbind(FE, VIF=FE.vif$coeftable[,"VIF"])
    } else {
      FE=cbind(FE, VIF=NA)
    }
    FE$VIF=formatF(FE$VIF, nsmall=nsmall)
    FE$VIF[1]=""
    FE$p=p.trans(FE$p)
    names(FE)[5:6]=c(" ", "[95% CI of b]")
    cat("\n")
    Print("<<underline FIXED EFFECTS:>>
           Outcome variable: {dv} (type = {model$family$family}) (<<italic N>> = {N})")
    print_table(FE, nsmalls=c(nsmall, nsmall, 2, 0, 0, 0, nsmall, 0))
    Print("<<blue OR = odds ratio.>>")

    ## Print: Robust SE ##
    if(robust!=FALSE | is.null(cluster)==FALSE) {
      if(robust==TRUE | (is.null(cluster)==FALSE & is.character(robust)==FALSE))
        robust="HC1" # default method in Stata
      summ.rob=jtools::summ(model, robust=robust, cluster=cluster)
      FE.rob=as.data.frame(summ.rob$coeftable)
      names(FE.rob)=c("b", "S.E.*", "z*", "p*")
      b=FE[,1]
      se.rob=FE.rob[,2]
      FE.rob=cbind(
        FE.rob,
        sig=sig.trans(FE.rob$`p*`),
        CI=paste0("[",
                  formatF(b-1.96*se.rob, nsmall), ", ",
                  formatF(b+1.96*se.rob, nsmall), "]"))
      FE.rob$`p*`=p.trans(FE.rob$`p*`)
      names(FE.rob)[5:6]=c(" ", "[95% CI of b]")
      cat("\n")
      Print("{ifelse(is.null(cluster), 'Heteroskedasticity', 'Cluster')}-robust standard errors:")
      print_table(FE.rob, nsmalls=c(nsmall, nsmall, 2, 0, 0, 0))
      Print("<<blue Robust S.E.: type = {robust}{ifelse(is.null(cluster), '', glue('; clustering variable = {paste(cluster, collapse=', ')}'))}.>>")
    }
  } else {
    stop("GLM_summary() can only deal with 'lm' or 'glm' models.", call.=TRUE)
  }
}




#### HLM Functions ####


## Automatically judging variable types in HLM
HLM_vartypes=function(model=NULL,
                      formula=model@call$formula,
                      level2.predictors="") {
  varlist=dimnames(summary(model)[["coefficients"]])[[1]]
  vartypes=c()
  L1.rand.vars=L2.vars=list()
  data=as.data.frame(model@frame)
  formula=formula_expand(formula)
  fx=as.character(formula)[3]
  ## Level-1 predictor variables with random slopes
  L1.rand.comp=str_split(gsub(" ", "", str_extract_all(fx, "(?<=\\()[^\\)]+(?=\\))", simplify=T)), "\\|")
  for(comp in L1.rand.comp) {
    vars.1=str_split(comp[1], "\\+")  # a list
    for(var in vars.1[[1]]) {
      if(var %in% names(data))
        if(is.factor(data[,var]))
          vars.1[[1]]=append(vars.1[[1]], paste0(var, levels(data[,var])))
    }
    L1.rand.vars[comp[2]]=vars.1
  }
  ## Level-2 predictor variables
  L2.comp=str_split(str_split(gsub(" ", "", level2.predictors), ";", simplify=T), ":")
  for(comp in L2.comp) {
    vars.2=str_split(comp[2], "\\+")  # a list
    for(var in vars.2[[1]]) {
      if(var %in% names(data))
        if(is.factor(data[,var]))
          vars.2[[1]]=append(vars.2[[1]], paste0(var, levels(data[,var])))
    }
    L2.vars[comp[1]]=vars.2
  }
  ## Judge variable types
  for(var in varlist) {
    fd1=find(var, L1.rand.vars)
    fd2=find(var, L2.vars)
    if(var=="(Intercept)") {
      vartype="Intercept"
    } else if(grepl(":", var)) {
      ## interaction term ##
      inter.vars=str_split(var, ":")[[1]]
      fd.1=find(inter.vars, L1.rand.vars)
      fd.2=find(inter.vars, L2.vars)
      if(fd.2$n==length(inter.vars)) {
        vartype=glue("L2-{fd.2$group[1]}") # warning: cross-classified may not be true
      } else if(fd.2$n>0) {
        vartype=glue("Cross-{fd.1$group[1]}-{paste(fd.1$var, collapse=':')}")
      } else if(fd1$n>0) {
        vartype=glue("L1random-{fd1$group}-{fd1$var}")
      } else {
        vartype="L1fixed"
      }
    } else {
      ## not interaction term ##
      if(fd2$n>0) {
        vartype=glue("L2-{fd2$group}")
      } else if(fd1$n>0) {
        vartype=glue("L1random-{fd1$group}-{fd1$var}")
      } else {
        vartype="L1fixed"
      }
    }
    vartypes[var]=vartype
  }
  return(vartypes)
}

# f1=as.formula(Y ~ 1 + X1 + X2 + X1:X2 + X3 + X4 + X4:W1 + W1 + W2 + W1:W2 + Z1 + (X3+X4|W) + (1|Z))
# f2=as.formula(Y ~ X1:X2:X3 + X1:X2:W1 + X1:W1:W2 + W1:W2:Z1 + (X1+X2|W) + (1|Z))
# v1=c("(Intercept)", "X1", "X2", "X3", "X4", "W1", "W2", "Z1", "X1:X2", "X4:W1", "W1:W2")
# v2=c("(Intercept)", "X1:X2:X3", "X1:X2:W1", "X1:W1:W2", "W1:W2:Z1")
# HLM_vartypes(formula=f1, varlist=v1, level2.predictors="W: W1+W2; Z: Z1")
# HLM_vartypes(formula=f2, varlist=v2, level2.predictors="W: W1+W2; Z: Z1")


## Calculating HLM df
HLM_df=function(sumModel, vartypes) {
  paras=sumModel[["devcomp"]][["dims"]][["p"]]
  df.l1=sumModel[["devcomp"]][["dims"]][["nmp"]] # N - all parameters
  df.l2=sumModel[["ngrps"]]
  Sq=sum(grepl("L2", vartypes)) # number of level-2 predictors
  q=df.l2
  for(grouptag in names(df.l2))
    q[grouptag]=sum(grepl(paste0("L2-", grouptag), vartypes))
  dfs=c()
  for(i in 1:paras) {
    if(vartypes[i]=="Intercept") {
      # df=min(df.l2)-Sq-1
      df=NA
    } else if(vartypes[i]=="L1fixed") {
      df=df.l1
    } else {
      vartemp=strsplit(vartypes[i], "-")[[1]]
      vartype=vartemp[1]
      grouptag=vartemp[2]
      if(vartype=="L2") {
        df=df.l2[grouptag]-q[grouptag]-1
      } else {
        # vartype=="L1random" | vartype=="Cross"
        l1var=vartemp[3]
        qc=sum(grepl(paste0("Cross-", grouptag, "-", l1var), vartypes))
        df=df.l2[grouptag]-qc-1
      }
    }
    dfs[i]=df
  }
  return(dfs)
}


## Testing random effects and computing intraclass correlation coefficient (ICC) for HLM
HLM_ICC=function(model, nsmall=3) {
  ## Extract components from model ##
  sumModel=summary(model)
  data=as.data.frame(model@frame)
  RE=as.data.frame(sumModel[["varcor"]])
  RE=RE[is.na(RE[[3]]), -3]

  ## Initialize ICC data.frame ##
  N=nrow(model@frame)
  K=sumModel[["ngrps"]][RE$grp]
  group.id=na.omit(names(K))
  ICC=data.frame(RE[1], K, RE[2], RE[3])
  names(ICC)=c("Group", "K", "Parameter", "Variance")
  var.resid=ICC[ICC$Group=="Residual", "Variance"]
  var.total=sum(ICC[ICC$Parameter=="(Intercept)" | ICC$Group=="Residual", "Variance"])

  ## Mean sample size of each group ##
  # n.mean=c()
  # for(group in group.id) n.mean=c(n.mean, (N-sum(data[,.(n=.N^2), by=group]$n)/N) / (nlevels(data[[group]])-1) )  # quite similar to "N / K"
  # n.mean=c(n.mean, NA)

  ## Test for variance (Wen Fuxing, 2009, pp.85-97; Snijders & Bosker, 2012, pp.190-191) ##
  var=ICC$Variance
  # var.se=var.resid * (2/N) * sqrt(1/(n.mean-1) + 2*var/var.resid + n.mean*var^2/var.resid^2)  # error !!!
  # var.se=sqrt(2*(var+var.resid/n.mean)^2/(K-1) + 2*var.resid^2/((N-K)*n.mean^2))
  # var.wald.z=var/var.se
  # var.p=p.z(var.wald.z)

  ## Compute and test for ICC (Snijders & Bosker, 2012, pp.20-21) ##
  icc=var/var.total
  # icc.se=(1-icc) * (1+(n.mean-1)*icc) * sqrt(2/(n.mean*(n.mean-1)*(N-1)))  # N or K ?!
  # icc.wald.z=icc/icc.se

  ## Combine results ##
  ICC$K=formatF(ICC$K, nsmall=0)
  ICC$Variance=formatF(ICC$Variance, nsmall=5)
  # ICC$S.E.=formatF(var.se, nsmall=nsmall)
  # ICC$Wald.Z=formatF(var.wald.z, nsmall=2)
  # ICC$p=p.trans(var.p)
  # ICC$sig=sig.trans(var.p)
  ICC$ICC=formatF(icc, nsmall=5)
  ICC[ICC$Group=="Residual", c("K", "Parameter", "ICC")] = ""
  ICC[ICC$Parameter!="(Intercept)" & ICC$Group!="Residual", c("Group", "K", "ICC")] = ""
  names(ICC)[1]=paste0("Cluster", rep_char(" ", max(nchar(ICC$Group))-7))
  names(ICC)[2]="K  "
  names(ICC)[3]=paste0("Parameter", rep_char(" ", max(nchar(ICC$Parameter))-9))
  # names(ICC)[6]="Wald Z"
  # names(ICC)[8]=" "

  return(ICC)
}


#' Tidy report of HLM (\code{lmer} and \code{glmer} models).
#'
#' @description
#' Nice report of \strong{Hierarchical Linear Model (HLM)}, also known as \strong{Multilevel Linear Model (MLM)} or \strong{Linear Mixed Model (LMM)}.
#' HLM, MLM, or LMM (the same) refers to a model with nested data (e.g., Level-1: participants, Level-2: city; or Level-1: repeated-measures within a participant, Level-2: participants).
#'
#' @details
#' Hierarchical Linear Model (HLM), aka. Multilevel Linear Model (MLM) or Linear Mixed Model (LMM), is more complex than General Linear Model (GLM; i.e., OLS regression).
#' Predictor variables at different levels may have five types:
#' \describe{
#'   \item{1. Intercept}{The overall intercept (\eqn{\gamma_{00}})}
#'   \item{2. L1fixed}{Level-1 predictor with \strong{fixed} slope}
#'   \item{3. L1random-GROUP-L1VAR}{Level-1 predictor with \strong{random} slopes nested with a grouping/clustering variable}
#'   \item{4. L2-GROUP}{Level-2 predictor (e.g., GDP per capita at city level), always with \strong{fixed} slope unless there is also a level-3 structure.
#'
#'   *** NOTE: the current version of \code{'HLM_summary'} function does not consider three-levels design, so you may only use this function in two-levels HLM or cross-classified HLM.}
#'   \item{5. Cross-GROUP-L1VAR}{Cross-level interaction consisting of level-1 and level-2 predictors}
#' }
#' The degrees of freedom (\emph{df}) of predictor variables in HLM vary across different levels and also depend on the variable types.
#' However, different software use different estimation methods and thus provide somewhat different \emph{df}s, which may be confusing.
#' Whereas the \code{lmerTest} package in R provides \emph{df}s that are estimated by the Satterthwaite's (1946) approximation (i.e., a data-driven approach without defining variable types),
#' the \code{HLM} software provides \emph{df}s that totally depend on the variable types (i.e., a theory-driven approach).
#'
#' @param model A model fitted by \code{lmer} or \code{glmer} function using the \code{lmerTest} package.
#' @param level2.predictors \strong{[Only for \code{lmer}]} [optional] Default is \code{NULL}.
#' If you have predictors at level 2, besides putting them into the formula in the \code{lmer} function as usual,
#' you may \strong{also} define here the level-2 grouping/clustering variables and corresponding level-2 predictor variables.
#'
#' *** Example: \code{level2.predictors="School: W1 + W2; House: 1"},
#' where \code{School} and \code{House} are two grouping variables,
#' \code{W1 & W2} are school-level predictors,
#' and there is no house-level predictor.
#'
#' *** If there is no level-2 predictor in the formula of \code{lmer}, just leave this argument blank.
#' @param vartypes \strong{[Only for \code{lmer}]} Manually setting variable types. Needless in most situations.
#' @param test.rand \strong{[Only for \code{lmer}]} \code{TRUE} or \code{FALSE} (default).
#' Test random effects (i.e., variance components) by using the likelihood-ratio test (LRT), which is asymptotically chi-square distributed. For large datasets, it is much time-consuming.
## *** Note that its results would be different from those in the default output of \code{HLM_summary()} (see "Wald \emph{Z} test" in the output),
## because they differ in the principle of statistics. The LRT is based on model comparison and the reduction of AIC, whereas the Wald \emph{Z} test is estimated by approximation.
## The Wald \emph{Z} test can also be seen in the output of SPSS (the \code{MIXED} syntax).
#' @param digits,nsmall Number of decimal places of output. Default is 3.
#' But for some statistics (e.g., \emph{R}^2, ICC), to provide more precise information, we fix the decimal places to 5.
#' @param ... Other arguments. You may re-define \code{formula}, \code{data}, or \code{family}.
#'
#' @return No return value.
#'
#' @examples
#' \donttest{library(lmerTest)
#'
#' ## Example 1: data from lme4::sleepstudy
#' # (1) 'Subject' is a grouping/clustering variable
#' # (2) 'Days' is a level-1 predictor nested within 'Subject'
#' # (3) No level-2 predictors
#' m1=lmer(Reaction ~ (1 | Subject), data=sleepstudy)
#' m2=lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
#' m3=lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
#' HLM_summary(m1)
#' HLM_summary(m2)
#' HLM_summary(m3)
#'
#' ## Example 2: data from lmerTest::carrots
#' # (1) 'Consumer' is a grouping/clustering variable
#' # (2) 'Sweetness' is a level-1 predictor
#' # (3) 'Age' and 'Frequency' are level-2 predictors
#' hlm.1=lmer(Preference ~ Sweetness + Age + Frequency +
#'              (1 | Consumer), data=carrots)
#' hlm.2=lmer(Preference ~ Sweetness + Age + Frequency +
#'              (Sweetness | Consumer) + (1 | Product), data=carrots)
#' HLM_summary(hlm.1, level2.predictors="Consumer: Age + Frequency")
#' HLM_summary(hlm.2, level2.predictors="Consumer: Age + Frequency")
#' anova(hlm.1, hlm.2)
#' }
#' @references
#' Hox, J. J. (2010).
#' \emph{Multilevel analysis: Techniques and applications} (2nd ed.).
#' New York, NY: Routledge. \doi{10.4324/9780203852279}
#'
#' Nakagawa, S., & Schielzeth, H. (2013).
#' A general and simple method for obtaining \emph{R}^2 from generalized linear mixed-effects models.
#' \emph{Methods in Ecology and Evolution, 4,} 133-142. \doi{10.1111/j.2041-210x.2012.00261.x}
#'
#' Xu, R. (2003).
#' Measuring explained variation in linear mixed effects models.
#' \emph{Statistics in Medicine, 22,} 3527-3541. \doi{10.1002/sim.1572}
#'
#' @seealso \code{\link{GLM_summary}}, \code{\link{regress}}
#'
#' @export
HLM_summary=function(model=NULL,
                     level2.predictors=NULL,
                     vartypes=NULL,
                     test.rand=FALSE,  # time-consuming in big datasets
                     digits=3, nsmall=digits,
                     ...) {
  dots=list(...)
  if(c("formula", "data") %allin% names(dots)) {
    # re-modeling
    formula=dots$formula
    data=dots$data
    if("family" %notin% names(dots))
      eval(parse(text=Glue("model=lmerTest::lmer({formula_paste(formula)}, data=data)")))
    else
      eval(parse(text=Glue("model=lme4::glmer({formula_paste(formula)}, data=data, family={dots$family})")))
  } else {
    formula=model@call[["formula"]]
  }
  dv=formula[[2]]
  sumModel=summary(model, cor=F)
  ngrps=sumModel[["ngrps"]]

  ## lmer vs. glmer ##
  if(class(model)=="lmerModLmerTest") {
    if(is.null(vartypes)==TRUE & is.null(level2.predictors)==FALSE) {
      tryCatch({
        vartypes=HLM_vartypes(model, formula, level2.predictors)
      }, error=function(e) {
        Print("\n\n\n<<red Please re-specify 'level2.predictors' or even re-define your model.>>")
        stop(e)
      })
    } else if(is.null(vartypes)==FALSE) {
      names(vartypes)=dimnames(summary(model)[["coefficients"]])[[1]]
    }
    # vartypes=c("Intercept",
    #            "L1fixed",
    #            "L1random-GROUP-L1VAR",
    #            "L2-GROUP",
    #            "Cross-GROUP-L1VAR")

    ## Print: Model Information ##
    Print("
    <<underline MODEL INFO:>>
    Model type: Linear Mixed Model (LMM)
    = Hierarchical Linear Model (HLM)
    = Multilevel Linear Model (MLM)

    Formula: {formula_paste(formula)}
    ")
    if(is.null(vartypes)==FALSE) {
      Print("
      Level-2 predictors: '{level2.predictors}'
      ")
      vt=as.data.frame(vartypes)
      names(vt)="Variable Type"
      cat("\n")
      print(vt)
    }

    ## Print: Sample Sizes ##
    # .prt.grps(ngrps=ngrps(model), nobs=nobs(model))
    Print("\n\n
    Level-1 Observations: <<italic N>> = {nobs(model)}
    Level-2 Groups/Clusters: {paste(paste(names(ngrps), ngrps, sep=', '), collapse='; ')}
    ")

    ## Print: Model Fit (Omega^2, Pseudo-R^2, and Information Criteria ##
    # logLik=sumModel[["logLik"]]
    # AIC = -2LL + 2p  [p = number of parameters]
    # BIC = -2LL + p*ln(N)  [N = number of cases]
    # cat("-2 Log Likelihood (-2LL, deviance):  ", -2*logLik, "\n")
    # cat("Akaike's Information Criterion (AIC):", AIC(logLik), "\n")
    # cat("Schwarz's Bayesian Criterion (BIC):  ", BIC(logLik), "\n")
    Omg2=1-var(residuals(model))/var(model.response(model.frame(model)))
    R2.glmm=suppressWarnings( MuMIn::r.squaredGLMM(model) ) # R2.glmm[1,1]; R2.glmm[1,2]
    # R2.glmm=sjstats::r2(model) # R2.glmm$rsq.marginal; R2.glmm$rsq.conditional
    # NOTE: MuMIn::r.squaredGLMM(model) is more robust in some situations !!!
    Print("\n\n
    <<underline MODEL FIT:>>
    AIC = {AIC(model):.{nsmall}}
    BIC = {BIC(model):.{nsmall}}
    <<italic R>>_(m)\u00b2 = {R2.glmm[1,1]:.5}  <<blue (<<italic Marginal R>>\u00b2: fixed effects)>>
    <<italic R>>_(c)\u00b2 = {R2.glmm[1,2]:.5}  <<blue (<<italic Conditional R>>\u00b2: fixed + random effects)>>
    Omega\u00b2 = {Omg2:.5}  <<blue (= 1 - proportion of unexplained variance)>>
    ")

    ## Print: ANOVA Table ##
    # aov.hlm=car::Anova(model, type=3)
    aov.hlm=stats::anova(model)
    if(nrow(aov.hlm)>0) {
      cat("\n")
      Print("ANOVA table:")
      print_table(aov.hlm, nsmalls=2)
    }

    ## Print: Fixed Effects ##
    FE=as.data.frame(sumModel[["coefficients"]])
    names(FE)=c("b", "S.E.", "df", "t", "p") # abbreviate("approx", 5)
    FE=cbind(
      FE[c(1,2,4,3,5)],
      sig=sig.trans(FE$p),
      CI=paste0("[",
                formatF(FE[,1]+qt(0.025, FE[,3])*FE[,2], nsmall), ", ",
                formatF(FE[,1]+qt(0.975, FE[,3])*FE[,2], nsmall), "]"))
    FE.raw=FE
    FE$p=p.trans(FE$p)
    names(FE)[6:7]=c(" ", "[95% CI of b]")
    cat("\n")
    Print("<<underline FIXED EFFECTS:>>
           Outcome variable: {dv}")
    print_table(FE, nsmalls=c(nsmall, nsmall, 2, 1, 0, 0, 0))
    Print("<<blue 'df' is estimated by Satterthwaite approximation.>>")
    if(nrow(FE)>1) {
      FE.std=as.data.frame(MuMIn::std.coef(model, partial.sd=FALSE))[-1, 1:2]
      # FE.std=sjstats::std_beta(model)[2:3]
      # row.names(FE.std)=row.names(FE)[-1]
      names(FE.std)=c("Beta*", "S.E.*")
      if(is.null(vartypes)==FALSE)
        df=HLM_df(sumModel, vartypes)[-1]
      else
        df=FE$df[-1]
      t=FE.std[,1]/FE.std[,2]  # FE$t[-1]
      p=p.t(t, df)
      FE.std=cbind(
        FE.std,
        `t*`=t,
        `df*`=df,
        `p*`=p,
        `sig*`=sig.trans(p),
        CI=paste0("[",
                  formatF(FE.std[,1]+qt(0.025, df)*FE.std[,2], nsmall), ", ",
                  formatF(FE.std[,1]+qt(0.975, df)*FE.std[,2], nsmall), "]"))
      FE.std$`p*`=p.trans(FE.std$`p*`)
      names(FE.std)[6:7]=c(" ", "[95% CI of Beta]")
      cat("\n")
      Print("Standardized coefficients: {dv}")
      print_table(FE.std, nsmalls=c(nsmall, nsmall, 2, 0, 0, 0, 0))
      if(is.null(vartypes)==FALSE)
        Print("<<blue 'df*' is calculated based on variable types.>>")
    }

    ## Print: Random Effects & ICC ##
    cat("\n")
    Print("<<underline RANDOM EFFECTS:>>")
    # RE=sumModel[["varcor"]]
    # res=sumModel[["sigma"]]^2
    # print(RE, comp="Variance")
    RE=HLM_ICC(model, nsmall=nsmall)
    print_table(RE, row.names=FALSE)
  } else if(class(model)=="glmerMod") {
    summ=jtools::summ(model, digits=nsmall, re.variance="var")
    # summ(model, digits=nsmall, stars=T, exp=T, confint=T, re.variance="var")

    ## Print: Model Information ##
    Print("
    <<underline MODEL INFO:>>
    Model type: Generalized Linear Mixed Model (GLMM)
    = Hierarchical Linear Model (HLM)
    = Multilevel Linear Model (MLM)

    Formula: {formula_paste(formula)}
    Level-1 Observations: <<italic N>> = {nobs(model)}
    Level-2 Groups/Clusters: {paste(paste(names(ngrps), ngrps, sep=', '), collapse='; ')}
    ")

    ## Print: Model Fit (Omega^2, Pseudo-R^2, and Information Criteria) ##
    R2.glmm=suppressWarnings( MuMIn::r.squaredGLMM(model) ) # R2.glmm[1,1]; R2.glmm[1,2]
    # R2.glmm=sjstats::r2(model) # R2.glmm$rsq.marginal; R2.glmm$rsq.conditional
    # NOTE: MuMIn::r.squaredGLMM(model) is more robust in some situations !!!
    Print("\n\n
    <<underline MODEL FIT:>>
    AIC = {AIC(model):.{nsmall}}
    BIC = {BIC(model):.{nsmall}}
    <<italic R>>_(m)\u00b2 = {R2.glmm[1,1]:.5}  <<blue (<<italic Marginal R>>\u00b2: fixed effects)>>
    <<italic R>>_(c)\u00b2 = {R2.glmm[1,2]:.5}  <<blue (<<italic Conditional R>>\u00b2: fixed + random effects)>>
    ")

    ## Print: Fixed Effects ##
    FE=as.data.frame(sumModel[["coefficients"]])
    names(FE)=c("b", "S.E.", "z", "p")
    b=FE[,1]
    se=FE[,2]
    FE=cbind(
      FE,
      sig=sig.trans(FE$p),
      CI=paste0("[",
                formatF(b-1.96*se, nsmall), ", ",
                formatF(b+1.96*se, nsmall), "]"),
      OR=exp(b))
    FE.raw=FE
    FE$p=p.trans(FE$p)
    names(FE)[5:6]=c(" ", "[95% CI of b]")
    cat("\n")
    Print("<<underline FIXED EFFECTS:>>
           Outcome variable: {dv}")
    print_table(FE, nsmalls=c(nsmall, nsmall, 2, 0, 0, 0, nsmall))
    Print("<<blue OR = odds ratio.>>")

    ## Print: Random Effects & ICC ##
    cat("\n")
    Print("<<underline RANDOM EFFECTS:>>")
    RE=as.data.frame(summ$rcoeftable)
    ICC=as.data.frame(summ$gvars)
    names(RE)=c("Group", "Parameter", "Variance")
    names(ICC)=c("Group", "K", "ICC")
    RE$Group=as.character(RE$Group)
    RE$Parameter=as.character(RE$Parameter)
    RE$Variance=as.numeric(as.character(RE$Variance))
    ICC$Group=as.character(ICC$Group)
    ICC$K=as.numeric(as.character(ICC$K))
    ICC$ICC=as.numeric(as.character(ICC$ICC))
    RE=left_join(cbind(left_join(RE[1], ICC[1:2], by="Group"),
                       RE[2:3]),
                 ICC[c(1,3)], by="Group")
    RE$K=formatF(RE$K, 0)
    RE$Variance=formatF(RE$Variance, 5)
    RE$ICC=formatF(RE$ICC, 5)
    RE[RE$Parameter!="(Intercept)", c("Group", "K", "ICC")] = ""
    RE$Group=sprintf(glue("%-{max(max(nchar(RE$Group)), 7)}s"), RE$Group)
    names(RE)[1]=paste0("Cluster", rep_char(" ", max(max(nchar(RE$Group))-7, 0)))
    names(RE)[2]="K "
    names(RE)[3]=paste0("Parameter", rep_char(" ", max(nchar(RE$Parameter))-9))
    print_table(RE, row.names=F)
    Print("<<blue Residual variance is not reported for generalized linear mixed models,
           but it is assumed to be \u03c0\u00b2/3 (\u2248 {pi^2/3:.2}) in logistic models (binary data)
           and log(1/exp(intercept)+1) in poisson models (count data).>>")
  } else {
    Print("Please fit your model with '<<red lmerTest::lmer()>>' or '<<red lme4::glmer()>>'!")
    stop("Model type.", call.=TRUE)
  }

  ## Print: Likelihood-Ratio Test (LRT) for Random Effects ##
  # ANOVA-like table for random-effects: Single term deletions
  if(test.rand) {
    cat("\n")
    RE.test=lmerTest::rand(model)  # the same: ranova()
    print(RE.test)
  }
}


#' Tidy report of HLM indices: ICC(1), ICC(2), and rWG/rWG(J).
#'
#' @description
#' Compute ICC(1) (non-independence of data),
#' ICC(2) (reliability of group means),
#' and rWG/rWG(J) (within-group agreement for single-item/multi-item measures)
#' in multilevel analysis (HLM).
#'
#' @details
#' \describe{
#'   \item{\strong{ICC(1) (intra-class correlation, or non-independence of data)}}{
#'     ICC(1) = var.u0 / (var.u0 + var.e) = \eqn{\sigma_{u0}^2 / (\sigma_{u0}^2 + \sigma_{e}^2)})
#'
#'     ICC(1) is the ICC we often compute and report in multilevel analysis
#'     (usually in the Null Model, where only the random intercept of group is included).
#'     It can be interpreted as either \strong{"the proportion of variance explained by groups"} (i.e., \emph{heterogeneity} between groups)
#'     or \strong{"the expectation of correlation coefficient between any two observations within any group"} (i.e., \emph{homogeneity} within groups).
#'   }
#'   \item{\strong{ICC(2) (reliability of group means)}}{
#'     ICC(2) = mean(var.u0 / (var.u0 + var.e / n.k)) = \eqn{\Sigma[\sigma_{u0}^2 / (\sigma_{u0}^2 + \sigma_{e}^2 / n_k)] / K}
#'
#'     ICC(2) is a measure of \strong{"the representativeness of group-level aggregated means for within-group individual values"}
#'     or \strong{"the degree to which an individual score can be considered a reliable assessment of a group-level construct"}.
#'   }
#'   \item{\strong{rWG/rWG(J) (within-group agreement for single-item/multi-item measures)}}{
#'     rWG = \eqn{1 - \sigma^2 / \sigma_{EU}^2}
#'
#'     rWG(J) = \eqn{1 - (\sigma_{MJ}^2 / \sigma_{EU}^2) / [J * (1 - \sigma_{MJ}^2 / \sigma_{EU}^2) + \sigma_{MJ}^2 / \sigma_{EU}^2]}
#'
#'     rWG/rWG(J) is a measure of within-group agreement or consensus. Each group has an rWG/rWG(J).
#'   }
#'   \item{* Note for the above formulas}{
#'   \itemize{
#'     \item \eqn{\sigma_{u0}^2}: between-group variance (i.e., tau00)
#'     \item \eqn{\sigma_{e}^2}: within-group variance (i.e., residual variance)
#'     \item \eqn{n_k}: group size of the k-th group
#'     \item \eqn{K}: number of groups
#'     \item \eqn{\sigma^2}: actual group variance of the k-th group
#'     \item \eqn{\sigma_{MJ}^2}: mean value of actual group variance of the k-th group across all J items
#'     \item \eqn{\sigma_{EU}^2}: expected random variance (i.e., the variance of uniform distribution)
#'     \item \eqn{J}: number of items
#'   }
#'   }
#' }
#'
#' @param data Data frame.
#' @param group Grouping variable.
#' @param icc.var Key variable for analysis (usually the dependent variable).
#' @param rwg.vars Default is \code{icc.var}. It can be:
#' \itemize{
#'   \item A single variable (\emph{single-item} measure), then computing rWG.
#'   \item Multiple variables (\emph{multi-item} measure), then computing rWG(J), where J = the number of items.
#' }
#' @param rwg.levels As rWG/rWG(J) compares the actual group variance to the expected random variance (i.e., the variance of uniform distribution, \eqn{\sigma_EU^2}),
#' it is required to specify which type of uniform distribution is.
#' \itemize{
#'   \item For \emph{continuous} uniform distribution, \eqn{\sigma_EU^2 = (max - min)^2 / 12}.
#'   Then \code{rwg.levels} is not useful and will be set to \code{0} (the default).
#'   \item For \emph{discrete} uniform distribution, \eqn{\sigma_EU^2 = (A^2 - 1) / 12},
#'   where A is the number of response options (levels).
#'   Then \code{rwg.levels} should be provided (= A in the above formula).
#'   For example, if the measure is a 5-point Likert scale, you should set \code{rwg.levels=5}.
#' }
#' @param digits,nsmall Number of decimal places of output. Default is 3.
#'
#' @return Invisibly return a list of results.
#'
#' @references
#' Bliese, P. D. (2000). Within-group agreement, non-independence, and reliability: Implications for data aggregation and Analysis.
#' In K. J. Klein & S. W. Kozlowski (Eds.), \emph{Multilevel theory, research, and methods in organizations} (pp. 349-381). San Francisco, CA: Jossey-Bass, Inc.
#'
#' James, L.R., Demaree, R.G., & Wolf, G. (1984). Estimating within-group interrater reliability with and without response bias. \emph{Journal of Applied Psychology, 69}, 85-98.
#'
#' @seealso
#' \href{https://CRAN.R-project.org/package=multilevel}{R package "multilevel"}
#'
#' @examples
#' data=lme4::sleepstudy  # continuous variable
#' HLM_ICC_rWG(data, group="Subject", icc.var="Reaction")
#'
#' data=lmerTest::carrots  # 7-point scale
#' HLM_ICC_rWG(data, group="Consumer", icc.var="Preference",
#'             rwg.vars="Preference",
#'             rwg.levels=7)
#' HLM_ICC_rWG(data, group="Consumer", icc.var="Preference",
#'             rwg.vars=c("Sweetness", "Bitter", "Crisp"),
#'             rwg.levels=7)
#'
#' @export
HLM_ICC_rWG=function(data, group, icc.var,
                     rwg.vars=icc.var,
                     rwg.levels=0,
                     digits=3, nsmall=digits) {
  data=as.data.frame(data)

  ## ICC(1) and ICC(2)
  model=lmerTest::lmer(as.formula(Glue("{icc.var} ~ (1 | {group})")), data=data)
  d=model@frame
  d.split=split(d[[icc.var]], d[[group]])
  N=nrow(d)
  n_k=sapply(d.split, length)
  variance=as.data.frame(summary(model)[["varcor"]])[["vcov"]]
  var.u0=variance[1]
  var.e=variance[2]
  ICC1=var.u0/(var.u0+var.e)
  ICC2=mean(var.u0/(var.u0+var.e/n_k))

  ## rWG and rWG(J)
  if(rwg.levels==0)
    var.ran=(max(data[rwg.vars], na.rm=TRUE)-min(data[rwg.vars], na.rm=TRUE))^2/12
  else
    var.ran=(rwg.levels^2-1)/12
  ds=na.omit(data.frame(data[rwg.vars], data[group]))
  ds.split=split(ds[, 1:(ncol(ds)-1)], ds[[group]])
  if(length(rwg.vars)==1) {
    rwg=sapply(ds.split, function(dsi) {
      if(length(dsi)>1) {
        var.dsi=min(var(dsi), var.ran)
        out=1-(var.dsi/var.ran)
        out
      } else {
        out=NA
        out
      }
    })
    n.k=sapply(ds.split, length)
    rwg.name="rWG"
    rwg.type="single-item measures"
  } else {
    rwg=sapply(ds.split, function(dsi) {
      J=ncol(dsi)
      if(nrow(dsi)>1) {
        var.dsi=min(mean(apply(dsi, 2, var, na.rm=TRUE)), var.ran)
        out=(J*(1-(var.dsi/var.ran))) / (J*(1-(var.dsi/var.ran)) + var.dsi/var.ran)
        out
      } else {
        out=NA
        out
      }
    })
    n.k=sapply(ds.split, nrow)
    rwg.name="rWG(J)"
    rwg.type="multi-item measures"
  }
  rwg.out=data.frame(group=names(ds.split), size=n.k, rwg=rwg)
  names(rwg.out)[3]=rwg.name

  Print("
  \n
  <<cyan ------ Sample Size Information ------>>

  Level 1: <<italic N>> = {N} observations (\"{icc.var}\")
  Level 2: <<italic K>> = {length(n.k)} groups (\"{group}\")
  \n
  ")
  summ_n_k=as.matrix(summary(n_k)[c(-2, -5)])
  colnames(summ_n_k)="n (group sizes)"
  print(summ_n_k)

  Print("
  \n
  <<cyan ------ ICC(1), ICC(2), and {rwg.name} ------>>

  ICC variable: \"{icc.var}\"

  ICC(1) = {formatF(ICC1, nsmall)} <<blue (non-independence of data)>>
  ICC(2) = {formatF(ICC2, nsmall)} <<blue (reliability of group means)>>

  {rwg.name} variable{ifelse(length(rwg.vars)==1, '', 's')}: \"{paste(rwg.vars, collapse='\", \"')}\"

  {rwg.name} <<blue (within-group agreement for {rwg.type})>>
  ")
  summ_rwg=as.data.frame(t(as.matrix(summary(rwg))))
  rownames(summ_rwg)=rwg.name
  print_table(summ_rwg, nsmalls=nsmall)
  cat("\n")

  invisible(list(ICC1=ICC1, ICC2=ICC2, rwg=rwg.out))
}


