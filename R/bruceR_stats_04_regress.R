#### Regression ####


#' Paste a formula into a string.
#'
#' @param formula R formula.
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
#' @inheritParams formula_paste
#'
#' @examples
#' formula_expand(y ~ a*b*c)
#'
#' @importFrom stringr str_extract str_extract_all str_remove_all
#' @importFrom stats terms.formula as.formula
#' @export
formula_expand=function(formula) {
  inter_expand=function(inter) paste(attr(terms.formula(as.formula(paste("~", inter))), "term.labels"), collapse=" + ")
  f=as.character(formula)
  fx=f[3]
  fx.R=str_extract_all(fx, "\\([^\\)]+\\)", simplify=T)
  if(length(fx.R)>0) for(i in 1:length(fx.R)) if(grepl("\\*", fx.R[i])) fx.R[i]=paste0("(", inter_expand(str_remove_all(fx.R[i], "\\(|\\|.*")), " ", str_extract(fx.R[i], "\\|.*"))
  fx.F=str_remove_all(fx, "[\\+ ]*\\([^\\)]+\\)[\\+ ]*")
  fx.F=ifelse(fx.F=="", "", inter_expand(fx.F))
  fx=paste(fx.F, paste(fx.R, collapse=" + "), sep=ifelse(length(fx.R)==0 | fx.F=="", "", " + "))
  f=as.formula(paste(f[2], f[1], fx))
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
#' @param data Data frame.
#' @param vars Variable(s) to be centered.
#' @param std Standardized or not. Default is \code{FALSE}.
#' @param add_suffix The suffix of the centered variable(s).
#' Default is \code{""}.
#' You may set it to \code{"_c"}, \code{"_center"}, etc.
#'
#' @examples
#' d=data.table(a=1:5, b=6:10)
#'
#' d.c=grand_mean_center(d, "a")
#' d.c
#'
#' d.c=grand_mean_center(d, c("a", "b"), add_suffix="_center")
#' d.c
#'
#' @seealso \code{\link{group_mean_center}}
#'
#' @importFrom data.table is.data.table as.data.table
#' @export
grand_mean_center=function(data, vars, std=FALSE, add_suffix="") {
  data_c=as.data.frame(data)
  for(var in vars)
    data_c[paste0(var, add_suffix)]=scale(data_c[var], center=TRUE, scale=std)
  if(is.data.table(data)) data_c=as.data.table(data_c)
  return(data_c)
}

#' Group-mean centering
#'
#' Compute group-mean centered variables.
#' Usually used for HLM level-1 predictors.
#'
#' @inheritParams grand_mean_center
#' @param by Grouping variable.
#' @param add_group_mean The suffix of the variable name(s) of group means.
#' Default is \code{"_mean"} (see Examples).
#'
#' @examples
#' d=data.table(x=1:9, g=rep(1:3, each=3))
#'
#' d.c=group_mean_center(d, "x", by="g")
#' d.c
#'
#' d.c=group_mean_center(d, "x", by="g", add_suffix="_c")
#' d.c
#'
#' @seealso \code{\link{grand_mean_center}}
#'
#' @importFrom data.table is.data.table as.data.table
#' @export
group_mean_center=function(data, vars, by,
                           std=FALSE,
                           add_suffix="",
                           add_group_mean="_mean") {
  data_c=as.data.frame(data)
  grouplist=sort(unique(data_c[[by]]))
  for(var in vars) {
    for(group in grouplist) {
      data_c[which(data_c[by]==group), paste0(var, add_group_mean)]=mean(data_c[which(data_c[by]==group),][[var]], na.rm=TRUE)
      data_c[which(data_c[by]==group), paste0(var, add_suffix)]=scale(data_c[which(data_c[by]==group), var], center=TRUE, scale=std)
    }
  }
  if(is.data.table(data)) data_c=as.data.table(data_c)
  return(data_c)
}


#' Regression analysis.
#'
#' @inheritParams GLM_summary
#' @inheritParams HLM_summary
#' @param formula Model formula like \code{y ~ x1 + x2} (for \code{lm, glm}) or \code{y ~ x1 + x2 + (1 | group)} (for \code{lmer, glmer}).
#' @param data \code{data.frame} or \code{data.table}.
#' @param family [optional] The same as in \code{glm} and \code{glmer} (e.g., \code{family=binomial} will fit a logistic model).
#'
#' @examples
#' ## lm
#' regress(Temp ~ Month + Day + Wind + Solar.R, data=airquality, robust=T)
#'
#' ## glm
#' regress(case ~ age + parity + education + spontaneous + induced,
#'         data=infert, family=binomial, robust="HC1", cluster="stratum")
#'
#' ## lmer
#' regress(Reaction ~ Days + (Days | Subject), data=lme4::sleepstudy)
#' regress(Preference ~ Sweetness + Gender * Age + Frequency + (1 | Consumer), data=lmerTest::carrots,
#'         level2.predictors="Consumer: Gender + Age + Frequency")
#'
#' ## glmer
#' data.glmm=MASS::bacteria
#' data.glmm$week.2=(data.glmm$week>2) %>% as.numeric()
#' regress(y ~ trt + week.2 + (1 | ID), data=data.glmm, family=binomial)
#'
#' @export
regress=function(formula, data, family=NULL, nsmall=3,
                 robust=FALSE, cluster=NULL,
                 level2.predictors="", vartypes=NULL, t2r=FALSE, test.rand=FALSE) {
  call=sys.call()[-1]  # get function call (parameter list)
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
      model=GLM_summary(model=NULL,
                        robust, cluster,
                        nsmall,
                        formula=formula, data=data)
    } else {
      # model=glm(formula=formula, data=data, family=family)
      model=GLM_summary(model=NULL,
                        robust, cluster,
                        nsmall,
                        formula=formula, data=data, family=family.text)
    }
  } else {
    # lmer & glmer
    if(is.null(family)) {
      # model=lmerTest::lmer(formula=formula, data=data)
      model=HLM_summary(model=NULL, level2.predictors, vartypes,
                        t2r,
                        test.rand, nsmall,
                        formula=formula, data=data)
    } else {
      # model=lme4::glmer(formula=formula, data=data, family=family)
      model=HLM_summary(model=NULL,
                        test.rand=test.rand, nsmall=nsmall,
                        formula=formula, data=data, family=family.text)
    }
  }
  invisible(model)
}




#### Model Summary ####


#' Tidy report of regression models (into R console, Word, or HTML).
#'
#' This function is an extension (and combination) of
#' \code{texreg::\link[texreg]{screenreg}},
#' \code{texreg::\link[texreg]{htmlreg}},
#' \code{MuMIn::\link[MuMIn]{std.coef}}, and
#' \code{MuMIn::\link[MuMIn]{r.squaredGLMM}}.
#'
#' @param model_list A single model or a list of models. The models should be of the same type.
#' @param std_coef Standardized coefficients? Default is \code{FALSE}.
#' Only applicable to linear models and linear mixed models.
#' Not applicable to generalized linear (mixed) models.
#' @param digits Number of decimal places of output. Default is \code{3}.
#' @param nsmall The same as \code{digits}.
#' @param zero Display "0" before "."? Default is \code{TRUE}.
#' @param modify_se Set custom values for standard errors.
#' Useful if you need to replace raw SEs with robust SEs.
#' New SEs should be provided as a list of numeric vectors.
#' See usage in \code{texreg::\link[texreg]{screenreg}}.
#' @param bold The p-value threshold below which the coefficient shall be formatted in a bold font.
#' For example, \code{bold = 0.05} will cause all coefficients that are significant at the 95\% level to be formatted in bold.
#' @param file File name of the Word or HTML document.
#' The extension should be \code{.doc} or \code{.html}.
#' @param ... Other parameters passed to the
#' \code{\link[texreg]{screenreg}} or
#' \code{\link[texreg]{htmlreg}} function.
#'
#' @return
#' Invisibly return the plain text of output.
#'
#' @examples
#' ## Example 1: Linear Model
#' lm1=lm(Temp ~ Month + Day, data=airquality)
#' lm2=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
#' model_summary(lm1)
#' model_summary(lm2)
#' model_summary(list(lm1, lm2))
#' model_summary(list(lm1, lm2), std=T, digits=2)
#' model_summary(list(lm1, lm2), file="OLS Models.doc")
#' model_summary(list(lm1, lm2), file="OLS Models.html")
#'
#' ## Example 2: Generalized Linear Model
#' glm1=glm(case ~ age + parity,
#'          data=infert, family=binomial)
#' glm2=glm(case ~ age + parity + education + spontaneous + induced,
#'          data=infert, family=binomial)
#' model_summary(list(glm1, glm2))  # "std_coef" is not applicable to glm
#' model_summary(list(glm1, glm2), file="GLM Models.doc")
#'
#' ## Example 3: Linear Mixed Model
#' library(lmerTest)
#' hlm1=lmer(Reaction ~ (1 | Subject), data=sleepstudy)
#' hlm2=lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
#' hlm3=lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
#' model_summary(list(hlm1, hlm2, hlm3))
#' model_summary(list(hlm1, hlm2, hlm3), std=T)
#' model_summary(list(hlm1, hlm2, hlm3), file="HLM Models.doc")
#'
#' ## Example 4: Generalized Linear Mixed Model
#' library(lmerTest)
#' data.glmm=MASS::bacteria
#' glmm1=glmer(y ~ trt + week + (1 | ID), data=data.glmm, family=binomial)
#' glmm2=glmer(y ~ trt + week + hilo + (1 | ID), data=data.glmm, family=binomial)
#' model_summary(list(glmm1, glmm2))  # "std_coef" is not applicable to glmm
#' model_summary(list(glmm1, glmm2), file="GLMM Models.doc")
#'
#' ## Example 5: Multinomial Logistic Model
#' library(nnet)
#' d=airquality
#' d$Month=as.factor(d$Month)  # Factor levels: 5, 6, 7, 8, 9
#' mn1=multinom(Month ~ Temp, data=d, Hess=T)
#' mn2=multinom(Month ~ Temp + Wind + Ozone, data=d, Hess=T)
#' model_summary(mn1)
#' model_summary(mn2)
#' model_summary(mn2, file="Multinomial Logistic Model.doc")
#'
#' @export
model_summary=function(model_list,
                       std_coef=FALSE,
                       digits=nsmall,
                       nsmall=3,
                       zero=ifelse(std_coef, FALSE, TRUE),
                       modify_se=NULL,
                       bold=0,
                       file=NULL,
                       ...) {
  if(class(model_list)[1]!="list") model_list=list(model_list)
  if(is.null(file)) {
    sumreg=texreg::screenreg
  } else {
    sumreg=texreg::htmlreg
  }

  model_y=function(model) {
    if(any(class(model) %in% c("lmerMod", "lmerModLmerTest", "glmerMod")))
      model@call[["formula"]][[2]]
    else if(any(class(model) %in% c("lme")))
      model$call[["fixed"]][[2]]
    else
      model$call[["formula"]][[2]]
  }
  model_std_coef=function(model) { MuMIn::std.coef(model, partial.sd=FALSE)[,1] }
  model_std_s.e.=function(model) { MuMIn::std.coef(model, partial.sd=FALSE)[,2] }
  model_R2m=function(model) { MuMIn::r.squaredGLMM(model)[1, "R2m"] %>% as.numeric() %>% round(digits) }
  model_R2c=function(model) { MuMIn::r.squaredGLMM(model)[1, "R2c"] %>% as.numeric() %>% round(digits) }
  model_R2mcfadden=function(model) { performance::r2_mcfadden(model)[1] %>% as.numeric() %>% round(digits) }
  model_R2nagelkerke=function(model) { performance::r2_nagelkerke(model) %>% as.numeric() %>% round(digits) }

  new.model.names=NULL
  try({
    new.model.names=lapply(model_list, model_y) %>% as.character()
    if(length(model_list)>1)
      new.model.names=paste(paste0("(", 1:length(model_list), ")"), new.model.names)
    if(any(unlist(lapply(model_list, class)) %in% "nnet")) {
      multinom.y=as.character(lapply(model_list, model_y))[1]
      multinom.ref=model_list[[1]][["lab"]][1]
      new.model.names=NULL
    }
  },
  silent=T)

  if(std_coef) {
    new.coef=lapply(model_list, model_std_coef)
    new.s.e.=lapply(model_list, model_std_s.e.)
    omit="Intercept"
  } else {
    new.coef=0
    new.s.e.=0
    omit=NULL
  }

  if(!is.null(modify_se)) new.s.e.=modify_se

  if(any(unlist(lapply(model_list, class)) %in% "glm")) {
    if(is.null(file))
      names.R2=c("McFadden's R^2",
                 "Nagelkerke's R^2")
    else
      names.R2=c("McFadden\u2019s R<sup>2</sup>",
                 "Nagelkerke\u2019s R<sup>2</sup>")
    new.R2=list(
      R2mcfadden=lapply(model_list, model_R2mcfadden) %>% as.numeric(),
      R2nagelkerke=lapply(model_list, model_R2nagelkerke) %>% as.numeric()
    )
    names(new.R2)=names.R2
  } else if(any(unlist(lapply(model_list, class)) %in% c("lme", "lmerMod", "lmerModLmerTest", "glmerMod"))) {
    if(is.null(file))
      names.R2=c("Marginal R^2",
                 "Conditional R^2")
    else
      names.R2=c("Marginal R<sup>2</sup>",
                 "Conditional R<sup>2</sup>")
    suppressWarnings({
      new.R2=list(
        R2m=lapply(model_list, model_R2m) %>% as.numeric(),
        R2c=lapply(model_list, model_R2c) %>% as.numeric()
      )
    })
    names(new.R2)=names.R2
  } else {
    new.R2=NULL
  }

  suppressWarnings({
    output=sumreg(
      model_list, file=NULL,
      leading.zero=zero, digits=digits, bold=bold,
      custom.model.names=new.model.names,
      override.coef=new.coef,
      override.se=new.s.e.,
      omit.coef=omit,
      custom.gof.rows=new.R2,
      margin=1,
      inline.css=FALSE,
      doctype=TRUE,
      html.tag=TRUE,
      head.tag=TRUE,
      body.tag=TRUE,
      caption.above=TRUE,
      caption=NULL,
      custom.note=ifelse(is.null(file), "* p < .05, ** p < .01, *** p < .001", ""),
      include.loglik=FALSE,
      include.deviance=FALSE,
      beside=TRUE,  # for 'multinom' models
      ...)
    if(is.null(file)) {
      print(output)
    } else {
      output=str_replace(output, "utf-8", "gbk") %>%
        str_replace_all("<td>-", "<td>\u2013")
      if(grepl(".doc$", file)) {
        output=str_replace(
          output,
          "<style>",
          "<style>\nbody {font-size: 10.5pt; font-family: Times New Roman}") %>%
          # str_replace("<tfoot>(.|\\n)*</tfoot>", "") %>%
          str_replace(
            "<body>",
            paste0(
              "<body>\n<b>Table X. Regression Models",
              ifelse(any(unlist(lapply(model_list, class)) %in% "nnet"),
                     paste0(" (Reference Group: ", multinom.y, " = \u2018", multinom.ref, "\u2019)"),
                     ""),
              ".</b>")) %>%
          str_replace(
            "</body>",
            paste0("<i>Note</i>. ",
                   ifelse(std_coef, "Standardized ", "Unstandardized "),
                   "regression coefficients are displayed, with standard errors in parentheses.<br/>",
                   "* <i>p</i> < .05. ** <i>p</i> < .01. *** <i>p</i> < .001.</p>",
                   "</body>"))
      }
      sink(file)
      cat(output)
      sink()
      Print("<<green \u2714>> The table was written to <<blue '{paste0(getwd(), '/', file)}'>>")
      cat("\n")
    }
  })

  if(is.null(file) & length(model_list)==1) {
    cat("\n")
    print(performance::check_collinearity(model_list[[1]]))
  }

  invisible(output)
}




#### GLM Functions ####


## Print ANOVA Table of GLM
GLM_anova=function(model, add.total=TRUE) {
  Print("ANOVA table:")
  aov.lm=as.data.frame(car::Anova(model, type=3))
  total.ss=sum(aov.lm[-1, "Sum Sq"])
  total.df=sum(aov.lm[-1, "Df"])
  resid.ss=aov.lm[nrow(aov.lm), "Sum Sq"]
  resid.df=aov.lm[nrow(aov.lm), "Df"]
  model.ss=total.ss-resid.ss
  model.df=total.df-resid.df
  model.F=(model.ss/model.df)/(resid.ss/resid.df)
  aov.lm=rbind(`[Model]`=c(model.ss, model.df, model.F, p.f(model.F, model.df, resid.df)),
               aov.lm[-1,])
  aov.lm$`Mean Sq`=aov.lm$`Sum Sq`/aov.lm$Df
  aov.lm$sig=sig.trans(aov.lm$`Pr(>F)`)
  aov.lm$eta2=mapply(function(f, df1, df2) {f*df1/(f*df1+df2)},
                     aov.lm$`F value`, aov.lm$Df, aov.lm$Df[nrow(aov.lm)])
  aov.lm$Df=formatF(aov.lm$Df, 0)
  aov.lm$`F value`=formatF(aov.lm$`F value`, 2)
  aov.lm$`Pr(>F)`=p.trans(aov.lm$`Pr(>F)`)
  aov.lm$eta2=formatF(aov.lm$eta2, 3)
  aov.lm=aov.lm[,c(1,2,5,3,4,6,7)]
  if(add.total) {
    aov.lm=rbind(aov.lm,
                 Total=c(total.ss, total.df, total.ss/total.df,
                         NA, NA, NA, NA))
    aov.lm[(nrow(aov.lm)-1):nrow(aov.lm),
           c("F value", "Pr(>F)", "sig", "eta2")]=""
  } else {
    aov.lm[nrow(aov.lm),
           c("F value", "Pr(>F)", "sig", "eta2")]=""
  }
  names(aov.lm)[5:7]=c("p", " ", "\u03b7\u00b2p")
  print_table(aov.lm, nsmalls=2)
}


#' Advanced report of GLM (\code{lm} and \code{glm} models).
#'
#' @param model A model fitted by \code{lm} or \code{glm} function.
#' @param robust \strong{[only for \code{lm} and \code{glm}]} \code{FALSE} (default), \code{TRUE}, or an option from \code{"HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5"}.
#' It will add a table with heteroskedasticity-robust standard errors (aka. Huber-White standard errors).
#' For details, see \code{\link[sandwich]{vcovHC}} and \code{\link[jtools]{summ.lm}}.
#'
#' *** \strong{\code{"HC1"}} is the default of Stata, whereas \strong{\code{"HC3"}} is the default suggested by the \code{sandwich} package.
#' Here we use \strong{\code{"HC1"}} as the default option.
#' @param cluster \strong{[only for \code{lm} and \code{glm}]} Cluster-robust standard errors are computed if cluster is set to the name of the input data's cluster variable or is a vector of clusters.
#' If you specify \code{cluster}, you may also specify the type of \code{robust}. If you do not specify \code{robust}, \strong{\code{"HC1"}} will be set as the default option.
#' @param nsmall Number of decimal places of output. Default is 3.
#' @param ... Other parameters. You may re-define \code{formula}, \code{data}, or \code{family}.
#'
#' @examples
#' ## Example 1: OLS regression
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
#'
#' @seealso \code{\link{HLM_summary}}, \code{\link{regress}}
#'
#' @importFrom stats qt
#' @importFrom jtools summ
#' @importFrom MuMIn std.coef
#' @importFrom performance r2_nagelkerke
#' @export
GLM_summary=function(model, robust=FALSE, cluster=NULL,
                     nsmall=3, ...) {
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
    cat("\n")
    GLM_anova(model)

    ## Print: Fixed Effects ##
    FE=as.data.frame(sumModel[["coefficients"]])
    names(FE)=c("b", "S.E.", "t", "p")
    df=model[["df.residual"]]
    FE=cbind(FE,
             sig=sig.trans(FE$p),
             LLCI=FE[,1]+qt(0.025, df)*FE[,2],
             ULCI=FE[,1]+qt(0.975, df)*FE[,2])
    if(nrow(FE)>2) {
      FE.vif=jtools::summ(model, vif=T)
      FE=cbind(FE, VIF=FE.vif$coeftable[,"VIF"])
    } else if(nrow(FE)==2) {
      FE=cbind(FE, VIF=c(NA, 1))
    } else {
      FE=cbind(FE, VIF=NA)
    }
    FE$VIF=formatF(FE$VIF, nsmall=nsmall)
    FE$VIF[1]=""
    FE$p=p.trans(FE$p)
    names(FE)[5:7]=c(" ", "[95% ", "  CI]")
    cat("\n")
    Print("<<underline FIXED EFFECTS:>>
           Outcome variable: {dv} (<<italic N>> = {N})")
    print_table(FE, nsmalls=c(nsmall, nsmall, 2, 0, 0, nsmall, nsmall, nsmall))

    ## Print: Robust SE ##
    if(robust!=FALSE | !is.null(cluster)) {
      if(robust==TRUE) robust="HC1"
      if(!is.null(cluster) & !is.character(robust)) robust="HC1"
      summ.rob=jtools::summ(model, robust=robust, cluster=cluster)
      FE.rob=as.data.frame(summ.rob$coeftable)
      names(FE.rob)=c("b", "S.E.*", "t*", "p*")
      FE.rob=cbind(FE.rob,
                   sig=sig.trans(FE.rob$`p*`),
                   LLCI=FE.rob[,1]+qt(0.025, df)*FE.rob[,2],
                   ULCI=FE.rob[,1]+qt(0.975, df)*FE.rob[,2])
      FE.rob$`p*`=p.trans(FE.rob$`p*`)
      names(FE.rob)[5:7]=c(" ", "[95% ", "  CI]")
      cat("\n")
      Print("{ifelse(is.null(cluster), 'Heteroskedasticity', 'Cluster')}-robust standard errors:")
      print_table(FE.rob, nsmalls=c(nsmall, nsmall, 2, 0, 0, nsmall, nsmall, nsmall))
      Print("<<blue Robust S.E.: type = {robust}{ifelse(is.null(cluster), '', glue('; clustering variable = {paste(cluster, collapse=', ')}'))}.>>")
    }

    ## Print: Standardized Coefficients ##
    if(nrow(FE)>1) {
      FE.std=as.data.frame(MuMIn::std.coef(model, partial.sd=FALSE))[-1, 1:2]
      # FE.std=sjstats::std_beta(model)[2:3]
      # row.names(FE.std)=row.names(FE)[-1]
      names(FE.std)=c("Beta*", "S.E.*")
      FE.rp=jtools::summ(model, part.corr=T)
      t=FE.std[,1]/FE.std[,2]  # FE$t[-1]
      p=p.t(t, df)
      FE.std=cbind(FE.std,
                   sig=sig.trans(p),
                   LLCI.std=FE.std[,1]+qt(0.025, df)*FE.std[,2],
                   ULCI.std=FE.std[,1]+qt(0.975, df)*FE.std[,2],
                   r.partial=FE.rp$coeftable[-1, "partial.r"],
                   r.part=FE.rp$coeftable[-1, "part.r"])
      cat("\n")
      Print("Standardized coefficients: {dv} (<<italic N>> = {N})")
      names(FE.std)[3:5]=c(" ", "[95% ", "  CI]")
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
    Chi2=sum(aov.glm$Deviance, na.rm=T)
    Df=sum(aov.glm$Df, na.rm=T)
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
    FE=cbind(FE,
             sig=sig.trans(FE$p),
             LLCI=b-1.96*se,
             ULCI=b+1.96*se)
    if(nrow(FE)>2) {
      FE.vif=jtools::summ(model, vif=T)
      FE=cbind(FE, VIF=FE.vif$coeftable[,"VIF"])
    } else if(nrow(FE)==2) {
      FE=cbind(FE, VIF=c(NA, 1))
    } else {
      FE=cbind(FE, VIF=NA)
    }
    FE$VIF=formatF(FE$VIF, nsmall=nsmall)
    FE$VIF[1]=""
    FE$p=p.trans(FE$p)
    sig=FE$sig
    names(FE)[5:7]=c(" ", "[95% ", "  CI]")
    cat("\n")
    Print("<<underline FIXED EFFECTS:>>
           Outcome variable: {dv} (type = {model$family$family}) (<<italic N>> = {N})")
    print_table(FE, nsmalls=c(nsmall, nsmall, 2, 0, 0, nsmall, nsmall, nsmall))

    ## Print: Robust SE ##
    if(robust!=FALSE | is.null(cluster)==FALSE) {
      if(robust==TRUE | (is.null(cluster)==FALSE & is.character(robust)==FALSE))
        robust="HC1" # default method in Stata
      summ.rob=jtools::summ(model, robust=robust, cluster=cluster)
      FE.rob=as.data.frame(summ.rob$coeftable)
      names(FE.rob)=c("b", "S.E.*", "z*", "p*")
      b=FE[,1]
      se.rob=FE.rob[,2]
      FE.rob=cbind(FE.rob,
                   sig=sig.trans(FE.rob$`p*`),
                   LLCI=b-1.96*se.rob,
                   ULCI=b+1.96*se.rob)
      FE.rob$`p*`=p.trans(FE.rob$`p*`)
      names(FE.rob)[5:7]=c(" ", "[95% ", "  CI]")
      cat("\n")
      Print("{ifelse(is.null(cluster), 'Heteroskedasticity', 'Cluster')}-robust standard errors:")
      print_table(FE.rob, nsmalls=c(nsmall, nsmall, 2, 0, 0, nsmall, nsmall, nsmall))
      Print("<<blue Robust S.E.: type = {robust}{ifelse(is.null(cluster), '', glue('; clustering variable = {paste(cluster, collapse=', ')}'))}.>>")
    }

    ## Print: Odds Ratio ##
    FE.OR=data.frame(OR=exp(b),
                     sig=sig,
                     OR.LLCI=exp(b-1.96*se),
                     OR.ULCI=exp(b+1.96*se),
                     xxx="  \u2502 ",
                     OR.rev=exp(-b))
    row.names(FE.OR)=row.names(FE)
    cat("\n")
    Print("Odds ratio (= exp(b)): {dv} (<<italic N>> = {N})")
    names(FE.OR)=c("Odds Ratio", " ", "[95% ", "  CI]", "  \u2502 ", "OR^(-1)")
    print_table(FE.OR, nsmalls=nsmall)
    # Print("<<blue OR = odds ratio. 95% CI of OR is reported.>>")
  } else {
    stop("'GLM_summary' can only deal with 'lm' or 'glm' models.")
  }

  invisible(list(model.summary=sumModel, FE=FE))
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
  L1.rand.comp=gsub(" ", "", str_extract_all(fx, "(?<=\\()[^\\)]+(?=\\))", simplify=T)) %>% str_split("\\|")
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
  L2.comp=level2.predictors %>% gsub(" ", "", .) %>%
    str_split(";", simplify=T) %>% str_split(":")
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
  data=as.data.table(model@frame)
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
  n.mean=c()
  for(group in group.id) n.mean=c(n.mean, (N-sum(data[,.(n=.N^2), by=group]$n)/N) / (nlevels(data[[group]])-1) )  # quite similar to "N / K"
  n.mean=c(n.mean, NA)

  ## Test for variance (Wen Fuxing, 2009, pp.85-97; Snijders & Bosker, 2012, pp.190-191) ##
  var=ICC$Variance
  # var.se=var.resid * (2/N) * sqrt(1/(n.mean-1) + 2*var/var.resid + n.mean*var^2/var.resid^2)  # error !!!
  var.se=sqrt(2*(var+var.resid/n.mean)^2/(K-1) + 2*var.resid^2/((N-K)*n.mean^2))
  var.wald.z=var/var.se
  var.p=p.z(var.wald.z)

  ## Compute and test for ICC (Snijders & Bosker, 2012, pp.20-21) ##
  icc=var/var.total
  # icc.se=(1-icc) * (1+(n.mean-1)*icc) * sqrt(2/(n.mean*(n.mean-1)*(N-1)))  # N or K ?!
  # icc.wald.z=icc/icc.se

  ## Combine results ##
  ICC$K=formatF(ICC$K, nsmall=0)
  ICC$Variance=formatF(ICC$Variance, nsmall=5)
  ICC$S.E.=formatF(var.se, nsmall=nsmall)
  ICC$Wald.Z=formatF(var.wald.z, nsmall=2)
  ICC$p=p.trans(var.p)
  ICC$sig=sig.trans(var.p)
  ICC$ICC=formatF(icc, nsmall=5)
  ICC[ICC$Group=="Residual", c("K", "Parameter", "S.E.", "Wald.Z", "p", "ICC")] = ""
  ICC[ICC$Parameter!="(Intercept)" & ICC$Group!="Residual", c("Group", "K", "ICC")] = ""
  names(ICC)[1]=paste0("Cluster", rep_char(" ", max(nchar(ICC$Group))-7))
  names(ICC)[2]="K "
  names(ICC)[3]=paste0("Parameter", rep_char(" ", max(nchar(ICC$Parameter))-9))
  names(ICC)[6]="Wald Z"
  names(ICC)[8]=" "

  return(ICC)
}


## Compute CI for random effects
print_variance_ci=function(model) {
  suppressMessages({
    varCI=confint(model, parm=c(".sig01", ".sig02", ".sig03",
                                ".sig04", ".sig05", ".sig06",
                                ".sig07", ".sig08", ".sig09",
                                ".sigma"))^2
  })
  varCI=as.data.frame(varCI)
  vc=row.names(varCI) %>% gsub("\\.", "", .) %>%
    gsub("sigma", "sigerror", .) %>%
    gsub("sig", "sigma_", .) %>%
    paste0("^2")
  varCI=cbind(`Variance Component`=vc, varCI)
  names(varCI)[2:3]=c("[95% ", "  CI]")
  cat("\n")
  print_table(varCI, row.names=FALSE, nsmalls=5)
  invisible(varCI)
}


#' Advanced report of HLM (\code{lmer} and \code{glmer} models).
#'
#' @description
#' Nice report of \strong{Hierarchical Linear Model (HLM)}, also known as \strong{Multilevel Linear Model (MLM)} or \strong{Linear Mixed Model (LMM)}.
#' HLM, MLM, or LMM (the same) refers to a model with nested data (e.g., Level-1: participants, Level-2: city; or Level-1: repeated-measures within a participant, Level-2: participants).
#'
#' @details
#' Hierarchical Linear Model (HLM), aka. Multilevel Linear Model (MLM) or Linear Mixed Model (LMM), is more complex than General Linear Model (GLM; i.e., OLS regression).
#' Predictor variables at different levels may have five types:
#' \describe{
#'   \item{1. Intercept}{The overall intercept (\eqn{\gamma_00})}
#'   \item{2. L1fixed}{Level-1 predicter with \strong{fixed} slope}
#'   \item{3. L1random-GROUP-L1VAR}{Level-1 predicter with \strong{random} slopes nested with a grouping/clustering variable}
#'   \item{4. L2-GROUP}{Level-2 predicter (e.g., GDP per capita at city level), always with \strong{fixed} slope unless there is also a level-3 structure.
#'
#'   *** NOTE: the current version of \code{'HLM_summary'} function does not consider three-levels design, so you may only use this function in two-levels HLM or cross-classified HLM.}
#'   \item{5. Cross-GROUP-L1VAR}{Cross-level interaction consisting of level-1 and level-2 predictors}
#' }
#' The degrees of freedom (\emph{df}) of predictor variables in HLM vary across different levels and also depend on the variable types.
#' However, different softwares use different estimation methods and thus provide somewhat different \emph{df}s, which may be confusing.
#' Whereas the \code{lmerTest} package in R provides \emph{df}s that are estimated by the Satterthwaite's (1946) approximation (i.e., a data-driven approach without defining variable types),
#' the \code{HLM} software provides \emph{df}s that totally depend on the variable types (i.e., a theory-driven approach).
#'
#' @param model A model fitted by \code{lmer} or \code{glmer} function using the \code{lmerTest} package.
#' @param level2.predictors \strong{[only for \code{lmer}]} [optional] Default is \code{NULL}.
#' If you have predictors at level 2, besides putting them into the formula in the \code{lmer} function as usual,
#' you may \strong{also} define here the level-2 grouping/clustering variables and corresponding level-2 predictor variables.
#'
#' *** Example: \code{level2.predictors="School: W1 + W2; House: 1"},
#' where \code{School} and \code{House} are two grouping variables,
#' \code{W1 & W2} are school-level predictors,
#' and there is no house-level predictor.
#'
#' *** If there is no level-2 predictor in the formula of \code{lmer}, just leave this parameter blank.
#' @param vartypes \strong{[only for \code{lmer}]} Manually setting variable types. Needless in most situations.
#' @param t2r \strong{[only for \code{lmer}]} \code{TRUE} or \code{FALSE} (default).
#' Add a column of another kind of multilevel effect sizes: standardized partial effect size \emph{r} by \emph{t}-to-\emph{r} transformation.
#'
#' *** See an example in Wei et al.'s paper (2017, \emph{\href{https://doi.org/10.1038/s41562-017-0240-0}{Nature Human Behaviour}}).
#' However, I personally did not recommend reporting this \emph{r}, because it could be misleading and sometimes ridiculous (e.g., an actually small effect may surprisingly have an \emph{r} > 0.6,
#' and the interpretation of this \emph{r} is not same as the Pearson's \emph{r} we are familiar with).
## @param variance.ci \strong{[only for \code{lmer} and \code{glmer}]} \code{TRUE} or \code{FALSE} (default).
## Print the confidence intervals (CI) for variance components.
#' @param test.rand \strong{[only for \code{lmer}]} \code{TRUE} or \code{FALSE} (default).
#' Test random effects (i.e., variance components) by using the likelihood-ratio test (LRT), which is asymptotically chi-square distributed. For large datasets, it is much time-consuming.
## *** Note that its results would be different from those in the default output of \code{HLM_summary()} (see "Wald \emph{Z} test" in the output),
## because they differ in the principle of statistics. The LRT is based on model comparison and the reduction of AIC, whereas the Wald \emph{Z} test is estimated by approximation.
#' The Wald \emph{Z} test can also be seen in the output of SPSS (the \code{MIXED} syntax).
#' @param nsmall Number of decimal places of output. Default is 3.
#' But for some statistics (e.g., \emph{R}^2, ICC), to provide more precise information, we fix the decimal places to 5.
#' @param ... Other optional parameters. You may re-define \code{formula}, \code{data}, or \code{family}.
#'
#' @examples
#' library(lmerTest)
#'
#' ## Example 1: data from lme4::sleepstudy
#' # 1) 'Subject' is a grouping/clustering variable
#' # 2) 'Days' is a level-1 predictor nested within 'Subject'
#' # 3) No level-2 predictors
#' m1=lmer(Reaction ~ (1 | Subject), data=sleepstudy)
#' m2=lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
#' m3=lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
#' HLM_summary(m1)
#' HLM_summary(m2)
#' HLM_summary(m3)
#'
#' ## Example 2: data from lmerTest::carrots
#' # 1) 'Consumer' is a grouping/clustering variable
#' # 2) 'Sweetness' is a level-1 predictor
#' # 3) 'Age' and 'Frequency' are level-2 predictors
#' hlm.1=lmer(Preference ~ Sweetness + Age + Frequency +
#'              (1 | Consumer), data=carrots)
#' hlm.2=lmer(Preference ~ Sweetness + Age + Frequency +
#'              (Sweetness | Consumer) + (1 | Product), data=carrots)
#' HLM_summary(hlm.1, level2.predictors="Consumer: Age + Frequency")
#' HLM_summary(hlm.2, level2.predictors="Consumer: Age + Frequency")
#' # anova(hlm.1, hlm.2)
#'
#' ## Example 3: data from MASS::bacteria
#' # GLMM with binomial outcome (multilevel logistic regression)
#' data.glmm=MASS::bacteria
#' data.glmm$week.2=(data.glmm$week>2) %>% as.numeric()
#' glmm=glmer(y ~ trt + week.2 + (1 | ID), data=data.glmm, family=binomial)
#' HLM_summary(glmm)
#'
#' @references
#' Hox, J. J. (2010). \emph{Multilevel analysis: Techniques and applications} (2nd ed.). New York, NY: Routledge. \url{https://doi.org/10.4324/9780203852279}
#'
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining \emph{R}^2 from generalized linear mixed-effects models. \emph{Methods in Ecology and Evolution, 4,} 133-142. \url{https://doi.org/10.1111/j.2041-210x.2012.00261.x}
#'
#' Xu, R. (2003). Measuring explained variation in linear mixed effects models. \emph{Statistics in Medicine, 22,} 3527-3541. \url{https://doi.org/10.1002/sim.1572}
#'
#' @seealso \code{\link{GLM_summary}}, \code{\link{regress}}
#'
#' @importFrom car Anova
#' @importFrom stats qt var model.response model.frame
#' @importFrom MuMIn std.coef r.squaredGLMM
#' @export
HLM_summary=function(model=NULL,
                     level2.predictors=NULL,
                     vartypes=NULL,
                     t2r=FALSE,
                     # variance.ci=FALSE,  # time-consuming in big datasets
                     test.rand=FALSE,  # time-consuming in big datasets
                     nsmall=3,
                     ...) {
  dots=list(...)
  ngrps=lme4::ngrps
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
    Level-2 Groups/Clusters: {paste(paste(names(ngrps(model)), ngrps(model), sep=', '), collapse='; ')}
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
    cat("\n")
    Print("ANOVA table:")
    aov.hlm=car::Anova(model, type=3)
    print_table(aov.hlm, nsmalls=c(2,0))

    ## Print: Fixed Effects ##
    FE=as.data.frame(sumModel[["coefficients"]])
    names(FE)=c("Gamma", "S.E.", "df", "t", "p") # abbreviate("approx", 5)
    FE=cbind(FE[c(1,2,4,3,5)], # 1:5 --> Estimate, S.E., df, t, p
             sig=sig.trans(FE$p),
             LLCI=FE[,1]+qt(0.025, FE[,3])*FE[,2],
             ULCI=FE[,1]+qt(0.975, FE[,3])*FE[,2])
    FE.raw=FE
    FE$p=p.trans(FE$p)
    names(FE)[6:8]=c(" ", "[95% ", "  CI]")
    cat("\n")
    Print("<<underline FIXED EFFECTS:>>
           Outcome variable: {dv}")
    print_table(FE, nsmalls=c(nsmall, nsmall, 2, 1, 0, 0, nsmall, nsmall))
    Print("<<blue 'df' is estimated by Satterthwaite approximation.>>")
    if(nrow(FE)>1) {
      FE.std=as.data.frame(MuMIn::std.coef(model, partial.sd=FALSE))[-1, 1:2]
      # FE.std=sjstats::std_beta(model)[2:3]
      # row.names(FE.std)=row.names(FE)[-1]
      names(FE.std)=c("Gamma*", "S.E.*")
      if(is.null(vartypes)==FALSE)
        df=HLM_df(sumModel, vartypes)[-1]
      else
        df=FE$df[-1]
      t=FE.std[,1]/FE.std[,2]  # FE$t[-1]
      p=p.t(t, df)
      FE.std=cbind(FE.std,
                   `t*`=t,
                   `df*`=df,
                   `p*`=p,
                   `sig*`=sig.trans(p),
                   LLCI.std=FE.std[,1]+qt(0.025, df)*FE.std[,2],
                   ULCI.std=FE.std[,1]+qt(0.975, df)*FE.std[,2],
                   r.HLM=sign(t)*sqrt(t^2/(t^2+df)))
      FE.std$`p*`=p.trans(FE.std$`p*`)
      names(FE.std)[6:8]=c(" ", "[95% ", "  CI]")
      cat("\n")
      Print("Standardized coefficients: {dv}")
      if(t2r)
        print_table(FE.std, nsmalls=c(nsmall, nsmall, 2, 0, 0, 0, nsmall, nsmall, nsmall))
      else
        print_table(FE.std[1:8], nsmalls=c(nsmall, nsmall, 2, 0, 0, 0, nsmall, nsmall))
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
    # print_table(RE, row.names=F)
    print_table(RE[c(1:4, 9)], row.names=F)
    # if(variance.ci) print_variance_ci(model)
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
    Level-2 Groups/Clusters: {paste(paste(names(ngrps(model)), ngrps(model), sep=', '), collapse='; ')}
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
    names(FE)=c("Gamma", "S.E.", "z", "p")
    b=FE[,1]
    se=FE[,2]
    FE=cbind(FE,
             sig=sig.trans(FE$p),
             OR=exp(b),
             OR.LLCI=exp(b-1.96*se),
             OR.ULCI=exp(b+1.96*se))
    FE.raw=FE
    FE$p=p.trans(FE$p)
    names(FE)[5]=" "
    names(FE)[7:8]=c("[95% ", "  CI]")
    cat("\n")
    Print("<<underline FIXED EFFECTS:>>
           Outcome variable: {dv}")
    print_table(FE, nsmalls=c(nsmall, nsmall, 2, 0, 0, nsmall, nsmall, nsmall))
    Print("<<blue OR = odds ratio. 95% CI of OR is reported.>>")

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
    RE=left_join(RE[1], ICC[1:2], by="Group") %>%
      cbind(RE[2:3]) %>% left_join(ICC[c(1,3)], by="Group")
    RE$K=formatF(RE$K, 0)
    RE$Variance=formatF(RE$Variance, 5)
    RE$ICC=formatF(RE$ICC, 5)
    RE[RE$Parameter!="(Intercept)", c("Group", "K", "ICC")] = ""
    RE$Group=sprintf(glue("%-{max(max(nchar(RE$Group)), 7)}s"), RE$Group)
    names(RE)[1]=paste0("Cluster", rep_char(" ", max(max(nchar(RE$Group))-7, 0)))
    names(RE)[2]="K "
    names(RE)[3]=paste0("Parameter", rep_char(" ", max(nchar(RE$Parameter))-9))
    print_table(RE, row.names=F)
    # if(variance.ci) print_variance_ci(model)
    Print("<<blue Residual variance is not reported for generalized linear mixed models,
           but it is assumed to be \u03c0\u00b2/3 (\u2248 {pi^2/3:.2}) in logistic models (binary data)
           and log(1/exp(intercept)+1) in poisson models (count data).>>")
  } else {
    Print("Please fit your model with '<<red lmerTest::lmer()>>' or '<<red lme4::glmer()>>'!")
    stop("Model type.")
  }

  ## Print: Likelihood-Ratio Test (LRT) for Random Effects ##
  # ANOVA-like table for random-effects: Single term deletions
  if(test.rand) {
    cat("\n")
    RE.test=lmerTest::rand(model) # the same: ranova()
    print(RE.test)
  }

  invisible(list(model.summary=sumModel, FE=FE.raw, RE=RE))
}

