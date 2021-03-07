#### Deprecated Functions ####


## Set random seeds with a specific version of R
##
## The new versions of R (>= 3.6.0; since April 2019) have changed the mechanism of generating random numbers.
## To have an exact replication of previous results based on random numbers, the version of R should be specified.
## By default, it sets the version to "3.5.0". All the versions earlier than 3.6.0 will generate the same result.
##
## @param seed Random seed.
## @param version Character string specifying the R version. Default is "3.5.0".
##
## @examples
## set.seeds(1, version="3.5.0")
## sample(1:10)  # 3  4  5  7  2  8  9  6 10  1
##
## set.seeds(1, version="3.6.0")
## sample(1:10)  # 9  4  7  1  2  5  3 10  6  8
##
## @seealso \code{\link{set.seed}}
##
## @export
# set.seeds=function(seed, version="3.5.0") {
#   suppressWarnings(RNGversion(version))
#   set.seed(seed)
# }


## Check update of \code{bruceR} package
##
## @importFrom xml2 read_html
## @importFrom rvest html_node html_text
##
## @examples
## bruceR::check_update()
##
## @export
# check_update=function() {
#   try({
#     # user.ver=sessionInfo()[["otherPkgs"]][["bruceR"]][["Version"]]
#     user.ver=as.character(packageVersion("bruceR"))
#     curr.ver=read_html("https://github.com/psychbruce/bruceR") %>% html_node("h2+ h3 code") %>% html_text()
#     Print("
#     <<blue
#     Your installed version of bruceR: <<green {user.ver}>>
#     The latest version on GitHub.com: <<green {curr.ver}>>
#     <<red {ifelse(user.ver==curr.ver, 'No need to update.', 'You can update!')}>>
#     >>
#     ")
#   }, silent=TRUE)
# }


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


## Compute 95\% confidence interval (CI) of a variable
##
## @param var Variable (e.g., \code{data$var}) or numeric vector.
## @param nsmall Number of decimal places of output. Default is 3.
## @param empirical \code{TRUE} (default, return 2.5\% and 97.5\% percentiles) or \code{FALSE} (basd on \emph{t} distribution).
## If \code{TRUE}, directly extract CI from data (instead of computing CI based on normal or \emph{t} distribution).
##
## @examples
## CI(bfi$age)
## CI(bfi$age, emp=FALSE)  # NOTE: "emp" can partially match "empirical"
##
## @export
# CI=function(var, nsmall=2, empirical=TRUE) {
#   M=mean(var)
#   N=length(var)
#   SE=sd(var)/sqrt(N)
#   LLCI=ifelse(empirical, quantile(var, 0.025), M + qt(0.025,N)*SE)
#   ULCI=ifelse(empirical, quantile(var, 0.975), M + qt(0.975,N)*SE)
#   Print("Mean = {M:.{nsmall}}, 95% CI [{LLCI:.{nsmall}}, {ULCI:.{nsmall}}]{ifelse(empirical, ' (empirical)', '')}")
# }


## Check many assumptions for (OLS and multilevel) regression models
##
## Based on the functions in \code{performance} (see \code{performance::\link[performance]{check_model}}), it checks for
## 1) multivariate normality,
## 2) multicollinearity (VIF),
## 3) homoscedasticity (vs. heteroscedasticity),
## 4) independence of residuals (vs. autocorrelation).
##
## @import performance
##
## @param model A model object (fitted by \code{lm, glm, lmer, glmer, ...}).
## @param plot Visualize the check results. Default is \code{TRUE}.
##
## @examples
## lm=lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
## model_check(lm)
##
## library(lmerTest)
## hlm.2=lmer(Preference ~ Sweetness + Gender * Age + Frequency + (Sweetness | Consumer) + (1 | Product), data=carrots)
## model_check(hlm.2)
##
## @export
# model_check=function(model, plot=TRUE) {
#   Print("<<bold <<underline 1>>) Multivariate normality:>>")
#   if(class(model) %in% c("lmerMod", "lmerModLmerTest")) {
#     Print("(please see plots)")
#   } else {
#     check_normality(model)
#   }
#   Print("\n\n\n<<bold <<underline 2>>) Multicollinearity (VIF):>>")
#   print(check_collinearity(model))
#   Print("\n\n\n<<bold <<underline 3>>) Homoscedasticity (vs. Heteroscedasticity):>>")
#   check_heteroscedasticity(model)
#   Print("\n\n\n<<bold <<underline 4>>) Independence of residuals (vs. Autocorrelation):>>")
#   check_autocorrelation(model)
#   if(plot) {
#     Print("\n\n\n\nPlotting...")
#     check_model(model, check=c("normality", "qq",
#                                "vif",  # multicollinearity
#                                "ncv",  # heteroscedasticity
#                                "reqq"))
#   }
# }


#### Simple Slope & Moderated Mediation ####


## Simple-slope analysis based on \emph{b} and \emph{SE}
##
## \strong{WARNING}: This function is NOT optimal.
## I suggest using the \code{interactions} package, see \code{interactions::\link[interactions]{sim_slopes}}
##
## @param b Coefficient of X (main predictor).
## @param SEb \emph{SE} of b.
## @param bmod Coefficient of moderator.
## @param SDmod \emph{SD} of moderator (not \emph{SE}), used for calculating simple slopes of X with moderator at "\emph{M} + \emph{SD}" and "\emph{M} - \emph{SD}".
## @param df Residual degree of freedom.
## @param nsmall Number of decimal places of output. Default is 3.
##
## @examples
## simple_slope(b=1.5, SEb=0.5, bmod=0.9, SDmod=1.0, df=300)
##
## @export
# simple_slope=function(b, SEb, bmod, SDmod, df, nsmall=3) {
#   b.h = b+bmod*SDmod
#   b.m = b
#   b.l = b-bmod*SDmod
#   Print("Moderator at <<italic M>> + <<italic SD>>: <<italic b>> = {b.h: .{nsmall}} (<<italic SE>> = {SEb:.{nsmall}}), <<italic t>>({df}) = {b.h/SEb:.{nsmall}}, <<italic p>> {p.trans2(p.t(b.h/SEb, df))}
#          Moderator at <<italic M>>     : <<italic b>> = {b.m: .{nsmall}} (<<italic SE>> = {SEb:.{nsmall}}), <<italic t>>({df}) = {b.m/SEb:.{nsmall}}, <<italic p>> {p.trans2(p.t(b.m/SEb, df))}
#          Moderator at <<italic M>> - <<italic SD>>: <<italic b>> = {b.l: .{nsmall}} (<<italic SE>> = {SEb:.{nsmall}}), <<italic t>>({df}) = {b.l/SEb:.{nsmall}}, <<italic p>> {p.trans2(p.t(b.l/SEb, df))}")
# }


## Moderated mediation
## @export
# mod_med=function(a1, SEa1, a3, SEa3, b, SEb, c1, c3, SDmod,
#                  nsmall) {
#   indirect.high=(a1+a3*SDmod)*b
#   indirect.mean=a1*b
#   indirect.low=(a1-a3*SDmod)*b
#   direct.high=c1+c3*SDmod
#   direct.mean=c1
#   direct.low=c1-c3*SDmod
#   total.high=indirect.high+direct.high
#   total.mean=indirect.mean+direct.mean
#   total.low=indirect.low+direct.low
#   effect=data.frame(total=c(total.low, total.mean, total.high),
#                     direct=c(direct.low, direct.mean, direct.high),
#                     indirect=c(indirect.low, indirect.mean, indirect.high))
#   effect=dplyr::mutate(effect,
#                        ratioTotal=indirect/total,
#                        ratioRelative=abs(indirect/direct))
#   row.names(effect)=c("low mod", "mean mod", "high mod")
#
#   Print("Effect:")
#   print_table(effect, nsmall=nsmall)
#   Print("Index of Moderated Mediation:")
#   med_mc(a3, SEa3, b, SEb, nsmall=nsmall)
#   Print("Low Moderator (<<italic z>> = -1):")
#   med_mc(a1-a3*SDmod, SEa1, b, SEb, nsmall=nsmall)
#   Print("Mean Moderator (<<italic z>> = 0):")
#   med_mc(a1, SEa1, b, SEb, nsmall=nsmall)
#   Print("High Moderator (<<italic z>> = +1):")
#   med_mc(a1+a3*SDmod, SEa1, b, SEb, nsmall=nsmall)
# }


#### Print HLM Notes ####
# HLM_summary_notes=function() {
#   Print("
#   <<italic Notes:>>
#
#   <<bold df>> is estimated by Satterthwaite's (1946) approximation.
#   <<bold df.HLM>> is calculated based on variable types.
#   <<bold r.HLM>> is calculated by <<italic t>>-to-<<italic r>> transformation.
#
#   <<bold <<blue ICC (intraclass correlation coefficient):
#   -->  = var(random.intercept.i) / [var(random.intercept.all) + var(residual)]>>>>
#   -->  proportion of level-2 (group) variance to total variance
#
#   <<bold <<red Marginal <<italic R>>\u00b2:
#   -->  = var(fixed) / [var(fixed) + var(random) + var(residual)]>>>>
#   -->  proportion of variance explained by fixed effects
#   <<bold <<red Conditional <<italic R>>\u00b2:
#   -->  = [var(fixed) + var(random)] / [var(fixed) + var(random) + var(residual)]>>>>
#   -->  proportion of variance explained by both fixed and random effects
#   <<bold <<red Omega\u00b2 (\u03a9\u00b2):
#   -->  = 1 - var(residual) / var(Y)>>>>
#   -->  1 - proportion of unexplained variance
#
#   For 'lmer' models, you may also specify 'level2.predictors'.
#   see ?HLM_summary
#   ")
#   # stop("Please input an 'lmer' or 'glmer' model.\n       For 'lmer' models, you may also specify 'level2.predictors'.")
# }


#### Many HLMs ####


## Generate all possible combinations of random effects
##
## @examples
## f = y ~ a * b * c + (a * b * c | sub) + (a * b * c | item)
## fs=HLMs_formulas(f)
##
## @export
# HLMs_formulas=function(formula.full.model) {
#   f=formula_expand(formula.full.model)
#   f=as.character(f)
#   fy=f[2]
#   fx=f[3]
#   fixed.comp=str_remove_all(fx, "[\\+ ]*\\([^\\)]+\\)[\\+ ]*") %>%
#     paste(f[2], f[1], .)
#   rand.comp=str_extract_all(fx, "(?<=\\()[^\\)]+(?=\\))", simplify=T) %>%
#     gsub(" ", "", .) %>% str_split("\\|")
#   re.list=list()
#   for(rand in rand.comp) {
#     re=rand[1] %>% str_split("\\+") %>% .[[1]]
#     cl=rand[2]
#     re.allcomb=c()
#     if(1 %notin% re) re.allcomb=c(re.allcomb, 1)
#     for(n in 1:length(re)) {
#       re.allcomb=combn(re, n) %>%
#         apply(2, function(x) paste(x, collapse=" + ")) %>%
#         c(re.allcomb, .)
#     }
#     re.allcomb=paste0("(", re.allcomb, " | ", cl, ")")
#     re.list[[cl]]=re.allcomb
#   }
#   f.list=expand.grid(re.list) %>% as.matrix() %>%
#     apply(1, function(x) paste(x, collapse=" + ")) %>%
#     paste(fixed.comp, ., sep=" + ")
#   return(f.list)
# }


## Run many HLMs
##
## @import lmerTest
## @import data.table
##
## @examples
## mf=HLMs_run(HLMs_formulas(Reaction ~ Days + (Days | Subject)), data=sleepstudy)
##
## @note
## Collaborated with \href{https://github.com/usplos}{Guang-Yao Zhang}
##
## @seealso \code{\link{HLMs_run_parallel}}
##
## @export
# HLMs_run=function(formulas.text, data, family=NULL) {
#   t0=Sys.time()
#   model.fit=data.table()
#   n=length(formulas.text)
#   for(i in 1:n) {
#     ft=formulas.text[i]
#     f=as.formula(ft)
#     if(is.null(family))
#       model=lmerTest::lmer(f, data)
#     else
#       model=lme4::glmer(f, data, family)
#     try({R2=NULL; R2=MuMIn::r.squaredGLMM(model)}, silent=TRUE)
#     if(is.null(R2)) R2=matrix(c(NA,NA), nrow=1)
#     model.fit.i=data.table(raw.id=i,
#                            formula=ft,
#                            singular=lme4::isSingular(model),
#                            AIC=AIC(model),
#                            BIC=BIC(model),
#                            R2.marginal=R2[1,1],
#                            R2.conditional=R2[1,2])
#     row.names(model.fit.i)=NULL
#     model.fit=rbind(model.fit, model.fit.i)
#     Print("{i/n*100:.1}%: Model '{ft}' is OK!")
#   }
#   model.fit=model.fit[order(singular, BIC, AIC, raw.id),]
#   Print("{n} HLMs are OK! (Total time cost: {dtime(t0, 'secs')})")
#   return(model.fit)
# }


# HLMs_onecore=function(f.id) {
#   ft=formulas.text[f.id]
#   f=as.formula(ft)
#   if(is.null(family))
#     model=lmerTest::lmer(f, data)
#   else
#     model=lme4::glmer(f, data, family)
#   try({R2=NULL; R2=MuMIn::r.squaredGLMM(model)}, silent=TRUE)
#   if(is.null(R2)) R2=matrix(c(NA, NA), nrow=1)
#   model.fit=data.frame(raw.id=f.id,
#                        formula=ft,
#                        singular=lme4::isSingular(model),
#                        AIC=AIC(model),
#                        BIC=BIC(model),
#                        R2.marginal=R2[1,1],
#                        R2.conditional=R2[1,2])
#   row.names(model.fit)=NULL
#   return(model.fit)
# }


## Run many HLMs (parallel version, much faster than \code{HLMs_run()})
##
## @import parallel
## @import data.table
##
## @return A data.table ordered by \code{singular}, \code{BIC}, and \code{AIC}.
##
## @note
## Collaborated with \href{https://github.com/usplos}{Guang-Yao Zhang}
##
## @seealso \code{\link{HLMs_run}}
##
## @export
# HLMs_run_parallel=function(formulas.text, data, family=NULL,
#                            cores=4) {
#   t0=Sys.time()
#   f.ids=sample(1:length(formulas.text), length(formulas.text))
#   # detectCores()
#   Print("{length(f.ids)} HLMs begin running with {cores} parallel cores...")
#   cl=makeCluster(cores)
#   clusterExport(cl, c("formulas.text", "data", "family"),
#                 envir=environment())
#   results=do.call("rbind", parLapply(cl, f.ids, HLMs_onecore))
#   stopCluster(cl)
#   Print("{length(f.ids)} HLMs are OK! (Total time cost: {dtime(t0, 'secs')})")
#   results=as.data.table(results)[order(singular, BIC, AIC, raw.id),]
#   return(results)
# }
