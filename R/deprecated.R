## [![GitHub-Commits](https://img.shields.io/github/commit-activity/y/psychbruce/bruceR?logo=github&label=commits&style=social)](https://github.com/psychbruce/bruceR/commits)


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


## Random sampling (like Excel's function \code{RANDBETWEEN}).
##
## @param range Numeric or character vector.
## @param n Sample size for sampling. Default is \code{1}.
## @param seed Random seed.
##
## @return Numeric or character vector (the same class as \code{range}).
##
## @examples
## RANDBETWEEN(1:10, n=1000) %>% Freq()
## RANDBETWEEN(LETTERS, n=1000) %>% Freq()
##
## @export
# RANDBETWEEN=function(range, n=1, seed=NULL) {
#   if(!is.null(seed)) set.seed(seed)
#   # floor(runif(n=n, min=bottom, max=up+1))
#   sample(range, n, replace=TRUE)
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


## Print ANOVA Table of GLM
# GLM_anova=function(model, add.total=TRUE) {
#   Print("ANOVA table:")
#   aov.lm=as.data.frame(car::Anova(model, type=3))
#   total.ss=sum(aov.lm[-1, "Sum Sq"])
#   total.df=sum(aov.lm[-1, "Df"])
#   resid.ss=aov.lm[nrow(aov.lm), "Sum Sq"]
#   resid.df=aov.lm[nrow(aov.lm), "Df"]
#   model.ss=total.ss-resid.ss
#   model.df=total.df-resid.df
#   model.F=(model.ss/model.df)/(resid.ss/resid.df)
#   aov.lm=rbind(`[Model]`=c(model.ss, model.df, model.F, p.f(model.F, model.df, resid.df)),
#                aov.lm[-1,])
#   aov.lm$`Mean Sq`=aov.lm$`Sum Sq`/aov.lm$Df
#   aov.lm$sig=sig.trans(aov.lm$`Pr(>F)`)
#   aov.lm$eta2=mapply(function(f, df1, df2) {f*df1/(f*df1+df2)},
#                      aov.lm$`F value`, aov.lm$Df, aov.lm$Df[nrow(aov.lm)])
#   aov.lm$Df=formatF(aov.lm$Df, 0)
#   aov.lm$`F value`=formatF(aov.lm$`F value`, 2)
#   aov.lm$`Pr(>F)`=p.trans(aov.lm$`Pr(>F)`)
#   aov.lm$eta2=formatF(aov.lm$eta2, 3)
#   aov.lm=aov.lm[,c(1,2,5,3,4,6,7)]
#   if(add.total) {
#     aov.lm=rbind(aov.lm,
#                  Total=c(total.ss, total.df, total.ss/total.df,
#                          NA, NA, NA, NA))
#     aov.lm[(nrow(aov.lm)-1):nrow(aov.lm),
#            c("F value", "Pr(>F)", "sig", "eta2")]=""
#   } else {
#     aov.lm[nrow(aov.lm),
#            c("F value", "Pr(>F)", "sig", "eta2")]=""
#   }
#   names(aov.lm)[5:7]=c("p", " ", "\u03b7\u00b2p")
#   print_table(aov.lm, nsmalls=2)
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



#### Indirect Effect: Sobel Test & MCMC ####


## Mediation analysis based on \emph{b} and \emph{SE} with Sobel test and Monte Carlo simulation.
##
## @description
## Estimating indirect effect from regression coefficients and standard errors (\emph{SE}) by using Sobel test and Monte Carlo simulation.
##
## Total effect (\strong{c}) = Direct effect (\strong{c'}) + Indirect effect (\strong{a*b})
##
## @param a Path \strong{a} (X -> Mediator).
## @param SEa \emph{SE} of path \strong{a}.
## @param b Path \strong{b} (Mediator -> Y).
## @param SEb \emph{SE} of path \strong{b}.
## @param direct [optional] Path \strong{c'} (X -> Y \strong{direct} effect, with M also included in model).
## @param total [optional] Path \strong{c} (X -> Y \strong{total} effect, without M).
## @param cov_ab Covariance between \strong{a} and \strong{b}.
##
## See \href{http://www.quantpsy.org/medmc/medmc.htm}{Selig & Preacher (2008)}:
##
## \emph{If you use SEM, path analysis, multilevel modeling, or some other multivariate method to obtain both a and b from a single model, then cov(a,b) can be found in the asymptotic covariance matrix of the parameter estimates.
## If you use regression to obtain a and b in separate steps, then cov(a,b) = 0.}
## @param seed Random seed.
## @param rep Number of repetitions for Monte Carlo simulation. Default is 50,000. More than 1,000 are recommended.
## @param nsmall Number of decimal places of output. Default is 3.
##
## @references
## Sobel, M. E. (1982). Asymptotic confidence intervals for indirect effects in Structural Equation Models. \emph{Sociological Methodology, 13,} 290-312.
##
## Selig, J. P., & Preacher, K. J. (2008). Monte Carlo method for assessing mediation: An interactive tool for creating confidence intervals for indirect effects. \url{http://www.quantpsy.org/medmc/medmc.htm}
##
## @examples
## med_mc(a=1.50, SEa=0.50, b=2.00, SEb=0.80)
## med_mc(a=1.50, SEa=0.50, b=2.00, SEb=0.80, total=4.50)
## med_mc(a=1.50, SEa=0.50, b=2.00, SEb=0.80, direct=1.50)
##
## @export
# med_mc=function(a, SEa, b, SEb, direct=NULL, total=NULL,
#                 cov_ab=0, seed=NULL, rep=50000, nsmall=3) {
#   indirect=a*b
#   if(is.null(total)) {
#     if(is.null(direct)) {
#       # Print("No input for 'direct' or 'total' effect.")
#     } else {
#       total=direct+indirect
#     }
#   } else {
#     if(is.null(direct)) {
#       direct=total-indirect
#     } else {
#       total=direct+indirect  # priority: direct > total
#       warning("Total effect is replaced by the sum of direct and indirect effects.")
#     }
#   }
#
#   ## Direct and Indirect Effects ##
#   if(is.null(total)==FALSE) {
#     effect=data.frame(total, direct, indirect,
#                       ratioTotal=indirect/total,
#                       ratioRelative=abs(indirect/direct))
#     names(effect)=c("Total", "Direct", "Indirect", "Ratio.Total", "Ratio.Relative")
#     Print("Direct and Indirect Effects:")
#     print_table(effect, row.names=FALSE, nsmalls=nsmall)
#     Print("<<blue Total = Direct + Indirect
#     Ratio.Total = Indirect / Total
#     Ratio.Relative = Indirect / Direct
#     \n>>")
#   }
#
#   ## Indirect Effect: Sobel Test & MCMAM ##
#   sobel=sobel(a, SEa, b, SEb)
#   mcmam=mcmam(a, SEa, b, SEb, cov_ab=cov_ab, seed=seed, rep=rep)
#   mediation=rbind(sobel, mcmam)
#   names(mediation)=c("a", "b", "a*b", "SE(a*b)", "z", "pval", "[95% ", "  CI]", "sig")
#   Print("Test for Indirect Effect (a*b):")
#   print_table(mediation, nsmalls=nsmall)
# }
#
#
# sobel=function(a, SEa, b, SEb) {
#   ab=a*b
#   SEab=sqrt(a^2*SEb^2 + b^2*SEa^2) # Sobel (1982) first-order solution
#   # SEab=sqrt(a^2*SEb^2 + b^2*SEa^2 - SEa^2*SEb^2) # Goodman (1960) unbiased solution
#   # SEab=sqrt(a^2*SEb^2 + b^2*SEa^2 + SEa^2*SEb^2) # Aroian (1944) second-order exact solution
#   z=ab/SEab
#   p=p.z(z)
#   abLLCI=ab-1.96*SEab
#   abULCI=ab+1.96*SEab
#   sig=sig.trans(p)
#   out=data.frame(a, b, ab, SEab, z, p, abLLCI, abULCI, sig)
#   row.names(out)="Sobel test"
#   return(out)
# }
#
#
# mcmam=function(a, SEa, b, SEb, cov_ab=0, seed=NULL, rep=50000, conf=0.95) {
#   # http://www.quantpsy.org/medmc/medmc.htm
#   if(!is.null(seed)) set.seed(seed)
#   acov=matrix(c(
#     SEa^2, cov_ab,
#     cov_ab, SEb^2
#   ), 2, 2)
#   mcmc=MASS::mvrnorm(rep, c(a, b), acov, empirical=FALSE)
#   abMC=mcmc[,1]*mcmc[,2]
#   ab=mean(abMC)
#   SEab=stats::sd(abMC)
#   # z=ab/SEab
#   # p=p.z(z)
#   abLLCI=as.numeric(stats::quantile(abMC, (1-conf)/2))  # 0.025
#   abULCI=as.numeric(stats::quantile(abMC, 1-(1-conf)/2))  # 0.975
#   sig=ifelse(abLLCI>0 | abULCI<0, "yes", "no")
#   out=data.frame(a, b, ab, SEab, z=NA, p=NA, abLLCI, abULCI, sig)
#   row.names(out)="Monte Carlo"
#   return(out)
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


## Compute CI for random effects
# print_variance_ci=function(model) {
#   suppressMessages({
#     varCI=confint(model, parm=c(".sig01", ".sig02", ".sig03",
#                                 ".sig04", ".sig05", ".sig06",
#                                 ".sig07", ".sig08", ".sig09",
#                                 ".sigma"))^2
#   })
#   varCI=as.data.frame(varCI)
#   vc=row.names(varCI) %>% gsub("\\.", "", .) %>%
#     gsub("sigma", "sigerror", .) %>%
#     gsub("sig", "sigma_", .) %>%
#     paste0("^2")
#   varCI=cbind(`Variance Component`=vc, varCI)
#   names(varCI)[2:3]=c("[95% ", "  CI]")
#   cat("\n")
#   print_table(varCI, row.names=FALSE, nsmalls=5)
#   invisible(varCI)
# }


#### HLM Variable Types ####


## Advanced %in% for factor variables (e.g., match "Sex" in "Sex1")
# `%varin%`=function(x, vector) {
#   any(grepl(paste0("^", x, "$"), vector))
# }


## Find and return something from a list
# find=function(vars, list) {
#   n=0; var=group=c(); N=length(unlist(list))
#   for(i in 1:length(list)) for(v in vars) if(v %varin% list[[i]]) {n=n+1; var=c(var, v); group=c(group, names(list[i]))}
#   # for(i in 1:length(list)) for(lv in list[[i]]) if(lv %varin% vars) {n=n+1; var=c(var, lv); group=c(group, names(list[i]))}
#   return(list(N=N, n=n, var=var, group=group))
# }


## Automatically judging variable types in HLM
# HLM_vartypes=function(model=NULL,
#                       formula=model@call$formula,
#                       level2.predictors="") {
#   varlist=dimnames(summary(model)[["coefficients"]])[[1]]
#   vartypes=c()
#   L1.rand.vars=L2.vars=list()
#   data=as.data.frame(model@frame)
#   formula=formula_expand(formula)
#   fx=as.character(formula)[3]
#   ## Level-1 predictor variables with random slopes
#   L1.rand.comp=str_split(gsub(" ", "", str_extract_all(fx, "(?<=\\()[^\\)]+(?=\\))", simplify=T)), "\\|")
#   for(comp in L1.rand.comp) {
#     vars.1=str_split(comp[1], "\\+")  # a list
#     for(var in vars.1[[1]]) {
#       if(var %in% names(data))
#         if(is.factor(data[,var]))
#           vars.1[[1]]=append(vars.1[[1]], paste0(var, levels(data[,var])))
#     }
#     L1.rand.vars[comp[2]]=vars.1
#   }
#   ## Level-2 predictor variables
#   L2.comp=str_split(str_split(gsub(" ", "", level2.predictors), ";", simplify=T), ":")
#   for(comp in L2.comp) {
#     vars.2=str_split(comp[2], "\\+")  # a list
#     for(var in vars.2[[1]]) {
#       if(var %in% names(data))
#         if(is.factor(data[,var]))
#           vars.2[[1]]=append(vars.2[[1]], paste0(var, levels(data[,var])))
#     }
#     L2.vars[comp[1]]=vars.2
#   }
#   ## Judge variable types
#   for(var in varlist) {
#     fd1=find(var, L1.rand.vars)
#     fd2=find(var, L2.vars)
#     if(var=="(Intercept)") {
#       vartype="Intercept"
#     } else if(grepl(":", var)) {
#       ## interaction term ##
#       inter.vars=str_split(var, ":")[[1]]
#       fd.1=find(inter.vars, L1.rand.vars)
#       fd.2=find(inter.vars, L2.vars)
#       if(fd.2$n==length(inter.vars)) {
#         vartype=glue("L2-{fd.2$group[1]}") # warning: cross-classified may not be true
#       } else if(fd.2$n>0) {
#         vartype=glue("Cross-{fd.1$group[1]}-{paste(fd.1$var, collapse=':')}")
#       } else if(fd1$n>0) {
#         vartype=glue("L1random-{fd1$group}-{fd1$var}")
#       } else {
#         vartype="L1fixed"
#       }
#     } else {
#       ## not interaction term ##
#       if(fd2$n>0) {
#         vartype=glue("L2-{fd2$group}")
#       } else if(fd1$n>0) {
#         vartype=glue("L1random-{fd1$group}-{fd1$var}")
#       } else {
#         vartype="L1fixed"
#       }
#     }
#     vartypes[var]=vartype
#   }
#   return(vartypes)
# }


# f1=as.formula(Y ~ 1 + X1 + X2 + X1:X2 + X3 + X4 + X4:W1 + W1 + W2 + W1:W2 + Z1 + (X3+X4|W) + (1|Z))
# f2=as.formula(Y ~ X1:X2:X3 + X1:X2:W1 + X1:W1:W2 + W1:W2:Z1 + (X1+X2|W) + (1|Z))
# v1=c("(Intercept)", "X1", "X2", "X3", "X4", "W1", "W2", "Z1", "X1:X2", "X4:W1", "W1:W2")
# v2=c("(Intercept)", "X1:X2:X3", "X1:X2:W1", "X1:W1:W2", "W1:W2:Z1")
# HLM_vartypes(formula=f1, varlist=v1, level2.predictors="W: W1+W2; Z: Z1")
# HLM_vartypes(formula=f2, varlist=v2, level2.predictors="W: W1+W2; Z: Z1")


## Calculating HLM df
# HLM_df=function(sumModel, vartypes) {
#   paras=sumModel[["devcomp"]][["dims"]][["p"]]
#   df.l1=sumModel[["devcomp"]][["dims"]][["nmp"]] # N - all parameters
#   df.l2=sumModel[["ngrps"]]
#   Sq=sum(grepl("L2", vartypes)) # number of level-2 predictors
#   q=df.l2
#   for(grouptag in names(df.l2))
#     q[grouptag]=sum(grepl(paste0("L2-", grouptag), vartypes))
#   dfs=c()
#   for(i in 1:paras) {
#     if(vartypes[i]=="Intercept") {
#       # df=min(df.l2)-Sq-1
#       df=NA
#     } else if(vartypes[i]=="L1fixed") {
#       df=df.l1
#     } else {
#       vartemp=strsplit(vartypes[i], "-")[[1]]
#       vartype=vartemp[1]
#       grouptag=vartemp[2]
#       if(vartype=="L2") {
#         df=df.l2[grouptag]-q[grouptag]-1
#       } else {
#         # vartype=="L1random" | vartype=="Cross"
#         l1var=vartemp[3]
#         qc=sum(grepl(paste0("Cross-", grouptag, "-", l1var), vartypes))
#         df=df.l2[grouptag]-qc-1
#       }
#     }
#     dfs[i]=df
#   }
#   return(dfs)
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
