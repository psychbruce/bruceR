r2MLM=function(data, within_covs, between_covs, random_covs,
               gamma_w, gamma_b, tau, sigma2, has_intercept=T, clustermeancentered=T) {
  if(has_intercept==T) {
    if(length(gamma_b)>1) gamma=c(1,gamma_w,gamma_b[2:length(gamma_b)])
    if(length(gamma_b)==1) gamma=c(1,gamma_w)
    if(is.null(within_covs)==T) gamma_w=0
  }
  if(has_intercept==F) {
    gamma=c(gamma_w,gamma_b)
    if(is.null(within_covs)==T) gamma_w=0
    if(is.null(between_covs)==T) gamma_b=0
  }
  if(is.null(gamma)) gamma=0
  ## compute phi
  phi=var(cbind(1, data[,c(within_covs)], data[,c(between_covs)]), na.rm=T)
  if(has_intercept==F) phi=var(cbind(data[,c(within_covs)], data[,c(between_covs)]), na.rm=T)
  if(is.null(within_covs)==T & is.null(within_covs)==T & has_intercept==F) phi=0
  phi_w=var(data[,within_covs], na.rm=T)
  if(is.null(within_covs)==T) phi_w=0
  phi_b=var(cbind(1, data[,between_covs]), na.rm=T)
  if(is.null(between_covs)==T) phi_b=0
  ## compute psi and kappa
  var_randomcovs=var(cbind(1, data[,c(random_covs)]), na.rm=T)
  if(length(tau)>1) psi=matrix(c(diag(tau)), ncol=1)
  if(length(tau)==1) psi=tau
  if(length(tau)>1) kappa=matrix(c(tau[lower.tri(tau)==TRUE]), ncol=1)
  if(length(tau)==1) kappa=0
  v=matrix(c(diag(var_randomcovs)),ncol=1)
  r=matrix(c(var_randomcovs[lower.tri(var_randomcovs)==TRUE]), ncol=1)
  if(is.null(random_covs)==TRUE){
    v=0
    r=0
    m=matrix(1,ncol=1)
  }
  if(length(random_covs)>0) m=matrix(c(colMeans(cbind(1, data[,c(random_covs)]), na.rm=T)), ncol=1)
  ## total variance
  totalvar_notdecomp=t(v)%*%psi + 2*(t(r)%*%kappa) + t(gamma)%*%phi%*%gamma + t(m)%*%tau%*%m + sigma2
  totalwithinvar=(t(gamma_w)%*%phi_w%*%gamma_w) + (t(v)%*%psi + 2*(t(r)%*%kappa)) + sigma2
  totalbetweenvar=(t(gamma_b)%*%phi_b%*%gamma_b) + tau[1]
  totalvar=totalwithinvar + totalbetweenvar
  ## total decomp
  decomp_fixed_notdecomp=(t(gamma)%*%phi%*%gamma) / totalvar
  decomp_fixed_within=(t(gamma_w)%*%phi_w%*%gamma_w) / totalvar
  decomp_fixed_between=(t(gamma_b)%*%phi_b%*%gamma_b) / totalvar
  decomp_fixed=decomp_fixed_within + decomp_fixed_between
  decomp_varslopes=(t(v)%*%psi + 2*(t(r)%*%kappa)) / totalvar
  decomp_varmeans=(t(m)%*%tau%*%m) / totalvar
  decomp_sigma=sigma2/totalvar
  ## within decomp
  decomp_fixed_within_w=(t(gamma_w)%*%phi_w%*%gamma_w) / totalwithinvar
  decomp_varslopes_w=(t(v)%*%psi + 2*(t(r)%*%kappa)) / totalwithinvar
  decomp_sigma_w=sigma2/totalwithinvar
  ## between decomp
  decomp_fixed_between_b=(t(gamma_b)%*%phi_b%*%gamma_b) / totalbetweenvar
  decomp_varmeans_b=tau[1] / totalbetweenvar
  # NEW measures
  if(clustermeancentered==TRUE) {
    R2_f=decomp_fixed
    R2_f1=decomp_fixed_within
    R2_f2=decomp_fixed_between
    R2_fv=decomp_fixed + decomp_varslopes
    R2_fvm=decomp_fixed + decomp_varslopes + decomp_varmeans
    R2_v=decomp_varslopes
    R2_m=decomp_varmeans
    R2_f_w=decomp_fixed_within_w
    R2_f_b=decomp_fixed_between_b
    R2_fv_w=decomp_fixed_within_w + decomp_varslopes_w
    R2_v_w=decomp_varslopes_w
    R2_m_b=decomp_varmeans_b
  }
  if(clustermeancentered==FALSE) {
    R2_f=decomp_fixed_notdecomp
    R2_fv=decomp_fixed_notdecomp + decomp_varslopes
    R2_fvm=decomp_fixed_notdecomp + decomp_varslopes + decomp_varmeans
    R2_v=decomp_varslopes
    R2_m=decomp_varmeans
  }
  if(clustermeancentered==TRUE) {
    decomp_table=matrix(c(decomp_fixed_within, decomp_fixed_between, decomp_varslopes, decomp_varmeans, decomp_sigma,
                          decomp_fixed_within_w, "NA", decomp_varslopes_w, "NA", decomp_sigma_w,
                          "NA", decomp_fixed_between_b, "NA", decomp_varmeans_b, "NA"), ncol=3)
    rownames(decomp_table)=c("fixed, within", "fixed, between", "slope variation", "mean variation", "sigma2")
    colnames(decomp_table)=c("total", "within", "between")
    R2_table=matrix(c(R2_f1, R2_f2, R2_v, R2_m, R2_f, R2_fv, R2_fvm,
                      R2_f_w, "NA", R2_v_w, "NA", "NA", R2_fv_w, "NA",
                      "NA", R2_f_b, "NA", R2_m_b, "NA", "NA", "NA")
                    , ncol=3)
    rownames(R2_table)=c("f1", "f2", "v", "m", "f", "fv", "fvm")
    colnames(R2_table)=c("total", "within", "between")
  }
  ## barchart
  if(clustermeancentered==TRUE) {
    contributions_stacked=matrix(c(decomp_fixed_within, decomp_fixed_between, decomp_varslopes, decomp_varmeans, decomp_sigma,
                                   decomp_fixed_within_w, 0, decomp_varslopes_w, 0, decomp_sigma_w,
                                   0, decomp_fixed_between_b, 0, decomp_varmeans_b, 0), 5, 3)
    colnames(contributions_stacked)=c("total", "within", "between")
    rownames(contributions_stacked)=c("fixed slopes (within)",
                                      "fixed slopes (between)",
                                      "slope variation (within)",
                                      "intercept variation (between)",
                                      "residual (within)")
    barplot(contributions_stacked, main="Decomposition", horiz=FALSE,
            ylim=c(0,1), col=c("darkred", "steelblue", "darkred", "midnightblue", "white"),
            ylab="proportion of variance",
            density=c(NA,NA,30,40,NA), angle=c(0,45,0,135,0), xlim=c(0,1), width=c(.3,.3))
    legend(.30, -.1, legend=rownames(contributions_stacked),
           fill=c("darkred", "steelblue", "darkred", "midnightblue", "white"),
           cex=.7, pt.cex=1, xpd=T, density=c(NA,NA,30,40,NA), angle=c(0,45,0,135,0))
  }
  if(clustermeancentered==FALSE) {
    decomp_table=matrix(c(decomp_fixed_notdecomp, decomp_varslopes, decomp_varmeans, decomp_sigma), ncol=1)
    rownames(decomp_table)=c("fixed", "slope variation", "mean variation", "sigma2")
    colnames(decomp_table)=c("total")
    R2_table=matrix(c(R2_f, R2_v, R2_m, R2_fv, R2_fvm), ncol=1)
    rownames(R2_table)=c("f", "v", "m", "fv", "fvm")
    colnames(R2_table)=c("total")
    ## barchar
    contributions_stacked=matrix(c(decomp_fixed_notdecomp, decomp_varslopes, decomp_varmeans, decomp_sigma), 4, 1)
    colnames(contributions_stacked)=c("total")
    rownames(contributions_stacked)=c("fixed slopes",
                                      "slope variation",
                                      "intercept variation",
                                      "residual")
    barplot(contributions_stacked, main="Decomposition", horiz=FALSE,
            ylim=c(0,1),col=c("darkblue", "darkblue", "darkblue", "white"),
            ylab="proportion of variance",
            density=c(NA,30,40,NA), angle=c(0,0,135,0), xlim=c(0,1), width=c(.6))
    legend(.30, -.1, legend=rownames(contributions_stacked),
           fill=c("darkblue", "darkblue", "darkblue", "white"),
           cex=.7, pt.cex=1, xpd=TRUE, density=c(NA,30,40,NA), angle=c(0,0,135,0))
  }
  Output=list(noquote(decomp_table), noquote(R2_table))
  names(Output)=c("Decompositions", "R2s")
  return(Output)
}


# NOTE: estimates in the input represent hypothetical results for a random slope model with two level-1 predictors and two level-2 predictors
# in practice a user would have previously obtained these input estimates by fitting their model in MLM software
# additionally, the input consists of hypothetical predictor data, whereas in practice a user would read-in their actual data
library(MASS)
data=matrix(NA,100,4)
xs=mvrnorm(n=100,mu=c(0,0), Sigma=matrix(c(2,.75,.75,1.5),2,2))
ws=mvrnorm(n=10,mu=c(0,2), Sigma=matrix(c(1,.5,.5,2),2,2))
data[,1:2]=xs
for(i in seq(10)) {
  data[(10*(i-1)+1):(i*10),1]=data[(10*(i-1)+1):(i*10),1] - mean(data[(10*(i-1)+1):(i*10),1])
  data[(10*(i-1)+1):(i*10),2]=data[(10*(i-1)+1):(i*10),2] - mean(data[(10*(i-1)+1):(i*10),2])
  data[(10*(i-1)+1):(i*10),3]=ws[i,1]
  data[(10*(i-1)+1):(i*10),4]=ws[i,2]
}
r2MLM(data, within_covs=c(1,2), between_covs=c(3,4), random_covs=c(1,2),
      gamma_w=c(2.5,-1), gamma_b=c(1,.25,1.5),
      tau=matrix(c(4,1,.75,1,1,.25,.75,.25,.5),3,3), sigma2=10)
