library(rjags)
library(ggplot2)
library(plotly)

genDat_Combination <- function(trt1=c(0,100,200,400),
                               trt2=c(0,100,200,400),
                               comb_rates=matrix(c(0.25,0.35,0.4,0.45,
                                                   0.2,0.3,0.35,0.4,
                                                   0.1,0.25,0.3,0.35,
                                                   0,0.1,0.2,0.25),nrow=4,byrow=T),
                               n=matrix(rep(10,16),nrow=4)){
  colnames(comb_rates) <- colnames(n) <- trt1
  rownames(comb_rates) <- rownames(n) <- rev(trt2)
  dat <- matrix(mapply(rbinom,c(n),c(comb_rates),n=1),nrow=nrow(comb_rates),byrow=FALSE)
  dat_long <- data.frame(resp=c(dat),n=c(n),trt1=rep(trt1,each=length(trt1)),
                         trt2=rep(rev(trt2),length(trt2)))
  return(dat_long)
}

analyze <- function(modText,dat_combination,plot_ed50=TRUE,burn=10000,samps=5000){
  dat_jags <- list(resp=dat_combination$resp,dose1=dat_combination$trt1,dose2=dat_combination$trt2,
                   dose12=sqrt(dat_combination$trt1*dat_combination$trt2),N=nrow(dat_combination),
                   n=dat_combination$n)
  model <- jags.model(textConnection(modText),data=dat_jags,n.adapt=1000,n.chains=3)
  update(model,burn)
  if(plot_ed50){
    samples <- coda.samples(model,variable.names=c("ed50_1","ed50_2","ed50_12"),n.iter=samps)
    plot(samples)
  }
  samples <- coda.samples(model,variable.names=c("prob"),n.iter=samps)
  summary(samples)
  res <- data.frame(trt1=dat$trt1,trt2=dat$trt2,mean=apply(samples[[1]],2,mean))
  resMat <- matrix(res$mean,nrow=4,byrow=FALSE)
  colnames(resMat) <- trt1
  rownames(resMat) <- rev(trt2)
  return(list(mat=resMat,long=res))
}

