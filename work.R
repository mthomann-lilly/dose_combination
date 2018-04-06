trt1=c(0,100,200,400)
trt2=c(0,100,200,400)

dat <- genDat_Combination() #get data

dat_jags <- list(resp=dat$resp,dose1=dat$trt1,dose2=dat$trt2,
                 dose12=sqrt(dat$trt1*dat$trt2),N=nrow(dat),n=dat$n)

model <- jags.model(textConnection(combination),data=dat_jags,n.adapt=1000,n.chains=3)
update(model,10000)
samples <- coda.samples(model,variable.names=c("ed50_1","ed50_2","ed50_12"),n.iter=5000)
plot(samples)
samples <- coda.samples(model,variable.names=c("prob"),n.iter=5000)
summary(samples)
res <- data.frame(trt1=dat$trt1,trt2=dat$trt2,mean=apply(samples[[1]],2,mean))
resMat <- matrix(res$mean,nrow=4,byrow=FALSE)
colnames(resMat) <- trt1
rownames(resMat) <- rev(trt2)
