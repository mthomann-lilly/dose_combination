library(rjags)

genDat_Combination <- function(trt1=c(0,100,200,400),
                               trt2=c(0,100,200,400),
                               comb_rates=matrix(c(0.25,0.35,0.4,0.45,
                                                   0.2,0.3,0.35,0.4,
                                                   0.1,0.25,0.3,0.35,
                                                   0.05,0.1,0.2,0.25),nrow=4,byrow=T),
                               n=matrix(rep(10,16),nrow=4)){
  colnames(comb_rates) <- colnames(n) <- trt1
  rownames(comb_rates) <- rownames(n) <- rev(trt2)
  dat <- matrix(mapply(rbinom,c(n),c(comb_rates),n=1),nrow=nrow(comb_rates),byrow=FALSE)
  dat_long <- data.frame(resp=c(dat),n=c(n),trt1=rep(trt1,each=length(trt1)),
                         trt2=rep(rev(trt2),length(trt2)))
<<<<<<< HEAD
  return(dat_long) 
}

=======
  return(dat_long)
}


dat_jags <- list(resp=dat_long$resp,dose1=dat_long$trt1,dose2=dat_long$trt2,
                 dose12=dat_long$trt1+dat_long$trt2,N=nrow(dat_long),n=dat_long$n)
>>>>>>> 8e12e0ffec7c054f4a11d49592a7579852d9dd49

