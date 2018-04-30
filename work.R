trt1=c(0,100,200,400)
trt2=c(0,100,200,400)

dat <- genDat_Combination() #get data

#res <- analyze(dat_combination = dat,modText = combination_emax)$long
resMat <- analyze(dat_combination = dat,modText = combination_emax)$mat
resList <- list(x=matrix(as.numeric(rownames(resMat)),ncol=1),
                y=matrix(as.numeric(colnames(resMat)),ncol=4),
                z=resMat,type="surface")

#plotly
resMat <- as.matrix(res,ncol=3,byrow=F)
p <- plot_ly(res,x=~trt1,y=~trt2,z=~mean,
             marker=list(color=~mean,showscale=TRUE,
                         colorscale = list(c(0, "rgb(255, 0, 0)"), list(1, "rgb(0, 255, 0)"))))
p

htmlwidgets::saveWidget(p,"/lrlhps/users/c216499/Projects/Documents/plot3d.html")


### linear
#res <- analyze(dat_combination = dat,modText = combination_linear)$long
resMat_lin <- analyze(dat_combination = dat,modText = combination_linear,plot_ed50 = FALSE)$mat
resList <- list(x=matrix(as.numeric(rownames(resMat_lin)),ncol=1),
                y=matrix(as.numeric(colnames(resMat_lin)),ncol=4),
                z=resMat_lin,type="surface")

#plotly
resMat <- as.matrix(res,ncol=3,byrow=F)
p2 <- plot_ly(res,x=~trt1,y=~trt2,z=~mean,
             marker=list(color=~mean,showscale=TRUE,
                         colorscale = list(c(0, "rgb(255, 0, 0)"), list(1, "rgb(0, 255, 0)"))))
p2