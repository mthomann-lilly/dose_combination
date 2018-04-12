trt1=c(0,100,200,400)
trt2=c(0,100,200,400)

dat <- genDat_Combination() #get data

res <- analyze(dat_combination = dat,modText = combination)$long
resMat <- analyze(dat_combination = dat,modText = combination)$mat
resList <- list(x=matrix(as.numeric(rownames(resMat)),ncol=1),
                y=matrix(as.numeric(colnames(resMat)),ncol=4),
                z=resMat,type="surface")

#test scatter3d
par(mfrow=c(1,1))
plot <- scatterplot3d(res,color="red",pch=16,grid=FALSE,box=FALSE)
addgrids3d(res,grid=c("xy","xz","yz"),col.grid="black")

#plotly
resMat <- as.matrix(res,ncol=3,byrow=F)
p <- plot_ly(res,x=~trt1,y=~trt2,z=~mean,
             marker=list(color=~mean,showscale=TRUE,
                         colorscale = list(c(0, "rgb(255, 0, 0)"), list(1, "rgb(0, 255, 0)"))))
p

#p <- plotly(resList),
#             marker=list(color=~mean,showscale=TRUE,
#                         colorscale = list(c(0, "rgb(255, 0, 0)"), list(1, "rgb(0, 255, 0)")))) %>% add_surface()
#p

htmlwidgets::saveWidget(p,"/lrlhps/users/c216499/Projects/Documents/plot3d.html")
