trt1=c(0,100,200,400)
trt2=c(0,100,200,400)

dat <- genDat_Combination() #get data

res <- analyze(dat_combination = dat,modText = combination)$long

#test scatter3d
par(mfrow=c(1,1))
plot <- scatterplot3d(res,color="red",pch=16,grid=FALSE,box=FALSE)
addgrids3d(res,grid=c("xy","xz","yz"),col.grid="black")

#plotly

p <- plot_ly(res,x=~trt1,y=~trt2,z=~mean,
             marker=list(color=~mean,showscale=TRUE))
p

htmlwidgets::saveWidget(p,"/lrlhps/users/c216499/Projects/Documents/plot3d.html")
