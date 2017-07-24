library(scatterplot3d)
year5_pc1 = rnorm(10)
year5_pc2 = rnorm(10,mean=0.1)

year10_pc1 = rnorm(10)
year10_pc2 = rnorm(10,mean=0.01)

year = c(rep(5,10),rep(10,10))

idx = which(year5_pc1>0)
p = scatterplot3d(x=year,y=c(year5_pc1,year10_pc1),z=c(year5_pc2,year10_pc2),pch=16,box=FALSE)
p$plane3d(5,0,0,draw_polygon = T,polygon_args = list(col=rgb(0,0,1,0.1)),draw_lines=F)
x0 <- 5
xyz1 <- p$xyz.convert(rep(x0, 2), rep(-1.5, 2), c(-2, 2))
xyz2 <- p$xyz.convert(rep(x0, 2), rep(1.5, 2), c(-2, 2))
polygon(x=c(xyz1$x,xyz2$x),y=c(xyz1$y,xyz2$y[length(xyz2):1]),col=rgb(0,0,1,0.1))


p$plane3d(10,0,0)
x1 <- 10
xyz1 <- p$xyz.convert(rep(x1, 2), rep(-1.5, 2), c(-2, 2))
xyz2 <- p$xyz.convert(rep(x1, 2), rep(1.5, 2), c(-2, 2))
polygon(x=c(xyz1$x,xyz2$x),y=c(xyz1$y,xyz2$y[length(xyz2):1]),col=rgb(0,0,1,0.1))

#p$plane3d(8,0,0)
p$points3d(x=c(5,10),y=c(year5_pc1[1],year10_pc1[1]),z=c(year5_pc2[1],year10_pc2[1]),pch=16,col="red",type="o")
p$points3d(x=c(5,10),y=c(year5_pc1[3],year10_pc1[3]),z=c(year5_pc2[3],year10_pc2[3]),pch=16,col="red",type="o")
p$points3d(x=c(5,10),y=c(year5_pc1[5],year10_pc1[5]),z=c(year5_pc2[5],year10_pc2[5]),pch=16,col="red",type="o")
p$points3d(x=c(5,10),y=c(year5_pc1[7],year10_pc1[7]),z=c(year5_pc2[7],year10_pc2[7]),pch=16,col="red",type="o")
p$points3d(x=c(5,10),y=c(year5_pc1[9],year10_pc1[9]),z=c(year5_pc2[9],year10_pc2[9]),pch=16,col="red",type="o")

p$points3d(x=c(5,10),y=c(year5_pc1[2],year10_pc1[2]),z=c(year5_pc2[2],year10_pc2[2]),pch=16,col="blue",type="o")
p$points3d(x=c(5,10),y=c(year5_pc1[4],year10_pc1[4]),z=c(year5_pc2[4],year10_pc2[4]),pch=16,col="blue",type="o")
p$points3d(x=c(5,10),y=c(year5_pc1[6],year10_pc1[6]),z=c(year5_pc2[6],year10_pc2[6]),pch=16,col="blue",type="o")
p$points3d(x=c(5,10),y=c(year5_pc1[8],year10_pc1[8]),z=c(year5_pc2[8],year10_pc2[8]),pch=16,col="blue",type="o")
p$points3d(x=c(5,10),y=c(year5_pc1[10],year10_pc1[10]),z=c(year5_pc2[10],year10_pc2[10]),pch=16,col="blue",type="o")

