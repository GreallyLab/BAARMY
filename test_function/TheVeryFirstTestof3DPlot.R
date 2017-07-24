library(scatterplot3d)
year5_pc1 = rnorm(10)
year5_pc2 = rnorm(10,mean=0.1)

year10_pc1 = rnorm(10)
year10_pc2 = rnorm(10,mean=0.01)

year = c(rep(5,10),rep(10,10))

idx = which(year5_pc1>0)
p = scatterplot3d(z=year,x=c(year5_pc1,year10_pc1),y=c(year5_pc2,year10_pc2),pch=16,box=FALSE)
p$plane3d(5,0,0)
p$plane3d(10,0,0)
#p$plane3d(8,0,0)
p$points3d(z=c(5,10),x=c(year5_pc1[1],year10_pc1[1]),y=c(year5_pc2[1],year10_pc2[1]),pch=16,col="red",type="ol")
p$points3d(z=c(5,10),x=c(year5_pc1[3],year10_pc1[3]),y=c(year5_pc2[3],year10_pc2[3]),pch=16,col="red",type="ol")
p$points3d(z=c(5,10),x=c(year5_pc1[5],year10_pc1[5]),y=c(year5_pc2[5],year10_pc2[5]),pch=16,col="red",type="ol")
p$points3d(z=c(5,10),x=c(year5_pc1[7],year10_pc1[7]),y=c(year5_pc2[7],year10_pc2[7]),pch=16,col="red",type="ol")
p$points3d(z=c(5,10),x=c(year5_pc1[9],year10_pc1[9]),y=c(year5_pc2[9],year10_pc2[9]),pch=16,col="red",type="ol")

p$points3d(z=c(5,10),x=c(year5_pc1[2],year10_pc1[2]),y=c(year5_pc2[2],year10_pc2[2]),pch=16,col="blue",type="ol")
p$points3d(z=c(5,10),x=c(year5_pc1[4],year10_pc1[4]),y=c(year5_pc2[4],year10_pc2[4]),pch=16,col="blue",type="ol")
p$points3d(z=c(5,10),x=c(year5_pc1[6],year10_pc1[6]),y=c(year5_pc2[6],year10_pc2[6]),pch=16,col="blue",type="ol")
p$points3d(z=c(5,10),x=c(year5_pc1[8],year10_pc1[8]),y=c(year5_pc2[8],year10_pc2[8]),pch=16,col="blue",type="ol")
p$points3d(z=c(5,10),x=c(year5_pc1[10],year10_pc1[10]),y=c(year5_pc2[10],year10_pc2[10]),pch=16,col="blue",type="ol")

