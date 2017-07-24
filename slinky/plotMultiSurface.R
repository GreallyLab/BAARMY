@param object1 

plotMultiSurface = function(object1, object2, group, 
                            object1_axis_one="PC1", object1_axis_two="PC2",
                            object2_axis_one="PC1", object2_axis_two="PC2")
  {
  # 1. check if the order of the data are the same
  if(!all.equal(rownames(data1),rownames(data2))|all.equal(rownames(data1),rownames(group))){
    message("sample ID doesn't match, please check the rownames of your input")
    }
  nGroup = length(unique(group$group))
  print(paste0("there are ",nrow(data1)," individuals forming ",nGroup," groups of clusters"))
  
  
  ## construct the basic 3D scatterplot
  # surface
  year = c(rep(5,nrow(data1)),rep(10,nrow(data2)))
  y = c(data1[,plot_content1],data2[,plot_content1])
  z = c(data1[,plot_content2],data2[,plot_content2])
  
  range_y = max(y)-min(y)
  y_limit = c(min(y)-0.05*range_y,max(y)+0.05*range_y)
  
  range_z = max(z)-min(z)
  z_limit = c(min(z)-0.05*range_z,max(z)+0.05*range_z)
  
  p = scatterplot3d(x=year,y=y,z=z,pch=16,box=FALSE,cex.symbols=0.8,
                    xlab="year",ylab=plot_content1,zlab=plot_content2,
                    x.ticklabs=c(5,"","","","",10))
  #p$plane3d(5,0,0,draw_polygon = T,polygon_args = list(col=rgb(0,0,1,0.1)),draw_lines=F)
  x0 <- 5
  xyz1 <- p$xyz.convert(rep(x0, 2), rep(y_limit[1], 2), z_limit)
  xyz2 <- p$xyz.convert(rep(x0, 2), rep(y_limit[2], 2), z_limit)
  polygon(x=c(xyz1$x,xyz2$x),y=c(xyz1$y,xyz2$y[length(xyz2):1]),col=rgb(0,0,1,0.1))
  
  #p$plane3d(10,0,0)
  x1 <- 10
  xyz3 <- p$xyz.convert(rep(x1, 2), rep(y_limit[1], 2), z_limit)
  xyz4 <- p$xyz.convert(rep(x1, 2), rep(y_limit[2], 2), z_limit)
  polygon(x=c(xyz3$x,xyz4$x),y=c(xyz3$y,xyz4$y[length(xyz4):1]),col=rgb(0,0,1,0.1))
  
  ## add trajectories and colour based on group
  col = brewer.pal(nGroup,"Set1")
  names(col) = unique(group$group)
  
  for (i in 1:nrow(data1)){
    p$points3d(x=c(5,10),
               y=c(data1[i,plot_content1],data2[i,plot_content1]),
               z=c(data1[i,plot_content2],data2[i,plot_content2]),
               pch=16,col=col[group[i,"group"]],type="o",cex=0.8)
  }
  
  
}