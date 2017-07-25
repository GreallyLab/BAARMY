#' @param object1 The matrix containing values you want to plot for dataset1
#' @param object2 The matrix containing values you want to plot for dataset2
#' @param group1 Vector containing the group information for each samples in dataset1
#' @param group2 Vector containing the group information for each samples in dataset2
#' @param object1_axis_one A character indicates what you want to plot on the first axis for dataset1
#' @param object1_axis_two A character indicates what you want to plot on the second axis for dataset1
#' @param object2_axis_one A character indicates what you want to plot on the first axis for dataset2
#' @param object2_axis_two A character indicates what you want to plot on the second axis for dataset2
#' @import scatterplot3d
#' @import RColorBrewer
plotMultiSurface = function(object1, object2, group1=NULL,group2=NULL,
                            object1_axis_one="PC1", object1_axis_two="PC2",
                            object2_axis_one="PC1", object2_axis_two="PC2")
  {
  # 1. check if the order of the data are the same
  if(!all.equal(rownames(object1),rownames(object2))){
    message("sample ID doesn't match, please check the rownames of your input")
  }

  # fill group information
  if(!is.null(group1)){
    nGroup1 = length(unique(group1))
    group1 = as.character(group1)
  }
  else{
    nGroup1 = 1
    group1 = rep("group1",nrow(object1))
  }

  if(!is.null(group2)){
    nGroup2 = length(unique(group2))
    group2 = as.character(group2)
  }
  else{
    nGroup2 = 1
    group2 = rep("group1",nrow(object2))
  }

  # match the group information for 2 sets of data
  if(all.equal(group1,group2)==T){
    group_line = group1
  }
  else{
    group_line = ifelse(group1==group2,group1,"unmatched")
  }

  print(paste0("there are ",nrow(object1)," individuals forming ",nGroup1," groups of clusters from dataset1"))
  print(paste0("there are ",nrow(object2)," individuals forming ",nGroup2," groups of clusters from dataset2"))

  ## construct the basic 3D scatterplot
  # create scattor plot
  x = c(rep(5,nrow(object1)),rep(10,nrow(object2)))
  y = c(object1[,object1_axis_one],object2[,object2_axis_one])
  z = c(object1[,object1_axis_two],object2[,object2_axis_two])

  range_y = max(y)-min(y)
  y_limit = c(floor(min(y)-0.05*range_y),ceiling(max(y)+0.05*range_y))

  range_z = max(z)-min(z)
  z_limit = c(floor(min(z)-0.05*range_z),ceiling(max(z)+0.07*range_z))

  p = scatterplot3d(x=x,y=y,z=z,pch=16,box=FALSE,cex.symbols=0.8,
                    xlab="dataset",ylab=paste0(object1_axis_one,"/",object2_axis_one),
                    zlab=paste0(object1_axis_two,"/",object2_axis_two),
                    x.ticklabs=c(deparse(substitute(object1)),"","","","",deparse(substitute(object2))),
                    xlim=c(5,10),ylim=y_limit,zlim=z_limit)

  # add planes
  # plane1
  x0 <- 5
  xyz1 <- p$xyz.convert(rep(x0, 2), rep(y_limit[1], 2), z_limit)
  xyz2 <- p$xyz.convert(rep(x0, 2), rep(y_limit[2], 2), z_limit)
  polygon(x=c(xyz1$x,xyz2$x),y=c(xyz1$y,xyz2$y[length(xyz2):1]),col=rgb(0,0,1,0.1))

  # plane2
  x1 <- 10
  xyz3 <- p$xyz.convert(rep(x1, 2), rep(y_limit[1], 2), z_limit)
  xyz4 <- p$xyz.convert(rep(x1, 2), rep(y_limit[2], 2), z_limit)
  polygon(x=c(xyz3$x,xyz4$x),y=c(xyz3$y,xyz4$y[length(xyz4):1]),col=rgb(0,0,1,0.1))

  ## add trajectories and colour based on group
  group = unique(c(group1,group2))
  col_point = brewer.pal(max(length(group),3),"Dark2")
  names(col_point) = group

  if(all.equal(group1,group2)==T){
    col_line = col_point
  }
  else{
    col_line = c(col_point,unmatched="gray40")
  }
  for (i in 1:nrow(object1)){
    # point on the first plane
    p$points3d(x=5,
               y=object1[i,object1_axis_one],
               z=object1[i,object1_axis_two],
               pch=16,col=col_point[as.character(group1[i])],type="p",cex=0.8)
    # point on the second plane
    p$points3d(x=10,
               y=object2[i,object2_axis_one],
               z=object2[i,object2_axis_two],
               pch=16,col=col_point[as.character(group2[i])],type="p",cex=0.8)
    # line connecting two points
    p$points3d(x=c(5,10),
               y=c(object1[i,object1_axis_one],object2[i,object2_axis_one]),
               z=c(object1[i,object1_axis_two],object2[i,object2_axis_two]),
               pch=16,col=col_line[group_line[i]],type="l",cex=0.8,
               lty=ifelse(group_line[i]=="unmatched",2,1),
               lwd=ifelse(group_line[i]=="unmatched",2,1))
  }
  output <- recordPlot()
}
