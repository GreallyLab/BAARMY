#' @param matrixList The list containing all the esets you want to plot
#' @param groupList The list containing all the group information you want to plot, the length of the list should equal to the length of the esetList
#' @param axisList The list containing all the information you want to plot on the first and second axis
#' @import scatterplot3d
#' @import RColorBrewer
plotMultiSurface_scale = function(matrixList, groupList, axisList)
{
  options(scipen=99)

  # 1. check if the input have the same length
  if(length(matrixList)!=length(groupList)|length(matrixList)!=length(axisList)|length(axisList)!=length(groupList)){
    "the length of inputs differs, please check your input"
  }

  # 2. Prepare the data to plot on each axis
  # x
  nPlane = length(matrixList)
  x_space = seq(3,nPlane*3,3)
  x = NULL
  for (i in 1:nPlane){
    tmp_x = rep(x_space[i],nrow(matrixList[[i]]))
    x = c(x,tmp_x)
  }

  # y
  y = NULL
  for (i in 1:nPlane){
    tmp_y = matrixList[[i]][,axisList[[i]][1]]
    y = c(y,tmp_y)
  }

  # z
  z = NULL
  for (i in 1:nPlane){
    tmp_z = matrixList[[i]][,axisList[[i]][2]]
    z = c(z,tmp_z)
  }

  ## 3. prepare group information
  # group for points on each plane
  for (i in 1:nPlane){
    tmp_group = as.character(groupList[[i]])
    assign(paste0("group",i),tmp_group)
  }

  # group information for lines connecting two planes, if they keep the same group, the line will be coloured the same way,
  # if the group changed between two planes, the group becomes unmatched, the colour will be grey
  for (i in 1:(nPlane-1)){
    tmp_group1 = as.character(groupList[[i]])
    tmp_group2 = as.character(groupList[[i+1]])
    tmp_group_line = ifelse(tmp_group1==tmp_group2,tmp_group1,"unmatched")
    assign(paste0("group_line",i,"_",i+1),tmp_group_line)
  }



  ## 4. construct the basic 3D scatterplot
  # prepare ranges for the axis
  range_y = max(y)-min(y)
  y_digit = min(which(strsplit(as.character(range_y),split="",fixed=T)[[1]][-c(1:2)]>0))
  y_limit = c(min(y)-signif(0.1*range_y,y_digit-1),max(y)+signif(0.1*range_y,digits = y_digit-1))

  range_z = max(z)-min(z)
  z_digit = min(which(strsplit(as.character(range_z),split="",fixed=T)[[1]][-c(1:2)]>0))
  z_limit = c(min(z)-signif(0.1*range_z,z_digit-1),max(z)+signif(0.1*range_z,digits = z_digit-1))

  # initiate 3D plot
  p = scatterplot3d(x=x,y=y,z=z,pch=16,box=FALSE,cex.symbols=0.8,
                    xlab="dataset",ylab=paste(sapply(axisList,function(x)x[1]),collapse = "/"),
                    zlab=paste(sapply(axisList,function(x)x[2]),collapse = "/"),
                    xlim=c(min(x),max(x)),ylim=y_limit,zlim=z_limit)


  ## 5. add planes
  plane_pos = unique(x)
  for (i in 1:nPlane){
    x0 <- plane_pos[i]
    xyz1 <- p$xyz.convert(rep(x0, 2), rep(y_limit[1], 2), z_limit)
    xyz2 <- p$xyz.convert(rep(x0, 2), rep(y_limit[2], 2), z_limit)
    polygon(x=c(xyz1$x,xyz2$x),y=c(xyz1$y,xyz2$y[length(xyz2):1]),col=rgb(0,0,1,0.1))
  }

  ## 6. add trajectories and colour based on group
  # set colour for groups
  group = unique(unlist(groupList))
  col_point = colorRampPalette(brewer.pal(8,"Dark2"))(length(group))
  names(col_point) = group
  # set colour for connecting line
  col_line = c(col_point,unmatched="gray40")
  # plot points and connecting line
  nSample = nrow(matrixList[[1]])
  for (i in 1:nPlane){
    # plot point on the plane
    for (j in 1:nSample){
      tmp_index = (i-1)*nSample + j
      p$points3d(x=x[tmp_index],
                 y=y[tmp_index],
                 z=z[tmp_index],
                 pch=16,
                 col=col_point[as.character(get(paste0("group",i))[j])],type="p",cex=0.8)

    }
    # plot connecting lines
    if(i>1){
      for (j in 1:nSample){
        tmp_index1 = (i-2)*nSample + j
        tmp_index2 = (i-1)*nSample + j
        group_line = as.character(get(paste0("group_line",i-1,"_",i)))[j]
        p$points3d(x=c(x[tmp_index1],x[tmp_index2]),
                   y=c(y[tmp_index1],y[tmp_index2]),
                   z=c(z[tmp_index1],z[tmp_index2]),
                   pch=16,col=col_line[group_line],
                   type="l",cex=0.8,lty=ifelse(group_line=="unmatched",2,1),
                   lwd=ifelse(group_line=="unmatched",2,1))
      }

    }
  }

  # add legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=1,              # suppress legend box, shrink text 50%
         title=names(groupList)[1],
         legend = unique(unlist(groupList)),
         fill=col_point)
  output <- recordPlot()
}
