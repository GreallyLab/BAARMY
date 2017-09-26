#' @param matrixList The list containing all the esets you want to plot
#' @param groupList The list containing all the group information you want to plot, the length of the list should equal to the length of the esetList
#' @param axisList The list containing all the information you want to plot on the first and second axis
#' @import scatterplot3d
#' @import RColorBrewer


plotMultiSurface_int_scale <- function(matrixList, groupList, axisList)
{
  options(scipen=999)

  # 1. check if the input have the same length
  if(length(matrixList)!=length(groupList)|length(matrixList)!=length(axisList)|length(axisList)!=length(groupList)){
    "the length of inputs differs, please check your input"
  }

  # 2. Prepare the data to plot on each axis
  # x
  nPlane = length(matrixList)
  x_space = seq(1,nPlane*1,1)
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
  group_col <- NULL
  for (i in 1:nPlane){
    tmp_group <- as.character(groupList[[i]])
    group_col <- c(group_col,tmp_group)
  }

  # group information for lines connecting two planes, if they keep the same group, the line will be coloured the same way,
  # if the group changed between two planes, the group becomes unmatched, the colour will be grey
  for (i in 1:(nPlane-1)){
    tmp_group1 <- as.character(groupList[[i]])
    tmp_group2 <- as.character(groupList[[i+1]])
    tmp_group_line <- ifelse(tmp_group1 == tmp_group2, tmp_group1, "unmatched")
    assign(paste0("group_line",i,"_",i+1),tmp_group_line)
  }

  # set colour for groups
  group <- unique(unlist(groupList))
  # assuming nominal values
  if (length(group) < 8){
    col_point <- brewer.pal(8,"Dark2")[1:length(group)]
  } else { # non-nominal
    col_point <- colorRampPalette(brewer.pal(8,"Spectral"))(length(group))
  }
  names(col_point) <- group
  # set colour for connecting line
  col_line = c(col_point, unmatched="#8c8c8c")

  ## 4. Make the 3D plot
  # This is to make a 3D plot with markers and lines
  p<-  plot_ly() %>%
    add_trace(x = x, y = y, z = z,
              color = group_col,
              type = "scatter3d",
              mode = "markers",
              colors = col_point,
              opacity=1,
              name = names(groupList)[1],
              hoverinfo = "x+y+z") %>%
    layout(title = "Insert Title Option",
           scene= list(xaxis = list(title = "dataset"),
                       yaxis = list(title = paste(sapply(axisList,function(x)x[1]),collapse = "/")),
                       zaxis = list(title = paste(sapply(axisList,function(x)x[2]),collapse = "/"))))

  ## 5. add trajectories based on group
  # plot points and connecting line
  nSample <- nrow(matrixList[[1]])
  i<-1
  j<-1
  for (i in 1:nPlane){
    # plot connecting lines
    if(i>1){
      for (j in 1:nSample){
        tmp_index1 <- (i-2)*nSample + j
        tmp_index2 <- (i-1)*nSample + j
        group_line <- as.character(get(paste0("group_line",i-1,"_",i)))[j]
        p <- p %>% add_trace(x = c(x[tmp_index1],x[tmp_index2]),
                             y = c(y[tmp_index1],y[tmp_index2]),
                             z = c(z[tmp_index1],z[tmp_index2]),
                             type = "scatter3d", mode = "lines",
                             line=list(width=ifelse(group_line=="unmatched",2,1),
                                       color = col_line[group_line]),
                             showlegend = FALSE)
      }

    }
  }
  p
}
