# New 3D plots using plotly

library(plotly)

# The follwoing code assumes that you have run some of the shiny_scale functio and
# most of the plotMultiSurface_scale function:
# see following
# loading the sample data
list_esets <- list_sample[1:3]

# from shiny scale:
cur_list <- list()
pheno_list <- list()
head(list_esets)
for (i in 1:length(list_esets)){
  obj_list <- list()
  eset <- list_esets[[i]]
  pheno <- pData(eset)
  pheno_list[[i]] <- pheno
  col_factor <- NULL
  for (j in 1:ncol(pheno)){
    col_factor[j] <- is.factor(pheno[,j])
  }
  eset_var <- varLabels(eset)[col_factor]
  if (i > 1){
    eset_var_final <- eset_var_final[which(eset_var_final %in% eset_var)]
  }
  if (i == 1) {
    eset_var_final <- eset_var
  }
  eset_expr <- exprs(eset)
  cur_list[[i]] <- prcomp(na.omit(eset_expr))$rotation
}
# get axis choices for ui
axis_choice <- colnames(cur_list[[i]])

# from multi plot scale

options(scipen=99)

matrixList <- cur_list
axisList <- list()
l_pheno <- list()
for (k in 1:length(list_esets)){
  pheno <- pheno_list[[k]]
  l_pheno[[k]] <- pheno[,colnames(pheno)=="married"]
}
names(l_pheno) <- rep("married", length(l_pheno))
groupList<- l_pheno

axisList[[1]] <- c(1,2)
axisList[[2]] <- c(1,2)
axisList[[3]] <- c(1,2)

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
  tmp_group = as.character(groupList[[i]])
  group_col <- c(group_col,tmp_group)
}

# group information for lines connecting two planes, if they keep the same group, the line will be coloured the same way,
# if the group changed between two planes, the group becomes unmatched, the colour will be grey
for (i in 1:(nPlane-1)){
  tmp_group1 = as.character(groupList[[i]])
  tmp_group2 = as.character(groupList[[i+1]])
  tmp_group_line = ifelse(tmp_group1==tmp_group2,tmp_group1,"unmatched")
  assign(paste0("group_line",i,"_",i+1),tmp_group_line)
}

# set colour for groups
group = unique(unlist(groupList))
col_point = colorRampPalette(brewer.pal(8,"Dark2"))(length(group))
names(col_point) = group
# set colour for connecting line
col_line = c(col_point,unmatched="gray40")

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


## 6. add trajectories and colour based on group
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
