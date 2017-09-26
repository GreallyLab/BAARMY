# New 3D plots using plotly

library(plotly)

# The follwoing code assumes that you have run some of the shiny_scale functio and
# most of the plotMultiSurface_scale function:
# see following
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

# to get the correct indices for sample # 3
j<-3
tmp_index1 = (i-2)*nSample + j
tmp_index2 = (i-1)*nSample + j

# This is to make a 3D plot with markers and lines
p<-  plot_ly() %>%
  add_trace(x = ~x, y = ~y, z = ~z,
            color = ~x,
            type = 'scatter3d',
            mode = 'markers',
            colors = c('#BF382A', '#0C4B8E'),
            opacity=1)
p <- p %>% add_trace(x=c(x[tmp_index1],x[tmp_index2]),
                     y=c(y[tmp_index1],y[tmp_index2]),
                     z=c(z[tmp_index1],z[tmp_index2]),
                     type="scatter3d",
                     mode="lines",
                     line=list(color='black', width=2))
p
