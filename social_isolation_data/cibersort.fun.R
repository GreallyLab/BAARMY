## Cibersort function 

cibersortAdjust <- function(fileNames="CIBERSORT*"){
  library(readr)
  if (grepl("*", fileNames)){
    fnames <- Sys.glob(paste0(fileNames))
  } else{
    fnames <- read_csv(paste(fileNames))
  }
  fnames1 <- gsub(".csv", "", fnames, fixed = TRUE)
  for(fileN in 1:length(fnames)){
    assign(fnames1[fileN], read_csv(paste(fnames[fileN])));
  }
  
  PC1 <- character(0)
  keep_PC <- character(0)
  for(x in fnames1[1:length(fnames1)]){
    PC1[x] <- paste0("PC1_", x)
    assign(PC1[x], prcomp(t(get(x)[,2:23])))
  }
  
  for(y in PC1){
    keep_PC[y] <- paste0("keep_", y)
    assign(keep_PC[y], get(y)[["rotation"]][,1:5])
  }
  
  for(z in keep_PC){
    assign(paste0(z), saveRDS(get(z), file=paste0(z, ".rds")))
  };
  
}
cibersortAdjust()

