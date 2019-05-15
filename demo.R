# This is a demo script for Slinky#
source("slinky/R/plotMultiSurface_scale.R")

library(scatterplot3d)
library(shinythemes)
library(RColorBrewer)
library(ggplot2)
library(Cairo)

obj1 <- readRDS("/Volumes/home/greally-lab/Hackathon/social_isolation_data/eset_Y5_ov_final.RDS")
obj2 <- readRDS("/Volumes/home/greally-lab/Hackathon/social_isolation_data/eset_Y7_ov_final.RDS")
obj3 <- readRDS("/Volumes/home/greally-lab/Hackathon/social_isolation_data/eset_Y8_ov_final.RDS")
obj4 <- readRDS("/Volumes/home/greally-lab/Hackathon/social_isolation_data/eset_Y9_ov_final.RDS")
obj5 <- readRDS("/Volumes/home/greally-lab/Hackathon/social_isolation_data/eset_Y10_ov_final.RDS")


list_sample <- list()
list_sample[[1]] <- obj1
list_sample[[2]] <- obj2
list_sample[[3]] <- obj3
list_sample[[4]] <- obj4
list_sample[[5]] <- obj5

#slinky_scale_live(list_sample[1])

slinky_scale_live3(list_sample[1:2])
slinky_scale_live3(list_sample[2:3])
slinky_scale_live3(list_sample[1:4])
slinky_scale_live3(list_sample[1:5])





