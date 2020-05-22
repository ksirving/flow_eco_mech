## all curves for TAC

library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
sp_depth_cat <- read.csv("output_data/05a_spawning_depth_continuous.csv")









## format spawning
sp_depth_catx <- sp_depth_cat[,c(4,15)]
sp_depth_catx$Depth <- sp_depth_catx$Depth..m.*100
sp_depth_catx$Abundance <- 1
sp_depth_catx$Dataset <- "Saiki"
sp_depth_catx<- sp_depth_catx[, 3:5]
sp_depth_catx$Life_Stage <- "Spawning"
sp_depth_catx