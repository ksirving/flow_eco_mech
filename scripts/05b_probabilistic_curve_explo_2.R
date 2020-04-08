#### probability on curve - take 2

library(tidyverse)
library(dplyr)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## depth data

## all data upload
## categorical
## smea
dep_ad_03 <- read.csv("output_data/00_SMEA_adult_depth_2003_abundance.csv")
dep_ad_04 <- read.csv("output_data/00_SMEA_adult_depth_2004_abundance.csv")

## format data into mid point
## make df with only abundance data
smea_ab <- cbind(dep_ad_04[,c(2,6,7)], dep_ad_03[,c(2,10)])
## add 2004 sites together
smea_ab$all_sites_2004 <-  smea_ab[,2] + smea_ab[,3]
smea_ab
# add years together
smea_ab$abundance <- smea_ab$all_sites_ab + smea_ab$all_sites_2004
# depth and abundance only, remove abundance row
ab_depth_sm <- smea_ab[-16, c(4,7)]
ab_depth_sm$Depth_mid <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 71)
ab_depth_sm




## continuous
wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel
envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")


