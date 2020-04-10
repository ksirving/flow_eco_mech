#### combine data for easy upload in prob curves

## upload each dataset and combine variables for each life stage

library(tidyverse)
library(dplyr)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## continuous
wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv") ## santa ana / san gabriel
envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")

vel_17 <- read.csv("output_data/00_Wulff_2017_velocity_abundance.csv")
vel_16 <- read.csv("output_data/00_Wulff_2016_velocity_abundance.csv")
vel_15 <- read.csv("output_data/00_Wulff_2015_velocity_abundance.csv")


## categorical
dep_ad_03 <- read.csv("output_data/00_SMEA_adult_depth_2003_abundance.csv")
dep_ad_04 <- read.csv("output_data/00_SMEA_adult_depth_2004_abundance.csv")
smea_03 <- read.csv("output_data/00_SMEA_adult_velocity_2003_abundance.csv")
smea_04 <- read.csv("output_data/00_SMEA_adult_velocity_2004_abundance.csv")

smea_03 <- read.csv("output_data/00_SMEA_juvenile_depth_2003_abundance.csv")
smea_04 <- read.csv("output_data/00_SMEA_juvenile_depth_2004_abundance.csv")
