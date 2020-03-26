### probablistic curve exploration

## Depth - adult has the most data to take a first stab

library(tidyverse)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## depth data

## all data upload
smea <- read.csv("output_data/00_SMEA_depth_adult.csv")
wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv")
envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")


## clean, subset into life stage and variables
head(saiki)
colnames(saiki)[6:15] <- c("Fish", "SL", "TL", "Weight", "Sex", "Life_stage", "Spawning", "Temp", "Depth_m", "Current_m_sec")
subset(saiki, Life_stage == "Larvae") #16.51
subset(saiki, Life_stage == "Juvenile")
