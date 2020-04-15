### probabilistic curves

library(tidyverse)
library(dplyr)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")


### upload data
## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
sp_depth_cat <- read.csv("output_data/05a_spawning_depth_continuous.csv")

## temoperature
ad_temp_con <- read.csv("output_data/05a_adult_temperature_continous.csv")
juv_temp_con <- read.csv("output_data/05a_juvenile_temperature_continous.csv")
sp_temp_con <- read.csv("output_data/05a_spawning_temperature_continuous.csv")

## velocity
ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous.csv")
ad_vel_cat <- read.csv("output_data/05a_adult_velocity_categorical.csv")
juv_vel_con <- read.csv("output_data/05a_juvenile_velocity_continuous.csv")
juv_vel_cat <- read.csv("output_data/05a_juvenile_velocity_categorical.csv")
sp_vel_cat <- read.csv("output_data/05a_spawning_velocity_continuous.csv")

