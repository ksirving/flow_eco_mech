## apply curve to hydraulic variables

library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## upload hydraulic data

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
head(hydraul)
tail(hydraul)

## convert unit from feet to meters



### upload curve data
## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
