### temp analysis

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)
library(zoo)
library(scales)
getwd()
## temp data

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

 ### change wd
setwd("input_data/LAR_WaterTemperature")
tp <- list.files(pattern="Sim_Avg")
tp
n=1
## change wd back 
setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## start loop
TempData <- read.csv(file=paste("input_data/LAR_WaterTemperature/", tp[n], sep=""))

head(TempData)
TempData

### species data
fitdata <- read.csv("output_data/adult_temp_prob_curve_data.csv")
head(fitdata)

head(fitdata)

## use smooth spline to predict on new data set
new_values <-smooth.spline(fitdata$temp_fit, fitdata$prob_fit)

all_data_max_max <- TempData %>%
  # group_by(variable) %>%
  mutate(prob_fit = predict(new_values, Sim_MWMaxT)$y) 
# rename(temp_c = Sim_MWAT)

all_data_max_max

all_data_max_mean <- TempData %>%
  # group_by(variable) %>%
  mutate(prob_fit = predict(new_values, Sim_MWAT)$y)# %>% ## using mean max value, check and change!!!!
  # rename(temp_c = Sim_MWAT)

all_data_max_mean

all_data_min_max <- TempData %>%
  # group_by(variable) %>%
  mutate(prob_fit = predict(new_values, Sim_MWMinT)$y) #%>% 
# rename(temp_c = Sim_MWAT)

all_data_min_max

all_data_mean_min <- TempData %>%
  # group_by(variable) %>%
  mutate(prob_fit = predict(new_values, Sim_MinWAT)$y) #%>% 
  # rename(temp_c = Sim_MWAT)

all_data_mean_min

all_data_min_min <- TempData %>%
  # group_by(variable) %>%
  mutate(prob_fit = predict(new_values, Sim_MinWMinT)$y) #%>% 
# rename(temp_c = Sim_MWAT)

all_data_min_min

