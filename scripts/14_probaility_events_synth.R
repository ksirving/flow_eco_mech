### synthesizing events of certain probabilities over the time series

library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(tidyr)
library(tidyverse)
install.packages("data.table")
library(data.table)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")


## data
load(file="output_data/13_depth_probs_2010_2017_TS.RData")
head(new_data)

## workflow
# subset low probs - 10,20,30% probs - under
# look at consequtive days/hours
# rules eg - 1, 5, 10 days of consecutive lows
# same with highs?

## less than 0.20 
## groups a low probability event and counts the consecutuve hours
new_data <- new_data %>%
  group_by(ID = data.table::rleid(prob_fit < 0.2)) %>%
  mutate(Consec_Hours = if_else(prob_fit < 0.2, row_number(), 0L))

## groups data by year and ID and reports maximum number of consecutive hours
conseq_ID20 <- new_data %>% 
  group_by(ID, month, year) %>%
  summarise(max(Consec_Hours)) %>%
  rename(max_Consec_Hours=`max(Consec_Hours)`) %>%
  mutate(max_Consec_Hours/24) %>%
  rename(max_Consec_Days=`max_Consec_Hours/24`)

conseq_ID20$max_Consec_Days[945:length(conseq_ID20$max_Consec_Days)]

## next steps
## add days for each month together
## want number of days, not consec days - or both??
## do for other %s
## find change point/sweet spot/break point



# 
# DT = data.table(grp=rep(c("A", "B", "C", "A", "B"), c(2,2,3,1,2)), value=1:10)
# DT
# rleid(DT$grp) # get run-length ids
# rleidv(DT, "grp") # same as above
# 
# rleid(DT$grp, prefix="grp") # prefix with 'grp'
# 
# # get sum of value over run-length groups
# DT[, sum(value), by=.(grp, rleid(grp))]
# DT[, sum(value), by=.(grp, rleid(grp, prefix="grp"))]

