### synthesizing events of certain probabilities over the time series

library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")


## data
load(file="output_data/13_depth_probs_2010_2017_TS.RData")
head(new_data)

## workflow
# subset low probs - 10,20,30% probs - under
# look at consequtive days/hours
# rules eg - 1, 5, 10 days of consecutive lows
# same with highs?
