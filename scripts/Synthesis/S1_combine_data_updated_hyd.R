### combine for overall 
library(tidyr)
library(tidyverse)
getwd()
## upload all species suitability data
## upload data

setwd("/Users/katieirving/Documents/git/flow_eco_mech/results")

## time stats
ts <- list.files(pattern="time_stats_updated_hyd")
length(ts) ## 21
ts
## curves
ts <- ts[c(1,3,4,5,8,9,15,16,17,20,21)] # 11
all_data <- NULL
s
## first 3 dfs have different format than 2nd 3, combine in sections 
for(s in 1: length(ts)) {
  
  time_stats <- read.csv(ts[s])
  colnames(time_stats)[10:12] <- c("Low", "Medium", "High")
  all_data <- rbind(all_data, time_stats)
  
}
head(all_data)
head(time_stats)
all_data_first <- all_data

## time stats
ts <- list.files(pattern="time_stats_updated_hyd")
length(ts) ## 21

## thresholds
ts <- ts[-c(1,3,4,5,8,9,15,16,17,20,21,22)] # 10
all_data <- NULL

for(s in 1: length(ts)) {
  
  time_stats <- read.csv(ts[s])
  
  all_data <- rbind(all_data, time_stats)
  
}

## reformat and combine the 2 dfs together 

head(all_data)
head(all_data_first)


names(all_data_first)
names(all_data)
## reformat


all_data_first <- all_data_first %>%
  pivot_longer(Low:High, names_to = "Type", values_to="TimePercentage") %>%
  select(-X)

all_data <- all_data %>%
  rename(Type = Season) %>%
  select(-X)


all_datax <- bind_rows(all_data_first, all_data)

write.csv(all_datax, "S1_all_suitability_all_years.csv")
head(all_datax)

time_stats <- all_datax  %>%
  select(Species, Life_Stage, Node,Hydraulic, water_year,TimePeriod, position,
         Suitability_Class) %>%
  distinct()


SuitClassOverYears <- time_stats %>%
  group_by(Species, Life_Stage, Hydraulic, TimePeriod, position, Node) %>%
  summarise(Overall_Class = tail(names(sort(table(Suitability_Class))), 1))

SuitClassOverYears


write.csv(SuitClassOverYears, "S1_all_suitability_combined_years.csv")

## find highest suitability for each slice
head(all_datax)

SuitabilityPerSlice <- all_datax %>%
  group_by(Species, Life_Stage, Hydraulic, position, Node) %>%
  mutate(MaxPercentage = max(TimePercentage)) %>%
  mutate(MeanPercentage = mean(TimePercentage)) %>%
  select(-water_year,  - TimePercentage, -Suitability_Class) %>%
  distinct()

head(SuitabilityPerSlice)
unique(SuitabilityPerSlice$Life_Stage)

write.csv(SuitabilityPerSlice, "S1_suitability_per_slice.csv")

SuitabilityPerSlice <- read.csv("S1_suitability_per_slice.csv")

# Q limits for high Probs -------------------------------------------------
# 
# ## different number of slices per node, so combine Q limits by node, not species
# ## does not work due to different num of positions - TRY LATER!!!! related to nodenames LA1, LA11 - all taken as LA1
# 
## define nodes
NodeNames <- unique(SuitabilityPerSlice$Node)
NodeNames

setwd("/Users/katieirving/Documents/git/flow_eco_mech")
## list all files
ts <- list.files("output_data/", pattern="Q_limits_updated_hyd")
# ts <- Filter(function(x) !grepl("High_Probs", x), ts)

ts

for(n in 1:length(NodeNames)) {
  ## subset per node
  limitsx <- NULL
  ns <- Filter(function(x) grepl(paste(NodeNames[n]), x), ts)
ns
  for(s in 1: length(ns)) {
    ## upload species per node and combine
    limits <- read.csv(file=paste("output_data/", ns[s], sep=""))
    limitsx <- rbind(limitsx, limits)

limitsx
limits
  }
  write.csv(limitsx, paste("results/S1_", NodeNames[n], "_Q_limits_all_species.csv", sep=""))
}

# # Number of days ----------------------------------------------------------
# 
# ## upload data
# 
# setwd("/Users/katieirving/Documents/git/flow_eco_mech/results")
# 
# ## time stats
# ts <- list.files(pattern="total_days_updated_hyd")
# length(ts) ## 1
# 
# ## dataframes
# total_daysx <- NULL
# SuitClassOverYearsx <- NULL
# # 
# # ## probs needed
# # probs <- c("Low", "Low.Seasonal", "High", "High.Seasonal")
# # probs
# 
# # ## just for ones with prob threshold
# # ts_con <- ts[c(1,3,4,7,8,13,14,15,18,19)]
# # ts_cat <- ts[-c(1,3,4,7,8,13,14,15,18,19)]
# # ts_con
# # ts_cat
# ## create DF of nodes to merge
# total_days <- read.csv(file=paste(ts[1], sep=""))
# head(total_days)
# 
# suit_df <- total_days %>%
#   mutate(pos_code = paste(Node, "_", position, sep="")) %>%
#   select(pos_code) %>%
#   distinct()
# 
# suit_df
# j=1
# j
# for (j in 1:length(ts)) {
#   
#   total_days <- read.csv(file=paste(ts[j], sep=""))
#   head(total_days)
#   
#   suit_df <- total_days %>%
#     mutate(pos_code = paste(Node, "_", position, sep="")) %>%
#     select(pos_code) %>%
#     distinct()
#   
#   total_days <- total_days %>%
#     select(Node, bottom_type, Species, Life_Stage, Hydraulic, water_year, TimePeriod, position,
#            Suitability_Class) %>%
#     mutate(pos_code = paste(Node, "_", position, sep="")) %>%
#     distinct()
#   
#   
#   SuitClassOverMonths <- total_days %>%
#     group_by(Node, Species, Life_Stage, Hydraulic, TimePeriod, position, pos_code, water_year) %>%
#     summarise(Overall_Class_Month = tail(names(sort(table(Suitability_Class))), 1))
#   
#   SuitClassOverYears <- SuitClassOverMonths %>%
#     group_by(Node, Species, Life_Stage, Hydraulic, TimePeriod, position, pos_code) %>%
#     summarise(Overall_Class_Year = tail(names(sort(table(Overall_Class_Month))), 1))
#   
#   
#   SuitClassOverYearsx <- rbind(SuitClassOverYearsx, SuitClassOverYears)
#   
# }
# 
