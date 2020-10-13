### merge results
library(tidyverse)
library(tidyr)
### work flow
## upload time stats
## converge all nodes per species
## converge all species
## calculate suitability per node position
## calculate suitability per node (if 1 position suitable, node is suitable)

## format

# TimePeriod position water_year Probability_Threshold TimePercentage
# 1 non_critical       MC       2010                   Low            100
# 2     critical       MC       2010                   Low            100
# 3 non_critical       MC       2011                   Low            100
# 4     critical       MC       2011                   Low            100
# 5 non_critical       MC       2012                   Low            100
# 6     critical       MC       2012                   Low            100
# Species Life_Stage Hydraulic   Node
# 1 Santa Ana Sucker   Juvenile     Depth LA20_2
# 2 Santa Ana Sucker   Juvenile     Depth LA20_2
# 3 Santa Ana Sucker   Juvenile     Depth LA20_2
# 4 Santa Ana Sucker   Juvenile     Depth LA20_2
# 5 Santa Ana Sucker   Juvenile     Depth LA20_2
# 6 Santa Ana Sucker   Juvenile     Depth LA20_2

## water year types 
water_year_type <- read.csv("input_data/LAR_WYT_1950_2019_ppt.csv")
layears <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
water_year_type <- water_year_type %>% 
  filter( WY %in% layears ) %>%
  select(WY, WYT) %>%
  rename(water_year = WY)
water_year_type

## threshold to change to annual 

ann_stats <- c("Low", "Medium", "High")

crit_stats <- c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal")

concrete <- c("F34D", "F37B_High", "F45B", "F319", "LA13", "LA1")

setwd("/Users/katieirving/Documents/git/flow_eco_mech")
### santa ana sucker
## velocity

## upload all time stats csvs

## time stats
ts <- list.files("output_data/", pattern="time_stats")
length(ts) ## 219
ts
ts <- Filter(function(x) grepl("fry", x), ts)
ts <- Filter(function(x) grepl("depth", x), ts)
ts <- ts[-19] ## old data
time_statsx <- NULL

head(time_stats)


for(j in 1: length(ts)) {
  ts <- list.files("output_data/", pattern="time_stats")
  length(ts) ## 219
  ts
  ts <- Filter(function(x) grepl("fry", x), ts)
  ts <- Filter(function(x) grepl("depth", x), ts)
  ts <- ts[-19] ## old data
  time_stats <- read.csv(file=paste("output_data/", ts[j], sep=""))
  ts <- gsub("F37B_High", "F37BHigh", ts)
  ts <- gsub("F37B_Low", "F37BLow", ts)
  ts <- gsub("LA20_1", "LA201", ts)
  ts <- gsub("LA20_2", "LA202", ts)
  string <- str_split(ts[j], "_", 5)
  stringx <- string[[1]]
  
  ######## juvenile
  time_stats <- time_stats %>% 
    select(-X) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    rename(TimePeriod = season) %>%
    mutate(Species = "Santa Ana Sucker", Life_Stage = "Fry", Hydraulic = "Depth", Node = paste(stringx[2])) %>%
    distinct()
  
  time_statsx <- rbind(time_statsx, time_stats)
  
}

head(time_statsx)
unique(time_statsx$TimePeriod)
## change time period to anual and add bottom and water year type

time_stats_ann <- time_statsx %>%
  select(-Seasonal) %>%
  rename(TimePercentage = Annual) %>%
  mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
  mutate(TimePeriod= ifelse(TimePeriod == "critcal", "critical", "non_critical")) %>%
  distinct()

## add water year 
time_stats_ann <- full_join(time_stats_ann, water_year_type, by = "water_year")
time_stats_ann <- na.omit(time_stats_ann)
## calculate suitability

probs <- seq(1, length(time_stats_ann$TimePeriod), 1)  

for(p in 1: length(probs)) {
  
  time_stats_ann$Suitability_Class[p] <- if(time_stats_ann$TimePercentage[p] >= 75) {
    paste("High") 
    
  } else if (time_stats_ann$TimePercentage[p] <= 25) {
    paste("Low") 
    
  } else {
    paste("Partial")
  }
  
}
time_stats_ann
## change time period to seasonal and add bottom and water year type

time_stats_seas <- time_statsx %>%
  select(-Annual) %>%
  rename(TimePercentage = Seasonal) %>%
  mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
  mutate(TimePeriod= ifelse(TimePeriod == "critcal", "critical", "non_critical")) %>%
  distinct()
time_stats_seas

## add water year 
time_stats_seas <- full_join(time_stats_seas, water_year_type, by = "water_year")
time_stats_seas <- na.omit(time_stats_seas)

## calculate suitability

probs <- seq(1, length(time_stats_seas$TimePeriod), 1)  

for(p in 1: length(probs)) {
  
  time_stats_seas$Suitability_Class[p] <- if(time_stats_seas$TimePercentage[p] >= 75) {
    paste("High") 
    
  } else if (time_stats_seas$TimePercentage[p] <= 25) {
    paste("Low") 

  } else {
    paste("Partial")
  }
  
}



head(time_stats_seas)
head(time_stats_ann)

## join back together and save
time_stats_all <- rbind(time_stats_ann, time_stats_seas)
write.csv(time_stats_all, "results/SAS_fry_Depth_time_stats.csv")

# Days per month ----------------------------------------------------------


### days per month
td <- list.files("output_data/", pattern="number_of_days")
length(td) ## 153

# td <- Filter(function(x) grepl("fry", x), td)
td <- Filter(function(x) grepl("depth", x), td)
td <- td[-c(20,19)]
td

total_daysx <- NULL
head(total_days)


for(j in 1: length(td)) {
  td <- list.files("output_data/", pattern="number_of_days")
  length(td) ## 153
  
  # td <- Filter(function(x) grepl("fry", x), td)
  td <- Filter(function(x) grepl("depth", x), td)
  td <- td[-c(20,19)]
  td
  total_days <- read.csv(file=paste("output_data/", td[j], sep=""))
  td <- gsub("F37B_High", "F37BHigh", td)
  td <- gsub("F37B_Low", "F37BLow", td)
  td <- gsub("LA20_1", "LA201", td)
  td <- gsub("LA20_2", "LA202", td)
  string <- str_split(td[j], "_", 5)
  stringx <- string[[1]]
  
  ######## juvenile
  total_days <- total_days %>% 
    select(-X) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    # mutate(season = ifelse(month %in% non_critical, "non_critical", "critical")) %>%
    rename(TimePeriod = season,  DaysPerMonth = days_per_water_month) %>%
    mutate(Species = "Santa Ana Sucker", Life_Stage = "Fry", Hydraulic = "Depths", Node = paste(stringx[2])) %>%
    distinct()
  
  total_daysx <- rbind(total_daysx, total_days)
  
}

head(total_daysx)


## change time period to annual and add bottom and water year type

total_days_ann <- total_daysx %>%
  # select(-TimePeriod) %>%
  mutate(TimePeriod = paste("Annual")) %>%
  mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
  mutate(TimePeriod= ifelse(TimePeriod == "critcal", "critical", "non_critical")) %>%
  distinct()

## add water year 
total_days_ann <- full_join(total_days_ann, water_year_type, by = "water_year")
total_days_ann <- na.omit(total_days_ann)

## suitability
head(total_days_ann)

probs <- seq(1, length(total_days_ann$TimePeriod), 1)  

for(p in 1: length(probs)) {
  
  total_days_ann$Suitability_Class[p] <- if(total_days_ann$DaysPerMonth[p] >= 21) {
    paste("High") 
    
  } else if (total_days_ann$DaysPerMonth[p] <= 7) {
    paste("Low") 

  } else {
    paste("Partial")
  }
  
}

## change time period to seasonal and add bottom and water year type

total_days_seas <- total_daysx %>%
  filter(TimePeriod == "critcal") %>%
  mutate(bottom_type = ifelse(Node %in% concrete, "Concrete", "Soft")) %>%
  mutate(TimePeriod= ifelse(TimePeriod == "critcal", "critical", "non_critical")) %>%
  distinct()

## add water year 
total_days_seas <- full_join(total_days_seas, water_year_type, by = "water_year")
total_days_seas <- na.omit(total_days_seas)

## suitability
head(total_days_seas)

probs <- seq(1, length(total_days_seas$TimePeriod), 1)  

for(p in 1: length(probs)) {
  
  total_days_seas$Suitability_Class[p] <- if(total_days_seas$DaysPerMonth[p] >= 21) {
    paste("High") 
    
  } else if (total_days_seas$DaysPerMonth[p] <= 7) {
    paste("Low") 
    
  } else {
    paste("Partial")
  }
  
}
head(total_days_ann)
head(total_days_seas)
### bind together and save
total_days_all <- rbind(total_days_ann, total_days_seas)
write.csv(total_days_all,"results/SAS_Fry_Depth_total_days.csv")

total_days_all
tail(total_days_all)

