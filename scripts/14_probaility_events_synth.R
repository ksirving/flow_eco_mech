### synthesizing events of certain probabilities over the time series

library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(tidyr)
library(tidyverse)
# install.packages("data.table")
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
## groups a low probability event and counts the consecutuve hours, plus add a column with month and year
new_data <- new_data %>%
  group_by(ID = data.table::rleid(prob_fit < 0.2)) %>%
  mutate(Consec_Hours = if_else(prob_fit < 0.2, row_number(), 0L)) 

# %>%
#   unite(month_year, month:year, remove=F)

## groups data by year, month and ID and reports maximum number of consecutive hours
conseq_ID20 <- new_data %>% 
  group_by(ID, month, year) %>%
  summarise(max(Consec_Hours)) %>%
  rename(max_Consec_Hours=`max(Consec_Hours)`) %>%
  mutate(max_Consec_Hours/24) %>%
  rename(max_Consec_Days=`max_Consec_Hours/24`) 

# conseq_ID20$max_Consec_Days[945:length(conseq_ID20$max_Consec_Days)]

# no of days/hours under low prob

## count number of events - below 20% and for 1 day - per month as well as number of times per month (hours) that the prob goes below zero
consec_days_1 <- conseq_ID20 %>%
  group_by(month, year) %>%
  select(-max_Consec_Hours) %>%
  summarize(n_times_lower_0.2 = n(),
            n_times_grtr_1_day = sum(max_Consec_Days >= 1)) %>%
  arrange(year)

# create year_month column       
consec_days_1 <- ungroup(consec_days_1) %>%
  unite(month_year, year:month, sep="-", remove=F) 

## convert month year to dat format
consec_days_1$month_year <-  zoo::as.yearmon(consec_days_1$month_year)

##  plot - number of days - this works!

    ggplot(consec_days_1, aes(x =month_year, y=n_times_grtr_1_day)) +
    geom_line(aes( y = n_times_grtr_1_day), color = "darkblue") +
    theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
    scale_x_continuous(breaks=as.numeric(consec_days_1$month_year), labels=format(consec_days_1$month_year,"%b %Y")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(title = "Suitability Events",
         y = "Number of days per Month",
         x = "Month") #+ theme_bw(base_size = 15)
    

  ##  plot - number of hours
    ggplot(consec_days_1, aes(x =month_year, y=n_times_lower_0.2)) +
      geom_line(aes( y = n_times_lower_0.2), color = "red") +
      theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
      scale_x_continuous(breaks=as.numeric(consec_days_1$month_year), labels=format(consec_days_1$month_year,"%b %Y")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(title = "Suitability Events",
           y = "Number of hours per Month",
           x = "Month") #+ theme_bw(base_size = 15)
  


## will have different definitions of "an event"
## below 20% for what number of days
## below 30% for same duration
## or no of days/hours under low prob
## try both 

## next steps
## do for other %s
## do for velocity/temoperature
## find change point/sweet spot/break point
## send to all


