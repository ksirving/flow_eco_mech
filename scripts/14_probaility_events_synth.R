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
  
### 0.1 ########################
    
    load(file="output_data/13_depth_probs_2010_2017_TS.RData")
    head(new_data)
    
    new_data <- new_data %>%
      group_by(ID = data.table::rleid(prob_fit < 0.1)) %>%
      mutate(Consec_Hours = if_else(prob_fit < 0.1, row_number(), 0L)) 
    
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
    
    ## convert month year to date format
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
## do for velocity/temperature
## find change point/sweet spot/break point
## send to all
    ## highs?

    ### 0.3 ########################
    
    load(file="output_data/13_depth_probs_2010_2017_TS.RData")
    head(new_data)
    
    new_data <- new_data %>%
      group_by(ID = data.table::rleid(prob_fit < 0.3)) %>%
      mutate(Consec_Hours = if_else(prob_fit < 0.3, row_number(), 0L)) 
    
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
    
    ## convert month year to date format
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
    
####### 0.15    
    load(file="output_data/13_depth_probs_2010_2017_TS.RData")
    head(new_data)
    
    new_data <- new_data %>%
      group_by(ID = data.table::rleid(prob_fit < 0.15)) %>%
      mutate(Consec_Hours = if_else(prob_fit < 0.15, row_number(), 0L)) 
    
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
    
    ## convert month year to date format
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
    
    
##### make data frame that includes all values 0.1-0.3 in 0.01 increments
    
    load(file="output_data/13_depth_probs_2010_2017_TS.RData")
    head(new_data)
 
    # all columns based on different probabilities
      new_data <- new_data %>%
        group_by(ID = data.table::rleid(prob_fit < 0.1)) %>%
        mutate(probability_0.1 = if_else(prob_fit < 0.1, row_number(), 0L)) %>% 
        ungroup() %>%
        group_by(ID = data.table::rleid(prob_fit < 0.15)) %>%
        mutate(probability_0.15 = if_else(prob_fit < 0.15, row_number(), 0L)) %>% 
        ungroup() %>%
        group_by(ID = data.table::rleid(prob_fit < 0.2)) %>%
        mutate(probability_0.2 = if_else(prob_fit < 0.2, row_number(), 0L)) %>% 
        ungroup() %>%
        group_by(ID = data.table::rleid(prob_fit < 0.25)) %>%
        mutate(probability_0.25 = if_else(prob_fit < 0.25, row_number(), 0L)) %>% 
        ungroup() %>%
        group_by(ID = data.table::rleid(prob_fit < 0.3)) %>%
        mutate(probability_0.3 = if_else(prob_fit < 0.3, row_number(), 0L)) %>% 
        ungroup() %>%
        group_by(ID = data.table::rleid(prob_fit < 0.35)) %>%
        mutate(probability_0.35 = if_else(prob_fit < 0.35, row_number(), 0L)) #%>% 
  
    ## melt data frame so that each probabilioty column are all in one row 
    ## select only columns needed
        names(new_data)
       
      
      new_datax <- new_data[, c(5,6,10,11:16)] # all probs
      new_datax <- new_data[, c(5,6,10,11, 13, 14,16)]
    ## melt
      melt_data<-reshape2::melt(new_datax, id=c("ID", "month", "year"))
      melt_data <- rename(melt_data, Probability_Threshold = variable)
      head(melt_data)
    
    ## groups data by year, month and ID and reports maximum number of consecutive hours - 
      ## hours are also consecuative, not number of hours under probability
    conseq_ID20 <- melt_data %>% 
      group_by(ID, month, year, Probability_Threshold) %>%
      summarise(max(value)) %>%
      rename(max_Consec_Hours=`max(value)`) %>%
      mutate(max_Consec_Hours/24) %>%
      rename(max_Consec_Days=`max_Consec_Hours/24`) 
    
    conseq_ID20
    
    # conseq_ID20$max_Consec_Days[945:length(conseq_ID20$max_Consec_Days)]
    
    # no of days/hours under low prob
    
    ## count number of events - below 20% and for 1 day - per month as well as number of times per month (hours) that the prob goes below zero
    consec_days <- conseq_ID20 %>%
      group_by(month, year, Probability_Threshold) %>%
      # select(-max_Consec_Hours) %>%
      summarize(n_hours_lower = sum(max_Consec_Hours), ## total consecuative hours,i.e. lumps of time plus more lumps of time = not all consecuative 
                n_times_grtr_1_day = sum(max_Consec_Days >= 1)) %>%
      arrange(year)
    
  
    
    # create year_month column       
    consec_days <- ungroup(consec_days) %>%
      unite(month_year, year:month, sep="-", remove=F) 
    
    ## convert month year to date format
    consec_days$month_year <-  zoo::as.yearmon(consec_days$month_year)
    
    ##  plot - number of days - this works!
    
    ggplot(consec_days, aes(x =month_year, y=n_times_grtr_1_day)) +
      geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
      scale_x_continuous(breaks=as.numeric(consec_days$month_year), labels=format(consec_days$month_year,"%b %Y")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(title = "Probability threshold: 1 day duration",
           y = "Number of events per Month",
           x = "Month") #+ theme_bw(base_size = 15)
 
    
    ### not easy to see, use fewer probs
    
    ## filter
    consec_days_sub <- filter(consec_days, Probability_Threshold == c("less_than_10_percent", "less_than_20_percent", "less_than_30_percent")  )
    
    ## plot
    ggplot(consec_days_sub, aes(x =month_year, y=n_times_grtr_1_day)) +
      geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
      scale_x_continuous(breaks=as.numeric(consec_days_sub$month_year), labels=format(consec_days_sub$month_year,"%b %Y")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(title = "Probability events of duration over 1 day",
           y = "Number of events over one day per Month",
           x = "Month") #+ theme_bw(base_size = 15)
    
     ## change variables names  - find depth associated
    
   ### percentage of time per month under a probability
    # days & hours
    
    load(file="output_data/13_depth_probs_2010_2017_TS.RData")
    head(new_data)
    max(new_data$prob_fit)
    
    ## need total number of hours per month 
    
    new_data <- new_data %>%
      group_by(month, year) %>%
      mutate(total_hours_in_the_month = n()) 
 
    
    # all columns based on different probabilities - cannot work out how to get all events, 
    #so all events 2 or more hours
    new_data <- new_data %>%
      group_by(ID = data.table::rleid(prob_fit < 0.1)) %>%
      mutate(probability_0.1 = if_else(prob_fit < 0.1, n(), 0L)) %>% 
      ungroup() %>%
      group_by(ID = data.table::rleid(prob_fit < 0.15)) %>%
      mutate(probability_0.15 = if_else(prob_fit < 0.15, n(), 0L)) %>%
      ungroup() %>%
      group_by(ID = data.table::rleid(prob_fit < 0.2)) %>%
      mutate(probability_0.2 = if_else(prob_fit < 0.2, n(), 0L)) %>% 
      ungroup() %>%
      group_by(ID = data.table::rleid(prob_fit < 0.25)) %>%
      mutate(probability_0.25 = if_else(prob_fit < 0.25, n(), 0L)) %>% 
      ungroup() %>%
      group_by(ID = data.table::rleid(prob_fit < 0.3)) %>%
      mutate(probability_0.3 = if_else(prob_fit < 0.3, n(), 0L)) %>% 
      ungroup() %>%
      group_by(ID = data.table::rleid(prob_fit < 0.35)) %>%
      mutate(probability_0.35 = if_else(prob_fit < 0.35, n(), 0L)) #%>%
    
    
    ## melt data frame so that each probabilioty column are all in one row 
    ## select only columns needed
    
  range(new_data$less_than_10_percent)
  range(new_data$less_than_20_percent)
  range(new_data$total_hours_in_the_month)
  
  names(new_data)
    ## melt data frame so that each probability column are all in one row 
    ## select only columns needed
    new_datax <- new_data[, c(5,6,10,11, 12, 14,15,17)]
  
    ## melt
    melt_data<-reshape2::melt(new_datax, id=c( "month", "year", "total_hours_in_the_month", "ID"))
    melt_data <- rename(melt_data, Probability_Threshold = variable)
    melt_data[1:10, ]
    melt_data$value <- as.numeric(as.character(melt_data$value))
    melt_data$value_bin <- ifelse(melt_data$value >0, 1, 0)
    ## groups data by year, month and ID and reports total number of hours - (2 hours or more)
    ## hours are not consecutive,so total number of hours under probability
    conseq_ID20 <- melt_data %>% 
      group_by(month, year, Probability_Threshold,total_hours_in_the_month ) %>%
      summarize(total_Hours = sum(value_bin)) %>%
      # rename(total_Hours=`sum()`) %>%
      mutate(percent_hours = total_Hours/total_hours_in_the_month*100) %>%
      arrange(year)
      
      conseq_ID20
    
   
    # conseq_ID20$max_Consec_Days[945:length(conseq_ID20$max_Consec_Days)
    
    # create year_month column       
    total_hours <- ungroup(conseq_ID20) %>%
      unite(month_year, year:month, sep="-", remove=F) 
    total_hours
    ## convert month year to date format
    total_hours$month_year <-  zoo::as.yearmon(total_hours$month_year)
    
    # total_hours$Probability_Threshold == "less_than_40_percent"
    range(total_hours$percent_hours)
    
    
    ggplot(total_hours, aes(x =month_year, y=percent_hours)) +
      geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
      scale_x_continuous(breaks=as.numeric(total_hours$month_year), labels=format(total_hours$month_year,"%b %Y")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(title = "Frequency of Events",
           y = "Events (%)",
           x = "Month") #+ theme_bw(base_size = 15)

    ## subset data
    
    ## filter
   total_hours_sub <- filter(total_hours, Probability_Threshold == c("probability_0.1", "probability_0.35"))
    
   ggplot(total_hours_sub, aes(x =month_year, y=percent_hours)) +
     geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
     scale_x_continuous(breaks=as.numeric(total_hours$month_year), labels=format(total_hours$month_year,"%b %Y")) +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
     labs(title = "Frequency of Events",
          y = "Events (%)",
          x = "Month") #+ theme_bw(base_size = 15)
 
    ##  highest (0.4) threshold still has variability - possibly to do with the different num of hours 
   ## a month and missing values
   

   

