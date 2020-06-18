## time series analysis

library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## upload hydraulic data

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
## select columns
hyd_dep <- hydraul[,c(1:3,9)]
colnames(hyd_dep)[4] <-"depth_ft"

## convert unit from feet to meters
hyd_dep$depth_cm <- (hyd_dep$depth_ft*0.3048)*100

## plot time series - use numbers for now, add dates in plot

hyd_dep$date_num <- seq(1,length(hyd_dep$DateTime), 1)
plot(hyd_dep$date_num, hyd_dep$depth_cm, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm)


### upload curve data
## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

## combine data adult
all_depth <- rbind(ad_depth_con, ad_depth_cat)

## uncount data
depth_freq <- all_depth %>% 
  uncount(Abundance)
unique(depth_freq$Dataset)

## remove Thompson
depth_freq <- subset(depth_freq, Dataset!="Thompson")
## curve data in dataframe - no plot

## scaled data
depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=10000)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

##  raw depth values
xfit_r <- seq(min(depth_freq$Depth), max(depth_freq$Depth), length=10000)

## data frame with probabilities and depth

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("depth_fit", "prob_fit")

## round the depths - don't need the high resolution

hyd_dep$depth_cm_round <- round(hyd_dep$depth_cm, digits=1 )
fitdata$depth_fit_round <- round(fitdata$depth_fit, digits=1)

plot(hyd_dep$date_num, hyd_dep$depth_cm_round, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm_round)

## merge node data and probabilities
all_data <- merge(hyd_dep, fitdata, by.x="depth_cm_round", by.y="depth_fit_round")
head(all_data)

all_data <- all_data[, c(1,2,7,9)]
# sum(is.na(all_data))

## remove duplicate date_num (date time) and order

all_data <- all_data[!duplicated(all_data$date_num),]
new_data <- all_data[order(all_data$date_num),]


## analysse the data as time series

head(new_data)

firstHour <- 24*(as.Date("2010-10-17 00:00:00")-as.Date("2010-1-1 00:00:00"))
new_dataTS <- ts(new_data$prob_fit,start=c(2010,firstHour),frequency=24*365)

plot.ts(new_dataTS)

## decompose data

new_dataTS_comp <- decompose(new_dataTS)
plot(new_dataTS_comp)

## look at data using lubridate etc

names(new_data)

new_data$DateTime<-as.POSIXct(new_data$DateTime,
                                    format = "%Y-%m-%d %H:%M",
                                    tz = "America/Los_Angeles")

## create year, month, day and julian day columns

new_data <- new_data %>%
  mutate(month = month(DateTime))

new_data <- new_data %>%
  mutate(year = year(DateTime))

new_data <- new_data %>%
  mutate(day = day(DateTime))

new_data <- new_data %>%
  mutate(hour = hour(DateTime))

new_data <- new_data %>%
  mutate(jd = yday(DateTime))

head(new_data)
# summarize data by year 

save(new_data, file="output_data/13_depth_probs_2010_2017_TS.RData")

year.sum <- new_data %>% 
  group_by(year) %>%  # group by year
  summarize(mean(prob_fit, na.rm=TRUE))

year.sum

# summarize data by month 

month.sum <- new_data %>% 
  group_by(month, day) %>%  # group by year
  summarize(mean(prob_fit, na.rm=TRUE))

month.sum

## mean is not tell us much approx range 0.22-0.26

## mean per month/jd
mon_jd_sum <- new_data %>% 
  group_by(year, jd) %>%  # group by year
  summarize(mean_mon_jd = mean(prob_fit, na.rm=TRUE)) 
  
mon_jd_sum
min(mon_jd_sum$mean_mon_jd) ## 0.03740324

## mean per month/day
mon_day_sum <- new_data %>% 
  group_by(month, day) %>%  # group by year
  summarize(mean_mon_day = mean(prob_fit, na.rm=TRUE)) 
mon_day_sum

### plot each month in facet wrap

new_data %>%
  ggplot(aes(x = day, y = prob_fit)) +
  geom_line(stat = "identity", col() = "darkorchid4") +
  facet_wrap(~ month, ncol = 3) +
  labs(title = "Habitat suitability by Depth",
       subtitle = "Data plotted by month",
       y = "Probability",
       x = "Month") + theme_bw(base_size = 15)

mean(new_data$prob_fit) ## 0.2386895

## look at min and max

## min per month/day
mon_day_min <- new_data %>% 
  group_by(month, day) %>%  # group by year
  summarize(min_mon_day = min(prob_fit, na.rm=TRUE)) 
mon_day_min

## min per month/day
mon_day_max <- new_data %>% 
  group_by(month, day) %>%  # group by year
  summarize(max_mon_day = max(prob_fit, na.rm=TRUE)) 
mon_day_max

# combine summary stats
sum_stats <- cbind(mon_day_sum, mon_day_min, mon_day_max)
sum_stats <- sum_stats[,c(1:3,6,9)]
sum_stats
# library(reshape2)
# sum_statsx <- melt(sum_stats, id=c("month", "day"))
## plot
sum_stats
sum_stats %>%
  ggplot(aes(x = day)) +
  geom_line(aes( y = max_mon_day), color = "darkorchid4") +
  geom_line(aes( y = min_mon_day), color = "darkblue") +
  geom_line(aes( y = mean_mon_day), color = "darkgreen") +
  geom_hline(yintercept=mean(new_data$prob_fit), linetype="dashed", color="red")+
  facet_wrap(~ month, ncol = 3) +
  labs(title = "Habitat suitability by Depth",
       subtitle = "Data plotted by month",
       y = "Probability",
       x = "Month") + theme_bw(base_size = 15)

### 3 months
sum_stats_nov <- filter(sum_stats, month==11)

sum_stats_nov%>%
  ggplot(aes(x = day)) +
  geom_line(aes( y = max_mon_day), color = "darkorchid4") +
  geom_line(aes( y = min_mon_day), color = "darkblue") +
  geom_line(aes( y = mean_mon_day), color = "darkgreen") +
  geom_hline(yintercept=mean(new_data$prob_fit), linetype="dashed", color="red")+
  # facet_wrap(~ month, ncol = 3) +
  labs(title = "Habitat suitability by Depth",
       subtitle = "Data plotted by day (November)",
       y = "Probability",
       x = "Month") + theme_bw(base_size = 15)

## now look at hourly 
head(new_data)

new_data_2011 <- filter(new_data, year==2011)

new_data_2011

## plot each month
new_data_2011 %>%
  ggplot(aes(x = day)) +
  geom_line(aes( y = prob_fit), color = "darkblue") +
  geom_hline(yintercept=mean(new_data$prob_fit), linetype="dashed", color="red")+
  facet_wrap(~ month, ncol = 3) +
  labs(title = "Habitat suitability by Depth",
       subtitle = "Data plotted by month (2011)",
       y = "Probability",
       x = "Day") + theme_bw(base_size = 15)

## take a month under the mean - november

new_data_2011_nov <- filter(new_data_2011, month==11 & day > 20)
new_data_2011_nov

## plot each month
new_data_2011_nov %>%
  ggplot(aes(x = hour)) +
  geom_line(aes( y = prob_fit), color = "darkblue") +
  geom_hline(yintercept=mean(new_data$prob_fit), linetype="dashed", color="red")+
  facet_wrap(~ day, ncol = 3) +
  labs(title = "Habitat suitability by Depth",
       subtitle = "Data plotted by day (November 2011)",
       y = "Probability",
       x = "Hour") + theme_bw(base_size = 15)

## 29th november
new_data_2011_nov29 <- filter(new_data_2011, month==11 & day == 29)
new_data_2011_nov29

new_data_2011_nov29 %>%
  ggplot(aes(x = hour)) +
  geom_line(aes( y = prob_fit), color = "darkblue") +
  geom_hline(yintercept=mean(new_data$prob_fit), linetype="dashed", color="red")+
  labs(title = "Habitat suitability by Depth",
       subtitle = "Data plotted by day (November 29th 2011)",
       y = "Probability",
       x = "Hour") + theme_bw(base_size = 15)

### number of events of low probability

head(new_data)
#workflow

## time series per year 
head(new_data)


## plot each month
new_data %>%
  ggplot(aes(x = month)) +
  geom_line(aes( y = prob_fit), color = "darkblue") +
  geom_hline(yintercept=mean(new_data$prob_fit), linetype="dashed", color="red")+
  facet_wrap(~ year, ncol = 3) +
  labs(title = "Habitat suitability by Depth",
       subtitle = "Data plotted by year (2010-2017)",
       y = "Probability",
       x = "Month") + theme_bw(base_size = 15)

