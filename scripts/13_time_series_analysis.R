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


# Formnat time series data ------------------------------------------------


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
head(hyd_dep)

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
save(all_data, file="output_data/13_adult_depth_discharge_probability_time_series")


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

save(new_data, file="output_data/13_depth_discharge_probs_2010_2017_TS.RData")


# summary statistics ------------------------------------------------------


# summarize data by year 

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


#  low probability events -------------------------------------------------

load(file="output_data/13_depth_probs_2010_2017_TS.RData") # new_data

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



# probability as a function of discharge -----------------------------------

load(file="output_data/13_adult_depth_discharge_probability_time_series")
head(all_data)
names(all_data)
## extract Q, not depth
all_data <- all_data[, c(1,2,3,9, 7)]

## remove duplicated date/times
all_data <- all_data[!duplicated(all_data$date_num),]
## plot
range(all_data$Q) ## 0.40 998.84
## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(all_data$prob_fit ~ all_data$Q)

newy2 <- 0.2
newx2 <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2,
                interval = c(0, 1000))$root

newy1 <- 0.1
newx1 <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy1,
                 interval = c(0, 1000))$root

newy3a <- 0.3
newx3a <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3a,
                 interval = c(0, 500))$root

newy3b <- 0.3
newx3b <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3b,
                  interval = c(500, 1000))$root
newx3a
newx3b
newx2
newx1

plot(all_data$Q, all_data$prob_fit, type="n", main = "Adult/Depth: Probability according to Q", xlab="Q (cfs I assume)", ylab="Probability")
lines(spl, col="black")
points(newx2, newy2, col="red", pch=19)
points(newx1, newy1, col="green", pch=19)
points(newx3a, newy3, col="blue", pch=19)
points(newx3b, newy3, col="blue", pch=19)

### plot discharge over time
load(file="output_data/13_depth_discharge_probs_2010_2017_TS.RData") ## new_data
head(new_data)


# create year_month column       
new_datax <- new_data %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_datax)

##  plot time series of discharge - 0.2 prob line

ggplot(new_datax) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2, linetype="dashed", color="red")+
  facet_wrap(~year, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

##  plot time series of discharge - subset to one year
new_datax_2016 <- filter(new_datax, year==2016)

ggplot(new_datax_2016) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2, linetype="dashed", color="red")+
  geom_hline(yintercept=newx1, linetype="dashed", color="green")+
  geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

## percentage of time below threshold
length(new_datax_2016$DateTime)
sum(new_datax_2016$Q >= newx2)/length(new_datax_2016$DateTime)*100
# 82.09077 %
sum(new_datax_2016$Q >= newx1)/length(new_datax_2016$DateTime)*100
# 99.49831
sum(new_datax_2016$Q >= newx3a & new_datax_2016$Q <= newx3b)/length(new_datax_2016$DateTime)*100
##3.966865


# 4.118539
sum(new_datax_2016$Q <= newx3b)/length(new_datax_2016$DateTime)*100
# 99.84833

# proportion of time under certain thresholds -----------------------------

## x axis = proportion of time
## y axis = discharge

load(file="output_data/13_depth_probs_2010_2017_TS.RData")
head(new_data)
mean(new_data$prob_fit) ## 0.2386895

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
  group_by(ID = data.table::rleid(prob_fit < 0.2386895)) %>%
  mutate(probability_mean = if_else(prob_fit < 0.2386895, n(), 0L)) %>% 
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

range(new_data$probability_0.1)
range(new_data$probability_0.2)
sum(new_data$total_hours_in_the_month)

sum(new_data$probability_0.1)
sum(new_data$probability_0.2)
dim(new_data) ## 60222 

names(new_data)
head(new_data)

## melt data frame so that each probability column are all in one row 
## select only columns needed
new_datax <- new_data[, c(5,6,10,11, 12:18)]

## melt
melt_data<-reshape2::melt(new_datax, id=c( "month", "year", "total_hours_in_the_month", "ID"))
melt_data <- rename(melt_data, Probability_Threshold = variable)
melt_data[1:10, ]

## change value to binary to count total
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

## plots lines
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

## plot box plots of monthly values per probability
## boxplots = contain % values per month

ggplot(total_hours, aes(x=Probability_Threshold, y=percent_hours)) + 
  geom_boxplot() +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Proportion of time spent under probability thresholds",
       y = "Events (%)",
       x = "Probability Threshold") 

unique(total_hours$Probability_Threshold)
ggplot(data = total_hours) + geom_boxplot(aes(x=Probability_Threshold, y=percent_hours))+
  geom_boxplot(data=total_hours[total_hours$Probability_Threshold=="probability_mean",],
               aes(x=Probability_Threshold, y=percent_hours),fill="red") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Proportion of time spent under probability thresholds",
       y = "Events (%)",
       x = "Probability Threshold") 

## save data frame

save(total_hours, file="output_data/13_percentage_time_prob_thresholds.RData")


# proportion of time and mean discharge -------------------------

## work flow
# monthly percentage of time, 
# mean discharge per month
# over month/season

load(file="output_data/13_depth_discharge_probs_2010_2017_TS.RData") ## new_data
head(new_data)
mean(new_data$prob_fit) ## 0.2386895
names(new_data)


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
  group_by(ID = data.table::rleid(prob_fit < 0.2386895)) %>%
  mutate(probability_mean = if_else(prob_fit < 0.2386895, n(), 0L)) %>% 
  ungroup() %>%
  group_by(ID = data.table::rleid(prob_fit < 0.25)) %>%
  mutate(probability_0.25 = if_else(prob_fit < 0.25, n(), 0L)) %>% 
  ungroup() %>%
  group_by(ID = data.table::rleid(prob_fit < 0.3)) %>%
  mutate(probability_0.3 = if_else(prob_fit < 0.3, n(), 0L)) %>% 
  ungroup() %>%
  group_by(ID = data.table::rleid(prob_fit < 0.35)) %>%
  mutate(probability_0.35 = if_else(prob_fit < 0.35, n(), 0L)) #%>%

names(new_data)

## total percentage of time

## melt data frame so that each probability column are all in one row 
## select only columns needed
new_datax <- new_data[, c(3,6,7,11, 12, 13:19)]
# head(new_datax)
# mean(new_datax$Q)
## melt
melt_data<-reshape2::melt(new_datax, id=c( "month", "year", "total_hours_in_the_month", "ID", "Q"))
melt_data <- rename(melt_data, Probability_Threshold = variable)
melt_data[1:10, ]
melt_data$value <- as.numeric(as.character(melt_data$value))
melt_data$value_bin <- ifelse(melt_data$value >0, 1, 0)

## groups data by year, month and ID and reports total number of hours - (2 hours or more) & mean Q per month
## hours are not consecutive,so total number of hours under probability

conseq_ID20 <- melt_data %>% 
  group_by(month, year, Probability_Threshold,total_hours_in_the_month ) %>%
  summarize(total_Hours = sum(value_bin), 
            mean_Q = mean(Q)) %>%
  # rename(total_Hours=`sum()`) %>%
  mutate(percent_hours = total_Hours/total_hours_in_the_month*100) %>%
  arrange(year)

conseq_ID20

# create year_month column       
total_hours <- ungroup(conseq_ID20) %>%
  unite(month_year, year:month, sep="-", remove=F) 
total_hours
## convert month year to date format
total_hours$month_year <-  zoo::as.yearmon(total_hours$month_year)

# total_hours$Probability_Threshold == "less_than_40_percent"
range(total_hours$percent_hours)

ylim.prim <- c(0, 100)   # in this example, percentage
ylim.sec <- c(0, 1000)    # in this example, discharge

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

ggplot(total_hours, aes(x=month_year, y=percent_hours))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  geom_line(aes(y = a + mean_Q*b), color = "red") +
  scale_y_continuous("Events (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Discharge")) +
  # scale_x_continuous("Month", breaks = 1:12) +
  ggtitle("frequency of event and dischrge)")  +
  facet_wrap(~Probability_Threshold, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Frequency of Events per threshold",
       y = "Events (%)",
       x = "Month") #+ theme_bw(base_size = 15)

## just one probability threshold at one time

## subset
names(total_hours)

total_hours_0.2 <- filter(total_hours, Probability_Threshold == "probability_0.2")
head(total_hours_0.2)

range(total_hours$mean_Q) # 41.50322 217.47795

ylim.prim <- c(0, 100)   # in this example, percentage
ylim.sec <- c(0, 220)    # in this example, discharge

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

ggplot(total_hours_0.2, aes(x=month, y=percent_hours))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  geom_line(aes(y = a + mean_Q*b), color = "red") +
  scale_y_continuous("Events (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Discharge")) +
  theme(axis.text.x = element_text(vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(total_hours$month), labels=format(total_hours$month)) +
  # scale_x_continuous("Month-Year", breaks = 1:84) +
  facet_wrap(~year, scales="free_x", nrow=4) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Frequency of probability < 0.2 events and discharge",
       y = "Events (%)",
       x = "Month") #+ theme_bw(base_size = 15)



# Probability & discharge -------------------------------------------------

load(file="output_data/13_depth_discharge_probs_2010_2017_TS.RData") ## new_data
head(new_data)




# Velocity ----------------------------------------------------------------


## velocity

ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous.csv")
ad_vel_con <- subset(ad_vel_con, Dataset !="Thompson")
unique(ad_vel_con$Dataset)


vel_freq <- ad_vel_con %>% 
  uncount(Abundance)
vel_freq <- na.omit(vel_freq)
mean(vel_freq$Velocity) ## 0.504274
min(vel_freq$Velocity)
dim(vel_freq) ## 854
head(vel_freq)
unique(vel_freq$Dataset)


## scaled data
vel_freq$Scaled_Velocity <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Velocity
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=10000)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

##  raw depth values
xfit_r <- seq(min(vel_freq$Velocity), max(vel_freq$Velocity), length=10000)

## data frame with probabilities and depth

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("velocity_fit", "prob_fit")

xfit_r

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
## select columns
names(hydraul)
hyd_vel <- hydraul[,c(1:3,8)]
colnames(hyd_vel)[4] <-"vel_ft"
hyd_vel$date_num <- seq(1,length(hyd_vel$DateTime), 1)
## convert unit from feet to meters
hyd_vel$vel_cm <- (hyd_vel$vel_ft*0.3048)

range(hyd_vel$vel_cm)

head(hyd_vel)
dim(hyd_vel)
dim(fitdata)
## round the depths - don't need the high resolution
unique(hyd_vel$vel_cm_round)
hyd_vel$vel_cm_round <- round(hyd_vel$vel_cm, digits=2 )
fitdata$vel_fit_round <- round(fitdata$velocity_fit, digits=2)
tail(fitdata$vel_fit_round)
hyd_vel$vel_cm_round
plot(hyd_dep$date_num, hyd_vel$vel_cm_round, type="n")
lines(hyd_dep$date_num, hyd_vel$vel_cm_round)

## merge node data and probabilities
all_data <- merge(hyd_vel, fitdata, by.x="vel_cm_round", by.y="vel_fit_round", all=T)
head(all_data)
dim(all_data)
tail(all_data)
unique(all_data$vel_cm_round)

all_data <- all_data[, c(1,2,3,6, 7,9)]
sum(is.na(all_data)) # 12683
range(all_data$vel_fit_round)
## remove duplicate date_num (date time) and order

all_data <- all_data[!duplicated(all_data$date_num),]
new_data <- all_data[order(all_data$date_num),]

range(all_data$vel_cm)
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


