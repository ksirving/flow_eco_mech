## Velocity curves - model and application
## adult & Juvenile

## produces probability curves for velocity, and application to sample node data (time series) for adult and Juvenile
## also data distributions 

## packages

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)

## upload data

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## adult
ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous_updated.csv")
# dim(ad_vel_con) ## 343
# unique(ad_vel_con$Dataset) ## 2 datasets


# Adult Data distribution -------------------------------------------------------

## data distribution by dataset

all_vel <- ad_vel_con

vel_freq <- all_vel %>% 
  uncount(Abundance)
vel_freq <- na.omit(vel_freq)

tx <- vel_freq$Dataset == "Saiki"
wx <- vel_freq$Dataset == "Wulff"

vel_freq$Dataset_num[tx] <- 1
vel_freq$Dataset_num[wx] <- 2

attach(vel_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Saiki", "Wulff"))
data.f
# plot densities
sm.density.compare(as.vector(Velocity), Dataset_num, xlab="Velocity m/s")
title(main="Adult Velocity Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


# Adult model build -------------------------------------------------------

## need to predict outside of the velocity range from the curve. upload hydraulic data first to get the values. 
## on the upper end of the curve e.g. 2.0 m/s so will be a low probability anyway

## upload hydraulic data
hydraul <- read.csv("input_data/demo_ts_F57C.csv")

## select columns
names(hydraul)
hyd_vel <- hydraul[,c(1:3,8)]
colnames(hyd_vel)[4] <-"vel_ft"
hyd_vel$date_num <- seq(1,length(hyd_vel$DateTime), 1)

## convert unit from feet to meters
hyd_vel$vel_cm <- (hyd_vel$vel_ft*0.3048)

# check data
unique(vel_freq$Dataset)
mean(vel_freq$Velocity) ## 0.6121954
dim(vel_freq) ## 1167

## probability curve
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=1000)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw velocity values
xfit_r <- seq(min(vel_freq$Velocity), max(hyd_vel$vel_cm), length=1000)
## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Velocity (m/s)', ylab='Probability', type='l', col='red', main = "Adult/Velocity: Probability curve" )
par(new=TRUE)
#add these now with axis
axis(2, at=pretty(range(yfit)))

## data frame with probabilities and depth

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("velocity_fit", "prob_fit")


# Combine with hydraulic data -------------------------------------------

range(hyd_vel$vel_cm) ## 0.000000 2.091465

## round the depths - don't need the high resolution

hyd_vel$vel_cm_round <- round(hyd_vel$vel_cm, digits=2 )
fitdata$vel_fit_round <- round(fitdata$velocity_fit, digits=2)


plot(hyd_dep$date_num, hyd_vel$vel_cm_round, type="n")
lines(hyd_dep$date_num, hyd_vel$vel_cm_round)

## merge node data and probabilities
all_data <- merge(hyd_vel, fitdata, by.x="vel_cm_round", by.y="vel_fit_round", all=T)

save(all_data, file="output_data/F2_F57C_adult_velocity_discharge_probability_time_series_all_columns.RData")
names(all_data)
## keep columns dpeth, datetime, Q date_num & prob_fit
all_data <- all_data[, c(1,2,3,6,7,9)]
# sum(is.na(all_data))
all_data <- na.omit(all_data)

## remove duplicate date_num (date time) and order

all_data <- all_data[!duplicated(all_data$date_num),]
new_data <- all_data[order(all_data$date_num),]

save(new_data, file="output_data/F2_F57C_adult_velocity_discharge_probability_time_series_red_columns.RData")


# format probability time series ------------------------------------------

## look at data using lubridate etc

names(new_data)

new_data$DateTime<-as.POSIXct(new_data$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "America/Los_Angeles")

## create year, month, day and hour columns

new_data <- new_data %>%
  mutate(month = month(DateTime))

new_data <- new_data %>%
  mutate(year = year(DateTime))

new_data <- new_data %>%
  mutate(day = day(DateTime))

new_data <- new_data %>%
  mutate(hour = hour(DateTime))


head(new_data)

save(new_data, file="output_data/F2_F57C_velocity_adult_discharge_probs_2010_2017_TS.RData")


# probability as a function of discharge -----------------------------------

head(new_data)

## plot
range(new_data$Q) ## 0.00 998.84

## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(new_data$prob_fit ~ new_data$Q)

## function for each probability
newy2a <- 0.2
newx2a <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2a,
                  interval = c(0, 500))$root

newy2b <- 0.2
newx2b <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2b,
                  interval = c(500, 1000))$root

newy1a <- 0.1
newx1a <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy1a,
                  interval = c(0, 500))$root

newy1b <- 0.3
newx1b <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy1b,
                  interval = c(500, 1000))$root

newy3a <- 0.3
newx3a <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3,
                 interval = c(0, 1000))$root

# newy3b <- 0.3
# newx3b <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3b,
#                   interval = c(500, 1000))$root


plot(new_data$Q, new_data$prob_fit, type="n", main = "Adult/Velocity: Probability according to Q", xlab="Q (cfs)", ylab="Probability")
lines(spl, col="black")
points(newx2, newy2, col="red", pch=19) # 0.2
points(newx1, newy1, col="green", pch=19) # 0.1
points(newx3a, newy3a, col="blue", pch=19) # 0.3 - lower limit
points(newx3b, newy3b, col="blue", pch=19) # 0.3 - upper limit

### plot discharge over time

# create year_month column       
new_datax <- new_data %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_datax)

# discharge time series plots with probability lines ----------------------

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

## plot each season of year - just winter & summer for now

winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months
new_datax_2016_winter <- filter(new_datax_2016, month %in% winter)
new_datax_2016_summer <- filter(new_datax_2016, month %in% summer)

ggplot(new_datax_2016_summer) +
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

ggplot(new_datax_2016_winter) +
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


# Statistics on Q probability - total amount of time ----------------------

## percentage of time below threshold - annual
length(new_datax_2016$DateTime)
total_0.2 <- sum(new_datax_2016$Q >= newx2)/length(new_datax_2016$DateTime)*100
# 82.09077 %
total_0.1 <- sum(new_datax_2016$Q >= newx1)/length(new_datax_2016$DateTime)*100
# 99.49831
total_0.3 <- sum(new_datax_2016$Q >= newx3a & new_datax_2016$Q <= newx3b)/length(new_datax_2016$DateTime)*100
##3.966865

## percentage of time below threshold - summer
length(new_datax_2016_summer$DateTime)
total_summer_0.2 <- sum(new_datax_2016_summer$Q >= newx2)/length(new_datax_2016_summer$DateTime)*100
# 74.09925 %
total_summer_0.1 <- sum(new_datax_2016_summer$Q >= newx1)/length(new_datax_2016_summer$DateTime)*100
# 100
total_summer_0.3 <-sum(new_datax_2016_summer$Q >= newx3a & new_datax_2016_summer$Q <= newx3b)/length(new_datax_2016_summer$DateTime)*100
## 0.9743938

length(new_datax_2016_winter$DateTime)
total_winter_0.2 <- sum(new_datax_2016_winter$Q >= newx2)/length(new_datax_2016_winter$DateTime)*100
# 90.57239 %
total_summer_0.1 <- sum(new_datax_2016_winter$Q >= newx1)/length(new_datax_2016_winter$DateTime)*100
# 98.96585
total_summer_0.3 <- sum(new_datax_2016_winter$Q >= newx3a & new_datax_2016_winter$Q <= newx3b)/length(new_datax_2016_winter$DateTime)*100
## 7.142857

# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month

load(file="output_data/F1_F57C_depth_adult_discharge_probs_2010_2017_TS.RData")

# all columns based on different probabilities
new_data <- new_data %>%
  group_by(month, year, ID = data.table::rleid(Q >= newx1)) %>%
  mutate(probability_0.1 = if_else(Q >= newx1, row_number(), 0L)) %>% 
  ungroup() %>%
  group_by(month, year, ID = data.table::rleid(Q >= newx2)) %>%
  mutate(probability_0.2 = if_else(Q >= newx2, row_number(), 0L)) %>% 
  ungroup() %>%
  group_by(month, year, ID = data.table::rleid(Q >= newx3a & Q <= newx3a)) %>%
  mutate(probability_0.3 = if_else(Q >= newx3a & Q <= newx3a, row_number(), 0L)) #%>% 

## need a way to count max, until it restarts from zero, then count the other max values
new_data %>% group_by(month, year) %>% summarise(sum(probability_0.1==24))

# d1 %>%   group_by(UserID, ItemName, days = c(0, cumsum(diff(Date) != 1))) %>%   
#   summarise(ConsecutiveDays=n(),             StartDate = first(Date))%>%   select(-days)

## melt data frame so that each probability column are all in one row 
## select only columns needed
names(new_data)
head(new_data)
new_datax <- new_data[, c(3,6,7,10,11:13)] # all probs

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID", "month", "year", "Q"))
melt_data <- rename(melt_data, Probability_Threshold = variable)
head(melt_data)

melt_data_2016 <- filter(melt_data, year==2016)
melt_data_2016

## groups data by year, month and ID and reports maximum number of consecutive hours - 
## hours are also consecuative, not number of hours under probability
conseq_ID20 <- melt_data_2016 %>% 
  group_by(ID, month, year, Probability_Threshold) %>%
  summarise(max(value)) %>%
  rename(max_Consec_Hours=`max(value)`) %>%
  mutate(max_Consec_Hours/24) %>%
  rename(max_Consec_Days=`max_Consec_Hours/24`) 

conseq_ID20

# no of days/hours under low prob

## count number of events - below 20% and for 1 day - per month as well as number of times per month (hours) that the prob goes below zero
total_days <- conseq_ID20 %>%
  group_by(month, year, Probability_Threshold) %>%
  # select(-max_Consec_Hours) %>%
  summarize(n_days = sum(max_Consec_Days)) %>% ## total number of days per month (not always consequtive)
  arrange(year)

# create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F) 

## convert month year to date format
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)

##  plot - number of days - 

ggplot(total_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days under discharge limit",
       y = "Number of events per Month",
       x = "Month") #+ theme_bw(base_size = 15)





