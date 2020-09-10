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

all_vel <- ad_vel_con

vel_freq <- all_vel %>% 
  uncount(Abundance)
vel_freq <- na.omit(vel_freq)

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

## need to predict outside of the velocity range from the curve. 

# check data
unique(vel_freq$Dataset)
mean(vel_freq$Velocity) ## 0.6121954
dim(vel_freq) ## 1167
range(vel_freq$Velocity)

## probability curve
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=1000)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw velocity values
xfit_r <- seq(min(vel_freq$Velocity), max(vel_freq$Velocity), length=1000)
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

## upload hydraulic data
hydraul <- read.csv("input_data/demo_ts_F57C.csv")

## select columns
names(hydraul)
hyd_vel <- hydraul[,c(1:3,8)]
colnames(hyd_vel)[4] <-"vel_ft"
hyd_vel$date_num <- seq(1,length(hyd_vel$DateTime), 1)
plot(hyd_vel$Q, hyd_vel$vel_ms,  main = "F57C: Velocity ~ Q", xlab="Q (cfs)", ylab="Velocity (m/s)")

## convert unit from feet to meters
hyd_vel$vel_ms <- (hyd_vel$vel_ft*0.3048)

range(hyd_vel$vel_ms) ## 0.000000 2.091465
range(fitdata$velocity_fit) ## 0.00 1.73
## round the values - don't need the high resolution

hyd_vel$vel_ms_round <- round(hyd_vel$vel_ms, digits=2 )
fitdata$vel_fit_round <- round(fitdata$velocity_fit, digits=2)


plot(hyd_vel$date_num, hyd_vel$vel_ms_round, type="n")
lines(hyd_vel$date_num, hyd_vel$vel_ms_round)

## merge node data and probabilities
all_data <- merge(hyd_vel, fitdata, by.x="vel_ms_round", by.y="vel_fit_round", all=T)
sum(is.na(all_data)) ## 3562
names(all_data)
## missing values - anything over 1.73 as not in suitability curve
## replace NA of probability with max probability of dataset

all_data[which(all_data$vel_ms_round > max(na.omit(all_data$velocity_fit))),"prob_fit"] <- min(na.omit(all_data$prob_fit))
sum(is.na(all_data)) # 18581

## remove rows with probabilities above the max hydraulic value
all_data <- filter(all_data, vel_ms_round <= max(hyd_vel$vel_ms_round))
sum(is.na(all_data)) # 2675 - no NAs in prob_fit - all present
# nas <- which(is.na(all_data$velocity_fit))
# nas <- which(is.na(all_data$DateTime))
# nas

save(all_data, file="output_data/F2_F57C_adult_velocity_discharge_probability_time_series_all_columns.RData")
names(all_data)
## keep columns velocity, datetime, Q date_num & prob_fit
all_data <- all_data[, c(1,2,3,6,7,9)]
sum(is.na(all_data)) # 1192
all_data <- na.omit(all_data) ## remaining NAs are dattime etc so can remove

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
load(file="output_data/F2_F57C_velocity_adult_discharge_probs_2010_2017_TS.RData")
head(new_data)

## plot
range(new_data$Q) ## 0.00 998.84
range(new_data$prob_fit) ## 0.0006865544 0.3989288644

## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(new_data$prob_fit ~ new_data$Q)

## find peak of prob v Q

## find peak of prob v Q

peak <- filter(new_data, prob_fit == max(prob_fit)) #%>%
peakQ <- select(peak, Q)
peakQ  <- peakQ[1,1]
peakQ ## 1.98

## function for each probability

newy1a <- 0.1
newx1a <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy1a,
                      interval = c(min(new_data$Q), peakQ))$root, silent=T)
## if no value, return an NA
newx1a <- ifelse(class(newx1a) == "try-error",  NA, newx1a)

newy1b <- 0.1
newx1b <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy1b,
                      interval = c(peakQ, max(new_data$Q)))$root, silent=T)
## if no value, return an NA
newx1b <- ifelse(class(newx1b) == "try-error",  NA, newx1b)

newy2a <- 0.2
newx2a <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2a,
                      interval = c(min(new_data$Q), peakQ))$root, silent=T)
newx2a <- ifelse(class(newx2a) == "try-error",  NA, newx2a)

newy2b <- 0.2
newx2b <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2b, 
                      interval = c(peakQ, max(new_data$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx2b <- ifelse(class(newx2b) == "try-error",  NA, newx2b)

newy3a <- 0.3
newx3a <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3a,
                      interval = c(min(new_data$Q), peakQ))$root, silent=T)
newx3a <- ifelse(class(newx3a) == "try-error",  NA, newx3a)

newy3b <- 0.3
newx3b <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3b,
                      interval = c(peakQ, max(new_data$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx3b <- ifelse(class(newx3b) == "try-error",  NA, newx3b)

# thresholds <- as.data.frame(matrix(ncol=2, nrow=6))
# colnames(thresholds) <- c("newy", "newx")
# thresholds$newy <- c("0.1", "0.1", "0.2", "0.2", "0.3", "0.3")
# thresholds$newx <- c(newx1a,newx1b, newx2a,newx2b, newx3a,newx3b)
# thresholds

plot(new_data$Q, new_data$prob_fit, type="n", main = "Adult/Velocity: Probability according to Q", xlab="Q (cfs)", ylab="Probability")
lines(spl, col="black")
points(newx2a, newy2a, col="red", pch=19) # 0.2
points(newx2b, newy2b, col="red", pch=19) # 0.2
points(newx1a, newy1a, col="green", pch=19) # 0.1
points(newx1b, newy1b, col="green", pch=19) # 0.1
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
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
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
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # # geom_hline(yintercept=newx1b, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # # facet_wrap(~month, scales="free_x", nrow=4) +
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
  geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1b, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

ggplot(new_datax_2016_winter) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
  geom_hline(yintercept=newx1b, linetype="dashed", color="green")+
  geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)


# Make dataframe for time stats -------------------------------------------


## make dataframe for all years 

head(new_datax)
names(new_datax)

## define seasons
winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months

new_datax <- new_datax %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )

## produces percentage of time for each year and season within year for each threshold

time_stats <- new_datax %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Medium = if(is.na(newx2b)){
    sum(Q >= newx2a)/length(DateTime)*100
  } else {
    sum(Q >= newx2a & Q <= newx2b)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low = if(is.na(newx1b)){
    sum(Q >= newx1a)/length(DateTime)*100
  } else {
    if (is.na(newx1a)) {
      sum(Q <= newx1b)/length(DateTime)*100
    } else {
      sum(Q >= newx1a & Q <= newx1b)/length(DateTime)*100
  }}) %>%
  
  dplyr::mutate(High = if(is.na(newx3b)){
    sum(Q >= newx3a)/length(DateTime)*100
  } else {
    sum(Q >= newx3a & Q <= newx3b)/length(DateTime)*100
  })  %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Medium.Seasonal = if(is.na(newx2b)){
    sum(Q >= newx2a)/length(DateTime)*100
  } else {
    sum(Q >= newx2a & Q <= newx2b)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low.Seasonal = if(is.na(newx1b)){
    sum(Q >= newx1a)/length(DateTime)*100
  } else {
    if (is.na(newx1a)) {
      sum(Q <= newx1b)/length(DateTime)*100
    } else {
      sum(Q >= newx1a & Q <= newx1b)/length(DateTime)*100
    }}) %>%
  dplyr::mutate(High.Seasonal = if(is.na(newx3b)){
    sum(nQ >= newx3a)/length(DateTime)*100
  } else {
    sum(Q >= newx3a & Q <= newx3b)/length(DateTime)*100
  }) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal)


time_stats

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season"))
melt_time <- rename(melt_time, Probability_Threshold = variable)

## subset annual stats
ann_stats <- unique(melt_time$Probability_Threshold)[1:3]
melt_time_ann <- melt_time %>% filter(Probability_Threshold %in% ann_stats ) %>%
  select(-season) %>% distinct()
head(melt_time_ann)
# melt_time_ann <- na.omit(melt_time_ann)
# melt_time_ann

## subset seasonal stats
seas_stats <- unique(melt_time$Probability_Threshold)[4:6]
melt_time_seas <- filter(melt_time, Probability_Threshold %in% seas_stats )
# melt_time_seas<- na.omit(melt_time_seas)
head(melt_time_seas)
melt_time_seas


## plot for annual stats - need probs in order
ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  scale_y_continuous("Percentage of time", limits= c(0,100)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time within discharge limit in relation to Velocity (Annual)",
       # y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

## plot for winter stats - need probs in order

melt_time_winter <- filter(melt_time_seas, season == "winter")
melt_time_winter

ggplot(melt_time_winter, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue")) +
  scale_y_continuous("Percentage of time", limits= c(0,100)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time within discharge limit in relation to Velocity (Winter)",
       # y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

## plot for summer stats - need probs in order

melt_time_summer <- filter(melt_time_seas, season == "summer")

ggplot(melt_time_summer, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue")) +
  scale_y_continuous("Percentage of time", limits= c(0,100)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time within discharge limit in relation to Velocity (Summer)",
       # y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month



library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)


load(file="output_data/F2_F57C_velocity_adult_discharge_probs_2010_2017_TS.RData")

## define thresholds again
# range(new_data$Q) ## 0.00 998.845 

## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(new_data$prob_fit ~ new_data$Q)

## find peak of prob v Q

peak <- filter(new_data, prob_fit == max(prob_fit)) #%>%
peakQ <- select(peak, Q)
peakQ  <- peakQ[1,1]
peakQ ## 1.98

## function for each probability
newy1a <- 0.1
newx1a <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy1a,
                      interval = c(min(new_data$Q), peakQ))$root, silent=T)
## if no value, return an NA
newx1a <- ifelse(class(newx1a) == "try-error",  NA, newx1a)

newy1b <- 0.1
newx1b <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy1b,
                      interval = c(peakQ, max(new_data$Q)))$root, silent=T)
## if no value, return an NA
newx1b <- ifelse(class(newx1b) == "try-error",  NA, newx1b)

newy2a <- 0.2
newx2a <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2a,
                      interval = c(min(new_data$Q), peakQ))$root, silent=T)
newx2a <- ifelse(class(newx2a) == "try-error",  NA, newx2a)

newy2b <- 0.2
newx2b <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2b, 
                      interval = c(peakQ, max(new_data$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx2b <- ifelse(class(newx2b) == "try-error",  NA, newx2b)

newy3a <- 0.3
newx3a <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3a,
                      interval = c(min(new_data$Q), peakQ))$root, silent=T)
newx3a <- ifelse(class(newx3a) == "try-error",  NA, newx3a)

newy3b <- 0.3
newx3b <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3b,
                      interval = c(peakQ, max(new_data$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx3b <- ifelse(class(newx3b) == "try-error",  NA, newx3b)


# all columns based on different probabilities
## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_data <- arrange(new_data, date_num)

nas <- ifelse(!is.na(newx1a) && !is.na(newx1b), print("Both"), print("one"))

if(nas == "Both") {
  new_data <- new_data %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1a & Q <= newx1b)) %>%
    mutate(Low = if_else(Q >= newx1a & Q <= newx1b, row_number(), 0L))
} else if (is.na(newx1a)) {
  new_data <- new_data %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1b)) %>%
    mutate(Low = if_else(Q <= newx1b, row_number(), 0L)) 
} else {
  new_data <- new_data %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1a)) %>%
    mutate(Low = if_else(Q >= newx1a, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx2a) && !is.na(newx2b), print("Both"), print("one"))

if(nas == "Both") {
  new_data <- new_data %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2a & Q <= newx2b)) %>%
    mutate(Medium = if_else(Q >= newx2a & Q <= newx2b, row_number(), 0L))
} else if (is.na(newx2a)) {
  new_data <- new_data %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2b)) %>%
    mutate(Medium = if_else(Q <= newx2b, row_number(), 0L)) 
} else {
  new_data <- new_data %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2a)) %>%
    mutate(Medium = if_else(Q >= newx2a, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx3a) && !is.na(newx3b), print("Both"), print("one"))

if(nas == "Both") {
  new_data <- new_data %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3a & Q <= newx3b)) %>%
    mutate(High = if_else(Q >= newx3a & Q <= newx3b, row_number(), 0L))
} else if (is.na(newx3a)) {
  new_data <- new_data %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3b)) %>%
    mutate(High = if_else(Q <= newx3b, row_number(), 0L)) 
} else {
  new_data <- new_data %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3a)) %>%
    mutate(High = if_else(Q >= newx2a, row_number(), 0L)) 
}
head(new_data)
names(new_data)

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_datax <- select(new_data, c(Q, month, year, day, ID01, Low, ID02, Medium, ID03, High) )# all probs
names(new_datax)

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "ID03", "day", "month", "year", "Q"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)


## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Probability_Threshold == "Low") %>% 
  group_by(ID01, day, month, year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 23, 1, 0)) # %>%
# total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year) %>%
  summarise(days_per_month_low = sum(n_days_low))

# total_days_per_month01

total_days02 <- melt_data %>% 
  filter(Probability_Threshold == "Medium") %>% 
  group_by(ID02, day, month, year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_medium = ifelse(n_hours >= 23, 1, 0)) # %>%

total_days_per_month02 <- total_days02 %>%
  group_by(month, year) %>%
  summarise(days_per_month_medium = sum(n_days_medium))

# total_days_per_month02

total_days03 <- melt_data %>% 
  filter(Probability_Threshold == "High") %>% 
  group_by(ID03, day, month, year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_high = ifelse(n_hours >= 23, 1, 0)) # %>%

total_days_per_month03 <- total_days03 %>%
  group_by(month, year) %>%
  summarise(days_per_month_high = sum(n_days_high))

# total_days_per_month03

## combine all thresholds
total_days <- cbind( total_days_per_month01,total_days_per_month02[,3], total_days_per_month03[,3])

# create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F) 

## convert month year to date format
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days

## change names of columns
total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)

## define seasons
winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months

total_days <- total_days %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )

# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "year", "month", "season"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)
##  plot - number of days 

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_y_continuous("Number of days per Month", limits= c(0,30)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Velocity",
       # y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)

## number of days separated per year

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_y_continuous("Number of days per Month", limits= c(0,30)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Depth",
       # y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)

## number of days separated per season

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_y_continuous("Number of days per Month", limits= c(0,30)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~season, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Depth",
       # y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)

