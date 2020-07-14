## probability v discharge analysis

## Depth

## from start to finish
## steps involved
# 1 - format hydraulic time series
# 2 - apply curve to depth data at node
# 3 - probability thresholds discharge limits 
  # 3a - percentage of time
  # 3b - number of days

## packages

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


# 1 $ 2- Format hydraulic time series data and apply probability curve to node ------------------------------------------------

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
# head(hyd_dep)

### upload curve data
## adult
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

## combine data adult
all_depth <- rbind(ad_depth_con, ad_depth_cat)
zeros <- subset(all_depth, Abundance == 0)
# X Depth Abundance  Dataset
# 145 145    13         0 Thompson
# 147 147    31         0 Thompson
# 148 148    18         0 Thompson
# 228   1     2         0     SMEA
# 229   2     8         0     SMEA
# 230   3    12         0     SMEA
# 232   5    22         0     SMEA
# 233   6    28         0     SMEA
## check for zeros
head(all_depth)
range(all_depth$Abundance)
## uncount data
depth_freq <- all_depth %>% 
  uncount(Abundance)
unique(depth_freq$Dataset)

## remove Thompson
depth_freq <- subset(depth_freq, Dataset!="Thompson")
## check zeros not counted
# depth_freq_zeros <- subset(depth_freq, Depth %in% zeros$Depth) ## it's fine!
hist(depth_freq$Depth, main="Adult/Depth Histogram", xlab="Depth (cm)")
## curve data in dataframe - no plot
range(depth_freq$Depth)
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

save(all_data, file="output_data/15_adult_depth_discharge_probability_time_series")

## keep columns dpeth, datetime, Q date_num & prob_fit
all_data <- all_data[, c(1,2,3,7,9)]
# sum(is.na(all_data))

## remove duplicate date_num (date time) and order

all_data <- all_data[!duplicated(all_data$date_num),]
new_data <- all_data[order(all_data$date_num),]


# Quick Analysis of time series -------------------------------------------

## analyse the data as time series

head(new_data)

firstHour <- 24*(as.Date("2010-10-17 00:00:00")-as.Date("2010-1-1 00:00:00"))
new_dataTS <- ts(new_data$prob_fit,start=c(2010,firstHour),frequency=24*365)

plot.ts(new_dataTS)

## decompose data

new_dataTS_comp <- decompose(new_dataTS)
plot(new_dataTS_comp)


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

save(new_data, file="output_data/15_depth_adult_discharge_probs_2010_2017_TS.RData")

# probability as a function of discharge -----------------------------------
head(new_data)

## plot
range(new_data$Q) ## 0.40 998.84

## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(new_data$prob_fit ~ new_data$Q)

## function for each probability
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


plot(all_data$Q, all_data$prob_fit, type="n", main = "Adult/Depth: Probability according to Q", xlab="Q (cfs I assume)", ylab="Probability")
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


# statistics --------------------------------------------------------------

## percentage of time below threshold - annual
length(new_datax_2016$DateTime)
sum(new_datax_2016$Q >= newx2)/length(new_datax_2016$DateTime)*100
# 82.09077 %
sum(new_datax_2016$Q >= newx1)/length(new_datax_2016$DateTime)*100
# 99.49831
sum(new_datax_2016$Q >= newx3a & new_datax_2016$Q <= newx3b)/length(new_datax_2016$DateTime)*100
##3.966865

## percentage of time below threshold - summer
length(new_datax_2016_summer$DateTime)
sum(new_datax_2016_summer$Q >= newx2)/length(new_datax_2016_summer$DateTime)*100
# 74.09925 %
sum(new_datax_2016_summer$Q >= newx1)/length(new_datax_2016_summer$DateTime)*100
# 100
sum(new_datax_2016_summer$Q >= newx3a & new_datax_2016_summer$Q <= newx3b)/length(new_datax_2016_summer$DateTime)*100
## 0.9743938

length(new_datax_2016_winter$DateTime)
sum(new_datax_2016_winter$Q >= newx2)/length(new_datax_2016_winter$DateTime)*100
# 90.57239 %
sum(new_datax_2016_winter$Q >= newx1)/length(new_datax_2016_winter$DateTime)*100
# 98.96585
sum(new_datax_2016_winter$Q >= newx3a & new_datax_2016_winter$Q <= newx3b)/length(new_datax_2016_winter$DateTime)*100
## 7.142857



# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month

load(file="output_data/15_depth_adult_discharge_probs_2010_2017_TS.RData")
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


# occasions of more than one string of consequtive hours, therefore more than 1 max value we want, 
# but we only get the highest max

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
  rename(max_Consec_Hours=`max(value)`) #%>%
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



