## Depth curves - model and application
## adult & Juvenile

## produces probability curves for depth, and application to sample node data (time series) for adult and Juvenile
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

## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous_updated.csv") ## all wulff incl and thompson removed - remove SAWA?
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
# sp_depth_cat <- read.csv("output_data/08a_spawning_depth_abundance.csv") ## removed from analysis


# Adult data distribution -------------------------------------------------------


# ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(ad_depth_con, ad_depth_cat)

unique(all_depth$Dataset) # 4 datasets, 

depth_freq <- all_depth %>% 
  uncount(Abundance)
# hist(depth_freq$Depth)

depth_freq <- subset(depth_freq, !Dataset=="SAWA")
dim(depth_freq) ## 1376

# ## compare different data sets
unique(depth_freq$Dataset)

sx <- depth_freq$Dataset == "Saiki"
wx <- depth_freq$Dataset == "Wulff"
smx <- depth_freq$Dataset == "SMEA"

depth_freq$Dataset_num[sx] <- 1
depth_freq$Dataset_num[wx] <- 2
depth_freq$Dataset_num[smx] <- 3


attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:3,
                 labels = c( "Saiki", "Wulff", "SMEA"))

# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Adult/Depth Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


# Adult model build -------------------------------------------------------

## check data
unique(depth_freq$Dataset) ## 3 datasets, 1376
mean(depth_freq$Depth) ## 44.4

## histogram with normal curve
x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Adult/Depth Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
# plot(density(depth_freq$Depth))


## probability curve - histogram scaled and centered depth, then transformed back to raw depth

depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
## x axis with raw depth values
xfit_r <- seq(min(depth_freq$Depth), max(depth_freq$Depth), length=120)
## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Adult/Depth: Probability curve" )
#add these now with axis
par(new=TRUE)
axis(2, at=pretty(range(yfit)))

## data frame with probabilities and depth - to combine with hydraulic data

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("depth_fit", "prob_fit")
head(fitdata)

write.csv(fitdata, "output_data/adult_depth_prob_curve_data.csv")

# Combine with hydraulic data -------------------------------------------

## upload hydraulic data

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
## select columns
hyd_dep <- hydraul[,c(1:3,9)]
colnames(hyd_dep)[4] <-"depth_ft"

## convert unit from feet to meters
hyd_dep$depth_cm <- (hyd_dep$depth_ft*0.3048)*100
# head(hyd_dep)
range(hyd_dep$depth_cm)
plot(hyd_dep$Q, hyd_dep$depth_cm,  main = "Adult: Q ~ Depth", xlab="Q (cfs)", ylab="Depth (cm)")


## add date_num and plot time series - use numbers for now

hyd_dep$date_num <- seq(1,length(hyd_dep$DateTime), 1)
plot(hyd_dep$date_num, hyd_dep$depth_cm, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm)

## round the depths - don't need the high resolution

hyd_dep$depth_cm_round <- round(hyd_dep$depth_cm, digits=0 )
fitdata$depth_fit_round <- round(fitdata$depth_fit, digits=0)

plot(hyd_dep$date_num, hyd_dep$depth_cm_round, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm_round)

## merge node data and probabilities
all_data <- merge(hyd_dep, fitdata, by.x="depth_cm_round", by.y="depth_fit_round", all=T)
# all_data[which(is.na(all_data)),]
# sum(is.na(all_data)) # 474
# head(all_data)

## missing values - anything under 4cm as not in suitability curve
## replace NA of probability with min probability of dataset

all_data[which(all_data$depth_cm_round < min(na.omit(all_data$depth_fit))),"prob_fit"] <- min(na.omit(all_data$prob_fit))
# sum(is.na(all_data)) # 426

## remove rows with probabilities above the max hydraulic value
all_data <- filter(all_data, depth_cm_round <= max(hyd_dep$depth_cm_round))
# sum(is.na(all_data)) # 48

save(all_data, file="output_data/F1_F57C_adult_depth_discharge_probability_time_series_all_columns.RData")

## keep columns dpeth, datetime, Q date_num & prob_fit
all_data <- all_data[, c(1,2,3,7,9)]
# sum(is.na(all_data)) # 0

## remove duplicate date_num (date time) and order

all_data <- all_data[!duplicated(all_data$date_num),]
new_data <- all_data[order(all_data$date_num),]

save(new_data, file="output_data/F1_F57C_adult_depth_discharge_probability_time_series_red_columns.RData")

# format probability time series ------------------------------------------

## look at data using lubridate etc

names(new_data)
## format date time
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

save(new_data, file="output_data/F1_F57C_depth_adult_discharge_probs_2010_2017_TS.RData")


# probability as a function of discharge -----------------------------------

head(new_data)

## plot
range(new_data$Q) ## 0.00 998.845 

## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(new_data$prob_fit ~ new_data$Q)

## find peak of prob v Q

peak <- filter(new_data, prob_fit == max(prob_fit)) #%>%
peakQ <- select(peak, Q)
peakQ  <- peakQ[1,1]
# peakQ

## function for each probability
newy2a <- 0.2
newx2a <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2a,
                  interval = c(min(new_data$Q), peakQ))$root

newy2b <- 0.2
newx2b <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2b, # returns error if no value, need it to return NA
                  interval = c(peakQ, max(new_data$Q)))$root

newy1a <- 0.1
newx1a <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy1a,
                  interval = c(min(new_data$Q), peakQ))$root

newy1b <- 0.1
newx1b <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy1b,
                  interval = c(peakQ, max(new_data$Q)))$root

newy3a <- 0.3
newx3a <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3a,
                  interval = c(min(new_data$Q), peakQ))$root

newy3b <- 0.3
newx3b <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3b,
                  interval = c(peakQ, max(new_data$Q)))$root


plot(new_data$Q, new_data$prob_fit, type="n", main = "Adult/Depth: Probability according to Q", xlab="Q (cfs)", ylab="Probability")
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
  geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
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
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
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
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
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
total_0.2 <- sum(new_datax_2016$Q >= newx2a)/length(new_datax_2016$DateTime)*100
# 82.09077 %
total_0.1 <- sum(new_datax_2016$Q >= newx1a)/length(new_datax_2016$DateTime)*100
# 99.49831
total_0.3 <- sum(new_datax_2016$Q >= newx3a & new_datax_2016$Q <= newx3b)/length(new_datax_2016$DateTime)*100
##3.966865

## percentage of time below threshold - summer
length(new_datax_2016_summer$DateTime)
total_summer_0.2 <- sum(new_datax_2016_summer$Q >= newx2a)/length(new_datax_2016_summer$DateTime)*100
# 74.09925 %
total_summer_0.1 <- sum(new_datax_2016_summer$Q >= newx1a)/length(new_datax_2016_summer$DateTime)*100
# 100
total_summer_0.3 <-sum(new_datax_2016_summer$Q >= newx3a & new_datax_2016_summer$Q <= newx3b)/length(new_datax_2016_summer$DateTime)*100
## 0.9743938

length(new_datax_2016_winter$DateTime)
total_winter_0.2 <- sum(new_datax_2016_winter$Q >= newx2a)/length(new_datax_2016_winter$DateTime)*100
# 90.57239 %
total_summer_0.1 <- sum(new_datax_2016_winter$Q >= newx1a)/length(new_datax_2016_winter$DateTime)*100
# 98.96585
total_summer_0.3 <- sum(new_datax_2016_winter$Q >= newx3a & new_datax_2016_winter$Q <= newx3b)/length(new_datax_2016_winter$DateTime)*100
## 7.142857

## make dataframe for all years 

head(new_datax)
names(new_datax)

## define seasons
winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months

new_datax <- new_datax %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )

## produces percentage of time for each year and season within year for each threshold

## need a universal solution to this where there's no "b"

time_stats <- new_datax %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(ann_total_0.2 = sum(Q >= newx2a)/length(DateTime)*100) %>%
  dplyr::mutate(ann_total_0.1 = sum(Q >= newx1a)/length(DateTime)*100) %>%
  dplyr::mutate(ann_total_0.3 = sum(Q >= newx3a & Q <= newx3b)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(seas_total_0.2 = sum(Q >= newx2a)/length(DateTime)*100) %>%
  dplyr::mutate(seas_total_0.1 = sum(Q >= newx1a)/length(DateTime)*100) %>%
  dplyr::mutate(seas_total_0.3 = sum(Q >= newx3a & Q <= newx3b)/length(DateTime)*100) %>%
  distinct(year, ann_total_0.2,ann_total_0.1,ann_total_0.3, seas_total_0.2, seas_total_0.1, seas_total_0.3)


time_stats

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season"))
melt_time <- rename(melt_time, Probability_Threshold = variable)

## subset annual stats
ann_stats <- unique(melt_time$Probability_Threshold)[1:3]
melt_time_ann <- melt_time %>% filter(Probability_Threshold %in% ann_stats ) %>%
  select(-season) %>% distinct()
# head(melt_time_ann)
# melt_time_ann

## subset seasonal stats
seas_stats <- unique(melt_time$Probability_Threshold)[4:6]
melt_time_seas <- filter(melt_time, Probability_Threshold %in% seas_stats )
head(melt_time_seas)
melt_time_seas


## plot for annual stats - need probs in order
ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time within discharge limit in relation to Depth (Annual)",
       y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

## plot for winter stats - need probs in order

melt_time_winter <- filter(melt_time_seas, season == "winter")
melt_time_winter

ggplot(melt_time_winter, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time under discharge limit in relation to Depth (Winter)",
       y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

## plot for summer stats - need probs in order

melt_time_summer <- filter(melt_time_seas, season == "summer")

ggplot(melt_time_summer, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time under discharge limit in relation to Depth (Summer)",
       y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

## add depth to figures

# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month

load(file="output_data/F1_F57C_depth_adult_discharge_probs_2010_2017_TS.RData")

# all columns based on different probabilities
## count number events within each threshold with a running total - max total is the number of consequative events (hours)
## order by datetime

new_data <- arrange(new_data, date_num)

## need universal solution to this when there's no "b"
new_data <- new_data %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1a)) %>%
  mutate(probability_0.1 = if_else(Q >= newx1a, row_number(), 0L)) %>% 
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2a)) %>%
  mutate(probability_0.2 = if_else(Q >= newx2a, row_number(), 0L)) %>% 
  ungroup() %>%
  group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3a & Q <= newx3a)) %>%
  mutate(probability_0.3 = if_else(Q >= newx3a & Q <= newx3a, row_number(), 0L)) #%>% 


## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_datax <- new_data[, c(3,6,7, 8, 10:15)] # all probs
names(new_datax)

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "ID03", "day", "month", "year", "Q"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)
tail(melt_data)
head(melt_data)
# melt_data_2016 <- filter(melt_data, year==2016)
# melt_data_2016

## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Probability_Threshold == "probability_0.1") %>% 
  group_by(ID01, day, month, year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days01 = ifelse(n_hours >= 23, 1, 0)) # %>%

## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year) %>%
  summarise(days_per_month01 = sum(n_days01))

# total_days_per_month01

total_days02 <- melt_data %>% 
  filter(Probability_Threshold == "probability_0.2") %>% 
  group_by(ID01, day, month, year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days02 = ifelse(n_hours >= 23, 1, 0)) # %>%

total_days_per_month02 <- total_days02 %>%
  group_by(month, year) %>%
  summarise(days_per_month02 = sum(n_days02))

# total_days_per_month02

total_days03 <- melt_data %>% 
  filter(Probability_Threshold == "probability_0.3") %>% 
  group_by(ID01, day, month, year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days03 = ifelse(n_hours >= 23, 1, 0)) # %>%

total_days_per_month03 <- total_days03 %>%
  group_by(month, year) %>%
  summarise(days_per_month03 = sum(n_days03))

# total_days_per_month03

## combine all thresholds
total_days <- cbind(total_days_per_month01, total_days_per_month02[,3], total_days_per_month03[,3])

# create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F) 

## convert month year to date format
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days

## change names of columns
total_days <- rename(total_days, Probability_0.1 = days_per_month01, Probability_0.2 = days_per_month02, Probability_0.3 = days_per_month03)

## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "year", "month"))
melt_days <- rename(melt_days, Probability_Threshold = variable, 
                    n_days = value)

head(melt_days)
##  plot - number of days 

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)

## number of days separated per year

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)

## figure like the above for summer and winter


# Juvenile depth ----------------------------------------------------------


## Juvenile depth - 2 datasets, 

all_depth <- rbind(juv_depth_con, juv_depth_cat)

## data distribution 
# ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(juv_depth_con, juv_depth_cat)

unique(all_depth$Dataset) # 2 datasets, observations (n=257)

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
mean(depth_freq$Depth) ## 36.55253
dim(depth_freq) ## 257
head(depth_freq)


# Data distribution -------------------------------------------------------


# ## compare different data sets
# ### get numbers for datasets
unique(depth_freq$Dataset)

sx <- depth_freq$Dataset == "Saiki"
smx <- depth_freq$Dataset == "SMEA"

depth_freq$Dataset_num[sx] <- 1
depth_freq$Dataset_num[smx] <- 2

attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c( "Saiki", "SMEA"))

# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Juvenile/Depth Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


# Juvenile model build -------------------------------------------------------

## check data
unique(depth_freq$Dataset) ## 2 datasets, 257
mean(depth_freq$Depth) ## 36.5

## histogram with normal curve
x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Adult/Depth Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
# plot(density(depth_freq$Depth))


## probability curve - histogram scaled and centered depth, then transformed back to raw depth

depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
## x axis with raw depth values
xfit_r <- seq(min(depth_freq$Depth), max(depth_freq$Depth), length=120)
## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Juvenile/Depth: Probability curve" )
#add these now with axis
par(new=TRUE)
axis(2, at=pretty(range(yfit)))

## data frame with probabilities and depth - to combine with hydraulic data

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("depth_fit", "prob_fit")
# head(fitdata)

write.csv(fitdata, "output_data/juvenile_depth_prob_curve_data.csv")

# Combine with hydraulic data -------------------------------------------

## upload hydraulic data

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
## select columns
hyd_dep <- hydraul[,c(1:3,9)]
colnames(hyd_dep)[4] <-"depth_ft"

## convert unit from feet to meters
hyd_dep$depth_cm <- (hyd_dep$depth_ft*0.3048)*100
# head(hyd_dep)

## add date_num and plot time series - use numbers for now

hyd_dep$date_num <- seq(1,length(hyd_dep$DateTime), 1)
plot(hyd_dep$date_num, hyd_dep$depth_cm, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm)

## round the depths - don't need the high resolution

hyd_dep$depth_cm_round <- round(hyd_dep$depth_cm, digits=1 )
fitdata$depth_fit_round <- round(fitdata$depth_fit, digits=1)

plot(hyd_dep$date_num, hyd_dep$depth_cm_round, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm_round)

## merge node data and probabilities
all_data <- merge(hyd_dep, fitdata, by.x="depth_cm_round", by.y="depth_fit_round")
head(all_data)

save(all_data, file="output_data/F1_F57C_juvenile_depth_discharge_probability_time_series_all_columns.RData")

## keep columns dpeth, datetime, Q date_num & prob_fit
all_data <- all_data[, c(1,2,3,7,9)]
# sum(is.na(all_data))

## remove duplicate date_num (date time) and order

all_data <- all_data[!duplicated(all_data$date_num),]
new_data <- all_data[order(all_data$date_num),]

save(new_data, file="output_data/F1_F57C_juvenile_depth_discharge_probability_time_series_red_columns.RData")

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

save(new_data, file="output_data/F1_F57C_depth_juvenile_discharge_probs_2010_2017_TS.RData")


# probability as a function of discharge -----------------------------------

head(new_data)

## plot
range(new_data$Q) ## 11.50 984.73

## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(new_data$prob_fit ~ new_data$Q)

## function for each probability
newy2a <- 0.2
newx2a <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2a,
                  interval = c(0, 500))$root

newy2b <- 0.2
newx2b <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy2b,
                  interval = c(500, 1000))$root

newy1 <- 0.1
newx1 <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy1,
                 interval = c(0, 1000))$root

newy3a <- 0.3
newx3a <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3a,
                  interval = c(0, 500))$root

newy3b <- 0.3
newx3b <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy3b,
                  interval = c(500, 1000))$root


plot(new_data$Q, new_data$prob_fit, type="n", main = "Juvenile/Depth: Probability according to Q", xlab="Q (cfs)", ylab="Probability")
lines(spl, col="black")
points(newx2a, newy2a, col="red", pch=19) # 0.2 - lower limit
points(newx2b, newy2b, col="red", pch=19) # 0.2 - upper limit
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
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
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
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
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
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
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
total_0.2 <- sum(new_datax_2016$Q >= newx2a & new_datax_2016$Q <= newx2b)/length(new_datax_2016$DateTime)*100
total_0.2 # 0.6726977 %

total_0.1 <- sum(new_datax_2016$Q >= newx1a & new_datax_2016$Q <= newx1b)/length(new_datax_2016$DateTime)*100
total_0.1 # 34.96868 %

total_0.3 <- sum(new_datax_2016$Q >= newx3a & new_datax_2016$Q <= newx3b)/length(new_datax_2016$DateTime)*100
total_0.3 ## 0.3479471 %

## percentage of time below threshold - summer
length(new_datax_2016_summer$DateTime)
total_0.2 <- sum(new_datax_2016_summer$Q >= newx2a & new_datax_2016_summer$Q <= newx2b)/length(new_datax_2016_summer$DateTime)*100
total_0.2 # 0 %

total_0.1 <- sum(new_datax_2016_summer$Q >= newx1a & new_datax_2016_summer$Q <= newx1b)/length(new_datax_2016_summer$DateTime)*100
total_0.1 # 48.28915 %

total_0.3 <- sum(new_datax_2016_summer$Q >= newx3a & new_datax_2016_summer$Q <= newx3b)/length(new_datax_2016_summer$DateTime)*100
total_0.3 ## 0 %

length(new_datax_2016_winter$DateTime)
total_0.2 <- sum(new_datax_2016_winter$Q >= newx2a & new_datax_2016_winter$Q <= newx2b)/length(new_datax_2016_winter$DateTime)*100
total_0.2 # 1.378 %

total_0.1 <- sum(new_datax_2016_winter$Q >= newx1a & new_datax_2016_winter$Q <= newx1b)/length(new_datax_2016_winter$DateTime)*100
total_0.1 # 21.00261 %

total_0.3 <- sum(new_datax_2016_winter$Q >= newx3a & new_datax_2016_winter$Q <= newx3b)/length(new_datax_2016_winter$DateTime)*100
total_0.3 ## 0.7127584 %

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
  dplyr::mutate(ann_total_0.2 = sum(Q >= newx2a & Q <= newx2b)/length(DateTime)*100) %>%
  dplyr::mutate(ann_total_0.1 = sum(Q >= newx1a & Q <= newx1b)/length(DateTime)*100) %>%
  dplyr::mutate(ann_total_0.3 = sum(Q >= newx3a & Q <= newx3b)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(seas_total_0.2 = sum(Q >= newx2a & Q <= newx2b)/length(DateTime)*100) %>%
  dplyr::mutate(seas_total_0.1 = sum(Q >= newx1a & Q <= newx1b)/length(DateTime)*100) %>%
  dplyr::mutate(seas_total_0.3 = sum(Q >= newx3a & Q <= newx3b)/length(DateTime)*100) %>%
  distinct(year, ann_total_0.2,ann_total_0.1,ann_total_0.3, seas_total_0.2, seas_total_0.1, seas_total_0.3)


time_stats

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season"))
melt_time <- rename(melt_time, Probability_Threshold = variable)

## subset annual stats
ann_stats <- unique(melt_time$Probability_Threshold)[1:3]
melt_time_ann <- melt_time %>% filter(Probability_Threshold %in% ann_stats ) %>%
  select(-season) %>% distinct()
# head(melt_time_ann)
# melt_time_ann

## subset seasonal stats
seas_stats <- unique(melt_time$Probability_Threshold)[4:6]
melt_time_seas <- filter(melt_time, Probability_Threshold %in% seas_stats )
head(melt_time_seas)
melt_time_seas


## plot for annual stats - need probs in order
ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time under discharge limit in relation to velocity (Annual)",
       y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

## plot for winter stats - need probs in order

melt_time_winter <- filter(melt_time_seas, season == "winter")
melt_time_winter

ggplot(melt_time_winter, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time under discharge limit in relation to velocity (Winter)",
       y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

## plot for summer stats - need probs in order

melt_time_summer <- filter(melt_time_seas, season == "summer")

ggplot(melt_time_summer, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time under discharge limit in relation to velocity (Summer)",
       y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)




# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month

load(file="output_data/F1_F57C_velocity_adult_discharge_probs_2010_2017_TS.RData")

# all columns based on different probabilities
new_data <- new_data %>%
  group_by(month, year, ID = data.table::rleid(Q >= newx1a & Q <= newx1b)) %>%
  mutate(probability_0.1 = if_else(Q >= newx1a & Q <= newx1b, row_number(), 0L)) %>% 
  ungroup() %>%
  group_by(month, year, ID = data.table::rleid(Q >= newx2a & Q <= newx2b)) %>%
  mutate(probability_0.2 = if_else(Q >= newx2a & Q <= newx2b, row_number(), 0L)) %>% 
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
new_datax <- new_data[, c(3,7,8,11:14)] # all probs

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID", "month", "year", "Q"))
melt_data <- rename(melt_data, Probability_Threshold = variable)
head(melt_data)

# melt_data_2016 <- filter(melt_data, year==2016)
# melt_data_2016

## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the velocity of each threshold - days are not necessarily conseq
total_days <- melt_data %>% 
  group_by(ID, month, year, Probability_Threshold) %>%
  summarise(n_days = sum(value==24)) # %>%
# rename(max_Consec_Hours=`sum(value)`) %>%
# mutate(max_Consec_Hours/24) %>%
# rename(duration_dayss=`max_Consec_Hours/24`) 


# ## count number of events - below 20% and for 1 day - per month as well as number of times per month (hours) that the prob goes below zero
# total_days <- conseq_ID20 %>%
#   group_by(month, year, Probability_Threshold) %>%
#   # select(-max_Consec_Hours) %>%
#   summarize(n_days = sum(max_Consec_Days)) %>% ## total number of days per month (not always consequtive)
#   arrange(year)

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
  facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days under discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)


