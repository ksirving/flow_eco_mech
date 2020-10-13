## Depth curves - model and application
## adult 

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


# Combine with hydraulic data -------------------------------------------

## upload hydraulic data

fitdata <- read.csv("output_data/adult_depth_prob_curve_data.csv")

## soft bottom reaches

# F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv")
F37B_High <- read.csv("input_data/HecRas/hydraulic_ts_F37B_High.csv")
# F45B <- read.csv("input_data/HecRas/hydraulic_ts_F45B.csv")
# F319 <- read.csv("input_data/HecRas/hydraulic_ts_F319.csv")
# LA13 <- read.csv("input_data/HecRas/hydraulic_ts_LA13.csv")
# LA1 <- read.csv("input_data/HecRas/hydraulic_ts_LA1.csv")


hydraul <- F37B_High[,-1]
names(hydraul)
head(hydraul)
## select columns
## change some names
hydraul <- hydraul %>%
  rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow)

# nas <- which(complete.cases(hyd_dep) == FALSE)
# hyd_dep[nas,]

## convert unit from feet to meters 

## change names and transform ft to cm
hyd_dep <- hydraul %>%
  select(c(DateTime, Q, node, Shear..lb.sq.ft..LOB, Hydr..Depth..ft..LOB,Shear..lb.sq.ft..MC, Hydr..Depth..ft..MC, 
           Shear..lb.sq.ft..ROB, Hydr..Depth..ft..ROB)) %>%
  rename(shear_ft_LOB = Shear..lb.sq.ft..LOB, depth_ft_LOB = Hydr..Depth..ft..LOB, shear_ft_MC = Shear..lb.sq.ft..MC,
         depth_ft_MC = Hydr..Depth..ft..MC, shear_ft_ROB = Shear..lb.sq.ft..ROB, depth_ft_ROB = Hydr..Depth..ft..ROB) %>%
  mutate(depth_cm_LOB = (depth_ft_LOB*0.3048)*100,
         depth_cm_MC = (depth_ft_MC*0.3048)*100,
         depth_cm_ROB = (depth_ft_ROB*0.3048)*100) %>%
  # mutate(shear_pa_LOB = (shear_ft_LOB/0.020885),
  #        shear_pa_MC = (shear_ft_MC/0.020885),
  #        shear_pa_ROB = (shear_ft_ROB/0.020885)) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

head(hyd_dep)
## 


# ## melt channel position data
hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num"))
hyd_dep <- filter(hyd_dep, variable == "depth_cm_MC")

labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")
png("figures/Application_curves/nodes/F37B_High_Depth_Q.png", width = 500, height = 600)

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F37B_High: Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()
## plot time series
png("figures/Application_curves/nodes/F37B_High_Depth_TS.png", width = 500, height = 600)

ggplot(hyd_dep, aes(x = date_num, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F37B_High: Depth ~ Time Series",
       y = "Depth (cm)",
       x = "Date") #+ theme_bw(base_size = 15)
dev.off()
## merge datasets with spline function

head(hyd_dep)
head(fitdata)

## use smooth spline to predict on new data set
new_values <-smooth.spline(fitdata$depth_fit, fitdata$prob_fit)

all_data <- hyd_dep %>%
  group_by(variable) %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(depth_cm = value)
      
all_data

nas <- which(complete.cases(all_data) == FALSE)
nas #0



save(all_data, file="output_data/F1_F37B_High_adult_depth_discharge_probability_time_series_all_columns.RData")
# load("output_data/F1_F37B_High_adult_depth_discharge_probability_time_series_all_columns.RData")
## keep columns dpeth, datetime, Q date_num & prob_fit

new_data <- all_data[order(all_data$date_num),]


save(new_data, file="output_data/F1_F37B_High_adult_depth_discharge_probability_time_series_red_columns.RData")

# format probability time series ------------------------------------------

## look at data using lubridate etc

names(new_data)
## format date time
new_data$DateTime<-as.POSIXct(new_data$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "GMT")

## create year, month, day and hour columns and add water year

new_data <- new_data %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime)) %>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))


head(new_data)
save(new_data, file="output_data/F1_F37B_High_depth_adult_discharge_probs_2010_2017_TS.RData")


# probability as a function of discharge -----------------------------------


## plot
range(new_data$Q) ## 26.22926 41750.16797 
range(new_data$prob_fit) ## -0.004518105  0.398942010

## bind shallow and deeper depths by 0.1 - 10cm & 120cm
## change all prob_fit lower than 0.1 to 0.1
new_data[which(new_data$prob_fit <  0.1),"prob_fit"] <- 0.1


peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQM <- filter(peak, variable=="depth_cm_MC")
peakQM  <- max(peakQM$Q)
peakQM 



## filter data by cross section position

new_dataM <- filter(new_data, variable == "depth_cm_MC")


## Main channel curves

load(file="root_interpolation_function.Rdata")


newx1a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.1)
newx1a <- c(min(new_dataM$Q), max(new_dataM$Q))

newx1a

newx2a  <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.2)

if(length(newx2a) > 4) {
  newx2a <- c(newx2a[1], newx2a[length(newx2a)])
} else {
  newx2a <- newx2a
}


newx3a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.3)
newx3a

if(min(new_data$prob_fit)>0.3) {
  newx3a <- min(new_data$Q)
} else {
  newx3a <- newx3a
}

if(length(newx3a) > 4) {
  newx3a <- c(newx3a[1], newx3a[length(newx3a)])
} else {
  newx3a <- newx3a
}

newx3a


## MAKE DF OF Q LIMITS

limits <- as.data.frame(matrix(ncol=3, nrow=12)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_1", "Low_Prob_2", "Low_Prob_3", "Low_Prob_4",
                    "Med_Prob_1", "Med_Prob_2", "Med_Prob_3", "Med_Prob_4",
                    "High_Prob_1", "High_Prob_2", "High_Prob_3", "High_Prob_4")



limits$MC <- c(newx1a[1], newx1a[2],newx1a[3], newx1a[4],
               newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
               newx3a[1], newx3a[2],newx3a[3],newx3a[4])

limits

## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
write.csv(limits, "output_data/F1_F37B_High_Q_limits.csv")

# plot discharge points ---------------------------------------------------

png("figures/Application_curves/Depth/F37B_High_adult_depth_prob_Q_thresholds.png", width = 500, height = 600)

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +

  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.1, x=newx1a[1]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.1, x=newx1a[2]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.1, x=newx1a[3]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.1, x=newx1a[4]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.2, x=newx2a[1]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.2, x=newx2a[2]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.2, x=newx2a[3]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.2, x=newx2a[4]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.3, x=newx3a[1]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.3, x=newx3a[2]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.3, x=newx3a[3]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.3, x=newx3a[4]), color="blue") +
  
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F37B_High: Adult/Depth: Probability ~ Q",
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()
### plot discharge over time

# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataMx)


# dataframe for stats -----------------------------------------------------

## make dataframe for all years 

## define critical period or season for adult as all year is critical
winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )



## produces percentage of time for each year and season within year for each threshold

## Main channel curves

load(file="expression_Q_limit_function.RData")

low_threshM <- expression_Q(newx1a, peakQM) 
low_threshM <-as.expression(do.call("substitute", list(low_threshM[[1]], list(limit = as.name("newx1a")))))

med_threshM <- expression_Q(newx2a, peakQM)
med_threshM <-as.expression(do.call("substitute", list(med_threshM[[1]], list(limit = as.name("newx2a")))))

high_threshM <- expression_Q(newx3a, peakQM)
high_threshM <-as.expression(do.call("substitute", list(high_threshM[[1]], list(limit = as.name("newx3a")))))

low_threshM
med_threshM
high_threshM

###### calculate amount of time

time_statsm <- new_dataMx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Low = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(eval(high_threshM))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(eval(high_threshM))/length(DateTime)*100) %>%
  distinct(water_year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="MC")

time_statsm


time_stats <- time_statsm

## melt
melt_time<-reshape2::melt(time_stats, id=c("season", "position", "water_year"))
melt_time <- rename(melt_time, Probability_Threshold = variable)
head(melt_time)
unique(melt_time$position)
write.csv(melt_time, "output_data/F1_F37B_High_adult_depth_time_stats.csv")

## subset annual stats
ann_stats <- unique(melt_time$Probability_Threshold)[1:3]
melt_time_ann <- melt_time %>% filter(Probability_Threshold %in% ann_stats ) %>%
  select(-season) %>% distinct()

## subset seasonal stats
seas_stats <- unique(melt_time$Probability_Threshold)[4:6]
melt_time_seas <- filter(melt_time, Probability_Threshold %in% seas_stats )

## plot for annual stats - need probs in order

png("figures/Application_curves/Depth/F37B_High_adult_depth_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time_ann, aes(x = water_year, y=value)) +
  geom_line(aes( group =c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Time within discharge limit in relation to Depth (Annual)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()
## plot for winter stats - need probs in order

melt_time_winter <- filter(melt_time_seas, season == "winter")
unique(melt_time_winter$Probability_Threshold)

png("figures/Application_curves/Depth/F37B_High_adult_depth_perc_time_above_threshold_winter.png", width = 500, height = 600)

ggplot(melt_time_winter, aes(x = water_year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Time within discharge limit in relation to Depth (Winter)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()
## plot for summer stats - need probs in order

melt_time_summer <- filter(melt_time_seas, season == "summer")

png("figures/Application_curves/Depth/F37B_High_adult_depth_perc_time_above_threshold_summer.png", width = 500, height = 600)

ggplot(melt_time_summer, aes(x = water_year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Time within discharge limit in relation to Depth (Summer)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------


# all columns based on different probabilities
## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(eval(low_threshM))) %>%
  mutate(Low = if_else(eval(low_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, water_year, ID02 = data.table::rleid(eval(med_threshM))) %>%
  mutate(Medium = if_else(eval(med_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, water_year, ID03 = data.table::rleid(eval(high_threshM))) %>%
  mutate(High = if_else(eval(high_threshM), row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")


## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_datax <- select(new_dataM, c(Q, month, water_year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs
names(new_datax)


## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "ID03", "day", "month", "water_year", "Q", "position"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)

melt_data
## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Probability_Threshold == "Low") %>% 
  group_by(ID01, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_low = sum(n_days_low))

total_days_per_month01

total_days02 <- melt_data %>% 
  filter(Probability_Threshold == "Medium") %>% 
  group_by(ID02, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month02 <- total_days02 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_medium = sum(n_days_medium))

# total_days_per_month02

total_days03 <- melt_data %>% 
  filter(Probability_Threshold == "High") %>% 
  group_by(ID03, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month03 <- total_days03 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_high = sum(n_days_high))

total_days_per_month03

## combine all thresholds
total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
head(total_days)

write.csv(total_days, "output_data/F1_F37B_High_total_days.csv")

# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)


## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)

## change names of columns
total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)

# total_hours <- rename(total_hours, Low = n_days_low, Medium = n_days_medium, High = n_days_high)

## define seasons
winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months

total_days <- total_days %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )


# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/F1_F37B_High_total_days_long.csv")


melt_daysx <- filter(melt_days, position=="MC")
library(scales)
  
## plot all ts
png("figures/Application_curves/Depth/F37B_High_adult_depth_lob_rob_mc_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()
## plot by year
png("figures/Application_curves/Depth/F37B_High_adult_depth_lob_rob_mc_no_days_within_Q_by_year.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%b")) +
  # scale_x_continuous(breaks=as.numeric(month_year), labels=format(month_year,"%b")) +
  facet_wrap(~water_year+position, scale="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Number of days within discharge limit in relation to Depth: Mid Channel",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)
dev.off()
## plot by season/critical period
png("figures/Application_curves/Depth/F37B_High_adult_depth_lob_rob_mc_no_days_within_Q_by_season.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%Y")) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%Y")) +
  facet_wrap(~season +position, scales="free", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()


#  Juvenile ---------------------------------------------------------------

## upload hydraulic data
fitdata <- read.csv("output_data/juvenile_depth_prob_curve_data.csv")

head(hyd_dep)
head(fitdata)

## use smooth spline to predict on new data set
new_values <-smooth.spline(fitdata$depth_fit, fitdata$prob_fit)

all_data <- hyd_dep %>%
  group_by(variable) %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(depth_cm = value)

all_data

nas <- which(complete.cases(all_data) == FALSE)
nas #0

save(all_data, file="output_data/F1_F37B_High_juvenile_depth_discharge_probability_time_series_all_columns.RData")

## in order of datenum

new_data <- all_data[order(all_data$date_num),]


save(new_data, file="output_data/F1_F37B_High_juvenile_depth_discharge_probability_time_series_red_columns.RData")

# format probability time series ------------------------------------------

## look at data using lubridate etc

names(new_data)
## format date time
new_data$DateTime<-as.POSIXct(new_data$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "GMT")

## create year, month, day and hour columns and add water year

new_data <- new_data %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime)) %>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))

save(new_data, file="output_data/F1_F37B_High_depth_juvenile_discharge_probs_2010_2017_TS.RData")


# probability as a function of discharge -----------------------------------


## plot
range(new_data$Q) ## 26.22926 41750.16797 
range(new_data$prob_fit) ## -3.2183121  0.3989423

## bind shallow and deeper depths by 0.1 - 10cm & 120cm
## change all prob_fit lower than 0.1 to 0.1
new_data[which(new_data$prob_fit <  0.1),"prob_fit"] <- 0.1

peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQM <- filter(peak, variable=="depth_cm_MC")
peakQM  <- max(peakQM$Q)
peakQM 


## filter data by cross section position

new_dataM <- filter(new_data, variable == "depth_cm_MC")

## Main channel curves

load(file="root_interpolation_function.Rdata")


newx1a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.1)
newx1a <- c(min(new_dataM$Q), max(new_dataM$Q))

newx1a

newx2a  <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.2)

if(length(newx2a) > 4) {
  newx2a <- c(newx2a[1], newx2a[length(newx2a)])
} else {
  newx2a <- newx2a
}


newx3a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.3)
newx3a

if(min(new_data$prob_fit)>0.3) {
  newx3a <- min(new_data$Q)
} else {
  newx3a <- newx3a
}

if(length(newx3a) > 4) {
  newx3a <- c(newx3a[1], newx3a[length(newx3a)])
} else {
  newx3a <- newx3a
}

newx3a


## MAKE DF OF Q LIMITS

limits <- as.data.frame(matrix(ncol=3, nrow=12)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_1", "Low_Prob_2", "Low_Prob_3", "Low_Prob_4",
                    "Med_Prob_1", "Med_Prob_2", "Med_Prob_3", "Med_Prob_4",
                    "High_Prob_1", "High_Prob_2", "High_Prob_3", "High_Prob_4")


limits$MC <- c(newx1a[1], newx1a[2],newx1a[3], newx1a[4],
               newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
               newx3a[1], newx3a[2],newx3a[3],newx3a[4])


limits

## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
write.csv(limits, "output_data/F1_F37B_High_juvenile_Q_limits.csv")

# plot discharge points ---------------------------------------------------
unique(new_data$variable)
png("figures/Application_curves/Depth/F37B_High_juvenile_depth_prob_Q_thresholds.png", width = 500, height = 600)
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.1, x=newx1a[1]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.1, x=newx1a[2]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.1, x=newx1a[3]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.1, x=newx1a[4]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.2, x=newx2a[1]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.2, x=newx2a[2]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.2, x=newx2a[3]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.2, x=newx2a[4]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.3, x=newx3a[1]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.3, x=newx3a[2]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.3, x=newx3a[3]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.3, x=newx3a[4]), color="blue") +
  
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F37B_High: Juvenile/Depth: Probability ~ Q",
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()
### plot discharge over time

# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataMx)


# discharge time series plots with probability lines ----------------------

##  plot time series of discharge - 0.2 prob line

ggplot(new_dataRx) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx1aR, linetype="dashed", color="red")+
  # facet_wrap(~year, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)


# dataframe for stats -----------------------------------------------------

## make dataframe for all years 

## define critical period or season for adult as all year is critical
## define seasons/critical period
non_critical <- c(1,2,8:12) 
critical <- c(3:7) 

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )


limits
# time stats - mid channel ------------------------------------------------


### define expression for low threshold 
## Main channel curves

load(file="expression_Q_limit_function.RData")

low_threshM <- expression_Q(newx1a, peakQM) 
low_threshM <-as.expression(do.call("substitute", list(low_threshM[[1]], list(limit = as.name("newx1a")))))

med_threshM <- expression_Q(newx2a, peakQM)
med_threshM <-as.expression(do.call("substitute", list(med_threshM[[1]], list(limit = as.name("newx2a")))))

high_threshM <- expression_Q(newx3a, peakQM)
high_threshM <-as.expression(do.call("substitute", list(high_threshM[[1]], list(limit = as.name("newx3a")))))

low_threshM
med_threshM
high_threshM

###### calculate amount of time

time_statsm <- new_dataMx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Low = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(eval(high_threshM))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(eval(high_threshM))/length(DateTime)*100) %>%
  distinct(water_year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="MC")

time_statsm


time_stats <- time_statsm

## melt
melt_time<-reshape2::melt(time_stats, id=c("season", "position", "water_year"))
melt_time <- rename(melt_time, Probability_Threshold = variable)
head(melt_time)
unique(melt_time$position)
write.csv(melt_time, "output_data/F1_F37B_High_juvenile_depth_time_stats.csv")

## subset annual stats
ann_stats <- unique(melt_time$Probability_Threshold)[1:3]
melt_time_ann <- melt_time %>% filter(Probability_Threshold %in% ann_stats ) %>%
  select(-season) %>% distinct()

## subset seasonal stats
seas_stats <- unique(melt_time$Probability_Threshold)[4:6]
melt_time_seas <- filter(melt_time, Probability_Threshold %in% seas_stats )
melt_time_seas
## plot for annual stats - need probs in order

png("figures/Application_curves/Depth/F37B_High_juvenile_depth_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time_ann, aes(x = water_year, y=value)) +
  geom_line(aes( group =c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Time within discharge limit in relation to Depth (Annual)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()
## plot for winter stats - need probs in order

melt_time_winter <- filter(melt_time_seas, season == "non_critical")
unique(melt_time_winter$season)

png("figures/Application_curves/Depth/F37B_High_juvenile_depth_perc_time_above_threshold_non_critical.png", width = 500, height = 600)

ggplot(melt_time_winter, aes(x = water_year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Time within discharge limit in relation to Depth (Non_critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()
## plot for summer stats - need probs in order

melt_time_summer <- filter(melt_time_seas, season == "critical")

png("figures/Application_curves/Depth/F37B_High_juvenile_depth_perc_time_above_threshold_critical.png", width = 500, height = 600)

ggplot(melt_time_summer, aes(x = water_year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Time within discharge limit in relation to Depth (critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------

# all columns based on different probabilities
## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(eval(low_threshM))) %>%
  mutate(Low = if_else(eval(low_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, water_year, ID02 = data.table::rleid(eval(med_threshM))) %>%
  mutate(Medium = if_else(eval(med_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, water_year, ID03 = data.table::rleid(eval(high_threshM))) %>%
  mutate(High = if_else(eval(high_threshM), row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_datax <- select(new_dataM, c(Q, month, water_year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "ID03", "day", "month", "water_year", "Q", "position"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)

melt_data
## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Probability_Threshold == "Low") %>% 
  group_by(ID01, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_low = sum(n_days_low))

total_days_per_month01

total_days02 <- melt_data %>% 
  filter(Probability_Threshold == "Medium") %>% 
  group_by(ID02, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month02 <- total_days02 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_medium = sum(n_days_medium))

# total_days_per_month02

total_days03 <- melt_data %>% 
  filter(Probability_Threshold == "High") %>% 
  group_by(ID03, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month03 <- total_days03 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_high = sum(n_days_high))

total_days_per_month03

## combine all thresholds
total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
head(total_days)

write.csv(total_days, "output_data/F1_F37B_High_juvenile_total_days.csv")

# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)

## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)

## change names of columns
total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)

# total_hours <- rename(total_hours, Low = n_days_low, Medium = n_days_medium, High = n_days_high)

## define seasons/critical period
non_critical <- c(1,2,8:12) 
critical <- c(3:7) 

total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

unique(total_days$season)
str(total_days)
# ## melt data

# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/F1_F37B_High_juvenile_total_days_long.csv")

library(scales)

## plot all ts
png("figures/Application_curves/Depth/F37B_High_juvenile_depth_lob_rob_mc_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()
## plot by year
png("figures/Application_curves/Depth/F37B_High_juvenile_depth_lob_rob_mc_no_days_within_Q_by_year.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%b")) +
  # scale_x_continuous(breaks=as.numeric(month_year), labels=format(month_year,"%b")) +
  facet_wrap(~water_year+position, scale="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)
dev.off()
## plot by season/critical period
png("figures/Application_curves/Depth/F37B_High_juvenile_depth_lob_rob_mc_no_days_within_Q_by_season.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%Y")) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%Y")) +
  facet_wrap(~season +position, scales="free", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()


