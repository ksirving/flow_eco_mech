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

fitdata <- read.csv("output_data/adult_velocity_prob_curve_data.csv")

# Combine with hydraulic data -------------------------------------------

## upload hydraulic data

## soft bottom reaches

# F57C <- read.csv("input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("input_data/HecRas/hydraulic_ts_LA8.csv")
# LA11 <- read.csv("input_data/HecRas/hydraulic_ts_LA11.csv")
LA20 <- read.csv("input_data/HecRas/hydraulic_ts_LA20_2.csv")

## go through script one at a time

hydraul <- LA20[,-1]

## select columns

hyd_vel <- hydraul[,c(1:3,4,8, 12)]
colnames(hyd_vel) <-c("DateTime", "node", "Q", "vel_ft_LOB", "vel_ft_MC", "vel_ft_ROB")

# nas <- which(complete.cases(hyd_dep) == FALSE)
## select column

hyd_vel <- hyd_vel %>%
  mutate(vel_m_LOB = (vel_ft_LOB*0.3048),
         vel_m_MC = (vel_ft_MC*0.3048),
         vel_m_ROB = (vel_ft_ROB*0.3048)) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))


hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num"))
head(hyd_vel)

labels <- c(vel_m_LOB = "Left Over Bank", vel_m_MC = "Main Channel", vel_m_ROB = "Right Over Bank")

png("figures/Application_curves/nodes/LA20_Velocity_Q.png", width = 500, height = 600)

ggplot(hyd_vel, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA20: Velocity ~ Q",
       y = "Velocity (m/s)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

## plot time series

png("figures/Application_curves/nodes/LA20_Velocity_TS.png", width = 500, height = 600)

ggplot(hyd_vel, aes(x = date_num, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA20: Velocity ~ Q",
       y = "Velocity (m/s)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

## merge datasets with spline function

head(hyd_vel)
head(fitdata)

## use smooth spline to predict on new data set
new_values <-smooth.spline(fitdata$velocity_fit, fitdata$prob_fit)

all_data <- hyd_vel %>%
  group_by(variable) %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(vel_m = value)

all_data

nas <- which(complete.cases(all_data) == FALSE)
nas #0


save(all_data, file="output_data/F2_LA20_SAS_adult_velocity_discharge_probability_time_series_all_columns.RData")
names(all_data)
## keep columns velocity, datetime, Q date_num & prob_fit
new_data <- all_data[order(all_data$date_num),]

save(new_data, file="output_data/F2_LA20_SAS_adult_velocity_discharge_probability_time_series_red_columns.RData")


# format probability time series ------------------------------------------

## look at data using lubridate etc

## look at data using lubridate etc

names(new_data)
## format date time
new_data$DateTime<-as.POSIXct(new_data$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "GMT")

## create year, month, day and hour columns

new_data <- new_data %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime))



head(new_data)

save(new_data, file="output_data/F2_LA20_SAS_velocity_adult_discharge_probs_2010_2017_TS.RData")


# probability as a function of discharge -----------------------------------
load(file="output_data/F2_LA20_velocity_adult_discharge_probs_2010_2017_TS.RData")
head(new_data)

## plot
range(new_data$Q) ## 24.50545 40888.74219
range(new_data$prob_fit) ## -0.4951386  0.3989409

## change all negative prob_fit  to 0
new_data[which(new_data$prob_fit <  0),"prob_fit"] <- 0

peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQM <- filter(peak, variable=="vel_m_MC")
peakQM  <- max(peakQM$Q)
peakQM ## 24.50545

peakQL <- filter(peak, variable=="vel_m_LOB")
peakQL  <- max(peakQL$Q) ## 
peakQL ## 499.4943

peakQR <- filter(peak, variable=="vel_m_ROB")
peakQR  <- max(peakQR$Q) ## 
peakQR ## 1162.968

## filter data by cross section position

new_dataM <- filter(new_data, variable == "vel_m_MC")
new_dataL <- filter(new_data, variable == "vel_m_LOB")
new_dataR <- filter(new_data, variable == "vel_m_ROB")

## Main channel curves

MC_curve <- spline(new_dataM$Q, new_dataM$prob_fit,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)

MC_curve_lower <- spline(new_dataM$Q, new_dataM$prob_fit,
                         xmin = min(new_dataM$Q), xmax = peakQM, ties = mean)
MC_curve_upper <- spline(new_dataM$Q, new_dataM$prob_fit,
                         xmin = peakQM, xmax = max(new_dataM$Q), ties = mean)

## main channel values
newx1a <- approx(x = MC_curve_lower$y, y = MC_curve_lower$x, xout = 0.1)$y
# newx1a <- min(MC_curve_lower$x)
newx1a <- NA

newx1b <- approx(x = MC_curve_upper$y, y = MC_curve_upper$x, xout = 0.1)$y
newx1b ## 
# newx1b <- max(MC_curve_upper$x)

newx2a <- approx(x = MC_curve_lower$y, y = MC_curve_lower$x, xout = 0.2)$y
newx2a
newx2a <- NA

newx2b <- approx(x = MC_curve_upper$y, y = MC_curve_upper$x, xout = 0.2)$y
newx2b

newx3a <- approx(x = MC_curve_lower$y, y = MC_curve_lower$x, xout = 0.3)$y
newx3a
newx3a <- NA

newx3b <- approx(x = MC_curve_upper$y, y = MC_curve_upper$x, xout = 0.3)$y
newx3b


## LOB curves

LOB_curve <- spline(new_dataL$Q, new_dataL$prob_fit,
                    xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)

LOB_curve_lower <- spline(new_dataL$Q, new_dataL$prob_fit,
                          xmin = min(new_dataL$Q), xmax = peakQL, ties = mean)
LOB_curve_upper <- spline(new_dataL$Q, new_dataL$prob_fit,
                          xmin = peakQL, xmax = max(new_dataL$Q), ties = mean)

newx1aL <- approx(x = LOB_curve_lower$y, y = LOB_curve_lower$x, xout = 0.1)$y
# newx1aL <- min(LOB_curve_lower$x)
newx1aL 

newx1bL <- approx(x = LOB_curve_upper$y, y = LOB_curve_upper$x, xout = 0.1)$y
newx1bL
# newx1bL <- max(LOB_curve_upper$x)

newx2aL <- approx(x = LOB_curve_lower$y, y = LOB_curve_lower$x, xout = 0.2)$y
newx2aL 

newx2bL <- approx(x = LOB_curve_upper$y, y = LOB_curve_upper$x, xout = 0.2)$y
newx2bL 

newx3aL <- approx(x = LOB_curve_lower$y, y = LOB_curve_lower$x, xout = 0.3)$y
newx3aL 

newx3bL <- approx(x = LOB_curve_upper$y, y = LOB_curve_upper$x, xout = 0.3)$y
newx3bL

## ROB curves

ROB_curve <- spline(new_dataR$Q, new_dataR$prob_fit,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)

ROB_curve_lower <- spline(new_dataR$Q, new_dataR$prob_fit,
                          xmin = min(new_dataR$Q), xmax = peakQR, ties = mean)
ROB_curve_upper <- spline(new_dataR$Q, new_dataR$prob_fit,
                          xmin = peakQR, xmax = max(new_dataR$Q), ties = mean)

## main channel values
newx1aR <- approx(x = ROB_curve_lower$y, y = ROB_curve_lower$x, xout = 0.1)$y
# newx1aR <- min(ROB_curve_lower$x)
newx1aR

newx1bR <- approx(x = ROB_curve_upper$y, y = ROB_curve_upper$x, xout = 0.1)$y
newx1bR
# newx1bR <- max(ROB_curve_upper$x)

newx2aR <- approx(x = ROB_curve_lower$y, y = ROB_curve_lower$x, xout = 0.2)$y
newx2aR

newx2bR <- approx(x = ROB_curve_upper$y, y = ROB_curve_upper$x, xout = 0.2)$y
newx2bR

newx3aR <- approx(x = ROB_curve_lower$y, y = ROB_curve_lower$x, xout = 0.3)$y
newx3aR

newx3bR <- approx(x = ROB_curve_upper$y, y = ROB_curve_upper$x, xout = 0.3)$y
newx3bR

## df for Q limits

limits <- as.data.frame(matrix(ncol=3, nrow=6)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_Lower", "Low_Prob_Upper", "Med_Prob_Lower",
                    "Med_Prob_Upper", "High_Prob_Lower", "High_Prob_Upper")

limits$LOB <- c(newx1aL, newx1bL, newx2aL, newx2bL, newx3aL, newx3bL)
limits$MC <- c(newx1a, newx1b, newx2a, newx2b, newx3a, newx3b)
limits$ROB <- c(newx1aR, newx1bR, newx2aR, newx2bR, newx3aR, newx3bR)

limits
# plot discharge points ---------------------------------------------------
unique(new_data$variable)

labels <- c(vel_m_LOB = "Left Over Bank", vel_m_MC = "Main Channel", vel_m_ROB = "Right Over Bank")

png("figures/Application_curves/Depth/LA20_adult_velocity_prob_Q_thresholds.png", width = 500, height = 600)

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  # name="Cross\nSection\nPosition",
  # breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"),
  #   labels = c("LOB", "MC", "ROB")) +
  
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  # geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.1, x=newx1a), color="green") +
  # geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.1, x=newx1b), color="green") +
  # geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.2, x=newx2a), color="red") +
  # geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.2, x=newx2b), color="red") +
  # geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.3, x=newx3a), color="blue") +
  # geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.3, x=newx3b), color="blue") +
  # 
  # geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.1, x=newx1aL), color="green") +
  # geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.1, x=newx1bL), color="green") +
  # geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.2, x=newx2aL), color="red") +
  # geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.2, x=newx2bL), color="red") +
# geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.3, x=newx3aL), color="blue") +
# geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.3, x=newx3bL), color="blue") +
# 
# geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.1, x=newx1aR), color="green") +
# geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.1, x=newx1bR), color="green") +
# geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.2, x=newx2aR), color="red") +
# geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.2, x=newx2bR), color="red") +
# geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.3, x=newx3aR), color="blue") +
# geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.3, x=newx3bR), color="blue") +

theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA20: Adult/Velocity: Probability ~ Q",
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

write.csv(limits, "output_data/F2_LA20_Adult_Velocity_Q_limits.csv")

# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataMx)

# create year_month column       
new_dataLx <- new_dataL %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataLx)

# create year_month column       
new_dataRx <- new_dataR %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataRx)

# discharge time series plots with probability lines ----------------------

# ##  plot time series of discharge - 0.2 prob line
# 
# ggplot(new_datax) +
#   geom_line(aes(x =DateTime, y=Q)) +
#   # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
#   # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
#   geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
#   geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
#   facet_wrap(~year, scales="free_x", nrow=4) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#   labs(title = "Discharge over time",
#        y = "Discharge",
#        x = "Time") #+ theme_bw(base_size = 15)

## make dataframe for all years 

## define critical period or season for adult as all year is critical
## define seasons/critical period

non_critical <- c(1,2,8:12) 
critical <- c(3:7) 

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

new_dataLx <- new_dataRx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

new_dataRx <- new_dataRx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )


## produces percentage of time for each year and season within year for each threshold

# sum(is.na(new_dataMx))
limits

time_statsm <- new_dataMx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(Q <= newx1b)/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(Q <= newx2b)/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(Q >= newx3a & Q <= newx3b)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(Q <= newx1b)/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(Q <= newx2b)/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(Q >= newx3a & Q <= newx3b)/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="MC")

time_statsm 

time_statsl <- new_dataLx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(Q <= newx1bL)/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(Q <= newx2bL)/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(Q >= newx3aL & Q <= newx3bL)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(Q <= newx1bL)/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(Q <= newx2bL)/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(Q >= newx3aL & Q <= newx3bL)/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="LOB")

time_statsl


time_statsr <- new_dataRx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(Q <= newx1bR)/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(Q <= newx2bR)/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(Q >= newx3aR & Q <= newx3bR)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(Q <= newx1bR)/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(Q <= newx2bR)/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(Q >= newx3aR & Q <= newx3bR)/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="ROB")

time_statsr


time_stats <- rbind(time_statsm, time_statsl, time_statsr)

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season", "position"))
melt_time <- rename(melt_time, Probability_Threshold = variable)
head(melt_time)
unique(melt_time$position)
write.csv(melt_time, "output_data/F2_LA20_adult_velocity_time_stats.csv")

## subset annual stats
ann_stats <- unique(melt_time$Probability_Threshold)[1:3]
melt_time_ann <- melt_time %>% filter(Probability_Threshold %in% ann_stats ) %>%
  select(-season) %>% distinct()

## subset seasonal stats
seas_stats <- unique(melt_time$Probability_Threshold)[4:6]
melt_time_seas <- filter(melt_time, Probability_Threshold %in% seas_stats )
melt_time_seas
## plot for annual stats - need probs in order
limits
png("figures/Application_curves/Velocity/LA20_adult_velocity_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group =c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA20: Time within discharge limit in relation to Velocity (Annual)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()
## plot for winter stats - need probs in order

melt_time_winter <- filter(melt_time_seas, season == "non_critical")
unique(melt_time_winter$season)

png("figures/Application_curves/Velocity/LA20_adult_velocity_perc_time_above_threshold_winter.png", width = 500, height = 600)

ggplot(melt_time_winter, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA20: Time within discharge limit in relation to Velocity (Non_critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()
## plot for summer stats - need probs in order

melt_time_summer <- filter(melt_time_seas, season == "critical")

png("figures/Application_curves/Velocity/LA20_adult_velocity_perc_time_above_threshold_critical.png", width = 500, height = 600)

ggplot(melt_time_summer, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA20: Time within discharge limit in relation to Velocity (Critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------

# all columns based on different probabilities
## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime
limits

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q <= newx1b)) %>%
  mutate(Low = if_else( Q <= newx1b, row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(Q <= newx2b)) %>%
  mutate(Medium = if_else( Q <= newx2b, row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3a & Q <= newx3b)) %>%
  mutate(High = if_else(Q >= newx3a & Q <= newx3b, row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")

new_dataL <- new_dataL %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q <= newx1bL)) %>%
  mutate(Low = if_else(Q <= newx1bL, row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(Q <= newx2bL)) %>%
  mutate(Medium = if_else(Q <= newx2bL, row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3aL & Q <= newx3bL)) %>%
  mutate(High = if_else(Q >= newx3aL & Q <= newx3bL, row_number(), 0L))

new_dataL <- mutate(new_dataL, position="LOB")

new_dataR <- new_dataR %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q <= newx1bR)) %>%
  mutate(Low = if_else(Q <= newx1bR, row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(Q <= newx2bR)) %>%
  mutate(Medium = if_else(Q <= newx2bR, row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3aR & Q <= newx3bR)) %>%
  mutate(High = if_else(Q >= newx3aR & Q <= newx3bR, row_number(), 0L))

new_dataR <- mutate(new_dataR, position="ROB")


## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_dataMx <- select(new_dataM, c(Q, month, year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs
names(new_dataMx)
new_dataLx <- select(new_dataL, c(Q, month, year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs
names(new_dataLx)
new_dataRx <- select(new_dataR, c(Q, month, year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs
names(new_dataRx)
## has some values but just becuase of the fake thresholds
# range(new_dataRx$Medium)
new_datax <- rbind(new_dataMx, new_dataLx, new_dataRx)
new_datax

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "ID03", "day", "month", "year", "Q", "position"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)

melt_data
## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Probability_Threshold == "Low") %>% 
  group_by(ID01, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_low = sum(n_days_low))

total_days_per_month01

total_days02 <- melt_data %>% 
  filter(Probability_Threshold == "Medium") %>% 
  group_by(ID02, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month02 <- total_days02 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_medium = sum(n_days_medium))

# total_days_per_month02

total_days03 <- melt_data %>% 
  filter(Probability_Threshold == "High") %>% 
  group_by(ID03, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month03 <- total_days03 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_high = sum(n_days_high))

total_days_per_month03

## combine all thresholds
total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
head(total_days)

write.csv(total_days, "output_data/F2_LA20_adult_velocity_total_days.csv")

# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F)


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


# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "year", "month", "season", "position"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/F2_LA20_adult_velocity_total_days_long.csv")


# melt_daysx <- filter(melt_days, position=="MC")
library(scales)

## plot all ts
png("figures/Application_curves/Velocity/LA20_adult_velocity_lob_rob_mc_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA20: Number of days within discharge limit in relation to Velocity",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()

## plot by year
png("figures/Application_curves/Velocity/LA20_adult_velocity_lob_rob_mc_no_days_within_Q_by_year.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%b")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(month_year), labels=format(month_year,"%b")) +
  facet_wrap(~year+position, scale="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA20: Number of days within discharge limit in relation to Velocity",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)
dev.off()

## plot by season/critical period
png("figures/Application_curves/Velocity/LA20_adult_velocity_lob_rob_mc_no_days_within_Q_by_season.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%Y")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%Y")) +
  facet_wrap(~season +position, scales="free", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA20: Number of days within discharge limit in relation to Velocity",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()

