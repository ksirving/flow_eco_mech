## migration model

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

# Upload hydraulic data -------------------------------------------

## soft bottom reaches

# F57C <- read.csv("input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("input_data/HecRas/hydraulic_ts_LA8.csv")
LA11 <- read.csv("input_data/HecRas/hydraulic_ts_LA11.csv")
# LA20 <- read.csv("input_data/HecRas/hydraulic_ts_LA20_2.csv")

hydraul <- LA11[,-1]
names(hydraul)

## change some names
hydraul <- hydraul %>%
  rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow)

## change names and transform ft to cm
hyd_dep <- hydraul %>%
  select(c(DateTime, Q, node, Avg..Vel...ft.s..LOB, Hydr..Depth..ft..LOB,Avg..Vel...ft.s..MC, Hydr..Depth..ft..MC, 
           Avg..Vel...ft.s..ROB, Hydr..Depth..ft..ROB)) %>%
  rename(vel_ft_LOB = Avg..Vel...ft.s..LOB, depth_ft_LOB = Hydr..Depth..ft..LOB, vel_ft_MC = Avg..Vel...ft.s..MC,
         depth_ft_MC = Hydr..Depth..ft..MC, vel_ft_ROB = Avg..Vel...ft.s..ROB, depth_ft_ROB = Hydr..Depth..ft..ROB) %>%
  mutate(depth_cm_LOB = (depth_ft_LOB*0.3048)*100,
         depth_cm_MC = (depth_ft_MC*0.3048)*100,
         depth_cm_ROB = (depth_ft_ROB*0.3048)*100) %>%
  mutate(vel_m_LOB = (vel_ft_LOB*0.3048),
         vel_m_MC = (vel_ft_MC*0.3048),
         vel_m_ROB = (vel_ft_ROB*0.3048)) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

head(hyd_dep)

## format date time
hyd_dep$DateTime<-as.POSIXct(hyd_dep$DateTime,
                             format = "%Y-%m-%d %H:%M",
                             tz = "GMT")


# Adult migration (In) ----------------------------------------------------


## create year, month, day and hour columns

hyd_dep <- hyd_dep %>%
  mutate(month = month(DateTime))%>%
  mutate(year = year(DateTime))%>%
  mutate(day = day(DateTime))%>%
  mutate(hour = hour(DateTime)) %>%
  mutate(season = ifelse(month == 12 | month == 1 | month == 2 | month == 3 | month == 4| month == 5 | month == 6, 
                         paste("critical"), paste("non_critical") ))

## rules for migration

## more than 18cm depth
## less than 3.1 m/s velocity

hyd_vel <- hyd_dep %>%
  select(-contains("depth"))
head(hyd_vel)

hyd_dep <- hyd_dep %>%
  select(-contains("vel"))
head(hyd_dep)
# ## melt channel position data

hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num", "month", "day", "year", "hour","season"))
hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num", "month", "day", "year", "hour", "season"))
head(hyd_dep)
head(hyd_vel)

# Depth -------------------------------------------------------------------


## plot
range(hyd_dep$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_dep) == FALSE)
# nas #0


## filter data by cross section position
new_dataM <- filter(hyd_dep, variable == "depth_cm_MC")
new_dataL <- filter(hyd_dep, variable == "depth_cm_LOB")
new_dataR <- filter(hyd_dep, variable == "depth_cm_ROB")

## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)
## main channel values
newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 18)$y
newx2a <- NA
newx3a <- NA


## LOB curve
LOB_curve <- spline(new_dataL$Q, new_dataL$value,
                   xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)
## LOB values
newx1aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 18)$y
newx1aL
newx2aL <- NA
newx3aL <- NA

## ROB curve
ROB_curve <- spline(new_dataR$Q, new_dataR$value,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)
## ROB values
newx1aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = 18)$y
newx1aR
newx2aR <- NA
newx3aR <- NA

limits <- as.data.frame(matrix(ncol=3, nrow=12)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_1", "Low_Prob_2", "Low_Prob_3", "Low_Prob_4",
                    "Med_Prob_1", "Med_Prob_2", "Med_Prob_3", "Med_Prob_4",
                    "High_Prob_1", "High_Prob_2", "High_Prob_3", "High_Prob_4")

limits$LOB <- c(newx1aL[1], newx1aL[2],newx1aL[3],newx1aL[4], 
                newx2aL[1], newx2aL[2],newx2aL[3], newx2aL[4],  
                newx3aL[1], newx3aL[2],newx3aL[3], newx3aL[4])

limits$MC <- c(newx1a[1], newx1a[2],newx1a[3], newx1aL[4],
               newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
               newx3a[1], newx3a[2],newx3a[3],newx3a[4])

limits$ROB <- c(newx1aR[1], newx1aR[2],newx1aR[3], newx1aR[4], 
                newx2aR[1], newx2aR[2],newx2aR[3], newx2aR[4],
                newx3aR[1], newx3aR[2],newx3aR[3], newx3aR[4])

limits

write.csv(limits, "output_data/F5_LA11_migration_IN_depth_Q_limits.csv")

## plot with thresholds
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

png("figures/Application_curves/Depth/LA11_Migration_In_Depth_Q.png", width = 500, height = 600)

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=18, x=newx1a), color="green") +
  geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=18, x=newx1aL), color="green") +
  geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=18, x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA11: Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


### time stats
newx1a
time_statsm <- new_dataM %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a)/length(DateTime)*100) %>%
  distinct(year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm

time_statsl <- new_dataL %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1aL)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1aL)/length(DateTime)*100) %>%
  distinct(year, Annual, Seasonal) %>%
  mutate(position="LOB")

time_statsr <- new_dataR %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1aR)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1aR)/length(DateTime)*100) %>%
  distinct(year, Annual, Seasonal) %>%
  mutate(position="ROB")


time_stats <- rbind(time_statsm, time_statsl, time_statsr)
time_stats <-time_stats %>%
  pivot_wider(names_from = season, values_from = Seasonal)

## melt
melt_time<-reshape2::melt(time_stats, id=c("year", "position"))
melt_time <- rename(melt_time, Time_Period = variable)

write.csv(melt_time, "output_data/F5_LA11_SH_migration_depth_time_stats.csv")


png("figures/Application_curves/Depth/LA11_migration_depth_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Time_Period)) +
  scale_color_manual(name = "Time Period", breaks = c("Annual", "other_season", "Migration_season"),
                     values=c( "blue", "green", "red"),
                     labels = c("Annual", "Non Critical", "Migration")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Time within discharge limit in relation to Depth",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------

## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1a)) %>%
  mutate(Min.Depth = if_else(Q >= newx1a, row_number(), 0L))
  
new_dataM <- mutate(new_dataM, position="MC")

new_dataL <- new_dataL %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1aL)) %>%
  mutate(Min.Depth = if_else(Q >= newx1aL, row_number(), 0L))

new_dataL <- mutate(new_dataL, position="LOB")

new_dataR <- new_dataR %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1aR)) %>%
  mutate(Min.Depth = if_else(Q >= newx1aR, row_number(), 0L))

new_dataR <- mutate(new_dataR, position="ROB")

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_dataMx <- select(new_dataM, c(Q, month, year, day, ID01, Min.Depth, position, DateTime))# all probs
names(new_dataMx)
new_dataLx <- select(new_dataL, c(Q, month, year, day, ID01, Min.Depth, position, DateTime) )# all probs
names(new_dataLx)
new_dataRx <- select(new_dataR, c(Q, month, year, day, ID01, Min.Depth, position, DateTime) )# all probs
names(new_dataRx)
## has some values but just becuase of the fake thresholds
# range(new_dataRx$Medium)
new_datax <- rbind(new_dataMx, new_dataLx, new_dataRx)

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "day", "month", "year", "Q", "position", "DateTime"))
melt_data <- rename(melt_data, Annual = variable, 
                    consec_hours = value)

unique(melt_data$Annual)

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Annual == "Min.Depth") %>% 
  group_by(ID01, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%

## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month = sum(n_days_low))

total_days <- total_days_per_month01

total_days
write.csv(total_days, "output_data/F5_LA11_migration_depth_total_days.csv")

# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F)


## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)


## define seasons/critical period
non_critical <- c(7:11) 
critical <- c(12, 1:6) 

total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") ) #%>%
  # pivot_wider(names_from = season, values_from = days_per_month)



# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "year", "month", "season", "position"))
melt_days <- rename(melt_days, Annual = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/F5_LA11_migration_depth_total_days_long.csv")

# 
# melt_daysx <- filter(melt_days, position=="MC")
# library(scales)

## plot all ts
png("figures/Application_curves/Depth/LA11_migration_depth_lob_rob_mc_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days) +
  geom_line(aes(x =month_year, y=n_days)) +
  # scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
  #                    values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()

## plot by year
png("figures/Application_curves/Depth/LA11_migration_depth_lob_rob_mc_no_days_within_Q_by_year.png", width = 500, height = 600)

ggplot(melt_days) +
  geom_line(aes(x =month_year, y=n_days)) +
  # scale_color_manual(name="Probability Threshold", breaks = c("Low", "Medium", "High"),
  #                    values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%b")) +
  # scale_x_continuous(breaks=as.numeric(month_year), labels=format(month_year,"%b")) +
  facet_wrap(~year+position, scale="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Number of days within discharge limit in relation to Depth (by year)",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)
dev.off()

## plot by season/critical period
png("figures/Application_curves/Depth/LA11_migration_depth_lob_rob_mc_no_days_within_Q_by_season.png", width = 500, height = 600)

ggplot(melt_days) +
  geom_line(aes(x =month_year, y=n_days)) +
  # scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
  #                    values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%Y")) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%Y")) +
  facet_wrap(~season +position, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Number of days within discharge limit in relation to Depth (Seasonal)",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()

# Velocity -------------------------------------------------------------------


## plot
range(hyd_vel$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_vel) == FALSE)
# nas #0
head(hyd_vel)
sum(is.na(hyd_vel))

## filter data by cross section position
new_dataM <- filter(hyd_vel, variable == "vel_m_MC")
new_dataL <- filter(hyd_vel, variable == "vel_m_LOB")
new_dataR <- filter(hyd_vel, variable == "vel_m_ROB")

head(new_dataM)
## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)
MC_curve
max(new_dataR$value)
## main channel values
newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 3.1)$y ## does not go up to 3.1, use max value instead
newx1a
newx2a <- NA
newx3a <- NA

## LOB curve
LOB_curve <- spline(new_dataL$Q, new_dataL$value,
                    xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)
## LOB values
newx1aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 3.1)$y
newx1aL
newx2aL <- NA
newx3aL <- NA
## ROB curve
ROB_curve <- spline(new_dataR$Q, new_dataR$value,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)
## ROB values
newx1aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = max(new_dataR$value))$y ## also does not go to 3.1
newx1aR
newx2aR <- NA
newx3aR <- NA


limits <- as.data.frame(matrix(ncol=3, nrow=12)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_1", "Low_Prob_2", "Low_Prob_3", "Low_Prob_4",
                    "Med_Prob_1", "Med_Prob_2", "Med_Prob_3", "Med_Prob_4",
                    "High_Prob_1", "High_Prob_2", "High_Prob_3", "High_Prob_4")

limits$LOB <- c(newx1aL[1], newx1aL[2],newx1aL[3],newx1aL[4], 
                newx2aL[1], newx2aL[2],newx2aL[3], newx2aL[4],  
                newx3aL[1], newx3aL[2],newx3aL[3], newx3aL[4])

limits$MC <- c(newx1a[1], newx1a[2],newx1a[3], newx1aL[4],
               newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
               newx3a[1], newx3a[2],newx3a[3],newx3a[4])

limits$ROB <- c(newx1aR[1], newx1aR[2],newx1aR[3], newx1aR[4], 
                newx2aR[1], newx2aR[2],newx2aR[3], newx2aR[4],
                newx3aR[1], newx3aR[2],newx3aR[3], newx3aR[4])

limits

write.csv(limits, "output_data/F5_LA11_migration_IN_velocity_Q_limits.csv")
## plot with thresholds
labels <- c(vel_m_LOB = "Left Over Bank", vel_m_MC = "Main Channel", vel_m_ROB = "Right Over Bank")

png("figures/Application_curves/nodes/LA11_Migration_In_Velocity_Q.png", width = 500, height = 600)

ggplot(hyd_vel, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_vel, variable =="vel_m_MC"), aes(y=3.1, x=newx1a), color="green") +
  geom_point(data = subset(hyd_vel, variable =="vel_m_LOB"), aes(y=3.1, x=newx1aL), color="green") +
  geom_point(data = subset(hyd_vel, variable =="vel_m_ROB"), aes(y=max(new_dataR$value), x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA11: Velocity ~ Q",
       y = "Velocity (m/s)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


### time stats

time_statsm <- new_dataM %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Annual = sum(Q <= newx1a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx1a)/length(DateTime)*100) %>%
  distinct(year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsl

time_statsl <- new_dataL %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Annual = sum(Q <= newx1aL)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx1aL)/length(DateTime)*100) %>%
  distinct(year, Annual, Seasonal) %>%
  mutate(position="LOB")

time_statsr <- new_dataR %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Annual = sum(Q <= newx1aR)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx1aR)/length(DateTime)*100) %>%
  distinct(year, Annual, Seasonal) %>%
  mutate(position="ROB")


time_stats <- rbind(time_statsm, time_statsl, time_statsr)
time_stats <-time_stats %>%
  pivot_wider(names_from = season, values_from = Seasonal)

## melt
melt_time<-reshape2::melt(time_stats, id=c("year", "position"))
melt_time <- rename(melt_time, Time_Period = variable)

write.csv(melt_time, "output_data/F5_LA11_SH_migration_velocity_time_stats.csv")
melt_time

png("figures/Application_curves/Depth/LA11_migration_in_velocity_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Time_Period)) +
  scale_color_manual(name = "Time Period", breaks = c("Annual", "non_critical", "critical"),
                     values=c( "blue", "green", "red"),
                     labels = c("Annual", "Non Critical", "Critical")) +
  scale_y_continuous(limits=c(0,100)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Time within discharge limit in relation to Velocity",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------

## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q <= newx1a)) %>%
  mutate(Min.Depth = if_else(Q <= newx1a, row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")

new_dataL <- new_dataL %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q <= newx1aL)) %>%
  mutate(Min.Depth = if_else(Q <= newx1aL, row_number(), 0L))

new_dataL <- mutate(new_dataL, position="LOB")

new_dataR <- new_dataR %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q <= newx1aR)) %>%
  mutate(Min.Depth = if_else(Q <= newx1aR, row_number(), 0L))

new_dataR <- mutate(new_dataR, position="ROB")

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_dataMx <- select(new_dataM, c(Q, month, year, day, ID01, Min.Depth, position, DateTime))# all probs
names(new_dataMx)
new_dataLx <- select(new_dataL, c(Q, month, year, day, ID01, Min.Depth, position, DateTime) )# all probs
names(new_dataLx)
new_dataRx <- select(new_dataR, c(Q, month, year, day, ID01, Min.Depth, position, DateTime) )# all probs
names(new_dataRx)
## has some values but just becuase of the fake thresholds
# range(new_dataRx$Medium)
new_datax <- rbind(new_dataMx, new_dataLx, new_dataRx)

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "day", "month", "year", "Q", "position", "DateTime"))
melt_data <- rename(melt_data, Annual = variable, 
                    consec_hours = value)

unique(melt_data$Annual)

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Annual == "Min.Depth") %>% 
  group_by(ID01, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%

## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month = sum(n_days_low))

total_days <- total_days_per_month01


write.csv(total_days, "output_data/F5_LA11_migration_in_velocity_total_days.csv")

# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F)


## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)


## define seasons/critical period
non_critical <- c(7:11) 
critical <- c(12, 1:6) 

total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") ) #%>%
# pivot_wider(names_from = season, values_from = days_per_month)

# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "year", "month", "season", "position"))
melt_days <- rename(melt_days, Annual = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/F5_LA11_migration_in_velocity_total_days_long.csv")

# 
# melt_daysx <- filter(melt_days, position=="MC")
# library(scales)

## plot all ts
png("figures/Application_curves/Velocity/LA11_migration_in_velocity_lob_rob_mc_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days) +
  geom_line(aes(x =month_year, y=n_days)) +
  # scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
  #                    values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Number of days within discharge limit in relation to Velocity",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()

## plot by year
png("figures/Application_curves/Velocity/LA11_migration_in_velocity_lob_rob_mc_no_days_within_Q_by_year.png", width = 500, height = 600)

ggplot(melt_days) +
  geom_line(aes(x =month_year, y=n_days)) +
  # scale_color_manual(name="Probability Threshold", breaks = c("Low", "Medium", "High"),
  #                    values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%b")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(month_year), labels=format(month_year,"%b")) +
  facet_wrap(~year+position, scale="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Number of days within discharge limit in relation to Velocity (by year)",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)
dev.off()

## plot by season/critical period
png("figures/Application_curves/Velocity/LA11_migration_in_velocity_lob_rob_mc_no_days_within_Q_by_season.png", width = 500, height = 600)

ggplot(melt_days) +
  geom_line(aes(x =month_year, y=n_days)) +
  # scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
  #                    values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%Y")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%Y")) +
  facet_wrap(~season +position, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA11: Number of days within discharge limit in relation to Velocity (Seasonal)",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()


