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


# F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv")
F37B_High <- read.csv("input_data/HecRas/hydraulic_ts_F37B_High.csv")
# F45B <- read.csv("input_data/HecRas/hydraulic_ts_F45B.csv")
# F319 <- read.csv("input_data/HecRas/hydraulic_ts_F319.csv")
# LA13 <- read.csv("input_data/HecRas/hydraulic_ts_LA13.csv")
# LA1 <- read.csv("input_data/HecRas/hydraulic_ts_LA1.csv")


hydraul <- F37B_High[,-1]
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
                         paste("critical"), paste("non_critical") )) %>%
mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))

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

hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num", "month", "day", "water_year","year", "hour","season"))
hyd_dep <- filter(hyd_dep, variable == "depth_cm_MC" )
hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num", "month", "day", "water_year","year", "hour", "season"))
hyd_vel <- filter(hyd_vel, variable == "vel_m_MC" )
head(hyd_dep)

# Depth -------------------------------------------------------------------


## plot
range(hyd_dep$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_dep) == FALSE)
# nas #0


## filter data by cross section position
new_dataM <- filter(hyd_dep, variable == "depth_cm_MC")


## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)

if(min(MC_curve$y)>18) {
  newx1a <- min(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 18)$y
}


## plot with thresholds
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

png("figures/Application_curves/Depth/F37B_High_Migration_In_Depth_Q.png", width = 500, height = 600)

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=18, x=newx1a), color="green") +
  # geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=18, x=newx1aL), color="green") +
  # geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=18, x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F37B_High: Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


### time stats


time_statsm <- new_dataM %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm


time_stats <- time_statsm
time_stats <-time_stats %>%
  pivot_wider(names_from = season, values_from = Seasonal)

## melt
melt_time<-reshape2::melt(time_stats, id=c("water_year", "position"))
melt_time <- rename(melt_time, Time_Period = variable)
head(melt_time)
write.csv(melt_time, "output_data/F5_F37B_High_SH_migration_depth_time_stats.csv")


# Number of days above discharge ------------------------------------------

## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(Q >= newx1a)) %>%
  mutate(Min.Depth = if_else(Q >= newx1a, row_number(), 0L))
  
new_dataM <- mutate(new_dataM, position="MC")


## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, water_year, day all IDs and probs
# names(new_data)

new_datax <- select(new_dataM, c(Q, month, water_year, day, ID01, Min.Depth, position, DateTime))# all probs

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "day", "month", "water_year", "Q", "position", "DateTime"))
melt_data <- rename(melt_data, Annual = variable, 
                    consec_hours = value)

unique(melt_data$Annual)
head(melt_data)

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Annual == "Min.Depth") %>% 
  group_by(ID01, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%

## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month = sum(n_days_low))

total_days <- total_days_per_month01
head(total_days)

write.csv(total_days, "output_data/F5_F37B_High_migration_depth_total_days.csv")

# # create water_year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)


## convert month water_year to date format
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

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Annual = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/F5_F37B_High_migration_depth_total_days_long.csv")

# 
# melt_daysx <- filter(melt_days, position=="MC")
# library(scales)


# Velocity -------------------------------------------------------------------


## plot
range(hyd_vel$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_vel) == FALSE)
# nas #0
head(hyd_vel)
sum(is.na(hyd_vel))

## filter data by cross section position
new_dataM <- filter(hyd_vel, variable == "vel_m_MC")

range(new_dataM$value)
## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)

## main channel values
if(min(MC_curve$y)>0.1) {
  newx1a <- min(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 0.1)$y
}


if(max(MC_curve$y)<3.1) {
  newx2a <- max(MC_curve$x)
} else {
  newx2a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 3.1)$y
}


## plot with thresholds
labels <- c(vel_m_LOB = "Left Over Bank", vel_m_MC = "Main Channel", vel_m_ROB = "Right Over Bank")

png("figures/Application_curves/nodes/F37B_High_Migration_In_Velocity_Q.png", width = 500, height = 600)

ggplot(hyd_vel, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_vel, variable =="vel_m_MC"), aes(y=3.1, x=newx1a), color="green") +
  # geom_point(data = subset(hyd_vel, variable =="vel_m_LOB"), aes(y=3.1, x=newx1aL), color="green") +
  # geom_point(data = subset(hyd_vel, variable =="vel_m_ROB"), aes(y=max(new_dataR$value), x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F37B_High: Velocity ~ Q",
       y = "Velocity (m/s)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


### time stats

time_statsm <- new_dataM %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q <= newx1a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx1a)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm

time_stats <- time_statsm
time_stats <-time_stats %>%
  pivot_wider(names_from = season, values_from = Seasonal)

## melt
melt_time<-reshape2::melt(time_stats, id=c("water_year", "position"))
melt_time <- rename(melt_time, Time_Period = variable)
melt_time$value <- ifelse(is.na(melt_time$value), 0,melt_time$value )

write.csv(melt_time, "output_data/F5_F37B_High_SH_migration_velocity_time_stats.csv")


png("figures/Application_curves/Depth/F37B_High_migration_in_velocity_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time, aes(x = water_year, y=value)) +
  geom_line(aes( group = c(), color = Time_Period)) +
  scale_color_manual(name = "Time Period", breaks = c("Annual", "other_season", "Migration_season"),
                     values=c( "blue", "green", "red"),
                     labels = c("Annual", "Non Critical", "Migration")) +
  scale_y_continuous(limits=c(0,100)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_water_year), labels=format(total_days$month_water_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Time within discharge limit in relation to Velocity",
       y = "Time (%)",
       x = "water_year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------

## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(Q <= newx1a)) %>%
  mutate(Min.Depth = if_else(Q <= newx1a, row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")

new_dataM
## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, water_year, day all IDs and probs
# names(new_data)

new_datax <- select(new_dataM, c(Q, month, water_year, day, ID01, Min.Depth, position, DateTime))# all probs


## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "day", "month", "water_year", "Q", "position", "DateTime"))
melt_data <- rename(melt_data, Annual = variable, 
                    consec_hours = value)

unique(melt_data$Annual)

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Annual == "Min.Depth") %>% 
  group_by(ID01, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%

## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month = sum(n_days_low))

total_days <- total_days_per_month01
total_days

write.csv(total_days, "output_data/F5_F37B_High_migration_in_velocity_total_days.csv")

# # create water_year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)


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

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Annual = variable,
                    n_days = value)
# melt_days <- melt_days %>%
#   mutate(n_days = ifelse(is.na(newx1a), 0, n_days))

melt_days

## save df
write.csv(melt_days, "output_data/F5_F37B_High_migration_in_velocity_total_days_long.csv")

# 
# melt_daysx <- filter(melt_days, position=="MC")
# library(scales)


# Migration out (smolts) --------------------------------------------------


# Depth -------------------------------------------------------------------


## plot
range(hyd_dep$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_dep) == FALSE)
# nas #0


## filter data by cross section position
new_dataM <- filter(hyd_dep, variable == "depth_cm_MC")


## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)

if(min(MC_curve$y)>12) {
  newx1a <- min(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 12)$y
}


## plot with thresholds
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

png("figures/Application_curves/Depth/F37B_High_Migration_OUT_Depth_Q.png", width = 500, height = 600)

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=18, x=newx1a), color="green") +
  # geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=18, x=newx1aL), color="green") +
  # geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=18, x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F37B_High: Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


### time stats


time_statsm <- new_dataM %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm


time_stats <- time_statsm
time_stats <-time_stats %>%
  pivot_wider(names_from = season, values_from = Seasonal)

## melt
melt_time<-reshape2::melt(time_stats, id=c("water_year", "position"))
melt_time <- rename(melt_time, Time_Period = variable)
head(melt_time)
write.csv(melt_time, "output_data/F5_F37B_High_SH_smolts_depth_time_stats.csv")


# Number of days above discharge ------------------------------------------

## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(Q >= newx1a)) %>%
  mutate(Min.Depth = if_else(Q >= newx1a, row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")


## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, water_year, day all IDs and probs
# names(new_data)

new_datax <- select(new_dataM, c(Q, month, water_year, day, ID01, Min.Depth, position, DateTime))# all probs

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "day", "month", "water_year", "Q", "position", "DateTime"))
melt_data <- rename(melt_data, Annual = variable, 
                    consec_hours = value)

unique(melt_data$Annual)
head(melt_data)

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Annual == "Min.Depth") %>% 
  group_by(ID01, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%

## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month = sum(n_days_low))

total_days <- total_days_per_month01
head(total_days)

write.csv(total_days, "output_data/F5_F37B_High_smolts_depth_total_days.csv")

# # create water_year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)


## convert month water_year to date format
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

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Annual = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/F5_F37B_High_smolts_depth_total_days_long.csv")

# 
# melt_daysx <- filter(melt_days, position=="MC")
# library(scales)


# Velocity -------------------------------------------------------------------


## plot
range(hyd_vel$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_vel) == FALSE)
# nas #0
head(hyd_vel)
sum(is.na(hyd_vel))

## filter data by cross section position
new_dataM <- filter(hyd_vel, variable == "vel_m_MC")

range(new_dataM$value)
## Main channel curve

MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)
## main channel values
if(min(MC_curve$y)>0.1) {
  newx1a <- min(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 0.1)$y
}
newx1a

if(max(MC_curve$y)<0.49) {
  newx2a <- max(MC_curve$x)
} else if (min(MC_curve$y) > 0.49) {
  newx2a <- min(MC_curve$x)
} else {   
  newx2a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 0.49)$y
}



## plot with thresholds
labels <- c(vel_m_LOB = "Left Over Bank", vel_m_MC = "Main Channel", vel_m_ROB = "Right Over Bank")

png("figures/Application_curves/nodes/F37B_High_Migration_OUT_Velocity_Q.png", width = 500, height = 600)

ggplot(hyd_vel, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_vel, variable =="vel_m_MC"), aes(y=3.1, x=newx1a), color="green") +
  # geom_point(data = subset(hyd_vel, variable =="vel_m_LOB"), aes(y=3.1, x=newx1aL), color="green") +
  # geom_point(data = subset(hyd_vel, variable =="vel_m_ROB"), aes(y=max(new_dataR$value), x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F37B_High: Velocity ~ Q",
       y = "Velocity (m/s)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


### time stats

time_statsm <- new_dataM %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q <= newx1a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx1a)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm

time_stats <- time_statsm
time_stats <-time_stats %>%
  pivot_wider(names_from = season, values_from = Seasonal)

## melt
melt_time<-reshape2::melt(time_stats, id=c("water_year", "position"))
melt_time <- rename(melt_time, Time_Period = variable)
melt_time$value <- ifelse(is.na(melt_time$value), 0,melt_time$value )

write.csv(melt_time, "output_data/F5_F37B_High_SH_smolts_velocity_time_stats.csv")


png("figures/Application_curves/Depth/F37B_High_migration_OUT_velocity_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time, aes(x = water_year, y=value)) +
  geom_line(aes( group = c(), color = Time_Period)) +
  scale_color_manual(name = "Time Period", breaks = c("Annual", "other_season", "Migration_season"),
                     values=c( "blue", "green", "red"),
                     labels = c("Annual", "Non Critical", "Migration")) +
  scale_y_continuous(limits=c(0,100)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_water_year), labels=format(total_days$month_water_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F37B_High: Time within discharge limit in relation to Velocity",
       y = "Time (%)",
       x = "water_year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------

## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(Q <= newx1a)) %>%
  mutate(Min.Depth = if_else(Q <= newx1a, row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")

new_dataM
## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, water_year, day all IDs and probs
# names(new_data)

new_datax <- select(new_dataM, c(Q, month, water_year, day, ID01, Min.Depth, position, DateTime))# all probs


## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "day", "month", "water_year", "Q", "position", "DateTime"))
melt_data <- rename(melt_data, Annual = variable, 
                    consec_hours = value)

unique(melt_data$Annual)

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Annual == "Min.Depth") %>% 
  group_by(ID01, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%

## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month = sum(n_days_low))

total_days <- total_days_per_month01
total_days

write.csv(total_days, "output_data/F5_F37B_High_smolts_in_velocity_total_days.csv")

# # create water_year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)


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

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Annual = variable,
                    n_days = value)
# melt_days <- melt_days %>%
#   mutate(n_days = ifelse(is.na(newx1a), 0, n_days))

melt_days

## save df
write.csv(melt_days, "output_data/F5_F37B_High_smolts_in_velocity_total_days_long.csv")

# 
# melt_daysx <- filter(melt_days, position=="MC")
# library(scales)



