## fry algorithm

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


## upload hydraulic data

# F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv")
# F37B_High <- read.csv("input_data/HecRas/hydraulic_ts_F37B_High.csv")
# F45B <- read.csv("input_data/HecRas/hydraulic_ts_F45B.csv")
# F319 <- read.csv("input_data/HecRas/hydraulic_ts_F319.csv")
# LA13 <- read.csv("input_data/HecRas/hydraulic_ts_LA13.csv")
LA1 <- read.csv("input_data/HecRas/hydraulic_ts_LA1.csv")

hydraul <- LA1[,-1]
head(hydraul)

## change names and transform ft to cm
hyd_dep <- hydraul %>%
  rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow) %>%
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

## create year, month, day and hour columns

hyd_dep <- hyd_dep %>%
  mutate(month = month(DateTime))%>%
  mutate(year = year(DateTime))%>%
  mutate(day = day(DateTime))%>%
  mutate(hour = hour(DateTime)) %>%
  mutate(season = ifelse(month == 3 | month == 4 | month == 5 | month == 6 | month == 7, paste("critcal"), paste("non_critical")))%>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))

## rules for fry 

## depth 3 - 10 cm
## velocity - low/negligable/undetectable - lowest velocity in adult curve = 0.000?
## temperature - 18-24 c

## separate velocity and depth

hyd_vel <- hyd_dep %>%
  select(-contains("depth"))
head(hyd_vel)

hyd_dep <- hyd_dep %>%
  select(-contains("vel"))
head(hyd_dep)
# ## melt channel position data

hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num", "month", "day", "year", "hour","season", "water_year"))
hyd_dep <- filter(hyd_dep, variable == "depth_cm_MC" )
hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num", "month", "day", "water_year","year", "hour", "season"))
hyd_vel <- filter(hyd_vel, variable == "vel_m_MC" )


# Depth -------------------------------------------------------------------

## plot
range(hyd_dep$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_dep) == FALSE)
# nas #0


## filter data by cross section position
new_dataM <- filter(hyd_dep, variable == "depth_cm_MC")

MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)


if(min(MC_curve$y)>3 && min(MC_curve$y)<10) {
  newx1a <- min(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 3)$y
}
newx1a
if(min(MC_curve$y)>10) {
  newx2a <- min(MC_curve$x)
} else {
  newx2a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 10)$y
}

newx2a
## MAKE DF OF Q LIMITS

limits <- as.data.frame(matrix(ncol=1, nrow=2)) %>%
  rename(MC = V1) 
rownames(limits)<-c("Low_Prob",
                    "Med_Prob")


limits$MC <- c(newx1a, newx2a)

limits

write.csv(limits, "output_data/F4_LA1_sas_fry_depth_Q_limits.csv")

## plot with thresholds
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

png("figures/Application_curves/Depth/LA1_SAS_Fry_depth_Q.png", width = 500, height = 600)

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=3, x=newx1a), color="green") +
  # geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=3, x=newx1aL), color="green") +
  # geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=3, x=newx1aR), color="green") +
  
  geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=10, x=newx2a), color="green") +
  # geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=10, x=newx2aL), color="green") +
  # geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=10, x=newx2aR), color="green") +
  # 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA1: Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

# time stats ------------------------------------------------
### time stats

time_statsm <- new_dataM %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1a & Q <= newx2a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a & Q <= newx2a)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm



time_stats <- time_statsm

time_stats
write.csv(time_stats, "output_data/F4_LA1_time_stats_fry_depth.csv")

# Number of days within Q limits ------------------------------------------

## depth
new_dataM <- filter(hyd_dep, variable=="depth_cm_MC")


## change year to water year and count hours within Q range
new_dataM  <- new_dataM %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a & Q <= newx2a)) %>%
  mutate(threshold = if_else(Q >= newx1a & Q <= newx2a,  row_number(), 0L)) %>%
  mutate(position="MC")

# select columns needed and combine dfs

new_datax <- select(new_dataM, c(Q, month, water_year, year, day, ID, threshold, position, season)) # all probs

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID", "day", "month", "year", "Q", "water_year", "position", "season"))
melt_data <- melt_data %>% rename(consec_hours = value) %>%
  select(-variable)

## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  group_by(ID, day, month, water_year, position, season) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(water_year, position, season, month) %>%
  summarise(days_per_water_month = sum(n_days)) #%>%

total_days_per_month01

write.csv(total_days_per_month01, "output_data/F4_LA1_number_of_days_fry_depth.csv")



# Velocity ----------------------------------------------------------------


## plot
range(hyd_vel$Q) ## 0.00 998.845 
range(new_dataM$value)
unique(sort(new_dataM$value))
## smooth spline the curve to get exact value of discharge at a given probability

new_dataM <- filter(hyd_vel, variable=="vel_m_MC")


## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)
## main channel values
if(min(MC_curve$y)>0.1 && min(MC_curve$y)<0.15) {
  newx1a <- min(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 0.1)$y
}

if(min(MC_curve$y)>0.15) {
  newx2a <- min(MC_curve$x)
} else {
  newx2a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 0.15)$y
}

newx2a 


limits <- as.data.frame(matrix(ncol=1, nrow=2)) %>%
  rename(MC = V1) 
rownames(limits)<-c("Low_Prob",
                    "Med_Prob")

limits$MC <- c(newx1a, newx2a)

limits

## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
write.csv(limits, "output_data/F1_LA1_fry_velocity_Q_limits.csv")


## plot with thresholds
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

png("figures/Application_curves/Depth/LA1_SAS_Fry_velocity_Q.png", width = 500, height = 600)

ggplot(hyd_vel, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_vel, variable =="vel_m_MC"), aes(y=0.1, x=newx1a), color="green") +
  geom_point(data = subset(hyd_vel, variable =="vel_m_MC"), aes(y=0.15, x=newx2a), color="green") +
  # geom_point(data = subset(hyd_vel, variable =="vel_m_LOB"), aes(y=0.15, x=newx1aL), color="green") +
  # geom_point(data = subset(hyd_vel, variable =="vel_m_ROB"), aes(y=0.15, x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA1: Velocity ~ Q",
       y = "Velocity (m/s)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


### time stats

time_statsm <- new_dataM %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1a & Q <= newx2a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a & Q <= newx2a)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm

time_stats <- time_statsm

time_stats
write.csv(time_stats, "output_data/F4_LA1_time_stats_fry_velocity.csv")


## velocity
new_dataM <- filter(hyd_vel, variable=="vel_m_MC")


## change year to water year and count hours within Q range
new_dataM  <- new_dataM %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a & Q <= newx2a)) %>%
  mutate(threshold = if_else(Q >= newx1a & Q <= newx2a,  row_number(), 0L)) %>%
  mutate(position="MC")

# select columns needed and combine dfs

new_datax <- select(new_dataM, c(Q, month, water_year, year, day, ID, threshold, position, season)) # all probs

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID", "day", "month", "year", "Q", "water_year", "position", "season"))
melt_data <- melt_data %>% rename(consec_hours = value) %>%
  select(-variable)

## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  group_by(ID, day, month, water_year, position, season) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(water_year, month, position, season) %>%
  summarise(days_per_water_month = sum(n_days)) #%>%

total_days_per_month01

write.csv(total_days_per_month01, "output_data/F4_LA1_number_of_days_fry_velocity.csv")

