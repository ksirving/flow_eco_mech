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

# F57C <- read.csv("input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("input_data/HecRas/hydraulic_ts_LA8.csv")
# LA11 <- read.csv("input_data/HecRas/hydraulic_ts_LA11.csv")
# LA20_2 <- read.csv("input_data/HecRas/hydraulic_ts_LA20_2.csv")
# F37B_Low <- read.csv("input_data/HecRas/hydraulic_ts_F37B_Low.csv")
# LA2 <- read.csv("input_data/HecRas/hydraulic_ts_LA2.csv")
# LA3 <- read.csv("input_data/HecRas/hydraulic_ts_LA3.csv")
# LA14 <- read.csv("input_data/HecRas/hydraulic_ts_LA14.csv")
# F300 <- read.csv("input_data/HecRas/hydraulic_ts_F300.csv")
GLEN <- read.csv("input_data/HecRas/hydraulic_ts_GLEN.csv")
# LA20_1 <- read.csv("input_data/HecRas/hydraulic_ts_LA20.csv")

# N11101250 <- read.csv("input_data/HecRas/hydraulic_ts_11101250.csv")
# F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv") ## not soft - just for dates

## go through script one at a time

N11101250 <- N11101250[-1,]
N11101250 <- N11101250 %>%
  mutate(Q_ts.datetime = F34D$Q_ts.datetime)

## LA20_2
LA20_2 <- LA20_2[-1,]
LA20_2 <- LA20_2 %>%
  mutate(Q_ts.datetime = F34D$Q_ts.datetime)

hydraul <- GLEN[,-1]
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
hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num", "month", "day", "year", "hour", "season", "water_year"))
head(hyd_vel)
unique(hyd_dep$variable)


# Depth -------------------------------------------------------------------

## plot
range(hyd_dep$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_dep) == FALSE)
# nas #0


## filter data by cross section position
new_dataM <- filter(hyd_dep, variable == "depth_cm_MC")
new_dataL <- filter(hyd_dep, variable == "depth_cm_LOB")
new_dataR <- filter(hyd_dep, variable == "depth_cm_ROB")

MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)


if(min(MC_curve$y)>3 && min(MC_curve$y)<10) {
  newx1a <- min(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 3)$y
}

if(min(MC_curve$y)>10) {
  newx2a <- min(MC_curve$x)
} else {
  newx2a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 10)$y
}

## LOB curve
LOB_curve <- spline(new_dataL$Q, new_dataL$value,
                    xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)
## LOB values
if(min(LOB_curve$y)>3 && min(LOB_curve$y)<10) {
  newx1aL <- min(LOB_curve$x)
} else {
  newx1aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 3)$y
}

if(min(LOB_curve$y)>10) {
  newx2aL <- min(LOB_curve$x)
} else {
  newx2aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 10)$y
}
newx2aL
## ROB curve
ROB_curve <- spline(new_dataR$Q, new_dataR$value,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)
## ROB values

if(min(ROB_curve$y)>3 && min(ROB_curve$y)<10) {
  newx1aR <- min(ROB_curve$x)
} else {
  newx1aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = 3)$y
}

if(min(ROB_curve$y)>10) {
  newx2aR <- min(ROB_curve$x)
} else {
  newx2aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = 10)$y
}
newx2aR
## MAKE DF OF Q LIMITS

limits <- as.data.frame(matrix(ncol=3, nrow=2)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob",
                    "Med_Prob")

limits$LOB <- c(newx1aL, newx2aL)

limits$MC <- c(newx1a, newx2a)

limits$ROB <- c(newx1aR, newx2aR)
limits

write.csv(limits, "output_data/F4_GLEN_sas_fry_depth_Q_limits.csv")

## plot with thresholds
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

png("figures/Application_curves/Depth/GLEN_SAS_Fry_depth_Q.png", width = 500, height = 600)

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=3, x=newx1a), color="green") +
  geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=3, x=newx1aL), color="green") +
  geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=3, x=newx1aR), color="green") +
  
  geom_point(data = subset(hyd_dep, variable =="depth_cm_MC"), aes(y=10, x=newx2a), color="green") +
  geom_point(data = subset(hyd_dep, variable =="depth_cm_LOB"), aes(y=10, x=newx2aL), color="green") +
  geom_point(data = subset(hyd_dep, variable =="depth_cm_ROB"), aes(y=10, x=newx2aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "GLEN: Depth ~ Q",
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

time_statsl <- new_dataL %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1aL & Q <= newx2aL)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1aL & Q <= newx2aL)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="LOB")

time_statsr <- new_dataR %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1aR & Q <= newx2aR)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1aR & Q <= newx2aR)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="ROB")

time_stats <- rbind(time_statsm, time_statsl, time_statsr)

time_stats
write.csv(time_stats, "output_data/F4_GLEN_time_stats_fry_depth.csv")

# Number of days within Q limits ------------------------------------------

## depth
new_dataM <- filter(hyd_dep, variable=="depth_cm_MC")
new_dataL<- filter(hyd_dep, variable=="depth_cm_LOB")
new_dataR <- filter(hyd_dep, variable=="depth_cm_ROB")

## change year to water year and count hours within Q range
new_dataM  <- new_dataM %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a & Q <= newx2a)) %>%
  mutate(threshold = if_else(Q >= newx1a & Q <= newx2a,  row_number(), 0L)) %>%
  mutate(position="MC")

new_dataL  <- new_dataL %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1aL & Q <= newx2aL)) %>%
  mutate(threshold = if_else(Q >= newx1aL & Q <= newx2aL,  row_number(), 0L)) %>%
  mutate(position="LOB")

new_dataR  <- new_dataR %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1aR & Q <= newx2aR)) %>%
  mutate(threshold = if_else(Q >= newx1aR & Q <= newx2aR,  row_number(), 0L)) %>%
  mutate(position="ROB")
new_dataR
# select columns needed and combine dfs

new_dataMx <- select(new_dataM, c(Q, month, water_year, year, day, ID, threshold, position, season)) # all probs
new_dataLx <- select(new_dataL, c(Q, month, water_year, year, day, ID, threshold, position, season))
new_dataRx <- select(new_dataR, c(Q, month, water_year, year, day, ID, threshold, position, season))

new_datax <- rbind(new_dataMx, new_dataLx, new_dataRx)
new_datax
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

write.csv(total_days_per_month01, "output_data/F4_GLEN_number_of_days_fry_depth.csv")



# Velocity ----------------------------------------------------------------


## plot
range(hyd_vel$Q) ## 0.00 998.845 
range(new_dataM$value)
unique(sort(new_dataM$value))
## smooth spline the curve to get exact value of discharge at a given probability

new_dataM <- filter(hyd_vel, variable=="vel_m_MC")
new_dataL<- filter(hyd_vel, variable=="vel_m_LOB")
new_dataR <- filter(hyd_vel, variable=="vel_m_ROB")


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
## LOB curve
LOB_curve <- spline(new_dataL$Q, new_dataL$value,
                    xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)
## LOB values
if(min(LOB_curve$y)>0.1 && min(LOB_curve$y)<0.15) {
  newx1aL <- min(LOB_curve$x)
} else {
  newx1aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 0.1)$y
}

if(min(LOB_curve$y)>0.15) {
  newx2aL <- min(LOB_curve$x)
} else {
  newx2aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 0.15)$y
}

## ROB curve
ROB_curve <- spline(new_dataR$Q, new_dataR$value,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)
## ROB values

if(min(ROB_curve$y)>0.1 && min(ROB_curve$y)<0.15) {
  newx1aR <- min(ROB_curve$x)
} else {
  newx1aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = 0.1)$y
}

if(min(ROB_curve$y)>0.15) {
  newx2aR <- min(ROB_curve$x)
} else {
  newx2aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = 0.15)$y
}

limits <- as.data.frame(matrix(ncol=3, nrow=2)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob",
                    "Med_Prob")

limits$LOB <- c(newx1aL, newx2aL)

limits$MC <- c(newx1a, newx2a)

limits$ROB <- c(newx1aR, newx2aR)
limits

## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
write.csv(limits, "output_data/F1_GLEN_fry_velocity_Q_limits.csv")


## plot with thresholds
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

png("figures/Application_curves/Depth/GLEN_SAS_Fry_velocity_Q.png", width = 500, height = 600)

ggplot(hyd_vel, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_vel, variable =="vel_m_MC"), aes(y=0.15, x=newx1a), color="green") +
  geom_point(data = subset(hyd_vel, variable =="vel_m_LOB"), aes(y=0.15, x=newx1aL), color="green") +
  geom_point(data = subset(hyd_vel, variable =="vel_m_ROB"), aes(y=0.15, x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "GLEN: Velocity ~ Q",
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

time_statsl <- new_dataL %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1aL & Q <= newx2aL)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1aL & Q <= newx2aL)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="LOB")

time_statsr <- new_dataR %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1aR & Q <= newx2aR)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1aR & Q <= newx2aR)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="ROB")


time_stats <- rbind(time_statsm, time_statsl, time_statsr)

time_stats
write.csv(time_stats, "output_data/F4_GLEN_time_stats_fry_velocity.csv")


## velocity
new_dataM <- filter(hyd_vel, variable=="vel_m_MC")
new_dataL<- filter(hyd_vel, variable=="vel_m_LOB")
new_dataR <- filter(hyd_vel, variable=="vel_m_ROB")

## change year to water year and count hours within Q range
new_dataM  <- new_dataM %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a & Q <= newx2a)) %>%
  mutate(threshold = if_else(Q >= newx1a & Q <= newx2a,  row_number(), 0L)) %>%
  mutate(position="MC")

new_dataL  <- new_dataL %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1aL & Q <= newx2aL)) %>%
  mutate(threshold = if_else(Q >= newx1aL & Q <= newx2aL,  row_number(), 0L)) %>%
  mutate(position="LOB")

new_dataR  <- new_dataR %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1aR & Q <= newx2aR)) %>%
  mutate(threshold = if_else(Q >= newx1aR & Q <= newx2aR,  row_number(), 0L)) %>%
  mutate(position="ROB")
new_dataR
# select columns needed and combine dfs

new_dataMx <- select(new_dataM, c(Q, month, water_year, year, day, ID, threshold, position, season)) # all probs
new_dataLx <- select(new_dataL, c(Q, month, water_year, year, day, ID, threshold, position, season))
new_dataRx <- select(new_dataR, c(Q, month, water_year, year, day, ID, threshold, position, season))

new_datax <- rbind(new_dataMx, new_dataLx, new_dataRx)
new_datax
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

write.csv(total_days_per_month01, "output_data/F4_GLEN_number_of_days_fry_velocity.csv")

