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

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
names(hydraul)

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
                             tz = "America/Los_Angeles")

## create year, month, day and hour columns

hyd_dep <- hyd_dep %>%
  mutate(month = month(DateTime))%>%
  mutate(year = year(DateTime))%>%
  mutate(day = day(DateTime))%>%
  mutate(hour = hour(DateTime)) %>%
  mutate(season = ifelse(month == 3 | month == 4 | month == 5 | month == 6 | month == 7, paste("Fry_season"), paste("other_season") ))

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

hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num", "month", "day", "year", "hour","season"))
hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num", "month", "day", "year", "hour", "season"))
head(hyd_vel)
### plot depth

labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        # name="Cross\nSection\nPosition",
                        breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB")) +
                          # labels = c("LOB", "MC", "ROB")) +
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F57C: Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

## plot velocity

labels <- c(vel_m_LOB = "Left Over Bank", vel_m_MC = "Main Channel", vel_m_ROB = "Right Over Bank")

ggplot(hyd_vel, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"),
                        # name="Cross\nSection\nPosition",
                        breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB")) +
  # labels = c("LOB", "MC", "ROB")) +
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F57C: Velocity ~ Q",
       y = "Velcocity (m/s)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

### define thresholds for each node position


# Depth -------------------------------------------------------------------


## plot
range(hyd_dep$Q) ## 0.00 998.845 

## smooth spline the curve to get exact value of discharge at a given probability

new_dataM <- filter(hyd_dep, variable=="depth_cm_MC")
splM <- smooth.spline(new_dataM$value ~ new_dataM$Q)
# 
new_dataL<- filter(hyd_dep, variable=="depth_cm_LOB")
splL <- smooth.spline(new_dataL$value ~ new_dataL$Q)
# 
new_dataR <- filter(hyd_dep, variable=="depth_cm_ROB")
splR <- smooth.spline(new_dataR$value ~ new_dataR$Q)

## find discharge point at 3 - 10cm

newy3 <- 3
newx3 <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy3,
                     interval = c(min(new_dataM$Q), max(new_dataM$Q)))$root, silent=T)
newx3 ## 0.7560936

newy10 <- 10
newx10 <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy10,
                     interval = c(min(new_dataM$Q), max(new_dataM$Q)))$root, silent=T)
newx10 # 4.517823

## if no value, return an NA
newx3 <- ifelse(class(newx3) == "try-error",  NA, newx3)
newx10 <- ifelse(class(newx10) == "try-error",  NA, newx10)
## find discharge point at 3 - 10cm

newy3L <- 3
newx3L <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy3L,
                     interval = c(min(new_dataL$Q), max(new_dataL$Q)))$root, silent=T)
newx3L 

newy10L <- 10
newx10L <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy10L,
                      interval = c(min(new_dataL$Q), max(new_dataL$Q)))$root, silent=T)
newx10L 
## if no value, return an NA
newx3L <- ifelse(class(newx3L) == "try-error",  NA, newx3L)
newx10L <- ifelse(class(newx10L) == "try-error",  NA, newx10L)

## find discharge point at 3 - 10cm

newy3R <- 3
newx3R <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy3R,
                      interval = c(min(new_dataR$Q), max(new_dataR$Q)))$root, silent=T)
newx3R 

newy10R <- 10
newx10R <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy10R,
                       interval = c(min(new_dataR$Q), max(new_dataR$Q)))$root, silent=T)
newx10R 

## if no value, return an NA
newx3R <- ifelse(class(newx3R) == "try-error",  NA, newx3R)
newx10R <- ifelse(class(newx10R) == "try-error",  NA, newx10R)

head(hyd_dep)

plot(new_dataM$Q, new_dataM$value, type="n", main = "SAS/Fry: Depth according to Q", xlab="Q (cfs)", ylab="Depth (cm)")
lines(splM, col="black")
lines(splL, col="black", lty= "dotted")
lines(splR, col="black", lty= "dashed")
points(newx3, newy3, col="red", pch=19) # 3cm
points(newx10, newy10, col="red", pch=19) # 10cm
# LOB
points(newx3L, newy3L, col="red", pch=19) # 3cm
points(newx10L, newy10L, col="red", pch=19) # 10cm
# ROB
points(newx3R, newy3R, col="red", pch=19) # 3cm
points(newx10R, newy10R, col="red", pch=19) # 10cm

### percentage of time above threshold

time_statsM <- new_dataM %>%
  dplyr::group_by(year,season) %>%
  dplyr::mutate(Threshold_MC = sum(Q >= newx3 & Q <= newx10)/length(DateTime)*100) %>%
  distinct(year, season, Threshold_MC)

time_statsL <- new_dataL %>%
  dplyr::group_by(year,season) %>%
  dplyr::mutate(Threshold_LOB = sum(Q >= newx3L & Q <= newx10L)/length(DateTime)*100) %>%
  distinct(year,season, Threshold_LOB)

time_statsR <- new_dataR %>%
  dplyr::group_by(year,season) %>%
  dplyr::mutate(Threshold_ROB = sum(Q >= newx3R & Q <= newx10R)/length(DateTime)*100) %>%
  distinct(year ,season, Threshold_ROB)

time_stats <- cbind(time_statsM, time_statsL[,3], time_statsR[,3])

time_stats
write.csv(time_stats, "output_data/F4_time_stats_fry_depth.csv")

## percentage of time in season - march to july



# Velocity ----------------------------------------------------------------


## plot
range(hyd_vel$Q) ## 0.00 998.845 
range(new_dataM$value)
unique(sort(new_dataM$value))
## smooth spline the curve to get exact value of discharge at a given probability

new_dataM <- filter(hyd_vel, variable=="vel_m_MC")
splM <- smooth.spline(new_dataM$value ~ new_dataM$Q)
# 
new_dataL<- filter(hyd_vel, variable=="vel_m_LOB")
splL <- smooth.spline(new_dataL$value ~ new_dataL$Q)
# 
new_dataR <- filter(hyd_vel, variable=="vel_m_ROB")
splR <- smooth.spline(new_dataR$value ~ new_dataR$Q)

## find discharge point at 0.01 - change!!!

newy0 <- 0.1
newx0 <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy0,
                     interval = c(min(new_dataM$Q), max(new_dataM$Q)))$root, silent=T)
newx0 ## 0.1105615

## if no value, return an NA
newx3 <- ifelse(class(newx0) == "try-error",  NA, newx3)

## find discharge point at 0.01 - change!!!

newy0L <- 0.1
newx0L <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy0L,
                      interval = c(min(new_dataL$Q), max(new_dataL$Q)))$root, silent=T)
newx0L 

## if no value, return an NA
newx0L <- ifelse(class(newx0L) == "try-error",  NA, newx0L)


## find discharge point at 0.01 - change!!!

newy0R <- 3
newx0R <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy0R,
                      interval = c(min(new_dataR$Q), max(new_dataR$Q)))$root, silent=T)
newx0R 

## if no value, return an NA
newx0R <- ifelse(class(newx0R) == "try-error",  NA, newx0R)


plot(new_dataM$Q, new_dataM$value, type="n", main = "SAS/Fry: Velocity according to Q", xlab="Q (cfs)", ylab="Velocity (m/s)")
lines(splM, col="black")
lines(splL, col="black", lty= "dotted")
lines(splR, col="black", lty= "dashed")
points(newx0, newy0, col="red", pch=19) # 
# LOB
points(newx0L, newy0L, col="red", pch=19) #
# ROB
points(newx0R, newy0R, col="red", pch=19) #

### percentage of time above threshold

time_statsM <- new_dataM %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Threshold_MC = sum(Q <= newx0)/length(DateTime)*100) %>%
  distinct(year,season, Threshold_MC)

time_statsL <- new_dataL %>%
  dplyr::group_by(year,season) %>%
  dplyr::mutate(Threshold_LOB = sum(Q <= newx0L)/length(DateTime)*100) %>%
  distinct(year ,season, Threshold_LOB)

time_statsR <- new_dataR %>%
  dplyr::group_by(year,season) %>%
  dplyr::mutate(Threshold_ROB = sum(Q <= newx0R)/length(DateTime)*100) %>%
  distinct(year ,season, Threshold_ROB)

time_stats <- cbind(time_statsM, time_statsL[,3], time_statsR[,3])

time_stats
write.csv(time_stats, "output_data/F4_time_stats_fry_velocity.csv")


# Number of days within Q limits ------------------------------------------

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)

## depth
new_dataM <- filter(hyd_dep, variable=="depth_cm_MC")
new_dataL<- filter(hyd_dep, variable=="depth_cm_LOB")
new_dataR <- filter(hyd_dep, variable=="depth_cm_ROB")

## change year to water year and count hours within Q range
new_dataM  <- new_dataM %>% 
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx3 & Q <= newx10)) %>%
  mutate(threshold = if_else(Q >= newx3 & Q <= newx10,  row_number(), 0L))%>%
  mutate(position="MC")

new_dataL  <- new_dataL %>% 
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx3L & Q <= newx10L)) %>%
  mutate(threshold = if_else(Q >= newx3L & Q <= newx10L,  row_number(), 0L))%>%
  mutate(position="LOB")

new_dataR  <- new_dataR %>% 
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx3R & Q <= newx10R)) %>%
  mutate(threshold = if_else(Q >= newx3R & Q <= newx10R,  row_number(), 0L)) %>%
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
total_days_per_year01 <- total_days01 %>%
  group_by(water_year, position, season) %>%
  summarise(days_per_water_year = sum(n_days)) #%>%

total_days_per_year01

write.csv(total_days_per_year01, "output_data/F4_number_of_days_fry_depth.csv")

## velocity
new_dataM <- filter(hyd_vel, variable=="vel_m_MC")
new_dataL<- filter(hyd_vel, variable=="vel_m_LOB")
new_dataR <- filter(hyd_vel, variable=="vel_m_ROB")

## change year to water year and count hours within Q range
new_dataM  <- new_dataM %>% 
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newy0)) %>%
  mutate(threshold = if_else(Q <= newy0,  row_number(), 0L))%>%
  mutate(position="MC")

new_dataL  <- new_dataL %>% 
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newy0L)) %>%
  mutate(threshold = if_else(Q <= newy0L,  row_number(), 0L))%>%
  mutate(position="LOB")

new_dataR  <- new_dataR %>% 
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newy0R)) %>%
  mutate(threshold = if_else(Q <= newy0R,  row_number(), 0L)) %>%
  mutate(position="ROB")

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
total_days_per_year01 <- total_days01 %>%
  group_by(water_year, position, season) %>%
  summarise(days_per_water_year = sum(n_days)) #%>%

total_days_per_year01

write.csv(total_days_per_year01, "output_data/F4_number_of_days_fry_velocity.csv")

