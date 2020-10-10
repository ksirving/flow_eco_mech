## Cladophora shear stress

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)
library(mgcv)

## 16.9 n m-3 - one event per year (Spring)

## upload hydraulic data

## soft bottom reaches

## soft bottom reaches

# F57C <- read.csv("input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("input_data/HecRas/hydraulic_ts_LA8.csv")
# LA11 <- read.csv("input_data/HecRas/hydraulic_ts_LA11.csv")
# LA20_2 <- read.csv("input_data/HecRas/hydraulic_ts_LA20_2.csv")
# F37B_Low <- read.csv("input_data/HecRas/hydraulic_ts_F37B_Low.csv")
LA2 <- read.csv("input_data/HecRas/hydraulic_ts_LA2.csv")
# LA3 <- read.csv("input_data/HecRas/hydraulic_ts_LA3.csv")
# LA14 <- read.csv("input_data/HecRas/hydraulic_ts_LA14.csv")
# F300 <- read.csv("input_data/HecRas/hydraulic_ts_F300.csv")
# GLEN <- read.csv("input_data/HecRas/hydraulic_ts_GLEN.csv")
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

head(LA2)
names(LA2)
## convert lb sq ft to n m-2 = lb sq ft/0.020885

hyd_shear <- LA2[,c(2:4,7,11,15)]
colnames(hyd_shear) <-c("DateTime", "node", "Q", "shear_lb_LOB", "shear_lb_MC", "shear_lb_ROB")

hyd_shear <- hyd_shear %>%
  mutate(shear_pa_LOB = (shear_lb_LOB/0.020885),
         shear_pa_MC = (shear_lb_MC/0.020885),
         shear_pa_ROB = (shear_lb_ROB/0.020885)) %>%
  select(-contains("lb")) %>%
  # rename(Q_ts.datetime = DateTime, Gage = node, Flow = Q) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

range(hyd_shear$shear_pa_ROB)

## format date time
hyd_shear$DateTime<-as.POSIXct(hyd_shear$DateTime,
                             format = "%Y-%m-%d %H:%M",
                             tz = "GMT")



## create year, month, day and hour columns

hyd_shear <- hyd_shear %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime)) %>%
  mutate(season = ifelse(month == 3 | month == 4 | month == 5 | month == 6 | month == 7, paste("critcal"), paste("non_critical")))%>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))

## rules for migration

# ## melt channel position data

hyd_shear<-reshape2::melt(hyd_shear, id=c("DateTime","Q", "node", "date_num", "month", "day","year", "water_year", "hour", "season"))
head(hyd_shear)

## plot
range(hyd_shear$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_dep) == FALSE)
# nas #0

unique(hyd_shear$variable)
## filter data by cross section position
new_dataM <- filter(hyd_shear, variable == "shear_pa_MC")
new_dataL <- filter(hyd_shear, variable == "shear_pa_LOB")
new_dataR <- filter(hyd_shear, variable == "shear_pa_ROB")

## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)
## main channel values
if(max(MC_curve$y)<25.6) {
  newx1a <- max(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 25.6)$y
}


## LOB curve
LOB_curve <- spline(new_dataL$Q, new_dataL$value,
                    xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)
## LOB values
if(max(LOB_curve$y)<25.6) {
  newx1aL <- max(MC_curve$x)
} else {
  newx1aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 25.6)$y
}

newx1aL
## ROB curve
ROB_curve <- spline(new_dataR$Q, new_dataR$value,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)
## ROB values

if(max(ROB_curve$y)<25.6) {
  newx1aR <- max(MC_curve$x)
} else {
  newx1aR <- approx(x = ROB_curve$y, y = ROB_curve$x, xout = 25.6)$y
}


limits <- as.data.frame(matrix(ncol=3, nrow=1))
colnames(limits) <- c("LOB", "MC", "ROB")
limits$LOB <- newx1aL
limits$MC <- newx1a
limits$ROB <- newx1aR
limits

write.csv(limits, "output_data/C4_LA2_clad_shear_Q_limits.csv")

## plot with thresholds
labels <- c(shear_pa_LOB = "Left Over Bank", shear_pa_MC = "Main Channel", shear_pa_ROB = "Right Over Bank")

png("figures/Application_curves/Shear/LA2_clad_shear_stress_Q.png", width = 500, height = 600)

ggplot(hyd_shear, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("shear_pa_LOB", "shear_pa_MC", "shear_pa_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_shear, variable =="shear_pa_MC"), aes(y=25.6, x=newx1a), color="green") +
  geom_point(data = subset(hyd_shear, variable =="shear_pa_LOB"), aes(y=25.6, x=newx1aL), color="green") +
  geom_point(data = subset(hyd_shear, variable =="shear_pa_ROB"), aes(y=25.6, x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA2: Shear ~ Q",
       y = "Shear Stress (Pa)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataMx)

# create year_month column       
new_dataLx <- new_dataL %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataLx)

# create year_month column       
new_dataRx <- new_dataR %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataRx)


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

### time stats

time_statsm <- new_dataMx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q <= newx1a)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx1a)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="MC")

time_statsm

time_statsl <- new_dataLx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q <= newx1aL)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx1aL)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="LOB")

time_statsr <- new_dataRx %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q <= newx1aR)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx1aR)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="ROB")


time_stats <- rbind(time_statsm, time_statsl, time_statsr)

time_stats <-time_stats %>%
  pivot_wider(names_from = season, values_from = Seasonal)

## melt
melt_time<-reshape2::melt(time_stats, id=c("water_year", "position"))
melt_time <- rename(melt_time, season = variable)

write.csv(melt_time, "output_data/C4_LA2_clad_shear_stress_time_stats.csv")

## change year to water year and count hours within Q range
new_dataM  <- new_dataM %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newx1a)) %>%
  mutate(threshold = if_else(Q <= newx1a,  row_number(), 0L))%>%
  mutate(position="MC")

new_dataL  <- new_dataL %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newx1aL)) %>%
  mutate(threshold = if_else(Q <= newx1aL,  row_number(), 0L))%>%
  mutate(position="LOB")

new_dataR  <- new_dataR %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newx1a)) %>%
  mutate(threshold = if_else(Q <= newx1a,  row_number(), 0L))%>%
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
  group_by(ID, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_low = sum(n_days_low))

total_days_per_month01

write.csv(total_days_per_month01, "output_data/C3_LA2_total_days_clad_shear.csv")



