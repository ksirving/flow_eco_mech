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

# F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv")
F37B_High <- read.csv("input_data/HecRas/hydraulic_ts_F37B_High.csv")
# F45B <- read.csv("input_data/HecRas/hydraulic_ts_F45B.csv")
# F319 <- read.csv("input_data/HecRas/hydraulic_ts_F319.csv")
# LA13 <- read.csv("input_data/HecRas/hydraulic_ts_LA13.csv")
# LA1 <- read.csv("input_data/HecRas/hydraulic_ts_LA1.csv")
## go through script one at a time

N11101250 <- N11101250[-1,]
N11101250 <- N11101250 %>%
  mutate(Q_ts.datetime = F34D$Q_ts.datetime)

## LA20_2
LA20_2 <- LA20_2[-1,]
LA20_2 <- LA20_2 %>%
  mutate(Q_ts.datetime = F34D$Q_ts.datetime)

head(F37B_High)
names(F37B_High)
## convert lb sq ft to n m-2 = lb sq ft/0.020885

hyd_shear <- F37B_High[,c(2:4,7,11,15)]
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
hyd_shear <- filter(hyd_shear, variable =="shear_pa_MC")
head(hyd_shear)

## plot
range(hyd_shear$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_dep) == FALSE)
# nas #0

unique(hyd_shear$variable)
## filter data by cross section position
new_dataM <- filter(hyd_shear, variable == "shear_pa_MC")
# new_dataL <- filter(hyd_shear, variable == "shear_pa_LOB")
# new_dataR <- filter(hyd_shear, variable == "shear_pa_ROB")

## Main channel curve
MC_curve <- spline(new_dataM$Q, new_dataM$value,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)
## main channel values
if(max(MC_curve$y)<25.6) {
  newx1a <- max(MC_curve$x)
} else {
  newx1a <- approx(x = MC_curve$y, y = MC_curve$x, xout = 25.6)$y
}



limits <- as.data.frame(matrix(ncol=1, nrow=1))
colnames(limits) <- c("MC")

limits$MC <- newx1a

limits

write.csv(limits, "output_data/C4_F37B_High_clad_shear_Q_limits.csv")

## plot with thresholds
labels <- c(shear_pa_LOB = "Left Over Bank", shear_pa_MC = "Main Channel", shear_pa_ROB = "Right Over Bank")

png("figures/Application_curves/Shear/F37B_High_clad_shear_stress_Q.png", width = 500, height = 600)

ggplot(hyd_shear, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("shear_pa_LOB", "shear_pa_MC", "shear_pa_ROB"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_shear, variable =="shear_pa_MC"), aes(y=25.6, x=newx1a), color="green") +
  # geom_point(data = subset(hyd_shear, variable =="shear_pa_LOB"), aes(y=25.6, x=newx1aL), color="green") +
  # geom_point(data = subset(hyd_shear, variable =="shear_pa_ROB"), aes(y=25.6, x=newx1aR), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F37B_High: Shear ~ Q",
       y = "Shear Stress (Pa)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_dataMx)



## make dataframe for all years 

## define critical period or season for adult as all year is critical
## define seasons/critical period

non_critical <- c(1,2,8:12) 
critical <- c(3:7) 

new_dataMx <- new_dataMx %>%
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

time_stats <- time_statsm

time_stats <-time_stats %>%
  pivot_wider(names_from = season, values_from = Seasonal)

## melt
melt_time<-reshape2::melt(time_stats, id=c("water_year", "position"))
melt_time <- rename(melt_time, season = variable)

write.csv(melt_time, "output_data/C4_F37B_High_clad_shear_stress_time_stats.csv")

## change year to water year and count hours within Q range
new_dataM  <- new_dataM %>% 
  # mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1)) %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newx1a)) %>%
  mutate(threshold = if_else(Q <= newx1a,  row_number(), 0L))%>%
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
  group_by(ID, day, month, water_year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month_low = sum(n_days_low))

total_days_per_month01

write.csv(total_days_per_month01, "output_data/C3_F37B_High_total_days_clad_shear.csv")



