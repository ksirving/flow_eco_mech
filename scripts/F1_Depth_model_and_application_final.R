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


## upload data

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous_updated.csv") ## all wulff incl and thompson removed - remove SAWA?
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

# ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(ad_depth_con, ad_depth_cat)

unique(all_depth$Dataset) # 4 datasets, 

depth_freq <- all_depth %>% 
  uncount(Abundance)
# hist(depth_freq$Depth)

depth_freq <- subset(depth_freq, !Dataset=="SAWA")
dim(depth_freq) ## 1376


# Adult data distribution -------------------------------------------------------


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
names(hydraul)
## select columns

hyd_dep <- hydraul[,c(1:3,5,9,13)]
colnames(hyd_dep)[4:6] <-c("depth_ft_LOB", "depth_ft_MC", "depth_ft_ROB")

## convert unit from feet to meters

hyd_dep <- hyd_dep %>%
  mutate(depth_cm_LOB = (depth_ft_LOB*0.3048)*100,
         depth_cm_MC = (depth_ft_MC*0.3048)*100,
         depth_cm_ROB = (depth_ft_ROB*0.3048)*100) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

head(hyd_dep)
# ## melt channel position data
# 
hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num"))


labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Mid Channel", depth_cm_ROB = "Right Over Bank")

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F57C: Adult/Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)


## plot time series

ggplot(hyd_dep, aes(x = date_num, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F57C: Adult/Depth ~ Time Series",
       y = "Depth (cm)",
       x = "Date") #+ theme_bw(base_size = 15)

## round the depths - don't need the high resolution

hyd_dep <- hyd_dep %>%
  group_by(variable) %>%
  mutate(depth_cm_round = round(value, digits=0))
        
head(hyd_dep)
head(fitdata)
range(fitdata$depth_fit)
range(hyd_dep$depth_cm_round)

fitdata<- fitdata %>%
  mutate(depth_fit_round = round(depth_fit, digits=0))


## merge node data and probabilities
all_data <- merge(hyd_dep, fitdata, by.x="depth_cm_round", by.y="depth_fit_round", all=T)
head(all_data)
# all_data[which(is.na(all_data)),]
sum(is.na(all_data)) # 241566
# head(all_data)

## missing values - anything under 4cm as not in suitability curve
## replace NA of probability with min probability of dataset

all_data[which(all_data$depth_cm_round < min(na.omit(all_data$depth_fit))),"prob_fit"]

all_data[which(all_data$depth_cm_round < min(na.omit(all_data$depth_fit))),"prob_fit"] <- min(na.omit(all_data$prob_fit))
sum(is.na(all_data)) # 120972

## remove rows with probabilities above the max hydraulic value
all_data <- filter(all_data, depth_cm_round <= max(hyd_dep$depth_cm_round))
# sum(is.na(all_data)) # 120594
# range(all_data$prob_fit)

save(all_data, file="output_data/F1_F57C_adult_depth_discharge_probability_time_series_all_columns.RData")
# load("output_data/F1_F57C_adult_depth_discharge_probability_time_series_all_columns.RData")
## keep columns dpeth, datetime, Q date_num & prob_fit

all_data <- all_data %>%
  select(-c(depth_fit, value))
# sum(is.na(all_data)) # 0

## remove duplicate date_num (date time) and order

# all_data <- all_data[!duplicated(all_data$date_num),]
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
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime))


head(new_data)

save(new_data, file="output_data/F1_F57C_depth_adult_discharge_probs_2010_2017_TS.RData")


# probability as a function of discharge -----------------------------------


load( file="output_data/F1_F57C_depth_adult_discharge_probs_2010_2017_TS.RData")
head(new_data)

## plot
range(new_data$Q) ## 0.00 998.845 
range(new_data$prob_fit)

## smooth spline the curve to get exact value of discharge at a given probability

new_dataM <- filter(new_data, variable=="depth_cm_MC")
splM <- smooth.spline(new_dataM$prob_fit ~ new_dataM$Q)

new_dataL<- filter(new_data, variable=="depth_cm_LOB")
splL <- smooth.spline(new_dataL$prob_fit ~ new_dataL$Q)

new_dataR <- filter(new_data, variable=="depth_cm_MC")
splR <- smooth.spline(new_dataR$prob_fit ~ new_dataR$Q)


## find peak of prob v Q
str(new_data)
peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%
  # distinct(prob_fit)

peakQM <- filter(peak, variable=="depth_cm_MC")
peakQM  <- max(peakQM$Q)
peakQM ## 433.26

peakQL <- filter(peak, variable=="depth_cm_LOB")
peakQL  <- min(peakQL$Q) ## when have values, change this to max!!!!!
peakQL ## 0

peakQR <- filter(peak, variable=="depth_cm_ROB")
peakQR  <- min(peakQR$Q) ## when have values, change this to max!!!!!
peakQR ## 0

## function for each probability

newy1a <- 0.1
newx1a <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy1a,
                  interval = c(min(new_dataM$Q), peakQM))$root, silent=T)
## if no value, return an NA
newx1a <- ifelse(class(newx1a) == "try-error",  NA, newx1a)

newy1b <- 0.1
newx1b <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy1b,
                  interval = c(peakQM, max(new_dataM$Q)))$root, silent=T)
## if no value, return an NA
newx1b <- ifelse(class(newx1b) == "try-error",  NA, newx1b)

newy2a <- 0.2
newx2a <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy2a,
                  interval = c(min(new_dataM$Q), peakQM))$root, silent=T)
newx2a <- ifelse(class(newx2a) == "try-error",  NA, newx2a)

newy2b <- 0.2
newx2b <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy2b, 
                      interval = c(peakQM, max(new_dataM$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx2b <- ifelse(class(newx2b) == "try-error",  NA, newx2b)

newy3a <- 0.3
newx3a <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy3a,
                  interval = c(min(new_dataM$Q), peakQM))$root, silent=T)
newx3a <- ifelse(class(newx3a) == "try-error",  NA, newx3a)

newy3b <- 0.3
newx3b <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy3b,
                      interval = c(peakQM, max(new_dataM$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx3b <- ifelse(class(newx3b) == "try-error",  NA, newx3b)


# LOB & ROB ---------------------------------------------------------------

## function for each probability

## LOB

newy1aL <- 0.1
newx1aL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy1aL,
                      interval = c(min(new_dataL$Q), peakQL))$root, silent=T)
## if no value, return an NA
newx1aL <- ifelse(class(newx1aL) == "try-error",  NA, newx1aL)

newy1bL <- 0.1
newx1bL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy1bL,
                      interval = c(peakQL, max(new_dataL$Q)))$root, silent=T)
## if no value, return an NA
newx1bL <- ifelse(class(newx1bL) == "try-error",  NA, newx1bL)

newy2aL <- 0.2
newx2aL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy2aL,
                      interval = c(min(new_dataL$Q), peakQL))$root, silent=T)
newx2aL <- ifelse(class(newx2aL) == "try-error",  NA, newx2aL)

newy2bL <- 0.2
newx2bL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy2bL, 
                      interval = c(peakQL, max(new_dataL$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx2bL <- ifelse(class(newx2bL) == "try-error",  NA, newx2bL)

newy3aL <- 0.3
newx3aL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy3aL,
                      interval = c(min(new_dataL$Q), peakQL))$root, silent=T)
newx3aL <- ifelse(class(newx3aL) == "try-error",  NA, newx3aL)

newy3bL <- 0.3
newx3bL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy3bL,
                      interval = c(peakQL, max(new_dataL$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx3bL <- ifelse(class(newx3bL) == "try-error",  NA, newx3bL)

## ROB

newy1aR <- 0.1
newx1aR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy1aR,
                       interval = c(min(new_dataR$Q), peakQR))$root, silent=T)
## if no value, return an NA
newx1aR <- ifelse(class(newx1aR) == "try-error",  NA, newx1aR)

newy1bR <- 0.1
newx1bR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy1bR,
                       interval = c(peakQR, max(new_dataR$Q)))$root, silent=T)
## if no value, return an NA
newx1bR <- ifelse(class(newx1bR) == "try-error",  NA, newx1bR)

newy2aR <- 0.2
newx2aR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy2aR,
                       interval = c(min(new_dataR$Q), peakQR))$root, silent=T)
newx2aR <- ifelse(class(newx2aR) == "try-error",  NA, newx2aR)

newy2bR <- 0.2
newx2bR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy2bR, 
                       interval = c(peakQR, max(new_dataR$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx2bR <- ifelse(class(newx2bR) == "try-error",  NA, newx2bR)

newy3aR <- 0.3
newx3aR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy3aR,
                       interval = c(min(new_dataR$Q), peakQR))$root, silent=T)
newx3aR <- ifelse(class(newx3aR) == "try-error",  NA, newx3aR)

newy3bR <- 0.3
newx3bR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy3bR,
                       interval = c(peakQR, max(new_dataR$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx3bR <- ifelse(class(newx3bR) == "try-error",  NA, newx3bR)


# plot discharge points ---------------------------------------------------
# ggplot(new_data, aes(x = Q, y=prob_fit)) +
#   geom_line(aes(group = variable, lty = variable)) +
#   # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
#   #                       name="Cross\nSection\nPosition",
#   #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
#   #                         labels = c("LOB", "MC", "ROB")) +
#   
#   # facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
#   geom_point( aes(x=newx1a, y=newy1a), col="red")+
#   geom_point( aes(x=newx1b, y=newy1b), col="red")+
#   geom_point( aes(x=newx2a, y=newy2a), col="red")+
#   geom_point( aes(x=newx2b, y=newy2b), col="red")+
#   geom_point( aes(x=newx3a, y=newy3a), col="red")+
#   geom_point( aes(x=newx3b, y=newy3b), col="red")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
#   labs(title = "F57C: Adult/Depth: Probability ~ Q",
#        y = "Probability",
#        x = "Q (cfs)") #+ theme_bw(base_size = 15)


ggplot(new_data)

plot(new_dataM$Q, new_dataM$prob_fit, type="n", main = "Adult/Depth: Probability according to Q", xlab="Q (cfs)", ylab="Probability")
lines(splM, col="black")
lines(splL, col="black", lty= "dotted")
lines(splR, col="black", lty= "dashed")
points(newx2a, newy2a, col="red", pch=19) # 0.2
points(newx2b, newy2b, col="red", pch=19) # 0.2
points(newx1a, newy1a, col="green", pch=19) # 0.1
points(newx1b, newy1b, col="green", pch=19) # 0.1
points(newx3a, newy3a, col="blue", pch=19) # 0.3 - lower limit
points(newx3b, newy3b, col="blue", pch=19) # 0.3 - upper limit
points(newx2aL, newy2aL, col="red", pch=19) # 0.2
points(newx2bL, newy2bL, col="red", pch=19) # 0.2
points(newx1aL, newy1aL, col="green", pch=19) # 0.1
points(newx1bL, newy1bL, col="green", pch=19) # 0.1
points(newx3aL, newy3aL, col="blue", pch=19) # 0.3 - lower limit
points(newx3bL, newy3bL, col="blue", pch=19) # 0.3 - upper limit
points(newx2aR, newy2aR, col="red", pch=19) # 0.2
points(newx2bR, newy2bR, col="red", pch=19) # 0.2
points(newx1aR, newy1aR, col="green", pch=19) # 0.1
points(newx1bR, newy1bR, col="green", pch=19) # 0.1
points(newx3aR, newy3aR, col="blue", pch=19) # 0.3 - lower limit
points(newx3bR, newy3bR, col="blue", pch=19) # 0.3 - upper limit


### plot discharge over time

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

##  plot time series of discharge - 0.2 prob line

ggplot(new_dataMx) +
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
new_datax_2016 <- filter(new_dataMx, year==2016)

ggplot(new_datax_2016) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
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
tail(new_datax_2016_summer$DateTime)
# break.vec <- c(as.Date("2016-05-01 00:00:00 PDT"),
#                seq(from=as.Date("2016-05-01 00:00:00 PDT"), to=as.Date("2016-10-31 23:00:00 PDT"), by="month"))
# head(new_datax_2016_summer)

ggplot(new_datax_2016_summer) +
  geom_line(aes(x =month_year, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m") +
  expand_limits(x=min(break.vec))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

ggplot(new_datax_2016_winter) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

# dataframe for stats -----------------------------------------------------

## make dataframe for all years 

head(new_datax)
names(new_datax)

## define seasons
winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )

new_dataLx <- new_dataRx %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )

new_dataRx <- new_dataRx %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )


## produces percentage of time for each year and season within year for each threshold

time_statsm <- new_dataMx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Medium = if(is.na(newx2b)){
     sum(Q >= newx2a)/length(DateTime)*100
  } else {
     sum(Q >= newx2a & Q <= newx2b)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low = if(is.na(newx1b)){
     sum(Q >= newx1a)/length(DateTime)*100
  } else {
    sum(Q >= newx1a & Q <= newx1b)/length(DateTime)*100
  }) %>%
  dplyr::mutate(High = if(is.na(newx3b)){
    sum(Q >= newx3a)/length(DateTime)*100
  } else {
     sum(Q >= newx3a & Q <= newx3b)/length(DateTime)*100
  })  %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Medium.Seasonal = if(is.na(newx2b)){
     sum(Q >= newx2a)/length(DateTime)*100
  } else {
    sum(Q >= newx2a & Q <= newx2b)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low.Seasonal = if(is.na(newx1b)){
    sum(Q >= newx1a)/length(DateTime)*100
  } else {
    sum(Q >= newx1a & Q <= newx1b)/length(DateTime)*100
  }) %>%
  dplyr::mutate(High.Seasonal = if(is.na(newx3b)){
    sum(Q >= newx3a)/length(DateTime)*100
  } else {
     sum(Q >= newx3a & Q <= newx3b)/length(DateTime)*100
  }) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="MC")

time_statsl <- new_dataLx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Medium = if(is.na(newx2bL)){
    sum(Q >= newx2aL)/length(DateTime)*100
  } else {
    sum(Q >= newx2aL & Q <= newx2bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low = if(is.na(newx1bL)){
    sum(Q >= newx1aL)/length(DateTime)*100
  } else {
    sum(Q >= newx1aL & Q <= newx1bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(High = if(is.na(newx3bL)){
    sum(Q >= newx3aL)/length(DateTime)*100
  } else {
    sum(Q >= newx3aL & Q <= newx3bL)/length(DateTime)*100
  })  %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Medium.Seasonal = if(is.na(newx2bL)){
    sum(Q >= newx2aL)/length(DateTime)*100
  } else {
    sum(Q >= newx2aL & Q <= newx2bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low.Seasonal = if(is.na(newx1bL)){
    sum(Q >= newx1aL)/length(DateTime)*100
  } else {
    sum(Q >= newx1aL & Q <= newx1bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(High.Seasonal = if(is.na(newx3bL)){
    sum(Q >= newx3aL)/length(DateTime)*100
  } else {
    sum(Q >= newx3aL & Q <= newx3bL)/length(DateTime)*100
  }) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="LOB")

time_statsr <- new_dataRx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Medium = if(is.na(newx2bR)){
    sum(Q >= newx2aR)/length(DateTime)*100
  } else {
    sum(Q >= newx2aR & Q <= newx2bR)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low = if(is.na(newx1bR)){
    sum(Q >= newx1aR)/length(DateTime)*100
  } else {
    sum(Q >= newx1aR & Q <= newx1bR)/length(DateTime)*100
  }) %>%
  dplyr::mutate(High = if(is.na(newx3bR)){
    sum(Q >= newx3aR)/length(DateTime)*100
  } else {
    sum(Q >= newx3aR & Q <= newx3bR)/length(DateTime)*100
  })  %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Medium.Seasonal = if(is.na(newx2bR)){
    sum(Q >= newx2aR)/length(DateTime)*100
  } else {
    sum(Q >= newx2aR & Q <= newx2bR)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low.Seasonal = if(is.na(newx1bR)){
    sum(Q >= newx1aR)/length(DateTime)*100
  } else {
    sum(Q >= newx1aR & Q <= newx1bR)/length(DateTime)*100
  }) %>%
  dplyr::mutate(High.Seasonal = if(is.na(newx3bR)){
    sum(Q >= newx3aR)/length(DateTime)*100
  } else {
    sum(Q >= newx3aR & Q <= newx3bR)/length(DateTime)*100
  }) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="ROB")

time_stats <- rbind(time_statsm, time_statsl, time_statsr)

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season", "position"))
melt_time <- rename(melt_time, Probability_Threshold = variable)

## subset annual stats
ann_stats <- unique(melt_time$Probability_Threshold)[1:3]
melt_time_ann <- melt_time %>% filter(Probability_Threshold %in% ann_stats ) %>%
  select(-season) %>% distinct()
head(melt_time_ann)
unique(melt_time_ann$Probability_Threshold)

## subset seasonal stats
seas_stats <- unique(melt_time$Probability_Threshold)[4:6]
melt_time_seas <- filter(melt_time, Probability_Threshold %in% seas_stats )
head(melt_time_seas)
melt_time_seas


## plot for annual stats - need probs in order
ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group =c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time within discharge limit in relation to Depth (Annual)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

## plot for winter stats - need probs in order

melt_time_winter <- filter(melt_time_seas, season == "winter")
unique(melt_time_winter$Probability_Threshold)

ggplot(melt_time_winter, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time within discharge limit in relation to Depth (Winter)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

## plot for summer stats - need probs in order

melt_time_summer <- filter(melt_time_seas, season == "summer")

ggplot(melt_time_summer, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time within discharge limit in relation to Depth (Summer)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)



# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month
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


load( file="output_data/F1_F57C_depth_adult_discharge_probs_2010_2017_TS.RData")
head(new_data)
str(new_data)
## define thresholds again
# range(new_data$Q) ## 0.00 998.845 

## smooth spline the curve to get exact value of discharge at a given probability

new_dataM <- filter(new_data, variable=="depth_cm_MC")
splM <- smooth.spline(new_dataM$prob_fit ~ new_dataM$Q)

new_dataL<- filter(new_data, variable=="depth_cm_LOB")
splL <- smooth.spline(new_dataL$prob_fit ~ new_dataL$Q)

new_dataR <- filter(new_data, variable=="depth_cm_MC")
splR <- smooth.spline(new_dataR$prob_fit ~ new_dataR$Q)


## find peak of prob v Q
str(new_data)
peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%
# distinct(prob_fit)

peakQM <- filter(peak, variable=="depth_cm_MC")
peakQM  <- max(peakQM$Q)
peakQM ## 433.26

peakQL <- filter(peak, variable=="depth_cm_LOB")
peakQL  <- min(peakQL$Q) ## when have values, change this to max!!!!!
peakQL ## 0

peakQR <- filter(peak, variable=="depth_cm_ROB")
peakQR  <- min(peakQR$Q) ## when have values, change this to max!!!!!
peakQR ## 0

## function for each probability

newy1a <- 0.1
newx1a <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy1a,
                      interval = c(min(new_dataM$Q), peakQM))$root, silent=T)
## if no value, return an NA
newx1a <- ifelse(class(newx1a) == "try-error",  NA, newx1a)

newy1b <- 0.1
newx1b <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy1b,
                      interval = c(peakQM, max(new_dataM$Q)))$root, silent=T)
## if no value, return an NA
newx1b <- ifelse(class(newx1b) == "try-error",  NA, newx1b)

newy2a <- 0.2
newx2a <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy2a,
                      interval = c(min(new_dataM$Q), peakQM))$root, silent=T)
newx2a <- ifelse(class(newx2a) == "try-error",  NA, newx2a)

newy2b <- 0.2
newx2b <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy2b, 
                      interval = c(peakQM, max(new_dataM$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx2b <- ifelse(class(newx2b) == "try-error",  NA, newx2b)

newy3a <- 0.3
newx3a <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy3a,
                      interval = c(min(new_dataM$Q), peakQM))$root, silent=T)
newx3a <- ifelse(class(newx3a) == "try-error",  NA, newx3a)

newy3b <- 0.3
newx3b <- try(uniroot(function(x) predict(splM, x, deriv = 0)$y - newy3b,
                      interval = c(peakQM, max(new_dataM$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx3b <- ifelse(class(newx3b) == "try-error",  NA, newx3b)


# LOB & ROB ---------------------------------------------------------------

## function for each probability

## LOB

newy1aL <- 0.1
newx1aL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy1aL,
                       interval = c(min(new_dataL$Q), peakQL))$root, silent=T)
## if no value, return an NA
newx1aL <- ifelse(class(newx1aL) == "try-error",  NA, newx1aL)

newy1bL <- 0.1
newx1bL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy1bL,
                       interval = c(peakQL, max(new_dataL$Q)))$root, silent=T)
## if no value, return an NA
newx1bL <- ifelse(class(newx1bL) == "try-error",  NA, newx1bL)

newy2aL <- 0.2
newx2aL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy2aL,
                       interval = c(min(new_dataL$Q), peakQL))$root, silent=T)
newx2aL <- ifelse(class(newx2aL) == "try-error",  NA, newx2aL)

newy2bL <- 0.2
newx2bL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy2bL, 
                       interval = c(peakQL, max(new_dataL$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx2bL <- ifelse(class(newx2bL) == "try-error",  NA, newx2bL)

newy3aL <- 0.3
newx3aL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy3aL,
                       interval = c(min(new_dataL$Q), peakQL))$root, silent=T)
newx3aL <- ifelse(class(newx3aL) == "try-error",  NA, newx3aL)

newy3bL <- 0.3
newx3bL <- try(uniroot(function(x) predict(splL, x, deriv = 0)$y - newy3bL,
                       interval = c(peakQL, max(new_dataL$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx3bL <- ifelse(class(newx3bL) == "try-error",  NA, newx3bL)

## ROB

newy1aR <- 0.1
newx1aR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy1aR,
                       interval = c(min(new_dataR$Q), peakQR))$root, silent=T)
## if no value, return an NA
newx1aR <- ifelse(class(newx1aR) == "try-error",  NA, newx1aR)

newy1bR <- 0.1
newx1bR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy1bR,
                       interval = c(peakQR, max(new_dataR$Q)))$root, silent=T)
## if no value, return an NA
newx1bR <- ifelse(class(newx1bR) == "try-error",  NA, newx1bR)

newy2aR <- 0.2
newx2aR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy2aR,
                       interval = c(min(new_dataR$Q), peakQR))$root, silent=T)
newx2aR <- ifelse(class(newx2aR) == "try-error",  NA, newx2aR)

newy2bR <- 0.2
newx2bR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy2bR, 
                       interval = c(peakQR, max(new_dataR$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx2bR <- ifelse(class(newx2bR) == "try-error",  NA, newx2bR)

newy3aR <- 0.3
newx3aR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy3aR,
                       interval = c(min(new_dataR$Q), peakQR))$root, silent=T)
newx3aR <- ifelse(class(newx3aR) == "try-error",  NA, newx3aR)

newy3bR <- 0.3
newx3bR <- try(uniroot(function(x) predict(splR, x, deriv = 0)$y - newy3bR,
                       interval = c(peakQR, max(new_dataR$Q)))$root, silent=T)
## if no 2nd value, return an NA
newx3bR <- ifelse(class(newx3bR) == "try-error",  NA, newx3bR)


# all columns based on different probabilities
## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_dataM <- arrange(new_dataM, date_num)

nas <- ifelse(!is.na(newx1a) && !is.na(newx1b), print("Both"), print("one"))

if(nas == "Both") {
    new_dataM <- new_dataM %>%
      ungroup() %>%
      group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1a & Q <= newx1b)) %>%
      mutate(Low = if_else(Q >= newx1a & Q <= newx1b, row_number(), 0L))
} else if (is.na(newx1a)) {
    new_dataM <- new_dataM %>%
      ungroup() %>%
      group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1b)) %>%
      mutate(Low = if_else(Q <= newx1b, row_number(), 0L)) 
} else {
    new_dataM <- new_dataM %>%
      ungroup() %>%
      group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1a)) %>%
      mutate(Low = if_else(Q >= newx1a, row_number(), 0L)) 
  }

nas <- ifelse(!is.na(newx2a) && !is.na(newx2b), print("Both"), print("one"))

if(nas == "Both") {
  new_dataM <- new_dataM %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2a & Q <= newx2b)) %>%
    mutate(Medium = if_else(Q >= newx2a & Q <= newx2b, row_number(), 0L))
} else if (is.na(newx2a)) {
  new_dataM <- new_dataM %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2b)) %>%
    mutate(Medium = if_else(Q <= newx2b, row_number(), 0L)) 
} else {
  new_dataM <- new_dataM %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2a)) %>%
    mutate(Medium = if_else(Q >= newx2a, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx3a) && !is.na(newx3b), print("Both"), print("one"))

if(nas == "Both") {
  new_dataM <- new_dataM %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3a & Q <= newx3b)) %>%
    mutate(High = if_else(Q >= newx3a & Q <= newx3b, row_number(), 0L))
} else if (is.na(newx3a)) {
  new_dataM <- new_dataM %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3b)) %>%
    mutate(High = if_else(Q <= newx3b, row_number(), 0L)) 
} else {
  new_dataM <- new_dataM %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3a)) %>%
    mutate(High = if_else(Q >= newx2a, row_number(), 0L)) 
}
head(new_dataM)
names(new_dataM)

new_dataM <- mutate(new_dataM, position="MC")

new_dataL <- arrange(new_dataL, date_num)

nas <- ifelse(!is.na(newx1aL) && !is.na(newx1bL), print("Both"), print("one"))

if(nas == "Both") {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1aL & Q <= newx1bL)) %>%
    mutate(Low = if_else(Q >= newx1aL & Q <= newx1bL, row_number(), 0L))
} else if (is.na(newx1aL)) {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1bL)) %>%
    mutate(Low = if_else(Q <= newx1b, row_number(), 0L)) 
} else {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1aL)) %>%
    mutate(Low = if_else(Q >= newx1aL, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx2aL) && !is.na(newx2bL), print("Both"), print("one"))

if(nas == "Both") {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2aL & Q <= newx2bL)) %>%
    mutate(Medium = if_else(Q >= newx2aL & Q <= newx2bL, row_number(), 0L))
} else if (is.na(newx2aL)) {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2bL)) %>%
    mutate(Medium = if_else(Q <= newx2bL, row_number(), 0L)) 
} else {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2aL)) %>%
    mutate(Medium = if_else(Q >= newx2aL, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx3aL) && !is.na(newx3bL), print("Both"), print("one"))

if(nas == "Both") {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3aL & Q <= newx3bL)) %>%
    mutate(High = if_else(Q >= newx3aL & Q <= newx3bL, row_number(), 0L))
} else if (is.na(newx3aL)) {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3bL)) %>%
    mutate(High = if_else(Q <= newx3bL, row_number(), 0L)) 
} else {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3aL)) %>%
    mutate(High = if_else(Q >= newx2aL, row_number(), 0L)) 
}
head(new_dataL)
names(new_dataL)
new_dataL <- mutate(new_dataL, position="LOB")

new_dataR <- arrange(new_dataR, date_num)

nas <- ifelse(!is.na(newx1aR) && !is.na(newx1bR), print("Both"), print("one"))

if(nas == "Both") {
  new_dataR <- new_dataR %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1aR & Q <= newx1bR)) %>%
    mutate(Low = if_else(Q >= newx1aR & Q <= newx1bR, row_number(), 0L))
} else if (is.na(newx1aR)) {
  new_dataR <- new_dataR %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1bR)) %>%
    mutate(Low = if_else(Q <= newx1bR, row_number(), 0L)) 
} else {
  new_dataR <- new_dataR %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1aR)) %>%
    mutate(Low = if_else(Q >= newx1aR, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx2aR) && !is.na(newx2bR), print("Both"), print("one"))

if(nas == "Both") {
  new_dataR <- new_dataR %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2aR & Q <= newx2bR)) %>%
    mutate(Medium = if_else(Q >= newx2aR & Q <= newx2bR, row_number(), 0L))
} else if (is.na(newx2aR)) {
  new_dataR <- new_dataR %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2bR)) %>%
    mutate(Medium = if_else(Q <= newx2bR, row_number(), 0L)) 
} else {
  new_dataR <- new_dataR %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2aR)) %>%
    mutate(Medium = if_else(Q >= newx2aR, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx3aR) && !is.na(newx3bR), print("Both"), print("one"))

if(nas == "Both") {
  new_dataR <- new_dataR %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3aR & Q <= newx3bR)) %>%
    mutate(High = if_else(Q >= newx3aR & Q <= newx3bR, row_number(), 0L))
} else if (is.na(newx3aR)) {
  new_dataR <- new_dataR %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3bR)) %>%
    mutate(High = if_else(Q <= newx3bR, row_number(), 0L)) 
} else {
  new_dataR <- new_dataR %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3aR)) %>%
    mutate(High = if_else(Q >= newx2aR, row_number(), 0L)) 
}
head(new_dataR)
names(new_dataR)

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
# total_days01 <- total_days01 %>% 
#   ungroup() %>%
#   select(-c(n_hours, ID01))
# 
# total_days02 <- total_days02 %>% 
#   ungroup() %>%
#   select(-c(n_hours, ID02))
# 
# total_days03 <- total_days03 %>% 
#   ungroup() %>%
#   select(-c(n_hours, ID03))
# total_days03
# 
# total_hours <- left_join(total_days01, total_days02, by=c("day", "month", "year", "position"))
# total_hours <- left_join(total_hours, total_days03, by=c("day", "month", "year", "position"))
# total_hours

# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F)

# total_hours <- ungroup(total_hours) %>%
#     unite(month_year, year:month, sep="-", remove=F)

## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)

# str(total_days)

# total_hours$month_year <-  zoo::as.yearmon(total_hours$month_year)
# total_hours

## change names of columns
total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)

# total_hours <- rename(total_hours, Low = n_days_low, Medium = n_days_medium, High = n_days_high)

## define seasons
winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months

total_days <- total_days %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )

# total_hours <- total_hours %>%
#   mutate(season = ifelse(month %in% winter, "winter", "summer") ) %>%
#   select(-day)

# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "year", "month", "season", "position"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)
range(na.omit(melt_days$n_days))
# melt_hours<-reshape2::melt(total_hours, id=c("month_year", "year", "month", "season", "position"))
# melt_hours <- rename(melt_hours, Probability_Threshold = variable,
#                     n_days = value)
# head(melt_hours)
# dim(melt_daysx)

##  plot - number of days 

melt_daysx <- filter(melt_days, position=="MC")
library(scales)
# +scale_x_datetime(labels = date_format("%b"))
str(melt_daysM)

# ggplot(melt_daysx, aes(fill=Probability_Threshold, x =month_year, y=n_days)) +
#   geom_bar(position="stack", stat = "identity") 
#   # facet_wrap(~position, nrow=3) 
  

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)

## number of days separated per year
## filter to only MC 
# melt_daysM <- filter(melt_days, position == "MC")

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%b")) +
  # scale_x_continuous(breaks=as.numeric(month_year), labels=format(month_year,"%b")) +
  facet_wrap(~year+position, scale="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Depth: Mid Channel",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%Y")) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%Y")) +
  facet_wrap(~season +position, scales="free", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)

