## apply curve to hydraulic variables

library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## upload hydraulic data

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
## select columns
hyd_dep <- hydraul[,c(1:3,9)]
colnames(hyd_dep)[4] <-"depth_ft"

## convert unit from feet to meters
hyd_dep$depth_cm <- (hyd_dep$depth_ft*0.3048)*100

## plot depth data

# plot(hyd_dep$DateTime, hyd_dep$depth_cm)
# dim(hyd_dep) ## 60273

## subset data to test

hyd_dep <- hyd_dep[1:1000,]
tail(hyd_dep)
plot(hyd_dep$DateTime, hyd_dep$depth_cm)

## plot time series - use numbers for now, add dates in plot

str(hyd_dep)

hyd_dep$date_num <- seq(1,length(hyd_dep$DateTime), 1)
plot(hyd_dep$date_num, hyd_dep$depth_cm, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm)
hyd_dep$depth_cm
### upload curve data
## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

## combine data adult
all_depth <- rbind(ad_depth_con, ad_depth_cat)

## uncount data
depth_freq <- all_depth %>% 
  uncount(Abundance)

## curve data in dataframe - no plot

## scaled data
depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=10000)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

##  raw depth values
xfit_r <- seq(min(depth_freq$Depth), max(depth_freq$Depth), length=10000)

## data frame with probabilities and depth

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("depth_fit", "prob_fit")
xfit_r


## round the depths - don't need the high resolution
?round
hyd_dep$depth_cm_round <- round(hyd_dep$depth_cm, digits=1 )
fitdata$depth_fit_round <- round(fitdata$depth_fit, digits=1)
# head(fitdata)
# head(hyd_dep)
# match(hyd_dep$depth_cm_round,fitdata$depth_fit_round)

plot(hyd_dep$date_num, hyd_dep$depth_cm_round, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm_round)

all_data <- merge(hyd_dep, fitdata, by.x="depth_cm_round", by.y="depth_fit_round")
head(all_data)
head(hyd_dep)

all_data <- all_data[, c(1,7,9)]
sum(is.na(all_data))

## remove duplicate date_num (date time) and order

all_data <- all_data[!duplicated(all_data$date_num),]
new_data <- all_data[order(all_data$date_num),]

## plot modelled depth with probability

plot(new_data$date_num, new_data$depth_cm_round, type="n")
lines(new_data$date_num, new_data$depth_cm_round)
par(new=TRUE)
plot(new_data$date_num, new_data$prob_fit, axes=F, type="n", xlab="", ylab="")
lines(new_data$date_num, new_data$prob_fit, col="red")
axis(4, at=pretty(all_data$prob_fit))
