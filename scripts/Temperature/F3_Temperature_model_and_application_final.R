## Temperature curves - model and application
## adult & Juvenile

## produces probability curves for temperature, and application to sample node data (time series) for adult and Juvenile
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

setwd("/Users/katieirving/Documents/git/flow_eco_mech")
## temperature
ad_temp_con <- read.csv("output_data/Old_data/05a_adult_temperature_continuous.csv")
juv_temp_con <- read.csv("output_data/Old_data/05a_juvenile_temperature_continuous.csv")


# Data distribution -------------------------------------------------------

all_temp <- ad_temp_con

unique(all_temp$Dataset) # 2 datasets, observations (n=1293)
mean(all_temp$Temp) # 17.09268

temp_freq <- all_temp %>% 
  uncount(abundance)
hist(temp_freq$Temp)
mean(temp_freq$Temp) ## 19.19
dim(temp_freq) ## 1293
head(temp_freq)

# ## compare different data sets
# ### get numbers for datasets
unique(temp_freq$Dataset)

sx <- temp_freq$Dataset == "Saiki"
swx <- temp_freq$Dataset == "SAWA"

temp_freq$Dataset_num[sx] <- 1
temp_freq$Dataset_num[swx] <- 2

attach(temp_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c( "Saiki", "SAWA"))
tail(data.f)

# plot densities
sm.density.compare(as.vector(Temp), Dataset_num, xlab="Temperature (Celsius)")
title(main="Adult/Temperature")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


# Build adult temperature model -------------------------------------------------

## check data
unique(temp_freq$Dataset) ## 2 datasets, 963
mean(temp_freq$Temp) ## 19.19221
dim(temp_freq)

temp_freq$Scaled_Temp <-scale(temp_freq$Temp, scale=T, center=T)
scaled_x <- temp_freq$Scaled_Temp
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw depth values
xfit_r <- seq(min(temp_freq$Temp), max(temp_freq$Temp), length=120)

## plot curve with raw depth axis

png("figures/Final_curves/Temperature/F3_SAS_Adult_temperature_Prob_curve.png", width = 700, height = 700)

plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r), cex.axis=2)
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Temperature (Celsius)', ylab='Probability', type='l', col='red', main = "Adult/Temperature",
     cex.main = 2, cex.axis=2, cex.lab=2)
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)), cex.axis=2)
dev.off()

head(ad_temp_con)

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("temp_fit", "prob_fit")
head(fitdata)

write.csv(fitdata, "output_data/adult_temp_prob_curve_data.csv")


# Juvenile ----------------------------------------------------------------

## data distribution 

all_temp <- juv_temp_con

unique(all_temp$Dataset) # 1 dataset

temp_freq <- all_temp %>% 
  uncount(Abundance)
hist(temp_freq$Temp)
mean(temp_freq$Temp) ## 17.02
dim(temp_freq) ## 9
head(temp_freq)


## probability curve 
temp_freq$Scaled_Temp <-scale(temp_freq$Temp, scale=T, center=T)
scaled_x <- temp_freq$Scaled_Temp
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw depth values
xfit_r <- seq(min(temp_freq$Temp), max(temp_freq$Temp), length=120)

png("figures/Final_curves/Temperature/F3_SAS_juvenile_temperature_Prob_curve.png", width = 700, height = 700)
## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r), cex.axis=2)
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Temperature (Celsius)', ylab='Probability', type='l', col='red', main = "Juvenile/Temperature",
     cex.main = 2, cex.axis=2, cex.lab=2)
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)), cex.axis=2)

dev.off()

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("temp_fit", "prob_fit")
head(fitdata)

write.csv(fitdata, "output_data/juvenile_temp_prob_curve_data.csv")

