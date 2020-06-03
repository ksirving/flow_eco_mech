## all curves for TAC

library(tidyverse)
library(dplyr)
# library(sm)

# library(devtools)
# install_github("cran/sm")
# library(sm)
## does not work with new r version

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
sp_depth_cat <- read.csv("output_data/08a_spawning_depth_abundance.csv")

## adult depth - 

ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(ad_depth_red, ad_depth_cat)

unique(all_depth$Dataset) # 4 datasets, observations (n=1293)

depth_freq <- all_depth %>% 
  uncount(Abundance)
# hist(depth_freq$Depth)
# mean(depth_freq$Depth) ## 44.79041
# dim(depth_freq) ## 1293
# head(depth_freq)

# ## compare different data sets
# ### get numbers for datasets
# unique(depth_freq$Dataset)
# tx <- depth_freq$Dataset == "Thompson"
# sx <- depth_freq$Dataset == "Saiki"
# wx <- depth_freq$Dataset == "Wulff"
# swx <- depth_freq$Dataset == "SAWA"
# smx <- depth_freq$Dataset == "SMEA"
# 
# depth_freq$Dataset_num[tx] <- 1
# depth_freq$Dataset_num[sx] <- 2
# depth_freq$Dataset_num[wx] <- 3
# depth_freq$Dataset_num[swx] <- 4
# depth_freq$Dataset_num[smx] <- 5
# 
# attach(depth_freq)
# 
# # create value labels
# data.f <- factor(Dataset_num, levels= 1:5,
#                  labels = c("Thompson", "Saiki", "Wulff", "SAWA", "SMEA"))
# data.f
# str(Depth)
# as.vector(Depth)
# # plot densities
# sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
# title(main="Depth Distribution by Dataset")
# 
# # add legend via mouse click
# colfill<-c(2:(2+length(levels(data.f))))
# legend(locator(1), levels(data.f), fill=colfill)


## probability curve without Thompson
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
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)))

## Juvenile depth - 2 datasets, 

head(juv_depth_con)
juv_depth_con
head(juv_depth_cat)
unique(juv_depth_con$Dataset) ## 1 dataset
all_depth <- rbind(juv_depth_con, juv_depth_cat)

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
mean(depth_freq$Depth) ## 36.55253
dim(depth_freq) ## 257
head(depth_freq)

## probability curve 
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
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Juvenile/Depth: Probability curve" )
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)))

