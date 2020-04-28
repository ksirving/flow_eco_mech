### creating intuitive figures

library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## work flow
## are the pnorm values the same as dnorm with scaled data?
## high/mid/low probability - quantiles - prob values to 1-0
## try model with removed lower occupied depths
## transform scaled data back to raw values

## upload data 
## Depth
## Adult

ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
sp_depth_cat <- read.csv("output_data/05a_spawning_depth_continuous.csv")


## combine data adult
all_depth <- rbind(ad_depth_con, ad_depth_cat)

depth_freq <- all_depthx %>% 
  uncount(Abundance)
hist(depth_freq$Depth)


### centered and scaled histogram probability
mean ## 33.5929
(71-33.5929)/4
subset(depth_freq, Scaled_Depth >=2)

depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h, xlab="Distance from mean Depth", ylab = "Percentage of observations",main = "Adult/Depth: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
scaled_x
dnorm()