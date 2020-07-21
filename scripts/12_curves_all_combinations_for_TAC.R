## all curves for TAC

library(tidyverse)
library(dplyr)
library(tidyr)
# library(sm)

library(devtools)
install_github("cran/sm")
library(sm)
install.packages("sm")

# install.packages("remotes")
# library(remotes)
# install_version("sm", "2.2-5.6")
## does not work with new r version

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous_updated.csv") ## all wulff and thompson removed - remove SAWA?
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
sp_depth_cat <- read.csv("output_data/08a_spawning_depth_abundance.csv")

## adult depth - 

## data distribution with thompson
# ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(ad_depth_con, ad_depth_cat)

unique(all_depth$Dataset) # 4 datasets, observations (n=1293)

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
mean(depth_freq$Depth) ## 44.79041
dim(depth_freq) ## 1293
head(depth_freq)

# ## compare different data sets
# ### get numbers for datasets
unique(depth_freq$Dataset)

sx <- depth_freq$Dataset == "Saiki"
wx <- depth_freq$Dataset == "Wulff"
swx <- depth_freq$Dataset == "SAWA"
smx <- depth_freq$Dataset == "SMEA"
tx <- depth_freq$Dataset == "Thompson"


depth_freq$Dataset_num[sx] <- 1
depth_freq$Dataset_num[wx] <- 2
depth_freq$Dataset_num[swx] <- 3
depth_freq$Dataset_num[smx] <- 4
depth_freq$Dataset_num[tx] <- 5
depth_freq[smx,]
attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:5,
                 labels = c( "Saiki", "Wulff", "SAWA", "SMEA", "Thompson"))
tail(data.f)
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

## data distribution without thompson
ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(ad_depth_red, ad_depth_cat)

unique(all_depth$Dataset) # 4 datasets, observations (n=1293)

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
mean(depth_freq$Depth) ## 44.79041
dim(depth_freq) ## 1293
head(depth_freq)

# ## compare different data sets
# ### get numbers for datasets
unique(depth_freq$Dataset)
# tx <- depth_freq$Dataset == "Thompson"
sx <- depth_freq$Dataset == "Saiki"
wx <- depth_freq$Dataset == "Wulff"
swx <- depth_freq$Dataset == "SAWA"
smx <- depth_freq$Dataset == "SMEA"

depth_freq$Dataset_num[sx] <- 1
depth_freq$Dataset_num[wx] <- 2
depth_freq$Dataset_num[swx] <- 3
depth_freq$Dataset_num[smx] <- 4
# depth_freq$Dataset_num[tx] <- 5
depth_freq[smx,]
attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:4,
                 labels = c( "Saiki", "Wulff", "SAWA", "SMEA"))
tail(data.f)
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

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

## data distribution 
# ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(juv_depth_con, juv_depth_cat)

unique(all_depth$Dataset) # 4 datasets, observations (n=1293)

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
mean(depth_freq$Depth) ## 36.55253
dim(depth_freq) ## 257
head(depth_freq)

# ## compare different data sets
# ### get numbers for datasets
unique(depth_freq$Dataset)

sx <- depth_freq$Dataset == "Saiki"
# wx <- depth_freq$Dataset == "Wulff"
# swx <- depth_freq$Dataset == "SAWA"
smx <- depth_freq$Dataset == "SMEA"
# tx <- depth_freq$Dataset == "Thompson"


depth_freq$Dataset_num[sx] <- 1
# depth_freq$Dataset_num[wx] <- 2
# depth_freq$Dataset_num[swx] <- 3
depth_freq$Dataset_num[smx] <- 2
# depth_freq$Dataset_num[tx] <- 5
depth_freq[smx,]
attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c( "Saiki", "SMEA"))
tail(data.f)
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

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

#### spawning depth
head(sp_depth_cat)
all_depth <- sp_depth_cat
all_depth <- na.omit(all_depth)

unique(all_depth$Dataset) # 1 datasets

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
mean(depth_freq$Depth) ## 34.5
dim(depth_freq) ## 12
head(depth_freq)

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
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Spawning/Depth: Probability curve" )
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)))

##############################################################################
## temperature

## temperature
ad_temp_con <- read.csv("output_data/05a_adult_temperature_continuous.csv")
juv_temp_con <- read.csv("output_data/05a_juvenile_temperature_continuous.csv")
sp_temp_con <- read.csv("output_data/05a_spawning_temperature_continuous.csv")

head(ad_temp_con)

## data distribution 

all_temp <- ad_temp_con

unique(all_temp$Dataset) # 2 datasets, observations (n=1293)

temp_freq <- all_temp %>% 
  uncount(abundance)
hist(temp_freq$Temp)
mean(temp_freq$Temp) ## 19.19
dim(temp_freq) ## 963
head(temp_freq)

# ## compare different data sets
# ### get numbers for datasets
unique(temp_freq$Dataset)
# tx <- depth_freq$Dataset == "Thompson"
sx <- temp_freq$Dataset == "Saiki"
# wx <- depth_freq$Dataset == "Wulff"
swx <- temp_freq$Dataset == "SAWA"
# smx <- depth_freq$Dataset == "SMEA"

# depth_freq$Dataset_num[sx] <- 1
temp_freq$Dataset_num[sx] <- 1
# depth_freq$Dataset_num[swx] <- 3
temp_freq$Dataset_num[swx] <- 2
# depth_freq$Dataset_num[tx] <- 5
temp_freq[swx,]
attach(temp_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c( "Saiki", "SAWA"))
tail(data.f)

# plot densities
sm.density.compare(as.vector(Temp), Dataset_num, xlab="Temperature (Celsius)")
title(main="Temperature Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

## probability curve without Thompson
temp_freq$Scaled_Temp <-scale(temp_freq$Temp, scale=T, center=T)
scaled_x <- temp_freq$Scaled_Temp
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw depth values
xfit_r <- seq(min(temp_freq$Temp), max(temp_freq$Temp), length=120)

## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Temperature (Celsius)', ylab='Probability', type='l', col='red', main = "Adult/Temperature: Probability curve" )
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)))

head(ad_temp_con)

## data distribution 

all_temp <- juv_temp_con

unique(all_temp$Dataset) # 2 datasets, observations (n=1293)

temp_freq <- all_temp %>% 
  uncount(Abundance)
hist(temp_freq$Temp)
mean(temp_freq$Temp) ## 17.02
dim(temp_freq) ## 9
head(temp_freq)


## probability curve without Thompson
temp_freq$Scaled_Temp <-scale(temp_freq$Temp, scale=T, center=T)
scaled_x <- temp_freq$Scaled_Temp
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw depth values
xfit_r <- seq(min(temp_freq$Temp), max(temp_freq$Temp), length=120)

## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Temperature (Celsius)', ylab='Probability', type='l', col='red', main = "Juvenile/Temperature: Probability curve" )
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)))


#############
# spawning

## combine data for distribution by life stage
ad_temp_con$Life_Stage <- "Adult"
juv_temp_con$Life_Stage <- "Juvenile"
sp_temp_con$Life_Stage <- "Spawning"

## format spawning data
sp_temp_con <- sp_temp_con[, c(14,17)]
sp_temp_con$Abundance <- 1
sp_temp_con$Dataset <- "Saiki"
names(sp_temp_con)[1] <- "Temp"
sp_temp_con <- sp_temp_con[,c(1,3,4,2)]

write.csv(sp_temp_con, "output_data/12_spawning_temp_continuous.csv")
sp_temp_con <- read.csv("output_data/12_spawning_temp_continuous.csv")

names(ad_temp_con)[3] <- "Abundance"

head(ad_temp_con)
head(juv_temp_con)
head(sp_temp_con)

all_temp <- rbind(ad_temp_con, juv_temp_con, sp_temp_con)

temp_freq <- all_temp %>% 
  uncount(Abundance)

##look at different life stages

tx <- temp_freq$Life_Stage == "Adult"
sx <- temp_freq$Life_Stage == "Juvenile"
wx <- temp_freq$Life_Stage == "Spawning"


temp_freq$Dataset_num[tx] <- 1
temp_freq$Dataset_num[sx] <- 2
temp_freq$Dataset_num[wx] <- 3
sum(is.na(temp_freq))

attach(temp_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:3,
                 labels = c("Adult", "Juvenile", "Spawning"))
data.f
# plot densities
sm.density.compare(as.vector(Temp), Dataset_num, xlab="Temperature (Celsius)")
title(main="Temperature Distribution by Life Stage")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

temp_freq <- sp_temp_con %>% 
  uncount(Abundance)
mean(temp_freq$Temp) ## 19.482
dim(temp_freq) ## 15
head(temp_freq)

## probability curve spawning
temp_freq$Scaled_Temp <-scale(temp_freq$Temp, scale=T, center=T)
scaled_x <- temp_freq$Scaled_Temp
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
xfit
## x axis with raw depth values
xfit_r <- seq(min(temp_freq$Temp), max(temp_freq$Temp), length=120)
max(temp_freq$Temp)
## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Temperature (Celsius)', ylab='Probability', type='l', col='red', main = "Spawning/Temperature: Probability curve" )
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)))

#######################
# Velocity

ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous.csv")
juv_vel_con <- read.csv("output_data/05a_juvenile_velocity_continuous.csv")
sp_vel_con <- read.csv("output_data/08a_spawning_velocity_abundance")
head(ad_vel_con)
head(juv_vel_con)
dim(juv_vel_con) # 6


ad_vel_con$Life_Stage <- "Adult"
juv_vel_con$Life_Stage <- "Juvenile"
sp_vel_con$Life_Stage <- "Spawning"

ad_vel_con <- subset(ad_vel_con, Dataset !="Thompson")
unique(ad_vel_con$Dataset)
head(ad_vel_con)
head(juv_vel_con)
head(sp_vel_con)
sp_vel_con <- sp_vel_con[,-2]

all_vel <- rbind(ad_vel_con, juv_vel_con, sp_vel_con)

vel_freq <- all_vel %>% 
  uncount(Abundance)
head(vel_freq)
##look at different life stages

tx <- vel_freq$Life_Stage == "Adult"
sx <- vel_freq$Life_Stage == "Juvenile"
wx <- vel_freq$Life_Stage == "Spawning"


vel_freq$Dataset_num[tx] <- 1
vel_freq$Dataset_num[sx] <- 2
vel_freq$Dataset_num[wx] <- 3
ns <- which(is.na(vel_freq))

vel_freq[ns,]
vel_freq <- na.omit(vel_freq)
attach(vel_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:3,
                 labels = c("Adult", "Juvenile", "Spawning"))
data.f
# plot densities
sm.density.compare(as.vector(Velocity), Dataset_num, xlab="Velocity m/s")
title(main="Velocity Distribution by Life Stage")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

### distribution without juvenile


all_vel <- rbind(ad_vel_con,sp_vel_con)

vel_freq <- all_vel %>% 
  uncount(Abundance)
head(vel_freq)
##look at different life stages

tx <- vel_freq$Life_Stage == "Adult"
# sx <- vel_freq$Life_Stage == "Juvenile"
wx <- vel_freq$Life_Stage == "Spawning"


vel_freq$Dataset_num[tx] <- 1
# vel_freq$Dataset_num[sx] <- 2
vel_freq$Dataset_num[wx] <- 2
ns <- which(is.na(vel_freq))

vel_freq[ns,]
vel_freq <- na.omit(vel_freq)
attach(vel_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Adult", "Spawning"))
data.f
# plot densities
sm.density.compare(as.vector(Velocity), Dataset_num, xlab="Velocity m/s")
title(main="Velocity Distribution by Life Stage")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

## data distribution by dataset

all_vel <- ad_vel_con

vel_freq <- all_vel %>% 
  uncount(Abundance)
head(vel_freq)
mean(vel_freq$Velocity)
vel_freq <- na.omit(vel_freq)
unique(vel_freq$Dataset)

tx <- vel_freq$Dataset == "Saiki"
# sx <- vel_freq$Life_Stage == "Juvenile"
wx <- vel_freq$Dataset == "Wulff"


vel_freq$Dataset_num[tx] <- 1
# vel_freq$Dataset_num[sx] <- 2
vel_freq$Dataset_num[wx] <- 2

attach(vel_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Saiki", "Wulff"))
data.f
# plot densities
sm.density.compare(as.vector(Velocity), Dataset_num, xlab="Velocity m/s")
title(main="Velocity Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


vel_freq <- ad_vel_con %>% 
  uncount(Abundance)
vel_freq <- na.omit(vel_freq)
mean(vel_freq$Velocity) ## 0.504274
min(vel_freq$Velocity)
dim(vel_freq) ## 854
head(vel_freq)
unique(vel_freq$Dataset)

## probability curve spawning
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
xfit
## x axis with raw depth values
xfit_r <- seq(min(vel_freq$Velocity), max(vel_freq$Velocity), length=120)
xfit_r
## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Velocity (m/s)', ylab='Probability', type='l', col='red', main = "Adult/Velocity: Probability curve" )
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)))

## spawning

vel_freq <- sp_vel_con %>% 
  uncount(Abundance)
vel_freq <- na.omit(vel_freq)
mean(vel_freq$Velocity) ## 0.2751097
min(vel_freq$Velocity)
dim(vel_freq) ##11
head(vel_freq)
unique(vel_freq$Dataset)

## probability curve spawning
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
xfit
## x axis with raw depth values
xfit_r <- seq(min(vel_freq$Velocity), max(vel_freq$Velocity), length=120)
xfit_r
## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r))
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Velocity (m/s)', ylab='Probability', type='l', col='red', main = "Spawning/Velocity: Probability curve" )
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)))
