### all probability curves

library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## work flow
## histograms as probability
## density or probability?
## scale and center using mean depth

## upload data 
## Depth
## Adult

ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
sp_depth_cat <- read.csv("output_data/05a_spawning_depth_continuous.csv")

## combine data
all_depth <- rbind(ad_depth_con, ad_depth_cat)

## uncount data into frequency

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

##look at different data sets

## compare different data sets
### get numbers for datasets
unique(depth_freq$Dataset)
tx <- depth_freq$Dataset == "Thompson"
sx <- depth_freq$Dataset == "Saiki"
wx <- depth_freq$Dataset == "Wulff"
swx <- depth_freq$Dataset == "SAWA"
smx <- depth_freq$Dataset == "SMEA"

depth_freq$Dataset_num[tx] <- 1
depth_freq$Dataset_num[sx] <- 2
depth_freq$Dataset_num[wx] <- 3
depth_freq$Dataset_num[swx] <- 4
depth_freq$Dataset_num[smx] <- 5

attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:5,
                 labels = c("Thompson", "Saiki", "Wulff", "SAWA", "SMEA"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

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

### spawning

## uncount data into frequency
sp_depth_cat
depth_freq <- sp_depth_cat %>% 
  uncount(abundance)
hist(depth_freq$Depth)

##look at different data sets

## compare different data sets
### get numbers for datasets
unique(depth_freq$Dataset)
tx <- depth_freq$Dataset == "Thompson"
sx <- depth_freq$Dataset == "Saiki"
wx <- depth_freq$Dataset == "Wulff"
swx <- depth_freq$Dataset == "SAWA"
smx <- depth_freq$Dataset == "SMEA"

depth_freq$Dataset_num[tx] <- 1
depth_freq$Dataset_num[sx] <- 2
depth_freq$Dataset_num[wx] <- 3
depth_freq$Dataset_num[swx] <- 4
depth_freq$Dataset_num[smx] <- 5

attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:5,
                 labels = c("Thompson", "Saiki", "Wulff", "SAWA", "SMEA"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Dataset")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

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


### juvenile

juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
# sp_depth_cat <- read.csv("output_data/05a_spawning_depth_continuous.csv")

## combine data
all_depth <- rbind(juv_depth_con, juv_depth_cat)

## uncount data into frequency

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

##look at different data sets

## compare different data sets
### get numbers for datasets
unique(depth_freq$Dataset)
sx <- depth_freq$Dataset == "Saiki"
smx <- depth_freq$Dataset == "SMEA"

depth_freq$Dataset_num[sx] <- 1
depth_freq$Dataset_num[smx] <- 2

attach(depth_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Saiki", "SMEA"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Dataset_num, xlab="Depth (cm)")
title(main="Depth Distribution by Dataset (Juvenile)")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

# mean ## 36.55253
depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h,  xlab="Distance from mean Depth", ylab = "Percentage of observations", main = "Juvenile/Depth: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
# axis(1, at=pretty(seq(0,120,1)))

### example of logistic regression with percentage

## frequency column
#### histogram
## use adult data
all_depth <- rbind(ad_depth_con, ad_depth_cat)

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

sum(h$counts)
x <-depth_freq$Depth
h<-hist(x)
fr <- h$counts
per <- ((h$counts/sum(h$counts))*100)
h$counts=(h$counts/sum(h$counts))*100

xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
diff(h$mids[1:2])
xfit ## depths
yfit ## frequencies

percs <- as.data.frame(matrix(ncol=2,nrow=12))
colnames(percs) <- c("freq", "percs")
percs$freq <- fr
percs$percs <- per
percs

freq_df <- cbind(xfit, yfit)

freq_df <- as.data.frame(freq_df)
names(freq_df) <- c("Depth", "Freq")
str(freq_df)
max(freq_df$Freq)


freq_df$f140 <- ifelse(freq_df$Freq <=140, 1,0) ## around 95% of observations
range(freq_df[which(freq_df$f140==1),"Depth"]) ## depths between 64 & 120


freq_df$f300 <- ifelse(freq_df$Freq  <= 300, 1,0) ## around 12.5% of observations
range(freq_df[which(freq_df$f300==1),"Depth"])

freq_df$f493 <- ifelse(freq_df$Freq <= 493 , 1,0) ## around 20.3% of observations
range(freq_df[which(freq_df$f493==1),"Depth"])

freq_df$fmax <- ifelse(freq_df$Freq > 493, 1,0) ## around 20.3% of observations
range(freq_df[which(freq_df$fmax==1),"Depth"])

freq_df

## model glm

freq140_glm <- glm(f140~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq140_glm)

freq300_glm <- glm(f300~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq300_glm)

freq493_glm <- glm(f493~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq493_glm)

freqmax_glm <- glm(fmax~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freqmax_glm)


# create range of depth values

xdepth  <- seq(0,130, 0.1)
## prediction

## percentage is in relation to the bins in histogram

ydepth <- predict(freq140_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f140, pch = 16, xlab = "Depth (cm)", ylab = "Presence", main="Adult/Depth: 5% Obvs")
lines(xdepth, ydepth) ## need to force through 0


ydepth <- predict(freq300_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f300, pch = 16, xlab = "Depth (cm)", ylab = "Presence", main="Adult/Depth: 12.5% Obvs")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freq493_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f493, pch = 16, xlab = "Depth (cm)", ylab = "Presence", main="Adult/Depth: 20% Obvs")
lines(xdepth, ydepth) ## need to force through 0

ydepth <- predict(freqmax_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$fmax, pch = 16, xlab = "Depth (cm)", ylab = "Presence", main="Adult/Depth: 46% Obvs")
lines(xdepth, ydepth) ## need to force through 0

## back to prob curves

## temperature
ad_temp_con <- read.csv("output_data/05a_adult_temperature_continuous.csv")
juv_temp_con <- read.csv("output_data/05a_juvenile_temperature_continuous.csv")
sp_temp_con <- read.csv("output_data/05a_spawning_temperature_continuous.csv")
juv_temp_con
## adult
ad_temp_con
## uncount data into frequency

temp_freq <- ad_temp_con %>% 
  uncount(abundance)
hist(temp_freq$Temp)

##look at different data sets

## compare different data sets
### get numbers for datasets
unique(temp_freq$Dataset)
sx <- temp_freq$Dataset == "Saiki"
swx <- temp_freq$Dataset == "SAWA"

temp_freq$Dataset_num[sx] <- 1
temp_freq$Dataset_num[swx] <- 2

attach(temp_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Saiki", "SAWA"))


# plot densities
sm.density.compare(as.vector(Temp), Dataset_num, xlab="Temp")
title(main="Temp Distribution by Dataset (Adult)")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

mean(temp_freq$Temp) ## 19.19221
temp_freq$Scaled_Temp <-scale(temp_freq$Temp, scale=T, center=T)
scaled_x <- temp_freq$Scaled_Temp
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h,  xlab="Distance from mean Temp", ylab = "Percentage of observations", main = "Adult/Temp: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
# axis(1, at=pretty(seq(0,120,1)))

## Juvenile

juv_temp_con
## adult
ad_temp_con
## uncount data into frequency

temp_freq <- juv_temp_con %>% 
  uncount(Abundance)
hist(temp_freq$Temp)

mean(temp_freq$Temp) ## 17.02222
temp_freq$Scaled_Temp <-scale(temp_freq$Temp, scale=T, center=T)
scaled_x <- temp_freq$Scaled_Temp
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h,  xlab="Distance from mean Temp", ylab = "Percentage of observations", main = "Juvenile/Temp: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
# axis(1, at=pretty(seq(0,120,1)))
min(temp_freq$Temp)

## velocity

ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous.csv")
# ad_vel_cat <- read.csv("output_data/05a_adult_velocity_categorical.csv") ## bottom velocity
juv_vel_con <- read.csv("output_data/05a_juvenile_velocity_continuous.csv")
# juv_vel_cat <- read.csv("output_data/05a_juvenile_velocity_categorical.csv") ## bottom velocity

ad_vel_con2 <- subset(ad_vel_con, Dataset=="Saiki")
ad_vel_con2
vel_freq <- ad_vel_con2 %>% 
  uncount(Abundance)
hist(vel_freq$Velocity)

##look at different data sets - need site info

## compare different data sets
### get numbers for datasets
unique(vel_freq$Dataset)
sx <- vel_freq$Dataset == "Saiki"
swx <- vel_freq$Dataset == "SAWA"

temp_freq$Dataset_num[sx] <- 1
temp_freq$Dataset_num[swx] <- 2

attach(temp_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Saiki", "SAWA"))


# plot densities
sm.density.compare(as.vector(Temp), Dataset_num, xlab="Temp")
title(main="Temp Distribution by Dataset (Adult)")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

#### curve
mean(vel_freq$Velocity) ## 0.3422552, -1 = 0.09ms
min(vel_freq$Velocity)
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x)
h$counts=(h$counts/sum(h$counts))*100
plot(h,  xlab="Distance from mean Velocity", ylab = "Percentage of observations", main = "Adult/Velocity: Probability curve")
par(new=TRUE)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='', ylab='', type='l', col='red' )
#add these now with axis
axis(4, at=pretty(range(yfit)))
# axis(1, at=pretty(seq(0,120,1)))
dnorm(-2, mean=mean(scaled_x),sd=sd(scaled_x))

vel_freq
