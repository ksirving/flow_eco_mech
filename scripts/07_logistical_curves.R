### turning density/ frequency into logistical model/curve

library(tidyverse)
library(dplyr)
library(sm)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")


### upload data
## depth
ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")
sp_depth_cat <- read.csv("output_data/05a_spawning_depth_continuous.csv")

## temperature
ad_temp_con <- read.csv("output_data/05a_adult_temperature_continous.csv")
juv_temp_con <- read.csv("output_data/05a_juvenile_temperature_continous.csv")
sp_temp_con <- read.csv("output_data/05a_spawning_temperature_continuous.csv")

## velocity
ad_vel_con <- read.csv("output_data/05a_adult_velocity_continuous.csv")
ad_vel_cat <- read.csv("output_data/05a_adult_velocity_categorical.csv")
juv_vel_con <- read.csv("output_data/05a_juvenile_velocity_continuous.csv")
juv_vel_cat <- read.csv("output_data/05a_juvenile_velocity_categorical.csv")
sp_vel_cat <- read.csv("output_data/05a_spawning_velocity_continuous.csv")


##### get histogram and density data frames

## depth - adult

ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

## combine data
all_depth <- rbind(ad_depth_con, ad_depth_cat)

## uncount data into frequency

depth_freq <- all_depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

#### histogram
x <-depth_freq$Depth
h<-hist(x, breaks=10, lty="white", xlab="Depth (cm)",
        main="Adult Depth")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

xfit ## depths
yfit ## frequencies
plot(density(depth_freq$Depth))
h$counts

freq_df <- cbind(xfit, yfit)
freq_df <- as.data.frame(freq_df)
names(freq_df) <- c("Depth", "Freq")
str(freq_df)

### logistic curve
### binary column for 300 freq

freq_df$f300 <- ifelse(freq_df$Freq >= 300, 1,0)
head(freq_df)

## model
freq300_glm <- glm(f300~Depth, data=freq_df,family=binomial(link = "logit"))
summary(freq300_glm)

# create range of depth values

xdepth  <- seq(0,130, 0.1)
## prediction
ydepth <- predict(freq300_glm, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f300, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepth) ## need to force through 0

freq300_glm_zero <- glm(f300~Depth-1, data=freq_df, family=binomial)

ydepthz <- predict(freq300_glm_zero, list(Depth = xdepth), type="response")
plot(freq_df$Depth, freq_df$f300, pch = 16, xlab = "Depth (m)", ylab = "Presence")
lines(xdepth, ydepthz) ## doesn't work!!!!!
length(xdepth)
length(ydepthz)

##### density

## density data
d_data <- density(depth_freq$Depth)

dens_df <- data.frame(matrix(nrow=length(d_data$y)))
dens_df[1] <- d_data$y
dens_df$Depth <- d_data$x
colnames(dens_df)[1] <- "density"
dens_df

plot(dens_df$Depth, dens_df$density)   

### binary column for 0.010

