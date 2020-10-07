## Stickle back depth

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
stick <- read.csv("input_data/stickleback_env_data.csv") ## all wulff incl and thompson removed - remove SAWA?

head(stick)
unique(stick$life_history_stage)

stick <- stick %>%
  select(-source) #%>%
  # filter(life_history_stage == "juvenile")

unique(depth$Dataset) # 2 datasets, 

### df for depth only

depth <- stick %>%
  select(occurrence, depth.cm, Dataset) %>%
  rename(Abundance = occurrence, Depth = depth.cm) %>%
  filter(Dataset == "Impact Sciences")
  

head(depth)
sum(is.na(depth))

depth <- na.omit(depth)

depth_freq <- depth %>% 
  uncount(Abundance)
hist(depth_freq$Depth)

dim(depth_freq) ## 66
head(depth_freq)

#### histogram curve
## check data
unique(depth_freq$Dataset) ## 
mean(depth_freq$Depth) ## 41.17731

## histogram with normal curve
x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Adult/Depth Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
# plot(density(depth_freq$Depth))

min(depth$Depth) ## 5.08
## probability curve - histogram scaled and centered depth, then transformed back to raw depth

depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F, breaks=10)
xfit<-seq(min(scaled_x),max(scaled_x),length=200)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
## x axis with raw depth values
xfit_r <- seq(min(depth_freq$Depth), max(depth_freq$Depth), length=200)

## plot curve with raw depth axis
png("figures/Final_curves/Depth/S1_stickelback_Adult_depth_Prob_curve_2_datasets.png", width = 700, height = 700)

plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r), cex.axis=2)
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Depth (cm)', ylab='Probability', type='l', col='red', main = "Adult/Depth",
     cex.main = 2, cex.axis=2, cex.lab=2)
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)), cex.axis=2)
dev.off()

## data frame with probabilities and depth - to combine with hydraulic data

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("depth_fit", "prob_fit")
head(fitdata)

## make DF of new data to predict
new_data <- as.data.frame(seq(0,200, 0.5))
colnames(new_data) <- "value"

range(depth_stick$Depth)

### plot with chub and stickleback
depth_stick <- depth
depth_chub <- read.csv("output_data/00_Wulff_Chub_depth_abundance.csv")
depth_chub <- depth_chub[,-1]

head(depth_stick)
head(depth_chub)


## make curve

depth_freq_chub <- depth_chub %>% 
  uncount(Abundance)
hist(depth_freq_chub$Depth)

depth_freq_stick <-  depth_stick %>% 
  uncount(Abundance)
hist(depth_freq_stick$Depth)


## probability curve - histogram scaled and centered depth, then transformed back to raw depth

## chub
depth_freq_chub$Scaled_Depth <-scale(depth_freq_chub$Depth, scale=T, center=T)
scaled_x <- depth_freq_chub$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit_c<-seq(min(scaled_x),max(scaled_x),length=200)
yfit_c<-dnorm(xfit_c,mean=mean(scaled_x),sd=sd(scaled_x))
## x axis with raw depth values
xfit_r_c <- seq(min(depth_freq_chub$Depth), max(depth_freq_chub$Depth), length=200)


## data frame with probabilities and depth - to combine with hydraulic data

fitdata_c <- data.frame(matrix(ncol=2, nrow=length(yfit_c)))
fitdata_c[,1] <- xfit_r_c
fitdata_c[,2] <- yfit_c
colnames(fitdata_c) <- c("depth_fit", "prob_fit")
head(fitdata_c)

## use smooth spline to predict on new data set
new_values_chub <-smooth.spline(fitdata_c$depth_fit, fitdata_c$prob_fit)

## stickleback 

depth_freq_stick$Scaled_Depth <-scale(depth_freq_stick$Depth, scale=T, center=T)
scaled_x <- depth_freq_stick$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit_s<-seq(min(scaled_x),max(scaled_x),length=200)
yfit_s<-dnorm(xfit_s,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw depth values
xfit_r_s <- seq(min(depth_freq_chub$Depth), max(depth_freq_stick$Depth), length=200)
xfit_r_s

## data frame with probabilities and depth - to combine with hydraulic data

fitdata_s <- data.frame(matrix(ncol=2, nrow=length(yfit_s)))
fitdata_s[,1] <- xfit_r_s
fitdata_s[,2] <- yfit_s
colnames(fitdata_s) <- c("depth_fit", "prob_fit")
head(fitdata_s)

## create smoothing spline
## use smooth spline to predict on new data set
new_values_stick <-smooth.spline(fitdata_s$depth_fit, fitdata_s$prob_fit)

## make DF of new data to predict
new_data <- as.data.frame(seq(0,140, 0.5))
colnames(new_data) <- "value"

all_data <- new_data %>%
  dplyr::mutate(stick_fit = predict(new_values_stick, value)$y) %>%
  dplyr::mutate(chub_fit = predict(new_values_chub, value)$y) %>%
  rename(depth_cm = value) 

head(all_data)


## melt data
melt_data<-reshape2::melt(all_data, id=c("depth_cm"))
melt_data <- rename(melt_data, Species = variable)
unique(melt_data$Species)
head(melt_data)

range(melt_data$value)

melt_data$value <- ifelse(melt_data$value < 0, 0, melt_data$value)

png("figures/Chub/Chub_stickle_depth_curve.png", width = 600, height = 500)

ggplot(melt_data, aes(x = depth_cm, y=value)) +
  geom_line(aes( group =c(), color = Species)) +
  scale_color_manual(name = "Species", breaks = c("stick_fit", "chub_fit"),
                     values=c( "red", "blue"),
                     labels = c("Unarmored Threespine Stickleback", "Arroyo Chub")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  geom_vline(xintercept = 1.3, linetype="dotted", 
             color = "green", size=1.5) +
  # scale_color_manual( values = c(Toad = "green")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Chubb & Stickleback Probability Curve",
       y = "Probability",
       x = "Depth (cm)") #+ theme_bw(base_size = 15)
dev.off()


