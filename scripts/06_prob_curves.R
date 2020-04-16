### probabilistic curves

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

## depth - adult

ad_depth_con <- read.csv("output_data/05a_adult_depth_continuous.csv")
ad_depth_cat <- read.csv("output_data/05a_adult_depth_categorical.csv")

all_depth <- rbind(ad_depth_con, ad_depth_cat)

ad_depth_con
plot(ad_depth_con$Depth, ad_depth_con$Abundance, col=ad_depth_con$Dataset)

summary(lm(Abundance~Depth, data=ad_depth_con))
hist(ad_depth_con$Depth)

depth_freq_con <- ad_depth_con %>%
  uncount(Abundance)
hist(depth_freq_con$Depth)

## need to uncount data into frequency

depth_freq <- ad_depth_cat %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
depth_freq

## add data type column

depth_freq_con$Data_Type <- "continuous"
depth_freq$Data_Type <- "categorical"
depth_freq_con$Data_Type_num <- 1
depth_freq$Data_Type_num <- 2

## add together to check histogram
all_data_freq <- rbind(depth_freq, depth_freq_con)
dim(all_data_freq)
all_data_freq$Depth <- as.numeric(as.character(all_data_freq$Depth))
hist(all_data_freq$Depth) ## 71 category 
str(all_data_freq)

############## models

## get probability from histogram
## all data
## includes 71 category and buffers the influence

## test with r tutorial

x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
plot(density(all_data_freq$Depth))

## compare different data sets
attach(all_data_freq)

# create value labels
data.f <- factor(Data_Type_num, levels= 1:2,
                labels = c("Continuous", "Categorical"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Data_Type_num, xlab="Depth (cm)")
title(main="Depth Distribution by Data Type")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


## categorical

plot(depth_freq$Depth, depth_freq$Abundance) ## many 1's skewing the curve
str(all_df)
## normal dist 
x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

plot(density(depth_freq$Depth))
######## continuous

## normal dist 
x <-depth_freq_con$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

plot(density(depth_freq_con$Depth))

### best all together but will need some form of normalisation
x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(all_data_freq$Depth))#;abline(v=50, col = 2)
  
min(all_data_freq$Depth)
### probabilities
d <- seq(1,130,1)
pfit <- dnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=130, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(1,130,1)
colnames(pfit_df) <- c("density", "Depth")
pfit_df

# density curve
plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability Density")


### get cumulative probability

### best all together but will need some form of normalisation
x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(all_data_freq$Depth))#;abline(v=50, col = 2)

min(all_data_freq$Depth)
### probabilities
d <- seq(1,130,1)
pfit <- pnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=130, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(1,130,1)
colnames(pfit_df) <- c("density", "Depth")
pfit_df

## cumulative
plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability")

## get probability density curve

x <-all_data_freq$Depth

plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability")

## get actual probability
 
?hist
h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit

ub <- 50
lb <- 20
mean=mean(x)
sd=sd(x)

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="blue")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) ## 0.585
# axis(1, at=seq(0, 120, 20), pos=0)

mean=mean(x)
sd=sd(x)
h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit


lb <- 5
ub <- 20

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) ## 0.17

h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit


lb <- 50
ub <- 80

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) # 0.18


## probabilities of abundance at a certain depth
tfit <- pnorm(90, mean=mean(x),sd=sd(x))
tfit ## 0.8142976
## depth at certain probabilities
qfit50 <- qnorm(0.5,mean=mean(x),sd=sd(x))
qfit90 <- qnorm(0.9, mean=mean(x),sd=sd(x))
qfit50

qfit90

## fit prob line at 0.5
# relate probilities to depth and density

## density data
d_data <- density(all_data_freq$Depth)

dens_df <- data.frame(matrix(nrow=length(d_data$y)))
dens_df[1] <- d_data$y
dens_df$Depth <- d_data$x
colnames(dens_df)[1] <- "density"
dens_df

dens_50 <- subset(dens_df, Depth>qfit50)
dens_50[1,1]

line05 <- dens_50[1,1] ## 0.5 line
line05
dens_90 <- subset(dens_df, Depth>qfit90)
line09 <- dens_90[1,1]
line09
plot(density(all_data_freq$Depth))
abline(line05, 0)
abline(line09,0)

## same line on histogram

x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

hist_df <- data.frame(matrix(nrow=length(yfit)))
colnames(hist_df)<- "freq"
hist_df$depth <- xfit
hist_df$freq <- yfit

hist_50 <- subset(hist_df, Depth>qfit50)
hist_50[1,1]

hist05 <- hist_50[1,1] ## 0.5 line
hist05
hist_90 <- subset(hist_df, Depth>qfit90)
hist09 <- hist_90[1,1]
hist09

abline(hist05,0)
abline(hist09,0)

#####################################################

## quadratic curve
## use abundance data

head(all_depth)

plot(all_depth$Depth, all_depth$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=all_depth)

summary(fit2)

pred_depth <- seq(0, 130, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(all_depth$Depth, all_depth$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)


## skewed due to all the 1s

one_depth <- subset(all_depth, Abundance>1)
dim(one_depth)

## remove categorised and mean data - worse
unique(all_depth$Dataset)[c(2,5)]
data_out <- droplevels(unique(all_depth$Dataset)[c(2,5)])
data_out
some_depth <- subset(all_depth, !Dataset %in% data_out)
some_depth

## try model again
plot(some_depth$Depth, some_depth$Abundance)

Depth2 <- all_depth$Depth^2

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=all_depth)


pred_depth <- seq(0, 130, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(some_depth$Depth, some_depth$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)
## skewed due to all the 1s

one_depth <- subset(all_depth, Abundance>1)
dim(one_depth)

#### log 

log(all_depth$Abundance)+1

all_depth$log_abundance <- log(all_depth$Abundance)+1

plot(all_depth$Depth, all_depth$Abundance)

fit2<-lm(log(Abundance+1)~poly(Depth,2,raw=TRUE), data=all_depth)

summary(fit2)

pred_depth <- seq(0, 130, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(all_depth$Depth, log(all_depth$Abundance+1), pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)
## skewed due to all the 1s

one_depth <- subset(all_depth, Abundance>1)
dim(one_depth)

### try with just smea

head(ad_depth_cat)

plot(ad_depth_cat$Depth, ad_depth_cat$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=ad_depth_cat)

summary(fit2)

pred_depth <- seq(0, 70, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(ad_depth_cat$Depth, ad_depth_cat$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)

## smea minus 70 category

ad_depth_catx <- ad_depth_cat[-15,]

head(ad_depth_catx)

plot(ad_depth_catx$Depth, ad_depth_catx$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=ad_depth_catx)

summary(fit2)

pred_depth <- seq(0, 70, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(ad_depth_catx$Depth, ad_depth_catx$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)


########################################
######################

## juvenile
juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")

juv_depth_con
juv_depth_cat


## need to uncount data into frequency

depth_freq <- juv_depth_cat %>% 
  uncount(Abundance)
hist(depth_freq$Depth)
head(depth_freq)

depth_freq_con <- juv_depth_con %>% 
  uncount(Abundance)
hist(depth_freq_con$Depth)
depth_freq_con

## add data type column

depth_freq_con$Data_Type <- "continuous"
depth_freq$Data_Type <- "categorical"
depth_freq_con$Data_Type_num <- 1
depth_freq$Data_Type_num <- 2

## add together to check histogram
all_data_freq <- rbind(depth_freq, depth_freq_con)
dim(all_data_freq)
all_data_freq$Depth <- as.numeric(as.character(all_data_freq$Depth))
hist(all_data_freq$Depth) 
str(all_data_freq)

############## models

## get probability from histogram
## all data
## includes 71 category and buffers the influence

## test with r tutorial

x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
plot(density(all_data_freq$Depth))

## compare different data sets
attach(all_data_freq)

# create value labels
data.f <- factor(Data_Type_num, levels= 1:2,
                 labels = c("Continuous", "Categorical"))
data.f
str(Depth)
as.vector(Depth)
# plot densities
sm.density.compare(as.vector(Depth), Data_Type_num, xlab="Depth (cm)")
title(main="Depth Distribution by Data Type")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)


## categorical

plot(depth_freq$Depth, depth_freq$Abundance) ## many 1's skewing the curve
str(all_df)
## normal dist 
x <-depth_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

plot(density(depth_freq$Depth))
######## continuous

## normal dist 
x <-depth_freq_con$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

plot(density(depth_freq_con$Depth))

### best all together but will need some form of normalisation
x <-all_data_freq$Depth
x
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=70)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(all_data_freq$Depth))#;abline(v=50, col = 2)

min(all_data_freq$Depth)
### probabilities
d <- seq(1,70,1)
pfit <- dnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=70, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(1,70,1)
colnames(pfit_df) <- c("density", "Depth")
pfit_df

# density curve
plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability Density")


### get cumulative probability

### best all together but will need some form of normalisation
x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=70)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
xfit

plot(density(all_data_freq$Depth))#;abline(v=50, col = 2)

min(all_data_freq$Depth)
### probabilities
d <- seq(1,70,1)
pfit <- pnorm(d, mean=mean(x),sd=sd(x))
str(pfit)
max(pfit)
pfitx <- unlist(pfit)
length(pfitx)
pfit_df <- data.frame(matrix(nrow=70, ncol=2))
pfit_df[,1] <- pfitx
pfit_df[,2] <- seq(1,70,1)
colnames(pfit_df) <- c("density", "Depth")
pfit_df
length(x)
## cumulative
plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability")

## get probability density curve

x <-all_data_freq$Depth

plot(pfit_df$Depth, pfit_df$density, xlab="Depth (cm)", ylab= "Probability")

## get actual probability

?hist
h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=70)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit

ub <- 50
lb <- 20
mean=mean(x)
sd=sd(x)

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="blue")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
mtext(result,3) ## 0.585
# axis(1, at=seq(0, 120, 20), pos=0)

# mean=mean(x)
# sd=sd(x)
# h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=130)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# yfit


lb <- 5
ub <- 20

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
result
mtext(result,3) ## 0.1

# h<-hist(x, xlab="Depth (cm)",col="white",lty="blank",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=130)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# yfit


lb <- 50
ub <- 80

i <- xfit >= lb & xfit <= ub
lines(xfit, yfit)
polygon(c(lb,xfit[i],ub), c(0,yfit[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< Depth <",ub,") =",
                signif(area, digits=3))
area # 0.18
mtext(result,3) # 0.18


## probabilities of abundance at a certain depth
tfit <- pnorm(90, mean=mean(x),sd=sd(x))
tfit ## 0.8142976
## depth at certain probabilities
qfit50 <- qnorm(0.5,mean=mean(x),sd=sd(x))
qfit90 <- qnorm(0.9, mean=mean(x),sd=sd(x))
qfit50

qfit90

## fit prob line at 0.5
# relate probilities to depth and density

## density data
d_data <- density(all_data_freq$Depth)

dens_df <- data.frame(matrix(nrow=length(d_data$y)))
dens_df[1] <- d_data$y
dens_df$Depth <- d_data$x
colnames(dens_df)[1] <- "density"
dens_df

dens_50 <- subset(dens_df, Depth>qfit50)
dens_50[1,1]

line05 <- dens_50[1,1] ## 0.5 line
line05
dens_90 <- subset(dens_df, Depth>qfit90)
line09 <- dens_90[1,1]
line09
plot(density(all_data_freq$Depth))
abline(line05, 0)
abline(line09,0)

## same line on histogram

x <-all_data_freq$Depth
h<-hist(x, breaks=10, col="red", xlab="Depth (cm)",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=130)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

hist_df <- data.frame(matrix(nrow=length(yfit)))
colnames(hist_df)<- "freq"
hist_df$depth <- xfit
hist_df$freq <- yfit

hist_50 <- subset(hist_df, Depth>qfit50)
hist_50[1,1]

hist05 <- hist_50[1,1] ## 0.5 line
hist05
hist_90 <- subset(hist_df, Depth>qfit90)
hist09 <- hist_90[1,1]
hist09

abline(hist05,0)
abline(hist09,0)

## try quad model

juv_depth_con <- read.csv("output_data/05a_juvenile_depth_continuous.csv")
juv_depth_cat <- read.csv("output_data/05a_juvenile_depth_categorical.csv")

juv_depth_cat
juv_depth_con
all_depth <- rbind(juv_depth_cat, juv_depth_con)
head(all_depth)

plot(all_depth$Depth, all_depth$Abundance)

fit2<-lm(Abundance~poly(Depth,2,raw=TRUE), data=all_depth)

summary(fit2)

pred_depth <- seq(0, 70, 1)
predictedcounts <- predict(fit2,list(Depth=pred_depth, Depth2=pred_depth^2))
plot(all_depth$Depth, all_depth$Abundance, pch=16, xlab = "Depth (cm)", 
     ylab = "Abundance", cex.lab = 1.3, col = "blue")
lines(pred_depth, predictedcounts, col = "darkgreen", lwd = 3)

####

## temperature
