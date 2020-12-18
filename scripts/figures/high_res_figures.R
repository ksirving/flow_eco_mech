## high res figures

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)

file.name3 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_existing_spp.jpg")
ggsave(p3, filename=file.name3, dpi=300, height=5, width=6)

out.dir <- "/Users/katieirving/Documents/git/flow_eco_mech/figures/high_res/"
## willow seedling
## Depth

### data upload
inund <- read.csv("/Users/katieirving/Documents/git/willow_model_application/Input_data/inundation.csv")

######## seedling inudation curve from data predicting percent mortality from duration and depth Halsell et al and Vandersande et al


inund <- inund %>% 
  filter(species == "Salix gooddingii")

head(inund)

p1 <- ggplot(data = inund, mapping = aes(x = depth_cm, y = mortality_prec))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  labs(x = "Depth (cm)", y = "Mortality (%)")+
  theme_classic()+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20))

file.name1 <- paste0(out.dir, "willow_seedling_depth.jpg")
ggsave(p1, filename=file.name1, dpi=300, height=5, width=6)



## shear stress

### data upload
shear <- read.csv("/Users/katieirving/Documents/git/willow_model_application/Input_data/shear.csv", skip = 2)
head(shear)
######## seedling inudation curve from data predicting percent mortality from duration and depth Halsell et al and Vandersande et al


names(shear)[c(1,2, 4)] <- c("shear", "Year", "mortality")
shear$Year <- as.factor(shear$Year)


p1 <- ggplot(data = shear, mapping = aes(x = shear, y = mortality))+
  geom_point(aes( group = Year, color = Year), size = 4) +
  # geom_point(size = 4)+
  geom_smooth(method = "lm")+
  labs(y = "Mortality (%)", x = "Bed Shear Stress (Pa)")+
  theme_classic()+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20), 
        legend.text=element_text(size=20), legend.title=element_text(size=20))



file.name <- paste0(out.dir, "willow_seedling_Shear_colours.jpg")
ggsave(p1, filename=file.name, dpi=300, height=5, width=6)


###### Typha
## seedling
## Depth

load("/Users/katieirving/Documents/git/typha_model_application/input_data/typha_depth.RData")


p1 <- ggplot(data = depth[!is.na(depth$seedling_survial_perc),], mapping = aes(x = depth_cm, y = seedling_survial_perc, color = Species))+
  geom_point()+
  labs(x = "Depth (cm)", y = "Seedling survival (%)")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  # scale_fill_discrete(labels = c(italic("Typha domingensis", "Typha latifolia")))+
  theme(text = element_text(size=20), legend.text = element_text(face = "italic"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

file.name <- paste0(out.dir, "Typha_seed_surve_both_sp.jpg")
ggsave(p1, filename=file.name, dpi=300, height=5, width=6)


p1 <- ggplot(data = depth[!is.na(depth$seedling_survial_perc),], mapping = aes(x = depth_cm, y = seedling_survial_perc))+
  geom_point()+
  labs(x = "Depth (cm)", y = "Seedling survival (%)")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

file.name <- paste0(out.dir, "Typha_seed_surve_intergrated_sp.jpg")
ggsave(p1, filename=file.name, dpi=300, height=5, width=6)

## Temperature

load("/Users/katieirving/Documents/git/typha_model_application/input_data/typha_temp.RData")

p1 <- ggplot(data = temp, aes(x = temp_midRng_c, y = germination_perc))+
  geom_jitter(alpha = 0.2, size = 3)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+
  labs(x = "Temperature Mid-Range (C)", y = "Germination (%)")

file.name <- paste0(out.dir, "Typha_temp_mid.jpg")
ggsave(p1, filename=file.name, dpi=300, height=5, width=6)

p1 <- ggplot(data = temp, aes(x = high_temperature_c, y = germination_perc))+
  geom_jitter(alpha = 0.2, size = 3)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+
  labs(x = "High Temperature (C)", y = "Germination (%)")

file.name <- paste0(out.dir, "Typha_temp_high.jpg")
ggsave(p1, filename=file.name, dpi=300, height=5, width=6)

# summary(tmp_germ_mdl <- (lm(germination_perc ~ temperature_range_c + high_temperature_c + I(high_temperature_c^2), data = temp)))
# save(tmp_germ_mdl, file = "tmp_germ_mdl.rda")

# 
# ggplot(data = temp, aes(x = temperature_range_c, y = germination_perc))+
#   geom_jitter(alpha = 0.2, size = 3)+
#   geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
#   theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+
#   labs(x = "Temperature Range (C)", y = "Germination (%)")
# 
# unique(temp$source[!is.na(temp$germination_perc)])
# ## Adult
## Depth

load("/Users/katieirving/Documents/git/typha_model_application/input_data/typha_depth.RData")

### model
#patch occurrence


p1 <- ggplot(data = depth, mapping = aes(x = depth_cm, y = occurrence))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Depth (cm)", y = "Probability of Occurrence")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=25), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

file.name <- paste0(out.dir, "Typha_adult_depth.jpg")
ggsave(p1, filename=file.name, dpi=300, height=5, width=6)


## Velocity

load("/Users/katieirving/Documents/git/typha_model_application/input_data/typha_velocity.RData")

p1 <- ggplot(data = velocity, mapping = aes(x = vel_m_s, y = occurrence))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Velocity (m/s)", y = "Probability")+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  theme(text = element_text(size=25), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

file.name <- paste0(out.dir, "Typha_adult_velocity.jpg")
ggsave(p1, filename=file.name, dpi=300, height=5, width=6)


#### Cladophora

## Depth
## plot

depth <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/Depth_2_Higgins_etal_2005.csv")
depth <- na.omit(depth)
depth

depth <- depth %>%
  mutate(depth_cm = depth_m*100) %>%
  mutate(max_biomass_percent = (maximum_biomass_g_DW_m..2/1230)*100) %>%
  mutate(presence_absence = ifelse(max_biomass_percent == 0, 0, 1)) %>%
  mutate(max_biomass_percent_log = log(max_biomass_percent+1))

p1 <- ggplot(data = depth, mapping = aes(x = depth_cm, y = max_biomass_percent))+
  geom_point(size = 2)+
  stat_smooth(method="lm", formula = y ~ x + I(x^2)) +
  # scale_y_continuous(trans=log1p_trans()) +
  # scale_y_log10()+
  labs(x = "Depth (cm)", y = "Biomass (%)")+
  theme_classic()+
  # scale_y_continuous(limits=c(,100)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20))

file.name <- paste0(out.dir, "Cladophora_depth.jpg")
ggsave(p1, filename=file.name, dpi=300, height=6, width=6)

## Velocity
set.seed(435)
## 1km reach at 0.25m grids
## presence = 0.68 mean sd = 0.19
## absence = 0.41 mean, sd = 0.2
## n = 1000/0.25 = 4000

## 1km reach at 1m grids
## presence = 0.7 mean sd = 0.2
## absence = 0.44 mean, sd = 0.2
## n = 1000/1 = 1000
?rnorm
pres_velo25 <- rnorm(4000, 0.68, 0.19 )
abs_velo25 <- rnorm(4000, 0.41, 0.2 )

t.test(pres_velo25, abs_velo25)

dfp <- as.data.frame(pres_velo25) %>%
  rename(Velocity = pres_velo25) %>%
  mutate(Occurrence = 1, Species = "Cladophora glomerata")

dfa <- as.data.frame(abs_velo25) %>%
  rename(Velocity = abs_velo25) %>%
  mutate(Occurrence = 0, Species = "Cladophora glomerata")

df <- rbind(dfp, dfa)
head(df)
mean(df$Velocity)
dim(df)

summary(glm(Occurrence~Velocity, data=df, family="binomial"))


png("figures/Final_curves/Velocity/C2_Cladophora_velocity_model_lower.png", width = 500, height = 600)


p1 <- ggplot(data = df, mapping = aes(x = Velocity, y = Occurrence))+
  geom_point(size = 2)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  # scale_y_continuous(trans=log1p_trans()) +
  # scale_y_log10()+
  labs(x = "Velocity m/s", y = "Probability of Occurrence")+
  theme_classic()+
  # scale_y_continuous(limits=c(,100)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20))

file.name <- paste0(out.dir, "Cladophora_velocity.jpg")
ggsave(p1, filename=file.name, dpi=300, height=7, width=6)


# SAS ---------------------------------------------------------------------


##### SAS
## Adult
## Depth & data distribution

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## depth
ad_depth_con <- read.csv("output_data/old_data/05a_adult_depth_continuous_updated.csv") ## all wulff incl and thompson removed - remove SAWA?
ad_depth_cat <- read.csv("output_data/old_data/05a_adult_depth_categorical.csv")

# ad_depth_red <- subset(ad_depth_con, !Dataset=="Thompson")
all_depth <- rbind(ad_depth_con, ad_depth_cat)

unique(all_depth$Dataset) # 4 datasets,

depth_freq <- all_depth %>%
  uncount(Abundance)
# hist(depth_freq$Depth)

depth_freq <- subset(depth_freq, !Dataset=="SAWA")
dim(depth_freq) ## 1376

# depth_freq <- read.csv("output_data/old_data/adult_depth_prob_curve_data.csv")



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
                 labels = c( "S", "W", "HB"))

# plot densities
out.dir
png(paste0(out.dir, "SAS_adult_depth_data_dist.png"), width = 1200, height = 1000)

par(mar=c(5, 8,4,6))

par(cex.axis=3, cex.lab = 3, las=1)

sm.density.compare(as.vector(Depth), Dataset_num, 
                   ylab="", 
                   xlab="Depth (cm)", 
                   col=c("red", "blue", "purple"),
                   lwd = 3,
                   lty=c(1,5,3))

# add legend via mouse click
colfill<-c("red", "blue", "purple")
legend("topright",levels(data.f), fill=colfill, bty = "n", cex = 3)
# axis(2, ylab="Data Density")

dev.off()

## probability curve - histogram scaled and centered depth, then transformed back to raw depth
## Prep data

depth_freq$Scaled_Depth <-scale(depth_freq$Depth, scale=T, center=T)
scaled_x <- depth_freq$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=120)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))
## x axis with raw depth values
xfit_r <- seq(min(depth_freq$Depth), max(depth_freq$Depth), length=120)

?ggsave
png(paste0(out.dir, "SAS_adult_depth.png"), width = 1500, height = 1200)

par(mgp=c(9,3,0), mar=c(9, 9,4,6))
par(cex.axis=5)

## plot curve with raw depth axis
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='white', main = "" )
axis(1, at=pretty(xfit_r), cex.axis=5, line = 0)
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE,
     xlab='', 
     ylab='', 
     type='l', 
     col='red',
     lwd = 5)
#add these now with axis
par(new=TRUE)
axis(2, at=pretty(range(yfit)), cex.axis=5, las = 2)
title(xlab = "Depth (cm)", line = 7, cex.lab=5)
dev.off()

# file.name <- paste0(out.dir, "SAS_adult_depth.jpg")
# ggsave(, filename=file.name, dpi=300, height=7, width=6)

## Velocity

## data distribution by dataset

vel_data <- read.csv("output_data/adult_velocity_prob_curve_data.csv")

all_vel <- ad_vel_con

vel_freq <- all_vel %>% 
  uncount(Abundance)
vel_freq <- na.omit(vel_freq)

tx <- vel_freq$Dataset == "Saiki"
wx <- vel_freq$Dataset == "Wulff"

vel_freq$Dataset_num[tx] <- 1
vel_freq$Dataset_num[wx] <- 2

attach(vel_freq)

# create value labels
data.f <- factor(Dataset_num, levels= 1:2,
                 labels = c("Saiki", "Wulff"))
data.f
# plot densities
sm.density.compare(as.vector(Velocity), Dataset_num, xlab="Velocity m/s")
title(main="Adult/Velocity")

# add legend via mouse click
colfill<-c(2:(2+length(levels(data.f))))
legend(locator(1), levels(data.f), fill=colfill)

# check data
unique(vel_freq$Dataset)
mean(vel_freq$Velocity) ## 0.6121954
dim(vel_freq) ## 1167
range(vel_freq$Velocity)

## probability curve
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=1000)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw velocity values
xfit_r <- seq(min(vel_freq$Velocity), max(vel_freq$Velocity), length=1000)

## plot curve with raw depth axis
png("figures/Final_curves/Velocity/F2_SAS_Adult_velocity_Prob_curve.png", width = 700, height = 700)

plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r), cex.axis=2)
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Velocity (m/s)', ylab='Probability', type='l', col='red', main = "Adult/Velocity",
     cex.main = 2, cex.axis=2, cex.lab=2)
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)), cex.axis=2)
dev.off()
##Temperature
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

## Juvenile
## Depth

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



## rating curve conceptual example
