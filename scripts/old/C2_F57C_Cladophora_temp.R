## cladophora

## 1,230 e–0.55 * Depth 

#max_depth = 1,230 e–0.55 * Depth 

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)
library(mgcv)

temp<- read.csv("input_data/Cambridge_etal_1987.csv")
temp
head(temp)

temp <- temp %>%
  rename(growth_perc_d = growth_percent_d..1) %>%
  mutate(temperature_celsius_f = as.factor(temperature_celsius))

# Model build -------------------------------------------------------------


## test for normality

shapiro.test(temp$growth_perc_d) ## significant
shapiro.test(log(temp$growth_perc_d+1)) ## significant
ggqqplot(temp$growth_perc_d) ## outliers!!!

### model exploration
str(temp)

library(mblm)


temp_mblm = mblm(growth_perc_d ~ temperature_celsius_f, data=temp)
warnings()
summary(temp_mblm)

n <- 50
sig <- 2
dat <- gamSim(1,n=n,scale=sig)
head(dat)
temp_gam <- gam(growth_perc_d ~ temperature_celsius_f, data=temp)
summary(temp_gam) ##  Deviance explained = 59.1%, 25 & 30 significant

temp_gam <- mgcv::gam(growth_perc_d ~ s(temperature_celsius, bs='ps', sp=0.6), data = temp)
summary(temp_gam) # R-sq.(adj) =  0.488   Deviance explained =   59%
plot(temp_gam)

p <- predict(temp_gam, type="lpmatrix")
beta <- coef(temp_gam)[grepl("temperature_celsius", names(coef(temp_gam)))]
s <- p[,grepl("temperature_celsius", colnames(p))] %*% beta
ggplot(data=cbind.data.frame(s, temp$temperature_celsius), aes(temp$temperature_celsius, y=s)) + geom_line()
p
?gam
b1 <- mgcv::gam(y ~ s(x1, bs='ps', sp=0.6) + s(x2, bs='ps', sp=0.6) + x3, data = dat)
summary(b1)
plot(b1)


# plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
p <- predict(b1, type="lpmatrix")
beta <- coef(b1)[grepl("x1", names(coef(b1)))]
s <- p[,grepl("x1", colnames(p))] %*% beta
ggplot(data=cbind.data.frame(s, dat$x1), aes(x=dat$x1, y=s)) + geom_line()
p


temp_leoss <- loess(growth_perc_d ~ temperature_celsius, data=temp)
summary(temp_leoss) ##  Deviance explained = 59.1%, 25 & 30 significant


## plot

png("figures/Final_curves/Temperature/C2_Cladophora_temp_model.png", width = 500, height = 600)

ggplot(data = temp, mapping = aes(x = temperature_celsius, y = growth_perc_d))+
  geom_point(aes(group = species_type, col = species_type)) +
  # stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
  geom_smooth(method="loess") +
  # scale_y_continuous(trans=log1p_trans()) +
  # scale_y_log10()+
  labs(x = "Temperature (c)", y = "Growth (%)")+
  theme_classic()+
  scale_y_continuous(limits=c(0,100)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20))

dev.off()

### use cut offs 15-35 degrees - cite Cambridge 1984
