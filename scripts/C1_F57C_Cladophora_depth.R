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

depth <- read.csv("input_data/Depth_2_Higgins_etal_2005.csv")
depth <- na.omit(depth)
depth
## convert depth to cm and biomass to % and presence/absence for glm

depth <- depth %>%
  mutate(depth_cm = depth_m*100) %>%
  mutate(max_biomass_percent = (maximum_biomass_g_DW_m..2/1230)*100) %>%
  mutate(presence_absence = ifelse(max_biomass_percent == 0, 0, 1)) %>%
  mutate(max_biomass_percent_log = log(max_biomass_percent+1))

depth

## test for normality

shapiro.test(depth$maximum_biomass_g_DW_m..2)
shapiro.test(depth$max_biomass_percent)
ggqqplot(depth$max_biomass_percent)

### model exploation
log(depth$max_biomass_percent+1)
depth_lm <- lm( max_biomass_percent~ depth_cm, data=depth)

summary(depth_lm) # p-value: 0.04156

depth_gam <- gam(max_biomass_percent ~ depth_cm, data=depth)
summary(depth_gam)
## Deviance explained = 68.7%

depth_lmq <- lm(max_biomass_percent ~ depth_cm + I(depth_cm^2), data=depth)
summary(depth_lmq) ## p-value: 0.02099

depth_glm <- glm(max_biomass_percent~depth_cm, data=depth, family="gaussian")
summary(depth_glm)

depth_lm_log <- lm( max_biomass_percent_log~ depth_cm, data=depth)
summary(depth_lm_log) ## p-value: 1.903e-05

depth_lmq <- lm(max_biomass_percent_log ~ depth_cm + I(depth_cm^2), data=depth)
summary(depth_lmq) ## p-value: 0.0003203

## plot

## best model 

depth_lmq <- lm(max_biomass_percent ~ depth_cm + I(depth_cm^2), data=depth)
summary(depth_lmq) ## p-value: 0.02099

plot(depth_lmq)

## qqplot awful
## qqplot not good but passed normality above

## plot

png("figures/Final_curves/Depth/C1_Cladophora_depth_model.png", width = 500, height = 600)

ggplot(data = depth, mapping = aes(x = depth_cm, y = max_biomass_percent))+
  geom_point(size = 2)+
  stat_smooth(method="lm", formula = y ~ x + I(x^2)) +
  # scale_y_continuous(trans=log1p_trans()) +
  # scale_y_log10()+
  labs(x = "Depth (cm)", y = "Biomass (%)")+
  theme_classic()+
  # scale_y_continuous(limits=c(,100)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20))

dev.off()


