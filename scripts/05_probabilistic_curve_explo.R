### probablistic curve exploration

## Depth - adult has the most data to take a first stab

library(tidyverse)
library(dplyr)

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

## depth data

## all data upload
dep_ad_03 <- read.csv("output_data/00_SMEA_adult_depth_2003_abundance.csv")
dep_ad_04 <- read.csv("output_data/00_SMEA_adult_depth_2004_abundance.csv")
# wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
# thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
# saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv")
# envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
# sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")
# 
# 
# ## clean, subset into life stage and variables
# head(saiki)
# colnames(saiki)[6:15] <- c("Fish", "SL", "TL", "Weight", "Sex", "Life_stage", "Spawning", "Temp", "Depth_m", "Current_m_sec")
# subset(saiki, Life_stage == "Larvae") #16.51
# subset(saiki, Life_stage == "Juvenile")

# workflow - sep for each variable & life stage
#1 - decide on category for data
#2 - decide on trust in source of data
 # criteria # a) location - similarity to la river, b) type of study (observation, field manipulation, ultilization)

# 3.	Single field-based manipulations (often not completely controlled)
# a.	Binned results 
# b.	Continuous response curves

## any categorized field data. SMEA is a good candidate for this approach as
# 1 - categorised
# 2 - includes "absences" - i.e. habitat utlization 

head(dep_ad_03)
head(dep_ad_04)
dep_ad_04


# format to character and change names

## how to calculate mean & variance of categorised data?
## how to calulate threshold
## how to relate variance to probability
## how to incorporate different sites/years - mixed model??
  ## what are the differences between sites?
  ## why aren't the sites the same between years?


# https://www.displayr.com/how-to-calculate-an-average-value-from-categorical-data/
#https://www.southampton.ac.uk/passs/confidence_in_the_police/multivariate_analysis/simple_linear_regression_several_categories.page

## options for analysis
#1) add all data years and sites together - accumulatively (add together) - n=15 (too low?) - this one doesn't work - see below
#2) add all data years and sites together - continously (add as rows)
#3) treat each site, each year separatley - similar to mixed model, 
      #but not as do not have 4 levels with reps in each
#4) use all_sites - all sites added per year, treat year as separate
#5) mioxed model wioth velocity data? and substrate? just for fun!

# 1 & 2 - both options add data together, if response is presence/absence then it makes more sense to add
# Option 1
## convert abundance to presence absence per category
dep_ad_04
dep_ad_03
# smea_ab$Depth
## make df with only abundance data
smea_ab <- cbind(dep_ad_04[,c(2,6,7)], dep_ad_03[,c(2,10)])
## add 2004 sites together
smea_ab$all_sites_2004 <-  smea_ab[,2] + smea_ab[,3]
# add years together
smea_ab$abundance <- smea_ab$all_sites_ab + smea_ab$all_sites_2004
# depth and abundance only, remove abundance row
ab_depth_sm <- smea_ab[-16, c(4,7)]
ab_depth_sm$Depth_mid <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 71)
ab_depth_sm

# # ## count frequency of observations in each category
dep_freq_sm <- ab_depth_sm %>%
  uncount(abundance)
dep_freq_sm
plot(dep_freq_sm$Depth)
str(ab_depth_sm)
hist(dep_freq_sm$Depth_mid)
mean(dep_freq_sm$Depth_mid) # 57.81657
## get mid of each category
unique(dep_freq_sm$Depth)



# # add presence column
# dep_freq_sm$presence <- paste("1")
# dep_freq_sm$presence
ab_depth_sm$presence <-ifelse(ab_depth_sm$abundance == 0, 0,1)
ab_depth_sm
summary(glm(ab_depth_sm$presence ~ ab_depth_sm$Depth, family=binomial()))
summary(glm(ab_depth_sm$abundance ~ ab_depth_sm$Depth, family=poisson()))

plot(ab_depth_sm$Depth,ab_depth_sm$abundance)


## option 2 - bind together all siotes and years, create dummy data
## r bind all site info
dep_ad_04
dep_ad_03
#2003
# select only abundance 
s_2003 <- select(dep_ad_03, ends_with("_ab"), -starts_with("all_s"), matches("Depth")) 
# remove total abundance row
s_2003 <- s_2003[-16,]
s_2003
# change column names to match - maybe add sites as a categorical variable?
colnames(s_2003)[1:3] <- "abundance"
# bind rows together
sall_2003 <- rbind(s_2003[,c(4,1)], s_2003[,c(4,2)], s_2003[,c(4,3)])
str(sall_2003)

summary(lm(sall_2003$abundance~sall_2003$Depth))

# select only abundance 
s_2004 <- select(dep_ad_04, ends_with("_ab"),  matches("Depth")) 
# remove total abundance row
s_2004 <- s_2004[-16,]
s_2004
# change column names to match - maybe add sites as a categorical variable?
colnames(s_2004)[1:2] <- "abundance"
# bind rows together
sall_2004 <- rbind(s_2004[,c(3,1)], s_2004[,c(3,2)])
str(sall_2004)

all_smea <- rbind(sall_2003, sall_2004)
all_smea

## create presence/ absence column - this is the response variable

all_smea$presence <-ifelse(all_smea$abundance == 0, 0,1)


## dummy predictor variables 

## example with R data
library(car)
# Load the data
data("Salaries", package = "car")
# Inspect the data
sample_n(Salaries, 3)


# Compute the model
model <- lm(salary ~ sex, data = Salaries)
summary(model)$coef
contrasts(Salaries$sex)

res <- model.matrix(~rank, data = Salaries)
head(res[, -1])

model2 <- lm(salary ~ yrs.service + rank + discipline + sex,
             data = Salaries)
Anova(model2)

### depth data

# ab_lm <- lm(all_smea$abundance~all_smea$Depth)

all_smea

# GLM on presence absence vs categorical depth 
# treating all sites and years as separate samples to increase n
pr_glm <- glm(all_smea$presence~all_smea$Depth, family=binomial(link = "logit"))

summary(pr_glm$fitted.values) # 0.3867
pr_glm$coefficients
pr_glm$rank
pr_glm$residuals
pr_glm$linear.predictors

sd(pr_glm$residuals)
## get probability of occurecne from GLM
# newdata = data.frame(Depth = "0-5")
# newdata

prob_occs <- predict(pr_glm, all_smea$Depth, type="response")
pred_df <- data.frame(all_smea$Depth,prob_occs)
pred_df
plot(pred_df)



abline(0.5,0)


#####################################

# 
# lm_pred <- lm(prob_occs~all_smea.Depth, data=pred_df)
# # probs <- 
# pred_df$all_smea.Depth
# contrasts(droplevels(all_smea$Depth))
# 
# ## variables are n=15 - can only apply to total abundance
# 
# summary(ab_lm$fitted.values)
# ## gives mean of response
# mean(all_smea$abundance)
# 
# ## model with n=15
# 
# ab_lm <- lm(abundance~Depth, data=ab_depth_sm)
# summary(ab_lm)
# contrasts(droplevels(ab_depth_sm$Depth))
# summary(glm(presence~Depth, data=ab_depth_sm, family=binomial(link = "logit")))
### does not work with small sample size

##Figure 1. Probability of a site having a true mean CSCI score above a threshold 
# (e.g., ≥ 0.79), based on a single observation and assuming a within-site standard 
# deviation of 0.11 (as reported in Mazor et al. 2016). 

## smea categorical data does not have repeated site visits to find the mean, 
## also he has a continous variable
## also threshold is already known

## theshold could be > 3cm 


## continuous data

# workflow - sep for each variable & life stage
#1 - decide on category for data
#2 - decide on trust in source of data
# criteria # a) location - similarity to la river, b) type of study (observation, field manipulation, ultilization)
# b.	Continuous data – plot percent mortality/occurrence vs. physical variable

wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity
saiki <- read.csv("input_data/abundance_env_vars_saiki_2000.csv")
# envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")
# 
