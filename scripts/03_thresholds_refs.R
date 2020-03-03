## finding thrsholds 

## temperature

##### temperature

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

envicraft <- read.csv("output_data/00_Envicraft_2010_Temp_abundance.csv")
sawa <- read.csv("output_data/00_SAWA_2014_env_hab_abundance.csv")
saiki <- read.csv("input_data/Saiki_2000_temp_life_stage.csv")

envicraft
sawa <- sawa[, c(2,3,13, 15)]
sawa
names(saiki)
colnames(saiki)[6:13] <- c("Fish", "SL", "TL", "Weight", "Sex", "Life_stage", "Spawning", "Temp")
dim(saiki)
sum(is.na(saiki)) # 87
saiki <- na.omit(saiki)

## general - <22c
## can survive 38c

# h.	0 – 80mm : 0+ yrs., SL 81 – 120mm : 1+ yrs, and SL 121mm+ : 2+ yrs. (sawa 2014, saiki 2000)

range(sawa$Temp)
# 18.0 28.3 - backed up by others e.g. less than 30 degrees
# min 15.9 degrees - envicraft
# saiki may have some for life stage
range(saiki$Temp)
## 6.93 29.24


# breeding - 22-25 (SMEA)
# adults - <28
# fry/eggs - 18-24
# juvenile - 15-22 (to avoid warmth) Feeney and Swift
# survival - <30
# 
# breeding - 0 (<15 >28), 1 (15-18, 26-27), 2 (22-25)
# fry/eggs - 0 (<10 >30), 1 (10-17, 25-30), 2 (18-24)
# juvenile - 0 (<5 >28), 1 (5-14, 23-28), 2 (15-22)
# adults - 0 (>28), 1 (22-28), 2 (<22)


### depth ##################

# adult
# upload data
# smea_2003 <- read.csv("output_data/00_SMEA_depth_adult_2003.csv")
# smea_2004 <- read.csv("output_data/00_SMEA_depth_adult_2004.csv")
smea <- read.csv("output_data/00_SMEA_depth_adult.csv")
wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv") # bottom velocity

wulff
thomp
smea
smea$Depth
smea$Depth
range(wulff$Depth_cm) # adult 20 - 120cm
plot(wulff$Number~wulff$Depth_cm)
?lm
lm(wulff$Number~wulff$Depth_cm)
hist(wulff$Depth_cm)
quantile(wulff$Depth_cm)
# 0%    25%    50%    75%   100% 
# 20.00  35.75  43.00  50.00 120.00 
names(wulff)
ab_depth <- wulff[,c(9,10)]

library(tidyverse)
ab_depth
sum(ab_depth$Number)

dep_freq <- ab_depth %>% 
  uncount(Number)
str(dep_freq)

hist(dep_freq$Depth_cm)
quantile(dep_freq$Depth_cm)
# 0%  25%  50%  75% 100% 
# 20   38   46   50  120 


names(thomp)
range(thomp$Depth_m) # 0.10 0.33
plot(thomp$ab_mean~thomp$Depth_m)

ab_depth_th <- thomp[,6:7]
ab_depth_th
dep_freq_th <- ab_depth_th %>%
  uncount(ab_mean)
dep_freq_th
hist(dep_freq_th$Depth_m)
quantile(dep_freq_th$Depth_m)

# 0%  25%  50%  75% 100% 
# 0.10 0.18 0.19 0.26 0.33 

smea_ab_03 <- read.csv("output_data/00_SMEA_adult_depth_2003_abundance.csv")
smea_ab_04 <- read.csv("output_data/00_SMEA_adult_depth_2004_abundance.csv")

smea_ab_03

smea_ab <- cbind(smea_ab_04[,c(2,6,7)], smea_ab_03[,c(2,9)])
smea_ab$all_sites_2004 <-  smea_ab[,2] + smea_ab[,3]
smea_ab$abundance <- smea_ab$all_sites_ab + smea_ab$all_sites_2004

ab_depth_sm <- smea_ab[-16, c(4,7)]
dep_freq_sm <- ab_depth_sm %>% 
  uncount(abundance)
dep_freq_sm 
plot(dep_freq_sm$Depth)
quantile(dep_freq_sm$Depth) ## over 30 used more



# brown <- read.csv("output_data/00_Brown_2000_abundance_env_vars.csv") # p/a data perhaps not reliable/useful

## juveniles 

# 25-45 cm


# larvae

# 5-10