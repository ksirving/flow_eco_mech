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
smea_2003 <- read.csv("output_data/00_SMEA_depth_adult_2003.csv")
smea_2004 <- read.csv("output_data/00_SMEA_depth_adult_2004.csv")
smea <- read.csv("output_data/00_SMEA_depth_adult.csv")
wulff <- read.csv("output_data/00_Wulff_depth_abundance.csv")
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv")


smea$Depth
smea$Depth
# < 15 = 0

# brown <- read.csv("output_data/00_Brown_2000_abundance_env_vars.csv") # p/a data perhaps not reliable/useful

## juveniles 

# 25-45 cm


# larvae

# 5-10