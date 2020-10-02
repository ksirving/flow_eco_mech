### make big file with time results

## upload data for each species
library(tidyverse)
library(tidyr)
library(dplyr) 
## time stats and Q limits

## santa Ana Sucker
sas_ad_depth_time <- read.csv("results/F1_LA11_adult_depth_time_stats.csv")
sas_ad_depth_Q_limits <- read.csv("results/F1_LA11_Q_limits.csv")

sas_juv_depth_time <- read.csv("results/F1_LA11_juvenile_depth_time_stats.csv")
sas_juv_depth_Q_limits <- read.csv("results/F1_LA11_juvenile_Q_limits.csv")

sas_ad_velocity_time <- read.csv("results/F2_LA11_adult_velocity_time_stats.csv")
sas_ad_velocity_Q_limits <- read.csv("results/F2_LA11_Adult_Velocity_Q_limits.csv")

## mrgation

mig_in_depth_time <- read.csv("results/F5_LA11_SH_migration_depth_time_stats.csv")
mig_in_depth_Q_limits <- read.csv("results/F5_LA11_migration_IN_depth_Q_limits.csv")

mig_in_velocity_time <- read.csv("results/F5_LA11_SH_migration_velocity_time_stats.csv")
mig_in_velocity_Q_limits <- read.csv("results/F5_LA11_migration_IN_velocity_Q_limits.csv")

## typha

typha_ad_depth_time <- read.csv("results/M1_LA11_typha_adult_patch_depth_time_stats.csv")
typha_ad_depth_Q_limits <- read.csv("results/M1_LA11_adult_patch_Depth_Q_limits.csv")

typha_seed_depth_time <- read.csv("results/M3_LA11_typha_seedling_depth_time_stats.csv")
typha_seed_depth_Q_limits <- read.csv("results/M3_LA11_Typha_seedling_Depth_Q_limits.csv")

typha_ad_velocity_time <- read.csv("results/M2_LA11_typha_adult_patch_velocity_time_stats.csv")
typha_ad_velocity_Q_limits <- read.csv("results/M2_LA11_adult_patch_velocity_Q_limits.csv")


ann_stats <- unique(sas_ad_depth_time$Probability_Threshold)[1:3]

crit_stats <- unique(sas_ad_depth_time$Probability_Threshold)[4:6]

## SAS

######## ADULT
sas_ad_depth_time_ann <- sas_ad_depth_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Santa Ana Sucker"), Life_Stage = ("Adult"), Hydraulic = "Depth") %>%
  distinct()

tail(sas_ad_depth_time_ann)
sas_ad_depth_time_ann
## add Q limits

## rownames as column, transform and add position column
rownames(sas_ad_depth_Q_limits)<- sas_ad_depth_Q_limits[,"X"]
sas_ad_depth_Q_limitsF <- as.data.frame(t(sas_ad_depth_Q_limits[,-1])) 
sas_ad_depth_Q_limitsF <- mutate(sas_ad_depth_Q_limitsF, position = rownames(sas_ad_depth_Q_limitsF))

## filter low probability then merges limits with stats df 

lows <- c("Low", "Low.Seasonal")

low_joiny <- select(sas_ad_depth_Q_limitsF, starts_with("Low"), position)
low_joinx <- filter(sas_ad_depth_time_ann, Probability_Threshold %in% lows ) #
low_join <- merge(low_joinx, low_joiny, by="position", all=T)
low_join <- rename(low_join, Q_limit1 = Low_Prob_1, Q_limit2 = Low_Prob_2, 
                   Q_limit3 = Low_Prob_3,  Q_limit4 = Low_Prob_4)

low_joinx

## medium
meds <- c("Medium", "Medium.Seasonal")

med_joiny <- select(sas_ad_depth_Q_limitsF, starts_with("Med"), position)
med_joinx <- filter(sas_ad_depth_time_ann, Probability_Threshold %in% meds ) #
med_join <- merge(med_joinx, med_joiny, by="position", all=T)
med_join <- rename(med_join, Q_limit1 = Med_Prob_1, Q_limit2 = Med_Prob_2, 
                   Q_limit3 = Med_Prob_3,  Q_limit4 = Med_Prob_4)
  
med_join

## high

highs <- c("High", "High.Seasonal")

high_joiny <- select(sas_ad_depth_Q_limitsF, starts_with("High"), position)
high_joinx <- filter(sas_ad_depth_time_ann, Probability_Threshold %in% highs) #
high_join <- merge(high_joinx, high_joiny, by="position", all=T)
high_join <- rename(high_join, Q_limit1 = High_Prob_1, Q_limit2 = High_Prob_2, 
                   Q_limit3 = High_Prob_3,  Q_limit4 = High_Prob_4)

high_join

## join together

sas_ad_depth_Q_time <- rbind(low_join, med_join, high_join)
sas_ad_depth_Q_time

####### JUVENILE
sas_juv_depth_time_ann <- sas_juv_depth_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Santa Ana Sucker"), Life_Stage = ("Juvenile"), Hydraulic = "Depth") %>%
  distinct()

head(sas_juv_depth_time_ann)
sas_juv_depth_Q_limits
## add Q limits

## rownames as column, transform and add position column
rownames(sas_juv_depth_Q_limits)<- sas_juv_depth_Q_limits[,"X"]
sas_juv_depth_Q_limitsF <- as.data.frame(t(sas_juv_depth_Q_limits[,-1])) 
sas_juv_depth_Q_limitsF <- mutate(sas_juv_depth_Q_limitsF, position = rownames(sas_juv_depth_Q_limitsF))

## filter low probability then merges limits with stats df 

## filter low probability then merges limits with stats df 

lows <- c("Low", "Low.Seasonal")

low_joiny <- select(sas_juv_depth_Q_limitsF, starts_with("Low"), position)
low_joinx <- filter(sas_juv_depth_time_ann, Probability_Threshold %in% lows ) #
low_join <- merge(low_joinx, low_joiny, by="position", all=T)
low_join <- rename(low_join, Q_limit1 = Low_Prob_1, Q_limit2 = Low_Prob_2, 
                   Q_limit3 = Low_Prob_3,  Q_limit4 = Low_Prob_4)

low_joinx

## medium
meds <- c("Medium", "Medium.Seasonal")

med_joiny <- select(sas_juv_depth_Q_limitsF, starts_with("Med"), position)
med_joinx <- filter(sas_juv_depth_time_ann, Probability_Threshold %in% meds ) #
med_join <- merge(med_joinx, med_joiny, by="position", all=T)
med_join <- rename(med_join, Q_limit1 = Med_Prob_1, Q_limit2 = Med_Prob_2, 
                   Q_limit3 = Med_Prob_3,  Q_limit4 = Med_Prob_4)

med_join

## high

highs <- c("High", "High.Seasonal")

high_joiny <- select(sas_juv_depth_Q_limitsF, starts_with("High"), position)
high_joinx <- filter(sas_juv_depth_time_ann, Probability_Threshold %in% highs) #
high_join <- merge(high_joinx, high_joiny, by="position", all=T)
high_join <- rename(high_join, Q_limit1 = High_Prob_1, Q_limit2 = High_Prob_2, 
                    Q_limit3 = High_Prob_3,  Q_limit4 = High_Prob_4)

high_join

## join together

sas_juv_depth_Q_time <- rbind(low_join, med_join, high_join)
sas_juv_depth_Q_time

####### VELOCITY
sas_ad_velocity_time_ann <- sas_ad_velocity_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Santa Ana Sucker"), Life_Stage = ("Adult/Juvenile"), Hydraulic = "Velocity") %>%
  distinct()

## add Q limits

## rownames as column, transform and add position column
rownames(sas_ad_velocity_Q_limits)<- sas_ad_velocity_Q_limits[,"X"]
sas_ad_velocity_Q_limitsF <- as.data.frame(t(sas_ad_velocity_Q_limits[,-1])) 
sas_ad_velocity_Q_limitsF <- mutate(sas_ad_velocity_Q_limitsF, position = rownames(sas_ad_velocity_Q_limitsF))

## filter low probability then merges limits with stats df 

lows <- c("Low", "Low.Seasonal")

low_joiny <- select(sas_ad_velocity_Q_limitsF, starts_with("Low"), position)
low_joinx <- filter(sas_ad_velocity_time_ann, Probability_Threshold %in% lows ) #
low_join <- merge(low_joinx, low_joiny, by="position", all=T)
low_join <- rename(low_join, Q_limit1 = Low_Prob_1, Q_limit2 = Low_Prob_2, 
                   Q_limit3 = Low_Prob_3,  Q_limit4 = Low_Prob_4)

low_joinx

## medium
meds <- c("Medium", "Medium.Seasonal")

med_joiny <- select(sas_ad_velocity_Q_limitsF, starts_with("Med"), position)
med_joinx <- filter(sas_ad_velocity_time_ann, Probability_Threshold %in% meds ) #
med_join <- merge(med_joinx, med_joiny, by="position", all=T)
med_join <- rename(med_join, Q_limit1 = Med_Prob_1, Q_limit2 = Med_Prob_2, 
                   Q_limit3 = Med_Prob_3,  Q_limit4 = Med_Prob_4)

med_join

## high

highs <- c("High", "High.Seasonal")

high_joiny <- select(sas_ad_velocity_Q_limitsF, starts_with("High"), position)
high_joinx <- filter(sas_ad_velocity_time_ann, Probability_Threshold %in% highs) #
high_join <- merge(high_joinx, high_joiny, by="position", all=T)
high_join <- rename(high_join, Q_limit1 = High_Prob_1, Q_limit2 = High_Prob_2, 
                    Q_limit3 = High_Prob_3,  Q_limit4 = High_Prob_4)

high_join

## join together

sas_ad_velocity_Q_time <- rbind(low_join, med_join, high_join)
sas_ad_velocity_Q_time
  
## TYPHA

### adult depth
typha_ad_depth_time_ann <- typha_ad_depth_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Adult"), Hydraulic = "Depth") %>%
  distinct()

## add Q limits

## rownames as column, transform and add position column
rownames(typha_ad_depth_Q_limits)<- typha_ad_depth_Q_limits[,"X"]
typha_ad_depth_Q_limitsF <- as.data.frame(t(typha_ad_depth_Q_limits[,-1])) 
typha_ad_depth_Q_limitsF <- mutate(typha_ad_depth_Q_limitsF, position = rownames(typha_ad_depth_Q_limitsF))

## filter low probability then merges limits with stats df 

lows <- c("Low", "Low.Seasonal")

low_joiny <- select(typha_ad_depth_Q_limitsF, starts_with("Low"), position)
low_joinx <- filter(sas_ad_velocity_time_ann, Probability_Threshold %in% lows ) #
low_join <- merge(low_joinx, low_joiny, by="position", all=T)
low_join <- rename(low_join, Q_limit1 = Low_Prob_1, Q_limit2 = Low_Prob_2, 
                   Q_limit3 = Low_Prob_3,  Q_limit4 = Low_Prob_4)

low_joinx

## medium
meds <- c("Medium", "Medium.Seasonal")

med_joiny <- select(typha_ad_depth_Q_limitsF, starts_with("Med"), position)
med_joinx <- filter(sas_ad_velocity_time_ann, Probability_Threshold %in% meds ) #
med_join <- merge(med_joinx, med_joiny, by="position", all=T)
med_join <- rename(med_join, Q_limit1 = Med_Prob_1, Q_limit2 = Med_Prob_2, 
                   Q_limit3 = Med_Prob_3,  Q_limit4 = Med_Prob_4)

med_join

## high

highs <- c("High", "High.Seasonal")

high_joiny <- select(typha_ad_depth_Q_limitsF, starts_with("High"), position)
high_joinx <- filter(typha_ad_depth_time_ann, Probability_Threshold %in% highs) #
high_join <- merge(high_joinx, high_joiny, by="position", all=T)
high_join <- rename(high_join, Q_limit1 = High_Prob_1, Q_limit2 = High_Prob_2, 
                    Q_limit3 = High_Prob_3,  Q_limit4 = High_Prob_4)

high_join
## join together

typha_ad_depth_Q_time <- rbind(low_join, med_join, high_join)
typha_ad_depth_Q_time

##### seedling depth
typha_seed_depth_time_ann <- typha_seed_depth_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Seedling"), Hydraulic = "Depth") %>%
  distinct()

## add Q limits

## rownames as column, transform and add position column
rownames(typha_seed_depth_Q_limits)<- typha_seed_depth_Q_limits[,"X"]
typha_seed_depth_Q_limitsF <- as.data.frame(t(typha_seed_depth_Q_limits[,-1])) 
typha_seed_depth_Q_limitsF <- mutate(typha_seed_depth_Q_limitsF, position = rownames(typha_seed_depth_Q_limitsF))

## filter low probability then merges limits with stats df 
## filter low probability then merges limits with stats df 

lows <- c("Low", "Low.Seasonal")

low_joiny <- select(typha_seed_depth_Q_limitsF, starts_with("Low"), position)
low_joinx <- filter(typha_seed_depth_time_ann, Probability_Threshold %in% lows ) #
low_join <- merge(low_joinx, low_joiny, by="position", all=T)
low_join <- rename(low_join, Q_limit1 = Low_Prob_1, Q_limit2 = Low_Prob_2, 
                   Q_limit3 = Low_Prob_3,  Q_limit4 = Low_Prob_4)

low_joinx

## medium
meds <- c("Medium", "Medium.Seasonal")

med_joiny <- select(typha_seed_depth_Q_limitsF, starts_with("Med"), position)
med_joinx <- filter(typha_seed_depth_time_ann, Probability_Threshold %in% meds ) #
med_join <- merge(med_joinx, med_joiny, by="position", all=T)
med_join <- rename(med_join, Q_limit1 = Med_Prob_1, Q_limit2 = Med_Prob_2, 
                   Q_limit3 = Med_Prob_3,  Q_limit4 = Med_Prob_4)

med_join

## high

highs <- c("High", "High.Seasonal")

high_joiny <- select(typha_seed_depth_Q_limitsF, starts_with("High"), position)
high_joinx <- filter(typha_seed_depth_time_ann, Probability_Threshold %in% highs) #
high_join <- merge(high_joinx, high_joiny, by="position", all=T)
high_join <- rename(high_join, Q_limit1 = High_Prob_1, Q_limit2 = High_Prob_2, 
                    Q_limit3 = High_Prob_3,  Q_limit4 = High_Prob_4)

high_join
## join together

typha_seed_depth_Q_time <- rbind(low_join, med_join, high_join)
typha_seed_depth_Q_time

### ad velocity
typha_ad_velocity_time_ann <- typha_ad_velocity_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Seedling"), Hydraulic = "Velocity") %>%
  distinct()

## add Q limits

## rownames as column, transform and add position column
rownames(typha_ad_velocity_Q_limits)<- typha_ad_velocity_Q_limits[,"X"]
typha_ad_velocity_Q_limitsF <- as.data.frame(t(typha_ad_velocity_Q_limits[,-1])) 
typha_ad_velocity_Q_limitsF <- mutate(typha_ad_velocity_Q_limitsF, position = rownames(typha_ad_velocity_Q_limitsF))

## filter low probability then merges limits with stats df 

lows <- c("Low", "Low.Seasonal")

low_joiny <- select(typha_ad_velocity_Q_limitsF, starts_with("Low"), position)
low_joinx <- filter(typha_ad_velocity_time_ann, Probability_Threshold %in% lows ) #
low_join <- merge(low_joinx, low_joiny, by="position", all=T)
low_join <- rename(low_join, Q_limit1 = Low_Prob_1, Q_limit2 = Low_Prob_2, 
                   Q_limit3 = Low_Prob_3,  Q_limit4 = Low_Prob_4)

low_joinx

## medium
meds <- c("Medium", "Medium.Seasonal")

med_joiny <- select(typha_ad_velocity_Q_limitsF, starts_with("Med"), position)
med_joinx <- filter(typha_ad_velocity_time_ann, Probability_Threshold %in% meds ) #
med_join <- merge(med_joinx, med_joiny, by="position", all=T)
med_join <- rename(med_join, Q_limit1 = Med_Prob_1, Q_limit2 = Med_Prob_2, 
                   Q_limit3 = Med_Prob_3,  Q_limit4 = Med_Prob_4)

med_join

## high

highs <- c("High", "High.Seasonal")

high_joiny <- select(typha_ad_velocity_Q_limitsF, starts_with("High"), position)
high_joinx <- filter(typha_ad_velocity_time_ann, Probability_Threshold %in% highs) #
high_join <- merge(high_joinx, high_joiny, by="position", all=T)
high_join <- rename(high_join, Q_limit1 = High_Prob_1, Q_limit2 = High_Prob_2, 
                    Q_limit3 = High_Prob_3,  Q_limit4 = High_Prob_4)

high_join
## join together

typha_ad_velocity_Q_time <- rbind(low_join, med_join, high_join)
typha_ad_velocity_Q_time

#### MIGRATION
names(mig_in_depth_time_ann)

######## ADULT (IN) Depth
mig_in_depth_time_ann <- mig_in_depth_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = Time_Period, TimePercentage = value) %>%
  mutate(Species = paste ("Steelhead"), Life_Stage = ("Migration (IN)"), Hydraulic = "Depth", Probability_Threshold = ">18cm") %>%
  distinct() %>%
  select(year, TimePeriod, position, Probability_Threshold, TimePercentage, Species, Life_Stage, Hydraulic)

mig_in_depth_time_ann
## add Q limits

## rownames as column, transform and add position column
rownames(mig_in_depth_Q_limits)<- mig_in_depth_Q_limits[,"X"]
mig_in_depth_Q_limitsF <- as.data.frame(t(mig_in_depth_Q_limits[,-1])) 
mig_in_depth_Q_limitsF <- mutate(mig_in_depth_Q_limitsF, position = rownames(mig_in_depth_Q_limitsF))

## filter low probability then merges limits with stats df 
mig_in_depth_Q_limitsF
low_joiny <- select(mig_in_depth_Q_limitsF, Low_Prob_1, position)
low_join <- merge(mig_in_depth_time_ann, low_joiny, by="position", all=T)

mig_in_depth_Q_time <- low_join %>% mutate( Q_limit2 = NA, Q_limit3 = NA, 
                                            Q_limit4 = NA, Probability_Threshold="Critical_Limit") %>%
  rename(Q_limit1 = Low_Prob_1)

mig_in_depth_Q_time


######## ADULT (IN) Velocity
mig_in_velocity_time_ann <- mig_in_velocity_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = Time_Period, TimePercentage = value) %>%
  mutate(Species = paste ("Steelhead"), Life_Stage = ("Migration (IN)"), Hydraulic = "Velocity", Probability_Threshold = "<3.1m/s") %>%
  distinct()%>%
  select(year, TimePeriod, position, Probability_Threshold, TimePercentage, Species, Life_Stage, Hydraulic)

## rownames as column, transform and add position column
rownames(mig_in_velocity_Q_limits)<- mig_in_velocity_Q_limits[,"X"]
mig_in_velocity_Q_limitsF <- as.data.frame(t(mig_in_velocity_Q_limits[,-1])) 
mig_in_velocity_Q_limitsF <- mutate(mig_in_velocity_Q_limitsF, position = rownames(mig_in_velocity_Q_limitsF))

## filter low probability then merges limits with stats df 
mig_in_velocity_Q_limitsF
low_joiny <- select(mig_in_velocity_Q_limitsF, Low_Prob_1, position)
low_join <- merge(mig_in_velocity_time_ann, low_joiny, by="position", all=T)

mig_in_velocity_Q_time <- low_join %>% mutate( Q_limit2 = NA, Q_limit3 = NA, 
                                 Q_limit4 = NA, Probability_Threshold="Critical_Limit") %>%
                            rename(Q_limit1 = Low_Prob_1)
  

mig_in_velocity_Q_time

all_time <- rbind(sas_ad_depth_Q_time, sas_juv_depth_Q_time, sas_ad_velocity_Q_time, 
                  typha_ad_depth_Q_time, typha_seed_depth_Q_time, typha_ad_velocity_Q_time, 
                  mig_in_depth_Q_time, mig_in_velocity_Q_time)


### format all data

head(all_time) 
unique(all_time$Probability_Threshold)

all_time <- all_time %>%
  mutate(Node = "LA11")

# all_time$TimePeriod <- ifelse(all_time$Probability_Threshold == ann_stats, "Annual", all_time$TimePeriod)
# all_data$Suitability <- ifelse(all_data$TimePercentage >= 75, "High", NA)
# all_data$Suitability <- ifelse(all_data$TimePercentage >= 75, "High", NA)
write.csv(all_time, "results/all_time_stats_LA11.csv") 

# number of days ----------------------------------------------------------

## time stats

## santa Ana Sucker
sas_ad_depth_days <- read.csv("results/F1_LA11_total_days_long.csv")
sas_juv_depth_days <- read.csv("results/F1_LA11_juvenile_total_days_long.csv")
sas_ad_velocity_days <- read.csv("results/F2_LA11_adult_velocity_total_days_long.csv")

# ## mrgation
# 
mig_in_depth_days <- read.csv("results/F5_LA11_migration_depth_total_days_long.csv")
mig_in_velocity_days <- read.csv("results/F5_LA11_migration_in_velocity_total_days_long.csv")

## typha

typha_ad_depth_days <- read.csv("results/M1_LA11_typha_adult_patch_depth_total_days_long.csv")
typha_seed_depth_days <- read.csv("results/M3_LA11_typha_seedling_depth_total_days_long.csv")
typha_seed_velocity_days <- read.csv("results/M2_LA11_typha_adult_patch_velocity_total_days_long.csv")

head(sas_ad_depth_days)
head(sas_juv_depth_days)
head(typha_ad_depth_days)

ann_stats <- unique(sas_ad_depth_time$Probability_Threshold)[1:3]

crit_stats <- unique(sas_ad_depth_time$Probability_Threshold)[4:6]

## SAS
head(sas_ad_depth_days)
######## ADULT
sas_ad_depth_days_ann <- sas_ad_depth_days %>% 
  select(-X, -month_year) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
  mutate(Species = paste ("Santa Ana Sucker"), Life_Stage = ("Adult"), Hydraulic = "Depth") %>%
  distinct()

head(sas_ad_depth_days_ann)

####### JUVENILE
sas_juv_depth_days_ann <- sas_juv_depth_days %>% 
  select(-X, -month_year) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
  mutate(Species = paste ("Santa Ana Sucker"), Life_Stage = ("Juvenile"), Hydraulic = "Depth") %>%
  distinct()

head(sas_juv_depth_days_ann)

####### VELOCITY
sas_ad_velocity_days_ann <- sas_ad_velocity_days %>% 
  select(-X, -month_year) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
  mutate(Species = paste ("Santa Ana Sucker"), Life_Stage = ("Adult/Juvenile"), Hydraulic = "Velocity") %>%
  distinct()

head(sas_ad_velocity_days_ann)

all_days <- rbind(sas_ad_velocity_days_ann, sas_juv_depth_days_ann, sas_ad_depth_days_ann)

write.csv(all_days, "results/all_days_stats_LA11.csv")

## TYPHA

### adult depth
typha_ad_depth_days_ann <- typha_ad_depth_days %>% 
  select(-X, -month_year) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Adult"), Hydraulic = "Depth") %>%
  distinct()

head(typha_ad_depth_days_ann)

##### seedling depth
typha_seed_depth_days_ann <- typha_seed_depth_days %>% 
  select(-X, -month_year) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Seedling"), Hydraulic = "Depth") %>%
  distinct()

### seedling velocity
typha_seed_velocity_days_ann <- typha_seed_velocity_days %>% 
  select(-X, -month_year) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Seedling"), Hydraulic = "Velocity") %>%
  distinct()

names(typha_seed_velocity_days_ann)
#### MIGRATION
head(mig_in_depth_days)
names(mig_in_depth_days_ann)

######## ADULT (IN) Depth
mig_in_depth_days_ann <- mig_in_depth_days %>% 
  select(-X, -month_year) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
  mutate(Species = paste ("Steelhead"), Life_Stage = ("Migration (IN)"), Hydraulic = "Depth", Probability_Threshold = ">18cm") %>%
  distinct() %>%
  select(year, month, TimePeriod, position, Probability_Threshold, DaysPerMonth, Species, Life_Stage, Hydraulic)

######## ADULT (IN) Velocity
mig_in_velocity_days_ann <- mig_in_velocity_days %>% 
  select(-X, -month_year) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
  mutate(Species = paste ("Steelhead"), Life_Stage = ("Migration (IN)"), Hydraulic = "Velocity", Probability_Threshold = "<3.1m/s") %>%
  distinct()%>%
  select(year, month, TimePeriod, position, Probability_Threshold, DaysPerMonth, Species, Life_Stage, Hydraulic)

names(mig_in_velocity_days_ann)

all_days <- rbind(sas_ad_depth_days_ann, sas_juv_depth_days_ann, sas_ad_velocity_days_ann, typha_ad_depth_days_ann, 
                  typha_seed_depth_days_ann, typha_seed_velocity_days_ann, mig_in_depth_days_ann, mig_in_velocity_days_ann)


### format all data

head(all_days) 
## add node
all_days <- all_days %>%
  mutate(Node = "LA11")

write.csv(all_days, "results/all_days_stats_LA11.csv") 

# all_data$TimePeriod <- ifelse(all_data$Probability_Threshold == ann_stats, "Annual", all_data$TimePeriod)

names(all_days)
names(all_time)

## add limits
