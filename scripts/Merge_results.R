### make big file with time results

## upload data for each species

## time stats

## santa Ana Sucker
sas_ad_depth_time <- read.csv("results/F1_LA11_adult_depth_time_stats.csv")
sas_juv_depth_time <- read.csv("results/F1_LA11_juvenile_depth_time_stats.csv")
sas_ad_velocity_time <- read.csv("results/F2_LA11_adult_velocity_time_stats.csv")

## mrgation

mig_in_depth_time <- read.csv("results/F5_LA11_SH_migration_depth_time_stats.csv")
mig_in_velocity_time <- read.csv("results/F5_LA11_SH_migration_velocity_time_stats.csv")

## typha

typha_ad_depth_time <- read.csv("results/M1_LA11_typha_adult_patch_depth_time_stats.csv")
typha_seed_depth_time <- read.csv("results/M3_LA11_typha_seedling_depth_time_stats.csv")
typha_seed_velocity_time <- read.csv("results/M2_LA11_typha_adult_patch_velocity_time_stats.csv")

head(sas_ad_depth_time)
head(sas_juv_depth_time)
head(typha_ad_depth_time)

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

sas_ad_depth_time_ann

####### JUVENILE
sas_juv_depth_time_ann <- sas_juv_depth_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Santa Ana Sucker"), Life_Stage = ("Juvenile"), Hydraulic = "Depth") %>%
  distinct()

sas_juv_depth_time_ann

####### VELOCITY
sas_ad_velocity_time_ann <- sas_ad_velocity_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Santa Ana Sucker"), Life_Stage = ("Adult/Juvenile"), Hydraulic = "Velocity") %>%
  distinct()

  
## TYPHA

### adult depth
typha_ad_depth_time_ann <- typha_ad_depth_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Adult"), Hydraulic = "Depth") %>%
  distinct()

##### seedling depth
typha_seed_depth_time_ann <- typha_seed_depth_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Seedling"), Hydraulic = "Depth") %>%
  distinct()

### seedling velocity
typha_seed_velocity_time_ann <- typha_seed_velocity_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Seedling"), Hydraulic = "Velocity") %>%
  distinct()

names(typha_seed_velocity_time_ann)
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

######## ADULT (IN) Velocity
mig_in_velocity_time_ann <- mig_in_velocity_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = Time_Period, TimePercentage = value) %>%
  mutate(Species = paste ("Steelhead"), Life_Stage = ("Migration (IN)"), Hydraulic = "Velocity", Probability_Threshold = "<3.1m/s") %>%
  distinct()%>%
  select(year, TimePeriod, position, Probability_Threshold, TimePercentage, Species, Life_Stage, Hydraulic)

names(mig_in_velocity_time_ann)

all_data <- rbind(sas_ad_depth_time_ann, sas_juv_depth_time_ann, sas_ad_velocity_time_ann, typha_ad_depth_time_ann, 
                  typha_seed_depth_time_ann, typha_seed_velocity_time_ann, mig_in_depth_time_ann, mig_in_velocity_time_ann)


### format all data

head(all_data) 

all_data <- all_data %>%
  mutate(Node = "LA11")

all_data$TimePeriod <- ifelse(all_data$Probability_Threshold == ann_stats, "Annual", all_data$TimePeriod)
# all_data$Suitability <- ifelse(all_data$TimePercentage >= 75, "High", NA)
# all_data$Suitability <- ifelse(all_data$TimePercentage >= 75, "High", NA)
write.csv(all_data, "results/all_time_stats_LA11.csv") 

# number of days ----------------------------------------------------------

## time stats

## santa Ana Sucker
sas_ad_depth_days <- read.csv("results/F1_LA11_total_days_long.csv")
sas_juv_depth_days <- read.csv("results/F1_LA11_juvenile_total_days_long.csv")
sas_ad_velocity_days <- read.csv("results/F2_LA11_adult_velocity_total_days_long.csv")

# ## mrgation
# 
# mig_in_depth_time <- read.csv("results/F5_LA11_SH_migration_depth_time_stats.csv")
# mig_in_velocity_time <- read.csv("results/F5_LA11_SH_migration_velocity_time_stats.csv")
# 
# ## typha
# 
# typha_ad_depth_time <- read.csv("results/M1_LA11_typha_adult_patch_depth_time_stats.csv")
# typha_seed_depth_time <- read.csv("results/M3_LA11_typha_seedling_depth_time_stats.csv")
# typha_seed_velocity_time <- read.csv("results/M2_LA11_typha_adult_patch_velocity_time_stats.csv")
# 
# head(sas_ad_depth_time)
# head(sas_juv_depth_time)
# head(typha_ad_depth_time)

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

sas_ad_depth_days_ann

####### JUVENILE
sas_juv_depth_days_ann <- sas_juv_depth_days %>% 
  select(-X, -month_year) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
  mutate(Species = paste ("Santa Ana Sucker"), Life_Stage = ("Juvenile"), Hydraulic = "Depth") %>%
  distinct()

sas_juv_depth_time_ann

####### VELOCITY
sas_ad_velocity_days_ann <- sas_ad_velocity_days %>% 
  select(-X, -month_year) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
  mutate(Species = paste ("Santa Ana Sucker"), Life_Stage = ("Adult/Juvenile"), Hydraulic = "Velocity") %>%
  distinct()



all_days <- rbind(sas_ad_velocity_days_ann, sas_juv_depth_days_ann, sas_ad_depth_days_ann)

write.csv(all_days, "results/all_days_stats_LA11.csv")

## TYPHA

### adult depth
typha_ad_depth_time_ann <- typha_ad_depth_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Adult"), Hydraulic = "Depth") %>%
  distinct()

##### seedling depth
typha_seed_depth_time_ann <- typha_seed_depth_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Seedling"), Hydraulic = "Depth") %>%
  distinct()

### seedling velocity
typha_seed_velocity_time_ann <- typha_seed_velocity_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = season, TimePercentage = value) %>%
  mutate(Species = paste ("Typha Spp"), Life_Stage = ("Seedling"), Hydraulic = "Velocity") %>%
  distinct()

names(typha_seed_velocity_time_ann)
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

######## ADULT (IN) Velocity
mig_in_velocity_time_ann <- mig_in_velocity_time %>% 
  select(-X) %>%
  # filter(Probability_Threshold == ann_stats) %>%
  rename(TimePeriod = Time_Period, TimePercentage = value) %>%
  mutate(Species = paste ("Steelhead"), Life_Stage = ("Migration (IN)"), Hydraulic = "Velocity", Probability_Threshold = "<3.1m/s") %>%
  distinct()%>%
  select(year, TimePeriod, position, Probability_Threshold, TimePercentage, Species, Life_Stage, Hydraulic)

names(mig_in_velocity_time_ann)

all_data <- rbind(sas_ad_depth_time_ann, sas_juv_depth_time_ann, sas_ad_velocity_time_ann, typha_ad_depth_time_ann, 
                  typha_seed_depth_time_ann, typha_seed_velocity_time_ann, mig_in_depth_time_ann, mig_in_velocity_time_ann)


### format all data

head(all_data) 

all_data <- all_data %>%
  mutate(Node = "LA11")

all_data$TimePeriod <- ifelse(all_data$Probability_Threshold == ann_stats, "Annual", all_data$TimePeriod)

