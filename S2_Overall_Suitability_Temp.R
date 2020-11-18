### overall suitability - Temp

library(tidyverse)
library(tidyr)
getwd()
setwd("/Users/katieirving/Documents/git/flow_eco_mech/output_data")
tp <- list.files(pattern="T1")
tp

SASAd <- read.csv(paste(tp[2]))
SASJuv <- read.csv(paste(tp[4]))

SASAd
SASJuv


# Juvenile ----------------------------------------------------------------


SASJuvMean <- SASJuv %>% 
  filter(Metric == "MeanTemp") %>%
  pivot_longer(cols = Low:High, names_to = "Probability_Threshold", values_to = "WeeksAboveThresh") %>%
  mutate(PercentAboveThresh = WeeksAboveThresh/No_of_Weeks*100 ) #%>%
  # pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh)

SASJuvMean <- SASJuvMean %>%
  select(-WeeksAboveThresh) %>%
  pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh) %>%
  mutate(SuitabilityClass = NA) 
  

probs <- seq(1, dim(SASJuvMean)[1], 1)  


for(p in 1: length(probs)) {

  SASJuvMean$SuitabilityClass[p] = if(SASJuvMean$High[p] >= 75) {
    paste("High")
  } else  if(SASJuvMean$Med[p] >= 25 & SASJuvMean$Med[p] <= 75 ){
    paste("Partial")
  } else  if(SASJuvMean$Low[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }

}

# SASJuvMean <- SASJuvMean %>%
#   mutate(Species = "SAS", LifeStage = "Juvenile")
SASJuvMean

write.csv(SASJuvMean, "/Users/katieirving/Documents/git/flow_eco_mech/results/S2_SAS_Juvenile_MeanTemp_suitability.csv")
SASJuv

SASJuvMax <- SASJuv %>% 
  filter(Metric == "MaxTemp") %>%
  pivot_longer(cols = Low:High, names_to = "Probability_Threshold", values_to = "WeeksAboveThresh") %>%
  mutate(PercentAboveThresh = WeeksAboveThresh/No_of_Weeks*100 ) #%>%
# pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh)

SASJuvMax <- SASJuvMax %>%
  select(-WeeksAboveThresh) %>%
  pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh) %>%
  mutate(SuitabilityClass = NA) 


probs <- seq(1, dim(SASJuvMean)[1], 1)  


for(p in 1: length(probs)) {
  
  SASJuvMax$SuitabilityClass[p] = if(SASJuvMax$High[p] >= 75) {
    paste("High")
  } else  if(SASJuvMax$Med[p] >= 25 & SASJuvMax$Med[p] <= 75 ){
    paste("Partial")
  } else  if(SASJuvMax$Low[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}

# SASJuvMean <- SASJuvMean %>%
#   mutate(Species = "SAS", LifeStage = "Juvenile")

SASJuvMax
write.csv(SASJuvMax, "/Users/katieirving/Documents/git/flow_eco_mech/results/S2_SAS_Juvenile_MaxTemp_suitability.csv")

SASJuvMin <- SASJuv %>% 
  filter(Metric == "MinTemp") %>%
  pivot_longer(cols = Low:High, names_to = "Probability_Threshold", values_to = "WeeksAboveThresh") %>%
  mutate(PercentAboveThresh = WeeksAboveThresh/No_of_Weeks*100 ) #%>%
# pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh)

SASJuvMin <- SASJuvMin %>%
  select(-WeeksAboveThresh) %>%
  pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh) %>%
  mutate(SuitabilityClass = NA) 


probs <- seq(1, dim(SASJuvMean)[1], 1)  


for(p in 1: length(probs)) {
  
  SASJuvMin$SuitabilityClass[p] = if(SASJuvMin$High[p] >= 75) {
    paste("High")
  } else  if(SASJuvMin$Med[p] >= 25 & SASJuvMin$Med[p] <= 75 ){
    paste("Partial")
  } else  if(SASJuvMin$Low[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}

# SASJuvMean <- SASJuvMean %>%
#   mutate(Species = "SAS", LifeStage = "Juvenile")

SASJuvMin
write.csv(SASJuvMin, "/Users/katieirving/Documents/git/flow_eco_mech/results/S2_SAS_Juvenile_MinTemp_suitability.csv")


# Adult -------------------------------------------------------------------

SASAdMean <- SASAd %>% 
  filter(Metric == "MeanTemp") %>%
  pivot_longer(cols = Low:High, names_to = "Probability_Threshold", values_to = "WeeksAboveThresh") %>%
  mutate(PercentAboveThresh = WeeksAboveThresh/No_of_Weeks*100 ) #%>%
# pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh)

SASAdMean <- SASAdMean %>%
  select(-WeeksAboveThresh) %>%
  pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh) %>%
  mutate(SuitabilityClass = NA) 


probs <- seq(1, dim(SASAdMean)[1], 1)  


for(p in 1: length(probs)) {
  
  SASAdMean$SuitabilityClass[p] = if(SASAdMean$High[p] >= 75) {
    paste("High")
  } else  if(SASAdMean$Med[p] >= 25 & SASAdMean$Med[p] <= 75 ){
    paste("Partial")
  } else  if(SASAdMean$Low[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}

# SASAdMean <- SASAdMean %>%
#   mutate(Species = "SAS", LifeStage = "Adenile")
SASAdMean

write.csv(SASAdMean, "/Users/katieirving/Documents/git/flow_eco_mech/results/S2_SAS_Adult_MeanTemp_suitability.csv")
SASAd

SASAdMax <- SASAd %>% 
  filter(Metric == "MaxTemp") %>%
  pivot_longer(cols = Low:High, names_to = "Probability_Threshold", values_to = "WeeksAboveThresh") %>%
  mutate(PercentAboveThresh = WeeksAboveThresh/No_of_Weeks*100 ) #%>%
# pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh)

SASAdMax <- SASAdMax %>%
  select(-WeeksAboveThresh) %>%
  pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh) %>%
  mutate(SuitabilityClass = NA) 


probs <- seq(1, dim(SASAdMean)[1], 1)  


for(p in 1: length(probs)) {
  
  SASAdMax$SuitabilityClass[p] = if(SASAdMax$High[p] >= 75) {
    paste("High")
  } else  if(SASAdMax$Med[p] >= 25 & SASAdMax$Med[p] <= 75 ){
    paste("Partial")
  } else  if(SASAdMax$Low[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}

# SASAdMean <- SASAdMean %>%
#   mutate(Species = "SAS", LifeStage = "Adenile")
SASAdMax

write.csv(SASAdMax, "/Users/katieirving/Documents/git/flow_eco_mech/results/S2_SAS_Adult_MaxTemp_suitability.csv")

SASAdMin <- SASAd %>% 
  filter(Metric == "MinTemp") %>%
  pivot_longer(cols = Low:High, names_to = "Probability_Threshold", values_to = "WeeksAboveThresh") %>%
  mutate(PercentAboveThresh = WeeksAboveThresh/No_of_Weeks*100 ) #%>%
# pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh)

SASAdMin <- SASAdMin %>%
  select(-WeeksAboveThresh) %>%
  pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh) %>%
  mutate(SuitabilityClass = NA) 


probs <- seq(1, dim(SASAdMean)[1], 1)  


for(p in 1: length(probs)) {
  
  SASAdMin$SuitabilityClass[p] = if(SASAdMin$High[p] >= 75) {
    paste("High")
  } else  if(SASAdMin$Med[p] >= 25 & SASAdMin$Med[p] <= 75 ){
    paste("Partial")
  } else  if(SASAdMin$Low[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}

# SASAdMean <- SASAdMean %>%
#   mutate(Species = "SAS", LifeStage = "Adenile")
SASAdMin

write.csv(SASAdMin, "/Users/katieirving/Documents/git/flow_eco_mech/results/S2_SAS_Adult_MinTemp_suitability.csv")


# Others (all with thresholds) ---------------------------------------------------------------------
tp
tp <- tp[-c(2,4)]
s = 2
NoWeeks <- SASJuv #
NoWeeks$Node_Metric <- paste(NoWeeks$Node, "_", NoWeeks$Metric, sep="")

NoWeeks <- NoWeeks %>%
  select(Node_Metric, No_of_Weeks)# %>%

for(s in 1:length(tp)) {
  
  SASAd <- read.csv(paste(tp[s]))
  SASAd$Node_Metric <- paste(SASAd$Node, "_", SASAd$Metric, sep="")
  
  SASAd <- left_join(SASAd, NoWeeks, by = "Node_Metric" )
  
  Species <- SASAd$Species[1]
  LifeStage <- SASAd$Life_Stage[1]
  
  SASAdMean <- SASAd %>% 
    filter(Metric == "MeanTemp") %>%
    pivot_longer(cols = Suitable:Unsuitable, names_to = "Probability_Threshold", values_to = "WeeksAboveThresh") %>%
    mutate(PercentAboveThresh = WeeksAboveThresh/No_of_Weeks*100 ) #%>%
  # pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh)
  
  SASAdMean <- SASAdMean %>%
    select(-WeeksAboveThresh) %>%
    pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh) %>%
    mutate(SuitabilityClass = NA) 
  
  probs <- seq(1, dim(SASAdMean)[1], 1)  
  
  
  for(p in 1: length(probs)) {
    
    SASAdMean$SuitabilityClass[p] = if(SASAdMean$Suitable[p] >= 75) {
      paste("High")
    } else  if(SASAdMean$Suitable[p] < 25){
      paste("Low")
    } else {
      paste("Partial")
    }
    
  }
  
  SASAdMean
  write.csv(SASAdMean, paste("/Users/katieirving/Documents/git/flow_eco_mech/results/S2_", Species, LifeStage, "_MeanTemp_suitability.csv", sep=""))
  
  
  SASAdMax <- SASAd %>% 
    filter(Metric == "MaxTemp") %>%
    pivot_longer(cols = Suitable:Unsuitable, names_to = "Probability_Threshold", values_to = "WeeksAboveThresh") %>%
    mutate(PercentAboveThresh = WeeksAboveThresh/No_of_Weeks*100 ) #%>%
  #pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh)
  
  SASAdMax <- SASAdMax %>%
    select(-WeeksAboveThresh) %>%
    pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh) %>%
    mutate(SuitabilityClass = NA) 
  
  
  probs <- seq(1, dim(SASAdMax)[1], 1)  
  
  
  for(p in 1: length(probs)) {
    
    SASAdMax$SuitabilityClass[p] = if(SASAdMax$Suitable[p] >= 75) {
      paste("High")
    } else  if(SASAdMax$Suitable[p] < 25){
      paste("Low")
    } else {
      paste("Partial")
    }
    
  }
  SASAdMax
  
  write.csv(SASAdMax, paste("/Users/katieirving/Documents/git/flow_eco_mech/results/S2_", Species, LifeStage, "_MaxTemp_suitability.csv", sep=""))
  
  SASAdMin <- SASAd %>% 
    filter(Metric == "MinTemp") %>%
    pivot_longer(cols = Suitable:Unsuitable, names_to = "Probability_Threshold", values_to = "WeeksAboveThresh") %>%
    mutate(PercentAboveThresh = WeeksAboveThresh/No_of_Weeks*100 ) #%>%
  # pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh)
  
  SASAdMin <- SASAdMin %>%
    select(-WeeksAboveThresh) %>%
    pivot_wider(names_from = Probability_Threshold, values_from = PercentAboveThresh) %>%
    mutate(SuitabilityClass = NA) 
  
  
  probs <- seq(1, dim(SASAdMin)[1], 1)  
  
  
  for(p in 1: length(probs)) {
    
    SASAdMin$SuitabilityClass[p] = if(SASAdMin$Suitable[p] >= 75) {
      paste("High")
    } else  if(SASAdMin$Suitable[p] < 25){
      paste("Low")
    } else {
      paste("Partial")
    }
    
  }
  
  write.csv(SASAdMin, paste("/Users/katieirving/Documents/git/flow_eco_mech/results/S2_", Species, "_",LifeStage, "_MinTemp_suitability.csv", sep=""))
  
  
  
}



#  calculate overall suitbaility ------------------------------------------
getwd()




