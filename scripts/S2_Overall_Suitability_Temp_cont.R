library(tidyverse)
library(tidyr)


setwd("/Users/katieirving/Documents/git/flow_eco_mech/results")

## format dfs
ts <- list.files( pattern="suitability")
length(ts) ## 21
ts <- ts[-c(1,23)]
ts
dfx <- NULL
s=1

for(s in 1:length(ts)) {
  
  df <- read.csv(file=paste(ts[s], sep=""))
  
  
  df <- df %>%
    select(Node, Metric, Species, Life_Stage, SuitabilityClass)
  
  dfx <- rbind(dfx, df)
  
}
dfx

dfx <- dfx %>%
  pivot_wider(names_from = "Metric", values_from = SuitabilityClass)

write.csv(dfx, "S2_all_species_temp_suitability.csv")
