### combine all results and condense

library(tidyverse)
library(tidyr)

## upload data

setwd("/Users/katieirving/Documents/git/flow_eco_mech/results")

## time stats
ts <- list.files(pattern="time_stats")
length(ts) ## 19
ts

## dataframes
time_statsx <- NULL
SuitClassOverYearsx <- NULL

## probs needed
probs <- c("Low", "Low.Seasonal", "High", "High.Seasonal")
probs

## just for ones with prob threshold
ts_con <- ts[c(2,4,5,6,9,14,15,16,19,20)]
ts_cat <- ts[-c(2,4,5,6,9,14,15,16,19,20)]

## create DF of nodes to merge
time_stats <- read.csv(file=paste(ts_con[1], sep=""))
head(time_stats)

suit_df <- time_stats %>%
  mutate(pos_code = paste(Node, "_", position, sep="")) %>%
  select(pos_code) %>%
  distinct()

suit_df

for (j in 1:length(ts_con)) {
  
  time_stats <- read.csv(file=paste(ts_con[j], sep=""))
  head(time_stats)
  
  suit_df <- time_stats %>%
    mutate(pos_code = paste(Node, "_", position, sep="")) %>%
    select(pos_code) %>%
    distinct()
  
  time_stats <- time_stats %>%
    filter(Probability_Threshold %in% probs, TimePeriod !="non_critical", Suitability_Class != "Partial") %>%
    select(Node, bottom_type, Species, Life_Stage, Hydraulic, water_year,WYT, TimePeriod, position,
           Probability_Threshold, Suitability_Class) %>%
    mutate(pos_code = paste(Node, "_", position, sep=""), suit_code = paste(Probability_Threshold, "_", Suitability_Class, sep="")) %>%
  distinct()
  

  SuitClassOverYears <- time_stats %>%
    group_by(Node, Species, Life_Stage, Hydraulic, TimePeriod, position, pos_code, suit_code) %>%
    summarise(Overall_Class = tail(names(sort(table(Suitability_Class))), 1))
  names(SuitClassOverYears)

  SuitClassOverYearsx <- rbind(SuitClassOverYearsx, SuitClassOverYears)
  
}

## merge together

full_df <- merge(suit_df, SuitClassOverYearsx, by="pos_code", all=T)
head(full_df)

### if one hydraulic varibale per species and per position is low/partial, position is low

full_df <- full_df %>%
  mutate(sp_code = paste(Species, "_", Life_Stage, sep=""))
head(full_df)

### remove columns and duplicates

dfad <- full_df %>%
  filter(Life_Stage == "Adult" & TimePeriod =="Annual")

dfjuv <- full_df %>%
  filter(Life_Stage != "Adult" & TimePeriod =="critical")
dfad
dfjuv

full_df <- rbind(dfad, dfjuv)



## subset by speices, then pos_code, then TimePeriod
pos <- unique(full_df$pos_code)
species <- unique(full_df$sp_code)
TP <- unique(full_df$TimePeriod)
p=1
s=1
t=1
tp_dfx <- NULL
tp_df


for(p in 1: length(pos)) {
  
  pos_df <- full_df %>%
    filter(pos_code == pos[p])
  
  species <- unique(pos_df$sp_code)
  
  for(s in 1:length(species)) {
    
    sp_df <- pos_df %>%
      filter(sp_code == species[s])
    
    TP <- unique(sp_df$TimePeriod)
    
    for(t in 1: length(TP)) {
      
      tp_df <- sp_df %>%
        filter(TimePeriod == TP[t])
      
      tp_df$Check_Class <- if(length(unique(tp_df$Hydraulic)) == 1) {
        paste(tp_df$Overall_Class[1])
      } else if (length(unique(tp_df$Overall_Class)) >1) {
        paste("low")
      } else (
        paste("High")
      )
      
      tp_dfx <- rbind(tp_dfx, tp_df)
      
      
      
    }
  }
}
tp_dfx

### if one position is high then node is high
## subset by species, then time period, then node
node <- unique(tp_dfx$Node)
species <- unique(tp_dfx$sp_code)
TP <- unique(tp_dfx$TimePeriod)
p=1
s=1
t=1

node_dfx <- NULL


for(s in 1: length(species)) {
  
  sp_df <- tp_dfx %>%
    filter(sp_code == species[s])
  
  
  TP <- unique(sp_df$TimePeriod)
  
  for(t in 1:length(TP)) {
    
    tp_df <- sp_df %>%
      filter(TimePeriod == TP[t])
    tp_df
    nd <- unique(tp_df$Node)
    nd[p]
    for(p in 1: length(nd)) {
      
      node_df <- tp_df %>%
        filter(Node == nd[p])
      node_df
      
      node_df$Check_Class2 <- if(length(node_df$Check_Class) > 3) {
        if("High" %in%  node_df$Check_Class) {
          paste("High")
        } else {
          paste("Low")
        }
      } else {
        if("High" %in%  node_df$Check_Class) {
          paste("High")
        } else {
          paste("Partial")
        }
      }
        
  
      node_dfx <- rbind(node_dfx, node_df)
      
      
      
    }
  }
}

head(node_dfx)

df <- node_dfx %>%
  select(Node, sp_code, Check_Class2, TimePeriod) %>%
  distinct()

df

write.csv(df, "all_low_high_nodes.csv")

### make wide

?pivot_wider

df_wide <- df %>%
  pivot_wider(names_from = sp_code, values_from = Check_Class2)

df_wide


## merge with all nodes
time_stats <- read.csv(file=paste(ts_con[1], sep=""))
head(time_stats)

suit_df <- time_stats %>%
  select(Node) %>%
  distinct()

suit_df

suit_dfx <- merge(suit_df, df_wide, by="Node", all=T)
# df_wide[is.na(df_wide)] <- "Partial"

suit_dfx <- suit_dfx[order(suit_dfx$TimePeriod),] 


write.csv(suit_dfx, "condensed_suits3.csv")

suit_df <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/results/condensed_suits3.csv")
head(suit_df)

suit_df[is.na(suit_df)] <- "Partial"


# categorical  ------------------------------------------------------------
ts
## dataframes
time_statsx <- NULL
SuitClassOverYearsx <- NULL

## just for ones with prob threshold
ts_con <- ts[c(2,4,5,6,9,14,15,16,19,20)]
ts_cat <- ts[-c(2,4,5,6,9,14,15,16,19,20)]

## create DF of nodes to merge
time_stats <- read.csv(file=paste(ts_cat[1], sep=""))
head(time_stats)
j=1

for (j in 1:length(ts_con)) {
  
  time_stats <- read.csv(file=paste(ts_cat[j], sep=""))
  head(time_stats)
  
  suit_df <- time_stats %>%
    mutate(pos_code = paste(Node, "_", position, sep="")) %>%
    select(pos_code) %>%
    distinct()
  
  time_stats <- time_stats %>%
    filter(TimePeriod !="non_critical", Suitability_Class != "Partial" ) %>%
    select(Node, bottom_type, Species, Life_Stage, Hydraulic, water_year,WYT, TimePeriod, position,
           Suitability_Class) %>%
    mutate(pos_code = paste(Node, "_", position, sep="")) %>%
    distinct()
  
  
  SuitClassOverYears <- time_stats %>%
    group_by(Node, Species, Life_Stage, Hydraulic, TimePeriod, position, pos_code) %>%
    summarise(Overall_Class = tail(names(sort(table(Suitability_Class))), 1))
  names(SuitClassOverYears)
  
  SuitClassOverYearsx <- rbind(SuitClassOverYearsx, SuitClassOverYears)
  
}
SuitClassOverYearsx
## merge together

full_df <- merge(suit_df, SuitClassOverYearsx, by="pos_code", all=T)
head(full_df)

full_df <- full_df %>%
  filter(Node != "F37B", Node != "LA20")


### if one hydraulic varibale per species and per position is low/partial, position is low

full_df <- full_df %>%
  mutate(sp_code = paste(Species, "_", Life_Stage, sep=""))
head(full_df)
unique(full_df$sp_code)
### remove columns and duplicates

filter(full_df, sp_code == "Willow_Adult")

dfad <- full_df %>%
  filter(Life_Stage == "Adult")

dfjuv <- full_df %>%
  filter(Life_Stage != "Adult" & TimePeriod =="critical")
dfad
dfjuv

full_df <- rbind(dfad, dfjuv)
unique(full_df$Node)


## subset by speices, then pos_code, then TimePeriod
pos <- unique(full_df$pos_code)
species <- unique(full_df$sp_code)
TP <- unique(full_df$TimePeriod)
p=1
s=1
t=1
tp_dfx <- NULL
tp_df
species

p
s
t
for(p in 1: length(pos)) {
  
  pos_df <- full_df %>%
    filter(pos_code == pos[p])
  pos_df
  species <- unique(pos_df$sp_code)
  
  for(s in 1:length(species)) {
    
    sp_df <- pos_df %>%
      filter(sp_code == species[s])
    sp_df 
    TP <- unique(sp_df$TimePeriod)
    
    for(t in 1: length(TP)) {
      
      tp_df <- sp_df %>%
        filter(TimePeriod == TP[t])
      
      tp_df
      
      tp_df$Check_Class <- if(length(unique(tp_df$Hydraulic)) == 1) {
        paste(tp_df$Overall_Class)
      } else if (length(unique(tp_df$Overall_Class)) == 1){ 
        paste(tp_df$Overall_Class)[1]
      } else if (length(unique(tp_df$Overall_Class)) > 1) {
        paste("Low")
      }

      
      tp_dfx <- rbind(tp_dfx, tp_df)
      
      
      
    }
  }
}
tp_dfx
tail(tp_dfx)

### if one position is high then node is high
## subset by species, then time period, then node
node <- unique(tp_dfx$Node)
species <- unique(tp_dfx$sp_code)
TP <- unique(tp_dfx$TimePeriod)
p=1
s=1
t=1

node_dfx <- NULL


for(s in 1: length(species)) {
  
  sp_df <- tp_dfx %>%
    filter(sp_code == species[s])
  
  
  TP <- unique(sp_df$TimePeriod)
  
  for(t in 1:length(TP)) {
    
    tp_df <- sp_df %>%
      filter(TimePeriod == TP[t])
    tp_df
    nd <- unique(tp_df$Node)
    nd[p]
    for(p in 1: length(nd)) {
      
      node_df <- tp_df %>%
        filter(Node == nd[p])
      node_df
      
      node_df$Check_Class2 <- if("High" %in%  node_df$Check_Class) {
          paste("High")
        } else if("Partial" %in%  node_df$Check_Class){
          paste("Partial")
        } else {
          paste("Low")
        }
     
      
      
      node_dfx <- rbind(node_dfx, node_df)
      
      
      
    }
  }
}

head(node_dfx)

df <- node_dfx %>%
  select(Node, sp_code, Check_Class2, TimePeriod) %>%
  distinct()

df

write.csv(df, "all_low_high_nodes_2.csv")

### make wide

?pivot_wider

df_wide <- df %>%
  pivot_wider(names_from = sp_code, values_from = Check_Class2) %>%
  select(-Willow_Seedling)

df_wide


## merge with all nodes
time_stats <- read.csv(file=paste(ts_con[1], sep=""))
head(time_stats)

suit_df <- time_stats %>%
  select(Node) %>%
  distinct()

suit_dfx

suit_dfx <- merge(suit_df, df_wide, by="Node", all=T)
# df_wide[is.na(df_wide)] <- "Partial"

suit_dfx <- suit_dfx[order(suit_dfx$TimePeriod),] 


write.csv(suit_dfx, "condensed_suits_2.csv")
suit_dfx[is.na(suit_dfx)] <- "Partial"


suit_df <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/results/condensed_suits3.csv")
head(suit_df)

suit_df[is.na(suit_df)] <- "Partial"

all_df <- cbind(suit_dfx, suit_df)
all_df
names(all_df)

all_df <- all_df[, -c(2,9,10,11)]

write.csv(all_df, "final_suitability3.csv")
