### combine all results and condense

library(tidyverse)
library(tidyr)

## upload data

setwd("/Users/katieirving/Documents/git/flow_eco_mech/results")

## time stats
ts <- list.files(pattern="time_stats")
length(ts) ## 17

ts
## dataframes
time_statsx <- NULL
SuitClassOverYearsx <- NULL
# 
# ## probs needed
# probs <- c("Low", "Low.Seasonal", "High", "High.Seasonal")
# probs

# ## just for ones with prob threshold
# ts_con <- ts[c(1,3,4,7,8,13,14,15,18,19)]
# ts_cat <- ts[-c(1,3,4,7,8,13,14,15,18,19)]
# ts_con
# ts_cat
## create DF of nodes to merge
time_stats <- read.csv(file=paste(ts[1], sep=""))
head(time_stats)

suit_df <- time_stats %>%
  mutate(pos_code = paste(Node, "_", position, sep="")) %>%
  select(pos_code) %>%
  distinct()

suit_df
j=1
j
for (j in 1:length(ts)) {
  
  time_stats <- read.csv(file=paste(ts[j], sep=""))
  head(time_stats)
  
  suit_df <- time_stats %>%
    mutate(pos_code = paste(Node, "_", position, sep="")) %>%
    select(pos_code) %>%
    distinct()
  
  time_stats <- time_stats %>%
    select(Node, bottom_type, Species, Life_Stage, Hydraulic, water_year, TimePeriod, position,
            Suitability_Class) %>%
    mutate(pos_code = paste(Node, "_", position, sep="")) %>%
  distinct()


  SuitClassOverYears <- time_stats %>%
    group_by(Node, Species, Life_Stage, Hydraulic, TimePeriod, position, pos_code) %>%
    summarise(Overall_Class = tail(names(sort(table(Suitability_Class))), 1))

  SuitClassOverYearsx <- rbind(SuitClassOverYearsx, SuitClassOverYears)
  
}
j
SuitClassOverYearsx
## merge together

full_df <- merge(suit_df, SuitClassOverYearsx, by="pos_code", all=T)
head(full_df)

write.csv(full_df, "Overall_Class_per_node_species_position.csv")

### if one hydraulic varibale per species and per position is low/partial, position is low

full_df <- full_df %>%
  mutate(sp_code = paste(Species, "_", Life_Stage, sep="")) %>%
  mutate(ClassOrder = Overall_Class) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "High", 3))


## subset by speices, then pos_code, then hydraulic
## rule: if one variable is low, then node is low
pos <- unique(full_df$pos_code)

sp_dfx <- NULL
sp_df


for(p in 1: length(pos)) {
  
  ## filters to node and postion
  pos_df <- full_df %>%
    filter(pos_code == pos[p])

  ## define species $ lifstage at node/position
  species <- unique(pos_df$sp_code)

  for(s in 1:length(species)) {
    ## filter to 1 species lifestage
    sp_df <- pos_df %>%
      filter(sp_code == species[s])


   if (length(unique(sp_df$Overall_Class)) >1) {
     ## if more than 1 suitbaility class then take the lowest one
    MinClass <- filter(sp_df, ClassOrder ==min(sp_df$ClassOrder))
    sp_df$Check_Class <- MinClass$Overall_Class[1]
      ## if only 1 class then take that class
    } else if (length(unique(sp_df$Overall_Class)) == 1){
      sp_df$Check_Class <-  paste(sp_df$Overall_Class[1])
  }
    
    sp_dfx <- rbind(sp_dfx, sp_df)
    

  }
}
sp_dfx ## datsa frame has overall class per node, position, species, life stage and hydraulic variable
names(sp_dfx)
write.csv(sp_dfx, "Overall_class_with_each_hydraulic.csv")

hp_dfx <- sp_dfx %>%
  select(Node, Species, Life_Stage, position, sp_code, Check_Class) %>%
  distinct()

unique(hp_dfx$sp_code)

write.csv(hp_dfx, "Overall_class_per_node_all_species.csv")

### if one position is high then node is high
## subset by species, then time period, then node
hp_dfx <- hp_dfx %>%
  mutate(ClassOrder = Check_Class) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "High", 3))

node <- unique(hp_dfx$Node)
species <- unique(hp_dfx$sp_code)
node


node_dfx <- NULL
s
p

for(s in 1: length(species)) {
  
  sp_df <- hp_dfx %>%
    filter(sp_code == species[s])

  
  nd <- unique(sp_df$Node)

    for(p in 1: length(nd)) {
      
      node_df <- sp_df %>%
        filter(Node == nd[p])

      
      if(length(unique(node_df$Check_Class)) == 1) {
         node_df$Node_Class <- node_df$Check_Class
      } else if (length(unique(node_df$Check_Class)) > 1){
        ## if more than 1 suitbaility class then take the highest one
        MaxClass <- filter(node_df, ClassOrder ==max(node_df$ClassOrder))
        node_df$Node_Class <- MaxClass$Check_Class[1]
      }
        
      node_dfx <- rbind(node_dfx, node_df)

    }
  }


head(node_dfx)

df <- node_dfx %>%
  select(Node, Species, Life_Stage, sp_code, Node_Class) %>%
  distinct()

df

write.csv(df, "All_Species_suit_class_Per_Node.csv")

### make wide
df_wide

df_wide <- df %>%
  pivot_wider(id_cols = Node, names_from = sp_code, values_from = Node_Class)

write.csv(df_wide, "All_species_suit_class_wide.csv")

