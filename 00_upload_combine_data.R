### flow-ecology - mechanistic species modelling
## Katie Irving

## consolidate data

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

library(tidyverse)
library(magrittr)

# various data sets from literature. fish info, e.g. abundance, with environmental variables in various formats.
# purpose here is to consolidate and standardize
# ideally need a dataset with sites/abundances/presences ~ env variables

# upload data from same source to combine
#  Thompson 2010

# coarse substrate and discharge info vs abundance. 3 x sites 
thomp_dish <- read.table("input_data/Thompson_2010_dis_v_abundance.txt", header=T)
thomp_dish

thomp_sub <- read.table("input_data/Thompson_2010_sub_v_abundancev2.txt", header=T)
thomp_sub


# merge by site and year - abundance should be the same per site and year?? check they are! 
# will need error adjustments for data thief


dis_sub <- merge(thomp_sub, thomp_dish, by=c("site", "year"), all=T)
dis_sub
 # 2 x b 2004 - check figure!

# thomp_dish$s_symbol <- 
 sites <- thomp_dish$site
  
  for(x in 1: length (sites)) {
    
    if(thomp_dish$site[x] == "a") {
      thomp_dish$s_symbol[x] <- paste("circle") 
      
    } else if  (thomp_dish$site[x] == "b") {
      thomp_dish$s_symbol[x] <- paste("square") 
      
    } else {
      thomp_dish$s_symbol[x] <- paste("diamond")
    }
    }
  
 thomp_dish
  years
years <- thomp_dish$year
 
 for(x in 1: length (years)) {
   
   if(thomp_dish$year[x] == "2003") {
     thomp_dish$y_symbol[x] <- paste("solid") 
     
   } else if  (thomp_dish$year[x] == "2004") {
     thomp_dish$y_symbol[x] <- paste("curved_diag") 
     
   } else if (thomp_dish$year[x] == "2005") { 
     thomp_dish$y_symbol[x] <- paste("open")
   
   } else if (thomp_dish$year[x] == "2006") {
     thomp_dish$y_symbol[x] <- paste("horizontal")  
     
   } else if (thomp_dish$year[x] == "2007") {
     thomp_dish$y_symbol[x] <- paste("vertical") 
     
   } else {
     thomp_dish$y_symbol[x] <- paste("diagonal") 
   }
     
     }
 
 
 thomp_dish # row 9 - b 2004 incorrect. change to 2008 (diagonal)
 thomp_dish[9,4] <- 2008
thomp_dish # correct

#  run if else again to change 

years <- thomp_dish$year

for(x in 1: length (years)) {
  
  if(thomp_dish$year[x] == "2003") {
    thomp_dish$y_symbol[x] <- paste("solid") 
    
  } else if  (thomp_dish$year[x] == "2004") {
    thomp_dish$y_symbol[x] <- paste("curved_diag") 
    
  } else if (thomp_dish$year[x] == "2005") { 
    thomp_dish$y_symbol[x] <- paste("open")
    
  } else if (thomp_dish$year[x] == "2006") {
    thomp_dish$y_symbol[x] <- paste("horizontal")  
    
  } else if (thomp_dish$year[x] == "2007") {
    thomp_dish$y_symbol[x] <- paste("vertical") 
    
  } else {
    thomp_dish$y_symbol[x] <- paste("diagonal") 
  }
  
}

thomp_dish

dis_sub <- merge(thomp_sub, thomp_dish, by=c("site", "year"), all=T)
dis_sub

## sites and years match - remove columns
names(dis_sub)
dis_sub <- dis_sub[, c(1,2,3,4, 7, 8)]
dis_sub

# abundance is sq root transformed
# inverse by squaring values

dis_sub$ab_x_sq <- dis_sub$abundance.x^2
dis_sub$ab_y_sq <- dis_sub$abundance.y^2
#  round up
dis_sub$ab_x_sq <- round(dis_sub$ab_x_sq, 2)
dis_sub$ab_y_sq <- round(dis_sub$ab_y_sq, 2)

# match abundances - should be more or less the same
r_ab_x <- round(dis_sub$ab_x_sq)
r_ab_y <-round(dis_sub$ab_y_sq)
#  use rounded figure and mean - most are only 1 or so off
dis_sub$ab_mean <-  round((r_ab_x + r_ab_y)/2, 0) 
dis_sub
 #  clean up/remove unwanted columns

dis_sub <- dis_sub[,c(1,2,4,6,9)]
dis_sub  

write.csv(dis_sub, "output_data/00_thompson_clean.csv")  

dis_sub <- read.csv("output_data/00_thompson_clean.csv")
dis_sub
# upload environmental varaiables from table
env_vars <- read.csv("input_data/Thompson_env_data.csv")
env_vars <- env_vars[-16,] # remove extra row
env_vars$Site <- as.character(env_vars$Site)
env_vars$X <- NULL
env_vars$Year <- as.numeric(as.character(env_vars$Year))
# replace small letters with capitals to match dfs
dis_sub_site <- dis_sub$site

dis_sub_site %<>%
  gsub("a","A", .) %>%
    gsub("b", "B", .) %>%
      gsub("c", "C", .)


dis_sub$site <- dis_sub_site
colnames(dis_sub) [2:3] <- c("Site", "Year")
dis_sub$Year <- as.numeric(as.character(dis_sub$Year))

str(dis_sub)
str(env_vars)
# merge datasets 
all_data <- merge(dis_sub, env_vars, by=c("Year", "Site"), all=T)
all_data$X <- NULL
all_data$method <- paste("electro3")
all_data$scale_m <- paste("100")
write.csv(all_data, "output_data/00_Thompson_all_data_clean.csv")
#################
# upload and clean SMEA data 

#  2002 - 2008 (missing 2006)
#  thompson & SMEA - survey method?? habitat utilization = observations

## upload SMEA data

dep_ad <- read.csv("input_data/SMEA_depth_2003_2004_adult.csv", header=T)
sub_ad <- read.csv("input_data/SMEA_substrate_2003_2004_adult.csv", header=T)
vel_ad <- read.csv("input_data/SMEA_velocity_2003_2004_adult.csv", header =T)


dep_ad

# transform percentage into abundance
#  seapare years

 
dep_ad_03 <- dep_ad[, 1:5]
dep_ad_04 <- dep_ad[,c(1,6:8)]
dep_ad_03

write.csv(dep_ad_03, "output_data/00_SMEA_depth_adult_2003.csv")
write.csv(dep_ad_04, "output_data/00_SMEA_depth_adult_2004.csv")

dep_ad_03$M_Blvd_ab <- round(dep_ad_03$M_Blvd/100*28)
