### flow-ecology - mechanistic species modelling
## Katie Irving

## consolidate data

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

library(tidyverse)

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
  
  
  
