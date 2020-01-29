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
#  adult

dep_ad <- read.csv("input_data/SMEA_depth_2003_2004_adult.csv", header=T)
sub_ad <- read.csv("input_data/SMEA_substrate_2003_2004_adult.csv", header=T)
vel_ad <- read.csv("input_data/SMEA_velocity_2003_2004_adult.csv", header =T)


dep_ad

# transform percentage into abundance
#  seapare years

 
dep_ad_03 <- dep_ad[, 1:5]
dep_ad_04 <- dep_ad[,c(1,6:8)]

install.packages("glue")
library(glue)

#  2003
write.csv(dep_ad_03, "output_data/00_SMEA_depth_adult_2003.csv")
write.csv(dep_ad_04, "output_data/00_SMEA_depth_adult_2004.csv")

dep_ad_03$M_Blvd_ab <- round(dep_ad_03$M_Blvd/100*dep_ad_03[16,2])
dep_ad_03$Highway_60_ab <- round(dep_ad_03$Highway_60/100*dep_ad_03[16,3])
dep_ad_03$Riverside_Dr_ab <- round(dep_ad_03$Riverside_Dr/100*dep_ad_03[16,4])
dep_ad_03$all_sites_ab <- round(dep_ad_03$all_sites/100*dep_ad_03[16,5])

#  change autocorrected date to deoth category and character
dep_ad_03$Depth <- as.character(dep_ad_03$Depth)
dep_ad_03[2,1] <- paste("6-10")
dep_ad_03[3,1] <- paste("11-15")
dep_ad_03

#  2004
dep_ad_04

colnames(dep_ad_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
dep_ad_04$july_Highway60_ab <- round(dep_ad_04$july_Highway60/100*dep_ad_04[16,2])
dep_ad_04$Aug_Highway60_Mission_ab <- round(dep_ad_04$Aug_Highway60_Mission/100*dep_ad_04[16,3])
dep_ad_04$total <- round(dep_ad_04$total/100*dep_ad_04[16,3])

dep_ad_04$Depth <- as.character(dep_ad_04$Depth)
dep_ad_04[2,1] <- paste("6-10")
dep_ad_04[3,1] <- paste("11-15")

# save 
write.csv(dep_ad_03, "output_data/00_SMEA_adult_depth_2003_abundance.csv")
write.csv(dep_ad_04, "output_data/00_SMEA_adult_depth_2004_abundance.csv")


####################
# transform percentage into abundance
#  seaparate years
#  substrate


sub_ad_03 <- sub_ad[, 1:5]
sub_ad_04 <- sub_ad[,c(1,6:8)]
sub_ad_03

write.csv(sub_ad_03, "output_data/00_SMEA_substrate_adult_2003.csv")
write.csv(sub_ad_04, "output_data/00_SMEA_substrate_adult_2004.csv")

# 2003
sub_ad_03$M_Blvd_ab <- round(sub_ad_03$M_Blvd/100*sub_ad_03[10,2])
sub_ad_03$Highway_60_ab <- round(sub_ad_03$Highway_60/100*sub_ad_03[10,3])
sub_ad_03$Riverside_Dr_ab <- round(sub_ad_03$Riverside_Dr/100*sub_ad_03[10,4])
sub_ad_03$all_sites_ab <- round(sub_ad_03$all_sites/100*sub_ad_03[10,5])
sub_ad_03

#  2004
sub_ad_04

colnames(sub_ad_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
sub_ad_04$july_Highway60_ab <- round(sub_ad_04$july_Highway60/100*sub_ad_04[10,2])
sub_ad_04$Aug_Highway60_Mission_ab <- round(sub_ad_04$Aug_Highway60_Mission/100*sub_ad_04[10,3])
sub_ad_04$total <- round(sub_ad_04$total/100*sub_ad_04[10,3])

# save 
write.csv(sub_ad_03, "output_data/00_SMEA_adult_substrate_2003_abundance.csv")
write.csv(sub_ad_04, "output_data/00_SMEA_adult_substrate_2004_abundance.csv")

########
## velocity

vel_ad_03 <- vel_ad[, 1:5]
vel_ad_04 <- vel_ad[,c(1,6:8)]
vel_ad_03

write.csv(vel_ad_03, "output_data/00_SMEA_velocity_adult_2003.csv")
write.csv(vel_ad_04, "output_data/00_SMEA_velocity_adult_2004.csv")

# 2003
vel_ad_03$M_Blvd_ab <- round(vel_ad_03$M_Blvd/100*vel_ad_03[7,2])
vel_ad_03$Highway_60_ab <- round(vel_ad_03$Highway_60/100*vel_ad_03[7,3])
vel_ad_03$Riverside_Dr_ab <- round(vel_ad_03$Riverside_Dr/100*vel_ad_03[7,4])
vel_ad_03$all_sites_ab <- round(vel_ad_03$all_sites/100*vel_ad_03[7,5])
vel_ad_03

#  2004
vel_ad_04

colnames(vel_ad_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
vel_ad_04$july_Highway60_ab <- round(vel_ad_04$july_Highway60/100*vel_ad_04[7,2])
vel_ad_04$Aug_Highway60_Mission_ab <- round(vel_ad_04$Aug_Highway60_Mission/100*vel_ad_04[7,3])
vel_ad_04$total <- round(vel_ad_04$total/100*vel_ad_04[7,3])

# save 
write.csv(vel_ad_03, "output_data/00_SMEA_adult_velocity_2003_abundance.csv")
write.csv(vel_ad_04, "output_data/00_SMEA_adult_velocity_2004_abundance.csv")


##################
##### juvenile

dep_juv <- read.csv("input_data/SMEA_depth_2003_2004_juvenile.csv", header=T)
sub_juv <- read.csv("input_data/SMEA_substrate_2003_2004_juvenile.csv", header=T)
vel_juv <- read.csv("input_data/SMEA_velocity_2003_2004_juvenile.csv", header =T)


dep_juv

# transform percentage into abundance
#  seapare years


dep_juv_03 <- dep_juv[, 1:5]
dep_juv_04 <- dep_juv[,c(1,6:8)]

install.packages("glue")
library(glue)

#  2003
write.csv(dep_juv_03, "output_data/00_SMEA_depth_juvenile_2003.csv")
write.csv(dep_juv_04, "output_data/00_SMEA_depth_juvenile_2004.csv")

dep_juv_03$M_Blvd_ab <- round(dep_juv_03$M_Blvd/100*dep_juv_03[15,2])
dep_juv_03$Highway_60_ab <- round(dep_juv_03$Highway_60/100*dep_juv_03[15,3])
dep_juv_03$Riverside_Dr_ab <- round(dep_juv_03$Riverside_Dr/100*dep_juv_03[15,4])
dep_juv_03$all_sites_ab <- round(dep_juv_03$all_sites/100*dep_juv_03[15,5])

#  change autocorrected date to deoth category and character
dep_juv_03$Depth <- as.character(dep_juv_03$Depth)
dep_juv_03[2,1] <- paste("6-10")
dep_juv_03[3,1] <- paste("11-15")
dep_juv_03

#  2004
dep_juv_04

colnames(dep_juv_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
dep_juv_04$july_Highway60_ab <- round(dep_juv_04$july_Highway60/100*dep_juv_04[15,2])
dep_juv_04$Aug_Highway60_Mission_ab <- round(dep_juv_04$Aug_Highway60_Mission/100*dep_juv_04[16,3])
dep_juv_04$total <- round(dep_juv_04$total/100*dep_juv_04[16,3])

dep_juv_04$Depth <- as.character(dep_juv_04$Depth)
dep_juv_04[2,1] <- paste("6-10")
dep_juv_04[3,1] <- paste("11-15")

# save 
write.csv(dep_juv_03, "output_data/00_SMEA_juvenile_depth_2003_abundance.csv")
write.csv(dep_juv_04, "output_data/00_SMEA_juvenile_depth_2004_abundance.csv")


####################
# transform percentage into abundance
#  seaparate years
#  substrate


sub_juv_03 <- sub_juv[, 1:5]
sub_juv_04 <- sub_juv[,c(1,6:8)]
sub_juv_03

write.csv(sub_juv_03, "output_data/00_SMEA_substrate_juvenile_2003.csv")
write.csv(sub_juv_04, "output_data/00_SMEA_substrate_juvenile_2004.csv")

# 2003
sub_juv_03$M_Blvd_ab <- round(sub_juv_03$M_Blvd/100*sub_juv_03[10,2])
sub_juv_03$Highway_60_ab <- round(sub_juv_03$Highway_60/100*sub_juv_03[10,3])
sub_juv_03$Riverside_Dr_ab <- round(sub_juv_03$Riverside_Dr/100*sub_juv_03[10,4])
sub_juv_03$all_sites_ab <- round(sub_juv_03$all_sites/100*sub_juv_03[10,5])
sub_juv_03

#  2004
sub_juv_04

colnames(sub_juv_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
sub_juv_04$july_Highway60_ab <- round(sub_juv_04$july_Highway60/100*sub_juv_04[10,2])
sub_juv_04$Aug_Highway60_Mission_ab <- round(sub_juv_04$Aug_Highway60_Mission/100*sub_juv_04[10,3])
sub_juv_04$total <- round(sub_juv_04$total/100*sub_juv_04[10,3])

# save 
write.csv(sub_juv_03, "output_data/00_SMEA_juvenile_substrate_2003_abundance.csv")
write.csv(sub_juv_04, "output_data/00_SMEA_juvenile_substrate_2004_abundance.csv")

########
## velocity

vel_juv_03 <- vel_juv[, 1:5]
vel_juv_04 <- vel_juv[,c(1,6:8)]
vel_juv_03

write.csv(vel_juv_03, "output_data/00_SMEA_velocity_juvenile_2003.csv")
write.csv(vel_juv_04, "output_data/00_SMEA_velocity_juvenile_2004.csv")

# 2003
vel_juv_03$M_Blvd_ab <- round(vel_juv_03$M_Blvd/100*vel_juv_03[7,2])
vel_juv_03$Highway_60_ab <- round(vel_juv_03$Highway_60/100*vel_juv_03[7,3])
vel_juv_03$Riverside_Dr_ab <- round(vel_juv_03$Riverside_Dr/100*vel_juv_03[7,4])
vel_juv_03$all_sites_ab <- round(vel_juv_03$all_sites/100*vel_juv_03[7,5])
vel_juv_03

#  2004
vel_juv_04

colnames(vel_juv_04) [2:3] <- c("july_Highway60", "Aug_Highway60_Mission")
vel_juv_04$july_Highway60_ab <- round(vel_juv_04$july_Highway60/100*vel_juv_04[7,2])
vel_juv_04$Aug_Highway60_Mission_ab <- round(vel_juv_04$Aug_Highway60_Mission/100*vel_juv_04[7,3])
vel_juv_04$total <- round(vel_juv_04$total/100*vel_juv_04[7,3])

# save 
write.csv(vel_juv_03, "output_data/00_SMEA_juvenile_velocity_2003_abundance.csv")
write.csv(vel_juv_04, "output_data/00_SMEA_juvenile_velocity_2004_abundance.csv")

# all will need last row removed - abundance.

#  SAS data
#  upload - cobble and sand data only
cobble <- read.csv("input_data/SAS_2014_abundance_v_cobble_correct_percent.csv")
cobble
sand <- read.csv("input_data/SAS_2014_abundance_v_sandsilt_percent.csv")
sand
#  round abundance to nearest whole number - data thief error
cobble$Abundance_r <- round(cobble$abundance)
sand$Abundance_r <- round(sand$Abundance)

#  they should match - they do!!
cobble$Abundance_r %in% sand$Abundance_r
cobble

# add reach number

reach <- read.csv("input_data/SAS_2014_abundance_v_reach_number.csv")
reach

reach_no_z <- subset(reach, !abundance ==0)
reach_no_z

cobble_no_z <- subset(cobble, !Abundance_r ==0)
sand_no_z <- subset(sand, !Abundance_r ==0)
cobble_no_z
sand_no_z

substrate <- merge(sand_no_z, cobble_no_z, by="Abundance_r")
substrate

#  remove old abundance columns
substrate <- substrate[,-c(2,5)]
substrate <- substrate[!duplicated(substrate),]

#  add reach number

reach_sub <- merge(reach_no_z, substrate, by.x="abundance", by.y="Abundance_r")
reach_sub

#  same with remaining zeros - reach will not match data in report but no other way of getting reach number
reach_no_z <- subset(reach, abundance ==0)
reach_no_z

cobble_no_z <- subset(cobble, Abundance_r ==0)
sand_no_z <- subset(sand, Abundance_r ==0)
cobble_no_z
sand_no_z

substrate <- merge(sand_no_z, cobble_no_z, by="Abundance_r")
substrate

#  remove old abundance columns
substrate <- substrate[,-c(2,5)]
substrate <- substrate[!duplicated(substrate),]

#  add reach number

reach_sub <- merge(reach_no_z, substrate, by.x="abundance", by.y="Abundance_r")
reach_sub


