### combine data into each characteristic

#  combine by each characteristic
# 1) substrate
# 2) temperature
# 3) depth
# 4) velocity

# and life stage

# substrate
# adult

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

# upload each data set
# Brown 2005
brown <- read.csv("output_data/00_Brown_2000_abundance_env_vars.csv")
brown
# only relevant columns
brown_sub <- brown[,c(2,5,8,9,11)]
names(brown_sub) [2] <- "substrate"
#  present in only sand/fine_sediment - life stage?? no info in paper
#  cataegories need to be standardised with other data

# saiki 2000 - more to add
saiki <- read.csv("output_data/00_Saiki_2000_substrate_dist.csv")
names(saiki) [2] <- "substrate"
saiki
#  one site does not have gravel, cobble and boulder available but sas convene over sand rather than silt
#   life stage from raw data 
#  SMEA adult

smea_2003 <- read.csv("output_data/00_SMEA_adult_substrate_2003_abundance.csv")
smea_2003 <- smea_2003[-10, c(2, 7:9)]
names(smea_2003) [1] <- "substrate"
smea_2003
#  need to match substrate categories

smea_2004 <- read.csv("output_data/00_SMEA_adult_substrate_2004_abundance.csv")
smea_2004 <- smea_2004[-10, c(2,6,7)]
names(smea_2004) [1] <- "substrate"
smea_2004
#  merge

smea <- merge(smea_2003, smea_2004, by="substrate")
smea

# Thompson
# sites are years and site together and treated as separate sites as have different conditions
thomp <- read.csv("output_data/00_Thompson_all_data_clean.csv")
names(thomp)
thomp <- thomp[, c(2,3, 4, 6, 17,19,21,23)]
thomp$site <- paste(thomp$Year, thomp$Site, sep="")
thomp
#  arcsin transform gravel
# ?sqrt
# sq_sub <- sqrt(thomp$gravel_cobble_m) 
# asin(sq_sub)

sites <- thomp$site

for(x in 1: length (sites)) {
  
  if(thomp$gravel_cobble_m[x] <= 0.2) {
    thomp$substrate [x] <- paste("sand") 
  } else {
    thomp$substrate [x] <- paste("gravel") 
  }
  
}

thomp <- thomp[, c(9,3,4)]
thomp


# WULFF

# has dominant sediment - no methods so do not know thresholds

wulff_2015 <- read.csv("output_data/00_Wulff_2015_substrate_abundance.csv")
head(wulff_2015)

# few sites with many passes

# unique(wulff_2015$coord_code)
# # check NV_NV - nas?
# nv_rows <- which(wulff_2015$coord_code == "NV_NV")
# wulff_2015[nv_rows,] # has substrate and fish info, not coordinates keep in for now

sites <- rownames(wulff_2015)

for(x in 1: length (sites)) {
  
  if(wulff_2015$Dominant.substrate[x] == "CO") {
    wulff_2015$substrate [x] <- paste("cobble") 
    
  } else if (wulff_2015$Dominant.substrate[x] == "GV") {
    wulff_2015$substrate [x] <- paste("gravel") 
    
  } else  {
    wulff_2015$substrate [x] <- paste("sand") 
    
  }
  
}

head(wulff_2015)
wulff_2015 <- wulff_2015[,c(12, 9)]
# wulff_2015$year <- paste("2015")

# 2016

wulff_2016 <- read.csv("output_data/00_Wulff_2016_substrate_abundance.csv")
head(wulff_2016)
unique(wulff_2016$Dominant.Substrate)

sites <- rownames(wulff_2016)

for(x in 1: length (sites)) {
  
  if(wulff_2016$Dominant.Substrate[x] == "CO") {
    wulff_2016$substrate [x] <- paste("cobble") 
    
  } else if (wulff_2016$Dominant.Substrate[x] == "GV") {
    wulff_2016$substrate [x] <- paste("gravel") 
    
  } else if (wulff_2016$Dominant.Substrate[x] == "BO") {
    wulff_2016$substrate [x] <- paste("boulder") 
    
  } else  {
    wulff_2016$substrate [x] <- paste("sand") 
    
  }
  
}

head(wulff_2016)
wulff_2016 <- wulff_2016[,c(14, 10)]

# 2017 

wulff_2017 <- read.csv("output_data/00_Wulff_2017_substrate_abundance.csv")
head(wulff_2017)
unique(wulff_2017$Dominant.Substrate)

sites <- rownames(wulff_2017)

for(x in 1: length (sites)) {
  
  if(wulff_2017$Dominant.Substrate[x] == "CO") {
    wulff_2017$substrate [x] <- paste("cobble") 
    
  # } else if (wulff_2017$Dominant.Substrate[x] == "GV") {
  #   wulff_2017$substrate [x] <- paste("gravel") 
  #   
  # } else if (wulff_2017$Dominant.Substrate[x] == "BO") {
  #   wulff_2017$substrate [x] <- paste("boulder") 
  #   
  } else  {
    wulff_2017$substrate [x] <- paste("gravel") 
    
  }
  
}

head(wulff_2017)
wulff_2017 <- wulff_2017[,c(10, 6)]

#  all adult datasets

wulff_2015
wulff_2016
wulff_2017
thomp
smea
saiki
brown_sub

names(wulff_2015) <- c("substrate", "wulf_15_ab")
names(wulff_2016) <- c("substrate", "wulf_16_ab")
names(wulff_2017) <- c("substrate", "wulf_17_ab")

names(thomp) [3] <- "thomp_ab"
#  do i keep the sites?

smea

smea[3, 2:6 ] <- (smea[3,2:6]+smea[4,2:6])# add gravel_cobble to gravel
smea[5, 2:6 ] <- (smea[5,2:6]+smea[6,2:6])# add sand_cobble to sand
smea[8, 2:6 ] <- (smea[8,2:6]+smea[9,2:6])# add silt_sand to silt

#   combining datasets will not work

#  use smea as a base data - best as habitat utilization. use others 
# to back up assumptions e.g. thompson - proportion gravel = 20%, saiki - sand perferred over silt

#substrate - categorical

#  temperature - range

# depth - relationship
# velocity - relationship



