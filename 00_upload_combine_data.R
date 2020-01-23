### flow-ecology - mechanistic species modelling
## Katie Irving

## consolidate data

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

# various data sets from literature. fish info, e.g. abundance, with environmental variables in various formats.
# purpose here is to consolidate and standardize
# ideally need a dataset with sites/abundances/presences ~ env variables

# upload data from same source to combine
#  Thompson 2010

# coarse substrate and discharge info vs abundance. 3 x sites 
thomp_dish <- read.table("input_data/Thompson_2010_dis_v_abundance.txt", header=T)
thomp_dish

thomp_sub <- read.csv("input_data/Thompson_2010_sub_v_abundance.csv", header=T)
thomp_sub
colnames(thomp_sub)[2] <- "percent_cor_subs"

# merge by site and year - abundance should be the same per site and year?? check they are! 
# will need error adjustments for data thief
