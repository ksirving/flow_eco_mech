## algorithm for substrate

#  adult
#  substrate

setwd("/Users/katieirving/Documents/git/flow_eco_mech")

# upload habitat

smea <- read.csv("output_data/00_smea_total_percentage.csv")
smea

# gravel == 1
# sand_gravel == 2
# gravel_cobble == 3
# sand == 4 
# boulder == 5
# cobble == 6
# silt == 7

# the last 2 are bad, cobble not as bad because with gravel is ok. any presence of silt is bad
# no info on concrete, but assume that 90% concrete is bad
# sand cobble & silt cobble mix is also bad


# use data from liesl

#upload and clean substrate data
lar_data <- read.csv("input_data/LAR_sites_PHab_metrics_env.csv")
head(lar_data)
unique(lar_data$grouptype)

# subset into only 
lar_substrate <- subset(lar_data, grouptype == "Substrate Size and Composition" )
dim(lar_substrate) #3200

head(lar_substrate)
unique(lar_substrate$variablename)
unique(lar_substrate$sampledate) # 2009 - 2019
unique(lar_substrate$LAR_sites) # 52 sites



concrete <- subset(lar_substrate, variablename == "Percent Concrete/Asphalt")
head(concrete)
concrete$result

gravel <- subset(lar_substrate, variablename ==c("Percent Gravel - fine","Percent Gravel - coarse") )
head(gravel)

gravel$result # 0-60%

test_site <- subset(lar_substrate, LAR_sites=="412CE0232")
test_site # sites have seeral years of data. value for every year


# adults

# assign scoring system to the substrate present - 

# if the substrate is 90% concrete = 0
# if the substrate is gravel (find and coarse)  less than 20% = 0, 20% = 7, 40% = 8, 60%+ = 9
# if the substrate is sand 20% = 20% = 3, 40% = 4, 60%+ = 5
# boulder 20% = 20% = 1, 40% = 2, 60%+ = 3
# silt = 0 #Percent Substrate Smaller than Sand (<2 mm)
# concrete = 0

# if the substrate is 

# gravel == 1 - 5
# sand_gravel == 2 - 5
# gravel_cobble == 3 - 3
# sand == 4 - 3
# boulder == 5 -1 
# cobble == 6 - 1
# silt == 7 - 0 
# concrete == 0

# the last 2 are bad, cobble not as bad because with gravel is ok. any presence of silt is bad
# no info on concrete, but assume that 90% concrete is bad
# sand cobble & silt cobble mix is also bad

date <- unique(lar_substrate$sampledate)
sites <- unique(lar_substrate$LAR_sites)

x=1
sites[x]


