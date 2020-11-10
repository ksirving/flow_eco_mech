#Reza Abdi, rabdi@mines.edu
#Aug., 2020
#Code calculates the thermal metrics from hourly data. 

# Clearing the environment
rm(list=ls(all=TRUE))
setwd("/Users/katieirving/Documents/git/flow_eco_mech")

# Weekely data analysis
output <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/LAR_WaterTemperature/Temp_data_first_half.csv", header=T)
# output2 <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/LAR_WaterTemperature/sample.csv", header=T)
n = 1
dd <- dim(output)
output_mat <- as.matrix(output)
dayNm <- dd[1]/24

head(output)

all_nodes <- colnames(output)[-c(1:2)]
all_nodes

for(n in 1:length(all_nodes)) {

  NodeName <- colnames(output[n+2])
  x <- seq_along(output[,1])
  max_timeseries <- matrix(0,dayNm,1)
  min_timeseries <- matrix(0,dayNm,1)
  xx <- seq_along(max_timeseries[,1])
  weekNm <- round(dayNm/7)
  MWMaxT = matrix(0,weekNm,1)
  MWAT = matrix(0,weekNm,1)
  MWMinT = matrix(0,weekNm,1)
  MinWAT = matrix(0,weekNm,1)
  MinWMinT = matrix(0,weekNm,1)
  
  d_i_dayly <- split(output_mat[,n+2], ceiling(x/24))

  
  for (j in seq(1:dayNm)){
    max_timeseries[j,1] = max(d_i_dayly[[j]])
    min_timeseries[j,1] = min(d_i_dayly[[j]])
  }
  d_i_weekly <- split(max_timeseries[,1], ceiling(xx/7)) ## max weekly values
  d_i_weekly_Mins <- split(min_timeseries[,1], ceiling(xx/7)) ## min weekly temperature
  
  for (i in seq(1:weekNm)){
    MWMaxT[i,1] = max(as.numeric(d_i_weekly[[i]])) ## max weekly max temp
    MWAT[i,1] = mean(as.numeric(d_i_weekly[[i]])) ## mean weekly max temp
    MWMinT[i,1] = min(as.numeric(d_i_weekly[[i]])) ## min weekly max temp
    MinWAT[i,1] = mean(as.numeric(d_i_weekly_Mins[[i]])) ## mean weekly min temp
    MinWMinT[i,1] = min(as.numeric(d_i_weekly_Mins[[i]])) ## min weekly min temp
  }
  
  
  #write.csv(MWMaxT,"MWMaxT.csv", row.names = FALSE)
  #write.csv(MWAT,"MWAT.csv", row.names = FALSE)
  #write.csv(MWMinT,"MWMinT.csv", row.names = FALSE)
  #write.csv(MinWAT,"MinWAT.csv", row.names = FALSE)
  #write.csv(MinWMinT,"MinWMinT.csv", row.names = FALSE)
  
  #Average of everything!
  Sim_Avg_time <- matrix(0,weekNm,5)
  colnames(Sim_Avg_time) <- c("Sim_MWMaxT", "Sim_MWAT", "Sim_MWMinT", "Sim_MinWAT", "Sim_MinWMinT")
  for (i in seq(1:weekNm)){
    Sim_Avg_time[i,1] <- MWMaxT[i,1]
    Sim_Avg_time[i,2] <- MWAT[i,1]
    Sim_Avg_time[i,3] <- MWMinT[i,1]
    Sim_Avg_time[i,4] <- MinWAT[i,1]
    Sim_Avg_time[i,5] <- MinWMinT[i,1]
    # Sim_Avg_time[i,6] <- NodeName
  }
  
  Sim_Avg_time <- as.data.frame(Sim_Avg_time)
  Sim_Avg_time$Node <- NodeName
  
  write.csv(Sim_Avg_time,paste("input_data/LAR_WaterTemperature/Sim_Avg_", NodeName, ".csv", sep=""), row.names = FALSE)
  
}

Sim_Avg_time

