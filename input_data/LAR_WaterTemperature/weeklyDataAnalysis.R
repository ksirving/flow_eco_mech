#Reza Abdi, rabdi@mines.edu
#Aug., 2020
#Code calculates the thermal metrics from hourly data. 

# Clearing the environment
rm(list=ls(all=TRUE))


# Weekely data analysis
output <- read.csv("sample.csv", header=T)
dd <- dim(output)
output_mat <- as.matrix(output)
dayNm <- dd[1]/24

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

d_i_dayly <- split(output_mat[,3], ceiling(x/24))


for (j in seq(1:dayNm)){
  max_timeseries[j,1] = max(d_i_dayly[[j]])
  min_timeseries[j,1] = min(d_i_dayly[[j]])
}
d_i_weekly <- split(max_timeseries[,1], ceiling(xx/7))
d_i_weekly_Mins <- split(min_timeseries[,1], ceiling(xx/7))

for (i in seq(1:weekNm)){
    MWMaxT[i,1] = max(as.numeric(d_i_weekly[[i]]))
    MWAT[i,1] = mean(as.numeric(d_i_weekly[[i]]))
    MWMinT[i,1] = min(as.numeric(d_i_weekly[[i]]))
    MinWAT[i,1] = mean(as.numeric(d_i_weekly_Mins[[i]]))
    MinWMinT[i,1] = min(as.numeric(d_i_weekly_Mins[[i]]))
}


#write.csv(MWMaxT,"MWMaxT.csv", row.names = FALSE)
#write.csv(MWAT,"MWAT.csv", row.names = FALSE)
#write.csv(MWMinT,"MWMinT.csv", row.names = FALSE)
#write.csv(MinWAT,"MinWAT.csv", row.names = FALSE)
#write.csv(MinWMinT,"MinWMinT.csv", row.names = FALSE)

#Average of everything!
Sim_Avg <- matrix(0,weekNm,5)
colnames(Sim_Avg_time) <- c("Sim_MWMaxT", "Sim_MWAT", "Sim_MWMinT", "Sim_MinWAT", "Sim_MinWMinT")
for (i in seq(1:weekNm)){
  Sim_Avg_time[i,1] <- MWMaxT[i,1]
  Sim_Avg_time[i,2] <- MWAT[i,1]
  Sim_Avg_time[i,3] <- MWMinT[i,1]
  Sim_Avg_time[i,4] <- MinWAT[i,1]
  Sim_Avg_time[i,5] <- MinWMinT[i,1]
}


write.csv(Sim_Avg,"Sim_Avg.csv", row.names = FALSE)

