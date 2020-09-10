### figures graveyard

##  plot time series of discharge - subset to one year
new_datax_2016 <- filter(new_dataMx, year==2016)

ggplot(new_datax_2016) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

## plot each season of year - just winter & summer for now

winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months
new_datax_2016_winter <- filter(new_datax_2016, month %in% winter)
new_datax_2016_summer <- filter(new_datax_2016, month %in% summer)
tail(new_datax_2016_summer$DateTime)
# break.vec <- c(as.Date("2016-05-01 00:00:00 PDT"),
#                seq(from=as.Date("2016-05-01 00:00:00 PDT"), to=as.Date("2016-10-31 23:00:00 PDT"), by="month"))
# head(new_datax_2016_summer)

ggplot(new_datax_2016_summer) +
  geom_line(aes(x =month_year, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m") +
  expand_limits(x=min(break.vec))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

ggplot(new_datax_2016_winter) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

## plot each season of year - just winter & summer for now

winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months
new_datax_2016_winter <- filter(new_datax_2016, month %in% winter)
new_datax_2016_summer <- filter(new_datax_2016, month %in% summer)

ggplot(new_datax_2016_summer) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
  geom_hline(yintercept=newx2a, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1b, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

ggplot(new_datax_2016_winter) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newx2b, linetype="dashed", color="red")+
  geom_hline(yintercept=newx1b, linetype="dashed", color="green")+
  geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

## time stats
time_statsl <- new_dataLx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Medium = if(is.na(newx2bL)){
    sum(Q >= newx2aL)/length(DateTime)*100
  } else {
    sum(Q >= newx2aL & Q <= newx2bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low = if(is.na(newx1bL)){
    sum(Q >= newx1aL)/length(DateTime)*100
  } else {
    sum(Q >= newx1aL & Q <= newx1bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(High = if(is.na(newx3bL)){
    sum(Q >= newx3aL)/length(DateTime)*100
  } else {
    sum(Q >= newx3aL & Q <= newx3bL)/length(DateTime)*100
  })  %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Medium.Seasonal = if(is.na(newx2bL)){
    sum(Q >= newx2aL)/length(DateTime)*100
  } else {
    sum(Q >= newx2aL & Q <= newx2bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(Low.Seasonal = if(is.na(newx1bL)){
    sum(Q >= newx1aL)/length(DateTime)*100
  } else {
    sum(Q >= newx1aL & Q <= newx1bL)/length(DateTime)*100
  }) %>%
  dplyr::mutate(High.Seasonal = if(is.na(newx3bL)){
    sum(Q >= newx3aL)/length(DateTime)*100
  } else {
    sum(Q >= newx3aL & Q <= newx3bL)/length(DateTime)*100
  }) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="LOB")

## countinf=g numb er of days
new_dataL <- arrange(new_dataL, date_num)

nas <- ifelse(!is.na(newx1aL) && !is.na(newx1bL), print("Both"), print("one"))

if(nas == "Both") {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1aL & Q <= newx1bL)) %>%
    mutate(Low = if_else(Q >= newx1aL & Q <= newx1bL, row_number(), 0L))
} else if (is.na(newx1aL)) {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1bL)) %>%
    mutate(Low = if_else(Q <= newx1b, row_number(), 0L)) 
} else {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID01 = data.table::rleid(Q >= newx1aL)) %>%
    mutate(Low = if_else(Q >= newx1aL, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx2aL) && !is.na(newx2bL), print("Both"), print("one"))

if(nas == "Both") {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2aL & Q <= newx2bL)) %>%
    mutate(Medium = if_else(Q >= newx2aL & Q <= newx2bL, row_number(), 0L))
} else if (is.na(newx2aL)) {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2bL)) %>%
    mutate(Medium = if_else(Q <= newx2bL, row_number(), 0L)) 
} else {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID02 = data.table::rleid(Q >= newx2aL)) %>%
    mutate(Medium = if_else(Q >= newx2aL, row_number(), 0L)) 
}

nas <- ifelse(!is.na(newx3aL) && !is.na(newx3bL), print("Both"), print("one"))

if(nas == "Both") {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3aL & Q <= newx3bL)) %>%
    mutate(High = if_else(Q >= newx3aL & Q <= newx3bL, row_number(), 0L))
} else if (is.na(newx3aL)) {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3bL)) %>%
    mutate(High = if_else(Q <= newx3bL, row_number(), 0L)) 
} else {
  new_dataL <- new_dataL %>%
    ungroup() %>%
    group_by(month, day, year, ID03 = data.table::rleid(Q >= newx3aL)) %>%
    mutate(High = if_else(Q >= newx2aL, row_number(), 0L)) 
}
head(new_dataL)
names(new_dataL)
new_dataL <- mutate(new_dataL, position="LOB")
